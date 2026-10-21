/* REXX *************************************************************/
/* USAGEGET - FETCH THE CLUSTER USAGE OF ONE DAY FROM               */
/*            VICTORIAMETRICS (PROMQL OVER HTTP)                     */
/*                                                                  */
/* ARG     : DATE=YYYY-MM-DD  USAGE DATE (DEFAULT: YESTERDAY)       */
/*           HOST=A.B.C.D     VICTORIAMETRICS (DEFAULT: 10.20.1.20) */
/* DD USAGE: 80-BYTE USAGE RECORDS, ONE PER CLASS AND NAME          */
/*           COLS  1-5  CLASS (K8S / HOST)                          */
/*           COLS  6-29 NAME                                        */
/*           COLS 30-65 CPU-H, MEM-GBH, NET-GB, STOR-GB, 9(7)V99    */
/* DD CTLCARD: USAGE DATE AND RUN DATE                              */
/* RC      : 0 OK, 8 ERROR                                          */
/*                                                                  */
/* NOTES                                                            */
/*  - THE REQUEST IS SENT IN ASCII (E2A) AND THE RESPONSE IS READ   */
/*    BACK IN EBCDIC (A2E).                                         */
/*  - SOME CHARACTERS SUCH AS [ ] DO NOT SURVIVE E2A, SO URLENC     */
/*    PERCENT-ENCODES THEM WITH ITS OWN TABLE.                      */
/********************************************************************/
NUMERIC DIGITS 15
UDATE = ''
VMHOST = '10.20.1.20'
DO W = 1 TO WORDS(ARG(1))
  PARSE VALUE WORD(ARG(1), W) WITH KW '=' KV
  SELECT
    WHEN TRANSLATE(KW) = 'DATE' THEN UDATE = KV
    WHEN TRANSLATE(KW) = 'HOST' THEN VMHOST = KV
    OTHERWISE DO
      SAY 'UNKNOWN ARGUMENT:' WORD(ARG(1), W)
      EXIT 8
    END
  END
END
VMPORT = 8427
CRLF = '0D25'X
CRLF2 = CRLF || CRLF
GIB = 1073741824

TODAY = DAYS(DATE('S'))
IF UDATE = '' THEN UDAY = TODAY - 1
ELSE UDAY = DAYS(SPACE(TRANSLATE(UDATE, ' ', '-'), 0))
/* EVALUATE AT 00:00 JST OF THE NEXT DAY, SO THAT THE [1d] WINDOWS  */
/* COVER THE USAGE DATE FROM 00:00 TO 24:00 JST                     */
EVALTS = (UDAY + 1) * 86400 - 9 * 3600
SAY 'USAGE DATE' CIVIL(UDAY) 'EVALUATED AT' EVALTS

/* ----------------------------------------------------------------- */
/* QUERIES: 1 CPU-H, 2 MEM-GBH, 3 NET-GB, 4 STOR-GB                  */
/* NOTE: A STRING DIRECTLY FOLLOWED BY X OR B IS A HEX/BINARY       */
/*       LITERAL IN REXX ('{'X), SO THE SELECTOR IS NAMED SEL.      */
/* ----------------------------------------------------------------- */
LBL.1 = 'namespace'
Q.1.1 = 'sum by (namespace) (increase(',
     || 'container_cpu_usage_seconds_total{container!=""}[1d])) / 3600'
Q.1.2 = 'sum by (namespace) (avg_over_time(',
     || 'container_memory_working_set_bytes{container!=""}[1d]))',
     || ' /' GIB '* 24'
Q.1.3 = 'sum by (namespace) (',
     || 'increase(container_network_receive_bytes_total',
     || '{namespace!=""}[1d]) + ',
     || 'increase(container_network_transmit_bytes_total',
     || '{namespace!=""}[1d])) / 1e9'
Q.1.4 = 'sum by (namespace) (',
     || 'kube_persistentvolumeclaim_resource_requests_storage_bytes)',
     || ' /' GIB

SEL = 'job="external-nodes"'
J = ' * on(instance) group_left(nodename) node_uname_info{'SEL'}'
LBL.2 = 'nodename'
Q.2.1 = 'sum by (nodename) (increase(',
     || 'node_cpu_seconds_total{'SEL',mode!="idle"}[1d])' J ') / 3600'
Q.2.2 = 'sum by (nodename) ((',
     || 'avg_over_time(node_memory_MemTotal_bytes{'SEL'}[1d]) - ',
     || 'avg_over_time(node_memory_MemAvailable_bytes{'SEL'}[1d]))',
     || J ') /' GIB '* 24'
Q.2.3 = 'sum by (nodename) ((',
     || 'increase(node_network_receive_bytes_total',
     || '{'SEL',device!="lo"}[1d]) + ',
     || 'increase(node_network_transmit_bytes_total',
     || '{'SEL',device!="lo"}[1d]))' J ') / 1e9'
Q.2.4 = 'sum by (nodename) ((',
     || 'node_filesystem_size_bytes{'SEL',mountpoint="/"} - ',
     || 'node_filesystem_avail_bytes{'SEL',mountpoint="/"})',
     || J ') /' GIB
CLASS.1 = 'K8S'
CLASS.2 = 'HOST'

/* ----------------------------------------------------------------- */
/* FETCH                                                             */
/* ----------------------------------------------------------------- */
CALL TCPINIT
NKEYS = 0
SEEN. = 0                              /* NAMES ALREADY COLLECTED  */
VAL. = 0                               /* VALUES DEFAULT TO ZERO   */
DO C = 1 TO 2
  DO I = 1 TO 4
    BODY = QUERY(Q.C.I)
    IF BODY = '' THEN DO
      CALL TCPTERM
      EXIT 8
    END
    CALL COLLECT
  END
END
CALL TCPTERM

IF NKEYS = 0 THEN DO
  SAY 'NO USAGE DATA'
  EXIT 8
END

/* ----------------------------------------------------------------- */
/* WRITE THE RECORDS                                                 */
/* ----------------------------------------------------------------- */
DO K = 1 TO NKEYS
  KEY = KEYS.K
  PARSE VAR KEY CL '/' NAME
  NUMS = ''
  DO I = 1 TO 4
    V = FORMAT(MAX(VAL.KEY.I, 0) * 100, , 0)
    NUMS = NUMS || RIGHT(MIN(V, 999999999), 9, '0')
  END
  OUT.K = LEFT(LEFT(CL, 5) || LEFT(TRANSLATE(NAME), 24) || NUMS, 80)
END
OUT.0 = NKEYS
"EXECIO * DISKW USAGE (STEM OUT. FINIS"
IF RC <> 0 THEN EXIT 8

CTL.1 = LEFT(CIVIL(UDAY) CIVIL(TODAY), 80)
CTL.0 = 1
"EXECIO * DISKW CTLCARD (STEM CTL. FINIS"
IF RC <> 0 THEN EXIT 8

SAY NKEYS 'RECORDS WRITTEN'
EXIT 0

/* ----------------------------------------------------------------- */
/* QUERY: RUN ONE PROMQL QUERY AND RETURN THE JSON BODY ('' ON ERROR)*/
/* ----------------------------------------------------------------- */
QUERY:
  PARSE ARG PROMQL
  REQ = 'GET /api/v1/query?time='EVALTS'&query='URLENC(PROMQL),
     || ' HTTP/1.0' || CRLF || 'Host: 'VMHOST || CRLF || CRLF
  IF TCPOPEN(VMHOST, VMPORT, 10) < 0 THEN DO
    SAY 'TCPOPEN FAILED:' VMHOST VMPORT
    RETURN ''
  END
  CALL TCPSEND _FD, E2A(REQ), 10
  RESP = ''
  DO FOREVER
    IF TCPRECEIVE(_FD, 30) <= 0 THEN LEAVE
    RESP = RESP || _DATA
  END
  PARSE VALUE A2E(RESP) WITH HEAD (CRLF2) RBODY
  PARSE VAR HEAD HEAD (CRLF) .
  IF POS('"status":"success"', RBODY) = 0 THEN DO
    SAY 'QUERY FAILED:' HEAD
    SAY SUBSTR(RBODY, 1, 200)
    RETURN ''
  END
  RETURN RBODY

/* ----------------------------------------------------------------- */
/* COLLECT: PICK (NAME, VALUE) PAIRS OF CLASS C, COLUMN I FROM BODY  */
/* ----------------------------------------------------------------- */
COLLECT:
  REST = BODY
  LABEL = '"'LBL.C'":"'
  DO FOREVER
    P = POS('"metric":', REST)
    IF P = 0 THEN LEAVE
    REST = SUBSTR(REST, P + 9)
    PARSE VAR REST SEG '"value":' REST
    PARSE VAR REST . ',"' NUM '"' REST
    P = POS(LABEL, SEG)
    IF P = 0 THEN ITERATE
    PARSE VALUE SUBSTR(SEG, P + LENGTH(LABEL)) WITH NAME '"' .
    IF NAME = '' THEN ITERATE
    KEY = CLASS.C'/'NAME
    IF SEEN.KEY = 0 THEN DO
      SEEN.KEY = 1
      NKEYS = NKEYS + 1
      KEYS.NKEYS = KEY
    END
    IF DATATYPE(NUM, 'N') = 0 THEN NUM = 0
    VAL.KEY.I = NUM
  END
  RETURN

/* ----------------------------------------------------------------- */
/* URLENC: PERCENT-ENCODE A QUERY STRING                             */
/* ----------------------------------------------------------------- */
URLENC: PROCEDURE
  PARSE ARG S
  SAFE = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
      || 'abcdefghijklmnopqrstuvwxyz0123456789._-'
  SPC = ' ()[]{}!="/*+,<>~|^:'
  HEX = '2028295B5D7B7D213D222F2A2B2C3C3E7E7C5E3A'
  R = ''
  DO I = 1 TO LENGTH(S)
    CH = SUBSTR(S, I, 1)
    IF POS(CH, SAFE) > 0 THEN R = R || CH
    ELSE DO
      P = POS(CH, SPC)
      IF P = 0 THEN DO
        SAY 'URLENC: UNSUPPORTED CHARACTER' C2X(CH)
        EXIT 8
      END
      R = R || '%' || SUBSTR(HEX, 2 * P - 1, 2)
    END
  END
  RETURN R

/* ----------------------------------------------------------------- */
/* DAYS : YYYYMMDD -> DAYS SINCE 1970-01-01                          */
/* CIVIL: DAYS SINCE 1970-01-01 -> YYYY-MM-DD                        */
/* ----------------------------------------------------------------- */
DAYS: PROCEDURE
  PARSE ARG Y +4 M +2 D +2
  IF M <= 2 THEN Y = Y - 1
  ERA = Y % 400
  YOE = Y - ERA * 400
  DOY = (153 * ((M + 9) // 12) + 2) % 5 + D - 1
  DOE = YOE * 365 + YOE % 4 - YOE % 100 + DOY
  RETURN ERA * 146097 + DOE - 719468

CIVIL: PROCEDURE
  Z = ARG(1) + 719468
  ERA = Z % 146097
  DOE = Z - ERA * 146097
  YOE = (DOE - DOE % 1460 + DOE % 36524 - DOE % 146096) % 365
  DOY = DOE - (365 * YOE + YOE % 4 - YOE % 100)
  MP = (5 * DOY + 2) % 153
  D = DOY - (153 * MP + 2) % 5 + 1
  IF MP < 10 THEN M = MP + 3
  ELSE M = MP - 9
  Y = YOE + ERA * 400
  IF M <= 2 THEN Y = Y + 1
  RETURN Y'-'RIGHT(M, 2, '0')'-'RIGHT(D, 2, '0')
