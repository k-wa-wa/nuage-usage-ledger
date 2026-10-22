/* REXX *************************************************************/
/* USAGEGET - FETCH THE CLUSTER USAGE FROM VICTORIAMETRICS           */
/*            (PROMQL OVER HTTP)                                    */
/*                                                                  */
/* ARG : MODE=REPORT    ONE DAY: DATE= OR YESTERDAY (DEFAULT)       */
/*       MODE=LEDGER    FROM THE NEXT BUSINESS DATE IN DD BIZDATE   */
/*                      TO YESTERDAY. IF BIZDATE IS EMPTY (FIRST    */
/*                      RUN), FROM START= OR YESTERDAY.             */
/*       DATE=YYYY-MM-DD   USAGE DATE (MODE=REPORT)                 */
/*       START=YYYY-MM-DD  FIRST BUSINESS DATE (MODE=LEDGER)        */
/*       HOST=A.B.C.D      VICTORIAMETRICS (DEFAULT: 10.20.1.20)    */
/*                                                                  */
/* DD USAGE  : 80-BYTE USAGE RECORDS, ONE PER DATE, CLASS AND NAME  */
/*             COLS  1-8  DATE YYYYMMDD                             */
/*             COLS  9-13 CLASS (K8S / HOST)                        */
/*             COLS 14-37 NAME                                      */
/*             COLS 38-73 CPU-H, MEM-GBH, NET-GB, STOR-GB 9(7)V99   */
/* DD CTLCARD: RUN RECORD, THEN ONE DAY RECORD PER DATE             */
/*             RUN YYYY-MM-DD(RUN) YYYY-MM-DD(FROM) YYYY-MM-DD(TO)  */
/*                 YYYYMMDD(NEXT BUSINESS DATE)                     */
/*             DAY YYYYMMDD M     (M: MONDAY)                       */
/* DD BIZDATE: (MODE=LEDGER) COLS 1-8 NEXT BUSINESS DATE YYYYMMDD   */
/* RC  : 0 OK, 4 NOTHING TO DO (LEDGER IS UP TO DATE), 8 ERROR      */
/*                                                                  */
/* NOTES                                                            */
/*  - THE REQUEST IS SENT IN ASCII (E2A) AND THE RESPONSE IS READ   */
/*    BACK IN EBCDIC (A2E).                                         */
/*  - SOME CHARACTERS SUCH AS [ ] DO NOT SURVIVE E2A, SO URLENC     */
/*    PERCENT-ENCODES THEM WITH ITS OWN TABLE.                      */
/*  - ALL QUERIES GO OVER ONE CONNECTION (HTTP/1.1 KEEP-ALIVE).     */
/*    SOCKET NUMBERS ARE NOT REUSED EVEN AFTER TCPCLOSE, AND BREXX  */
/*    FAILS ONCE THEY REACH 64 (SELECT). THE NUMBERS ARE RESET ONLY */
/*    BY RESTARTING HERCULES (THE WEEKLY IPL OF NUAGE-CLUSTER).     */
/********************************************************************/
NUMERIC DIGITS 15
MODE = 'REPORT'
UDATE = ''
START = ''
VMHOST = '10.20.1.20'
DO W = 1 TO WORDS(ARG(1))
  PARSE VALUE WORD(ARG(1), W) WITH KW '=' KV
  SELECT
    WHEN TRANSLATE(KW) = 'MODE' THEN MODE = TRANSLATE(KV)
    WHEN TRANSLATE(KW) = 'DATE' THEN UDATE = KV
    WHEN TRANSLATE(KW) = 'START' THEN START = KV
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

/* ----------------------------------------------------------------- */
/* DATES (DAYS SINCE 1970-01-01, LOCAL TIME = JST)                   */
/* ----------------------------------------------------------------- */
TODAY = DAYS(DATE('S'))
LASTDAY = TODAY - 1
IF MODE = 'REPORT' THEN DO
  IF UDATE = '' THEN FIRSTDAY = LASTDAY
  ELSE FIRSTDAY = DAYS(YMD(UDATE))
  LASTDAY = FIRSTDAY
END
ELSE IF MODE = 'LEDGER' THEN DO
  "EXECIO * DISKR BIZDATE (STEM BIZ. FINIS"
  IF RC > 2 THEN DO
    SAY 'CANNOT READ BIZDATE, RC=' RC
    EXIT 8
  END
  IF BIZ.0 > 0 THEN FIRSTDAY = DAYS(SUBSTR(BIZ.1, 1, 8))
  ELSE IF START <> '' THEN FIRSTDAY = DAYS(YMD(START))
  ELSE FIRSTDAY = LASTDAY
  IF FIRSTDAY > LASTDAY THEN DO
    SAY 'NOTHING TO DO: NEXT BUSINESS DATE IS' CIVIL(FIRSTDAY)
    EXIT 4
  END
END
ELSE DO
  SAY 'UNKNOWN MODE:' MODE
  EXIT 8
END
SAY MODE 'FROM' CIVIL(FIRSTDAY) 'TO' CIVIL(LASTDAY)

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
/* FETCH: ONE SET OF QUERIES PER DAY, EVALUATED AT 00:00 JST OF THE  */
/* NEXT DAY SO THAT THE [1d] WINDOWS COVER THAT DAY (00:00-24:00)    */
/* ----------------------------------------------------------------- */
CALL TCPINIT
IF TCPOPEN(VMHOST, VMPORT, 10) < 0 THEN DO
  SAY 'TCPOPEN FAILED:' VMHOST VMPORT
  CALL TCPTERM
  EXIT 8
END
SOCK = _FD
SAY 'CONNECTED TO' VMHOST VMPORT 'SOCKET' SOCK
BUF = ''                               /* RECEIVED, NOT YET USED   */
NKEYS = 0
SEEN. = 0                              /* KEYS ALREADY COLLECTED   */
VAL. = 0                               /* VALUES DEFAULT TO ZERO   */
DO DAY = FIRSTDAY TO LASTDAY
  EVALTS = (DAY + 1) * 86400 - 9 * 3600
  DO C = 1 TO 2
    DO I = 1 TO 4
      BODY = QUERY(Q.C.I)
      IF BODY = '' THEN DO
        CALL TCPCLOSE SOCK
        CALL TCPTERM
        EXIT 8
      END
      CALL COLLECT
    END
  END
END
CALL TCPCLOSE SOCK
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
  PARSE VAR KEY D '/' CL '/' NAME
  NUMS = ''
  DO I = 1 TO 4
    V = FORMAT(MAX(VAL.KEY.I, 0) * 100, , 0)
    NUMS = NUMS || RIGHT(MIN(V, 999999999), 9, '0')
  END
  OUT.K = LEFT(YMD(CIVIL(D)) || LEFT(CL, 5) ||,
               LEFT(TRANSLATE(NAME), 24) || NUMS, 80)
END
OUT.0 = NKEYS
"EXECIO * DISKW USAGE (STEM OUT. FINIS"
IF RC <> 0 THEN EXIT 8

CTL.1 = LEFT('RUN' CIVIL(TODAY) CIVIL(FIRSTDAY) CIVIL(LASTDAY),
             YMD(CIVIL(LASTDAY + 1)), 80)
N = 1
DO DAY = FIRSTDAY TO LASTDAY
  N = N + 1
  /* DAY 0 (1970-01-01) WAS A THURSDAY, SO (DAY + 3) // 7 = 0 IS MON */
  IF (DAY + 3) // 7 = 0 THEN MON = 'M'
  ELSE MON = ' '
  CTL.N = LEFT('DAY' YMD(CIVIL(DAY)) MON, 80)
END
CTL.0 = N
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
     || ' HTTP/1.1' || CRLF || 'Host: 'VMHOST || CRLF,
     || 'Connection: keep-alive' || CRLF || CRLF
  IF TCPSEND(SOCK, E2A(REQ), 10) < 0 THEN DO
    SAY 'TCPSEND FAILED'
    RETURN ''
  END
  /* THE HEADER ENDS WITH AN EMPTY LINE                              */
  DO WHILE POS(CRLF2, BUF) = 0
    IF RECEIVE() = 0 THEN RETURN ''
  END
  PARSE VAR BUF HEAD (CRLF2) BUF
  UHEAD = TRANSLATE(HEAD)
  P = POS('CONTENT-LENGTH:', UHEAD)
  IF P = 0 THEN DO
    SAY 'NO CONTENT-LENGTH:' HEAD
    RETURN ''
  END
  PARSE VALUE SUBSTR(UHEAD, P + 15) WITH CLEN (CRLF) .
  CLEN = STRIP(CLEN)
  /* THE BODY IS EXACTLY CONTENT-LENGTH BYTES                        */
  DO WHILE LENGTH(BUF) < CLEN
    IF RECEIVE() = 0 THEN RETURN ''
  END
  RBODY = LEFT(BUF, CLEN)
  BUF = SUBSTR(BUF, CLEN + 1)
  PARSE VAR HEAD STATUSLINE (CRLF) .
  IF WORD(STATUSLINE, 2) <> '200' |,
     POS('"status":"success"', RBODY) = 0 THEN DO
    SAY 'QUERY FAILED:' STATUSLINE
    SAY SUBSTR(RBODY, 1, 200)
    RETURN ''
  END
  RETURN RBODY

/* ----------------------------------------------------------------- */
/* RECEIVE: APPEND THE NEXT CHUNK (IN EBCDIC) TO BUF. 0 ON ERROR     */
/* ----------------------------------------------------------------- */
RECEIVE:
  IF TCPRECEIVE(SOCK, 30) <= 0 THEN DO
    SAY 'TCPRECEIVE FAILED OR TIMED OUT'
    RETURN 0
  END
  BUF = BUF || A2E(_DATA)
  RETURN 1

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
    KEY = DAY'/'CLASS.C'/'NAME
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
/* YMD  : YYYY-MM-DD -> YYYYMMDD                                     */
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

YMD: PROCEDURE
  RETURN SPACE(TRANSLATE(ARG(1), ' ', '-'), 0)

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
