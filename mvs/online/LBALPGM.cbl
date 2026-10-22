       IDENTIFICATION DIVISION.
       PROGRAM-ID. LBALPGM.
      *****************************************************************
      * LBALPGM - ACCOUNT INQUIRY (KICKS TRANSACTION LBAL)            *
      *                                                               *
      * SHOWS ONE ACCOUNT OF THE INQUIRY MASTER (BALANCE, ALLOCATED,  *
      * USED, DATES, STATUS) AND ITS JOURNAL, NEWEST FIRST, 10 LINES  *
      * A PAGE, WITH THE BALANCE AFTER EACH ENTRY. READ ONLY.         *
      *                                                               *
      * FILES : INQMAST - INQUIRY MASTER, KEY = ACCOUNT NO (10)       *
      *                   KEY 0000000000 IS THE CONTROL RECORD        *
      *         INQJRNL - INQUIRY JOURNAL, KEY = ACCOUNT NO (10)      *
      *                   + INVERTED DATE (8) + INVERTED SEQ (6)      *
      *         BOTH ARE MADE BY THE BATCH JOB LEDGINQ AFTER CLOSING. *
      *                                                               *
      * PSEUDO-CONVERSATIONAL. THE COMMAREA KEEPS THE ACCOUNT, THE    *
      * PAGE AND, FOR EVERY PAGE, THE KEY OF ITS FIRST ENTRY AND THE  *
      * BALANCE AFTER THAT ENTRY. A COMMAREA OF 10 BYTES IS AN        *
      * ACCOUNT NO FROM THE ACCOUNT LIST (LLST).                      *
      *                                                               *
      * KEYS: ENTER = INQUIRE THE ACCOUNT TYPED, PF5 = ACCOUNT LIST,  *
      *       PF7 / PF8 = NEWER / OLDER ENTRIES, PF3 OR CLEAR = END   *
      *                                                               *
      * THE BALANCE AFTER EACH ENTRY IS COMPUTED BACKWARDS FROM THE   *
      * BALANCE OF THE MASTER: AN ENTRY CREDITING 2100 (ALLOCATION)   *
      * RAISED THE BALANCE, ONE DEBITING 2100 (USAGE) LOWERED IT.     *
      *****************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-RESP                 PIC S9(8) COMP.
       77  WS-I                    PIC S9(4) COMP.
       77  WS-ALARM                PIC X     VALUE 'N'.
       77  WS-END                  PIC X.
       77  WS-RUNBAL               PIC S9(11) COMP-3.
       77  WS-EFFECT               PIC S9(11) COMP-3.
       77  WS-EDIT                 PIC ---,---,---,--9.
      *    LENGTH OF CA-AREA. ANY OTHER COMMAREA (NONE, OR WHAT KICKS
      *    GIVES THE FIRST TRANSACTION AT STARTUP) IS A FIRST TIME.
       77  WS-CA-LEN               PIC S9(4) COMP VALUE +1543.
       77  WS-ENDMSG               PIC X(60) VALUE
           'LBAL ENDED. TYPE LLST, LBAL OR KSSF (SIGN OFF).'.
      *
      * COMMAREA
      *
       01  CA-AREA.
           05  CA-ACCT             PIC X(10).
           05  CA-PAGE             PIC 9(3).
           05  CA-NEXT-KEY         PIC X(24).
           05  CA-NEXT-BAL         PIC S9(11) COMP-3.
           05  CA-TOP              OCCURS 50 TIMES.
               10  CA-TOP-KEY      PIC X(24).
               10  CA-TOP-BAL      PIC S9(11) COMP-3.
      *
      * INQUIRY MASTER RECORD (= ACCOUNT MASTER RECORD), 100 BYTES
      *
       01  IM-REC.
           05  IM-ACCT.
               10  IM-ACCT-CLS     PIC X(3).
               10  IM-ACCT-SER     PIC X(7).
           05  IM-CLASS            PIC X(5).
           05  IM-NAME             PIC X(24).
           05  IM-BAL              PIC S9(11) COMP-3.
           05  IM-ALLOC            PIC S9(11) COMP-3.
           05  IM-USED             PIC S9(11) COMP-3.
           05  IM-OPENED           PIC X(8).
           05  IM-LAST             PIC X(8).
           05  IM-STATUS           PIC X.
           05  FILLER              PIC X(26).
      *
      * CONTROL RECORD OF THE INQUIRY MASTER, 100 BYTES
      *
       01  IC-REC.
           05  IC-KEY              PIC X(10).
           05  IC-TAG              PIC X(5).
           05  IC-ASOF             PIC X(8).
           05  FILLER              PIC X(77).
      *
      * INQUIRY JOURNAL RECORD, 104 BYTES
      *
       01  IJ-REC.
           05  IJ-KEY.
               10  IJ-ACCT         PIC X(10).
               10  FILLER          PIC X(14).
           05  IJ-DATE             PIC X(8).
           05  IJ-SEQ              PIC X(6).
           05  IJ-DEBIT            PIC X(4).
           05  IJ-CREDIT           PIC X(4).
           05  IJ-JACCT            PIC X(10).
           05  IJ-TYPE             PIC X.
           05  IJ-AMOUNT           PIC 9(11).
           05  FILLER              PIC X(36).
       01  WS-CTL-KEY              PIC X(10) VALUE '0000000000'.
       01  WS-KEY.
           05  WS-KEY-ACCT         PIC X(10).
           05  WS-KEY-REST         PIC X(14).
      *
      * ACCOUNT NO AS TYPED: 1000000001 OR 100-0000001
      *
       01  WS-INPUT.
           05  WS-IN               PIC X(11).
           05  WS-IN-H REDEFINES WS-IN.
               10  WS-IN-CLS       PIC X(3).
               10  WS-IN-DASH      PIC X.
               10  WS-IN-SER       PIC X(7).
           05  WS-IN-P REDEFINES WS-IN.
               10  WS-IN-10        PIC X(10).
               10  WS-IN-LAST      PIC X.
       01  WS-ACCT.
           05  WS-ACCT-CLS         PIC X(3).
           05  WS-ACCT-SER         PIC X(7).
       01  WS-ACCT-SHOW.
           05  WS-SHOW-CLS         PIC X(3).
           05  FILLER              PIC X     VALUE '-'.
           05  WS-SHOW-SER         PIC X(7).
      *
      * DATE YYYYMMDD AND YYYY-MM-DD
      *
       01  WS-YMD.
           05  WS-YMD-Y            PIC X(4).
           05  WS-YMD-M            PIC X(2).
           05  WS-YMD-D            PIC X(2).
       01  WS-DATE.
           05  WS-DATE-Y           PIC X(4).
           05  FILLER              PIC X     VALUE '-'.
           05  WS-DATE-M           PIC X(2).
           05  FILLER              PIC X     VALUE '-'.
           05  WS-DATE-D           PIC X(2).
      *
      * ONE JOURNAL LINE ON THE SCREEN, 77 BYTES
      *
       01  JL-LINE.
           05  JL-DATE             PIC X(10).
           05  FILLER              PIC X(2)  VALUE SPACES.
           05  JL-SEQ              PIC X(6).
           05  FILLER              PIC X     VALUE SPACE.
           05  JL-TYPE             PIC X(12).
           05  FILLER              PIC X(2)  VALUE SPACES.
           05  JL-DEBIT            PIC X(4).
           05  FILLER              PIC X     VALUE '/'.
           05  JL-CREDIT           PIC X(4).
           05  FILLER              PIC X(2)  VALUE SPACES.
           05  JL-AMOUNT           PIC ---,---,---,--9.
           05  FILLER              PIC X(3)  VALUE SPACES.
           05  JL-BAL              PIC ---,---,---,--9.
       01  WS-LINES.
           05  WS-LINE             PIC X(77) OCCURS 10 TIMES.

       COPY LBALSET.

       COPY KIKAID.

       LINKAGE SECTION.
       01  KIKCOMMAREA.
           05  LK-ACCT             PIC X(10).
           05  FILLER              PIC X(1533).

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE LOW-VALUE TO LBALMAPO.
           MOVE 'N' TO WS-END.
           IF EIBCALEN = WS-CA-LEN
               MOVE KIKCOMMAREA TO CA-AREA
               PERFORM 3000-KEY THRU 3000-EXIT
           ELSE
           IF EIBCALEN = 10
               MOVE LK-ACCT TO WS-ACCT
               PERFORM 2000-NEW-ACCOUNT THRU 2000-EXIT
           ELSE
               PERFORM 1000-FIRST THRU 1000-EXIT.
           IF WS-END = 'Y'
               EXEC KICKS
                   SEND TEXT FROM(WS-ENDMSG) ERASE FREEKB
               END-EXEC
               EXEC KICKS
                   RETURN
               END-EXEC.
           PERFORM 8000-SEND THRU 8000-EXIT.
           EXEC KICKS
               RETURN TRANSID('LBAL') COMMAREA(CA-AREA)
           END-EXEC.

      *****************************************************************
      * FIRST TIME: AN EMPTY SCREEN
      *****************************************************************
       1000-FIRST.
           MOVE SPACES TO CA-AREA.
           MOVE ZERO TO CA-PAGE.
           PERFORM 7000-ASOF THRU 7000-EXIT.
           MOVE 'TYPE AN ACCOUNT NO. AND PRESS ENTER' TO MSGO.
       1000-EXIT.
           EXIT.

      *****************************************************************
      * SHOW THE ACCOUNT IN WS-ACCT FROM ITS NEWEST ENTRY
      *****************************************************************
       2000-NEW-ACCOUNT.
           MOVE SPACES TO CA-AREA.
           MOVE ZERO TO CA-PAGE.
           EXEC KICKS
               READ FILE('INQMAST') INTO(IM-REC) RIDFLD(WS-ACCT)
                    RESP(WS-RESP)
           END-EXEC.
           IF WS-RESP = KIKRESP(NOTFND)
               PERFORM 7000-ASOF THRU 7000-EXIT
               MOVE WS-ACCT-CLS TO WS-SHOW-CLS
               MOVE WS-ACCT-SER TO WS-SHOW-SER
               MOVE WS-ACCT-SHOW TO ACCTO
               MOVE 'NO SUCH ACCOUNT' TO MSGO
               MOVE 'Y' TO WS-ALARM
               GO TO 2000-EXIT.
           IF WS-RESP NOT = KIKRESP(NORMAL)
               PERFORM 9000-FILE-ERROR THRU 9000-EXIT
               GO TO 2000-EXIT.
           MOVE WS-ACCT TO CA-ACCT.
           MOVE 1 TO CA-PAGE.
           MOVE WS-ACCT TO WS-KEY-ACCT.
           MOVE ALL '0' TO WS-KEY-REST.
           MOVE WS-KEY TO CA-TOP-KEY (1).
           MOVE IM-BAL TO CA-TOP-BAL (1).
           PERFORM 4000-SHOW THRU 4000-EXIT.
       2000-EXIT.
           EXIT.

      *****************************************************************
      * THE KEY THE USER PRESSED
      *****************************************************************
       3000-KEY.
           IF EIBAID = KIKPF3 OR EIBAID = KIKCLEAR
               MOVE 'Y' TO WS-END
               GO TO 3000-EXIT.
      *    PA KEYS SHOW THE SAME PAGE AGAIN (KICKS ALSO SENDS A PA2
      *    TO REDRAW THE SCREEN AFTER THE TSO '***' PROMPT)
           IF EIBAID = KIKPA1 OR EIBAID = KIKPA2 OR EIBAID = KIKPA3
               PERFORM 3300-AGAIN THRU 3300-EXIT
               GO TO 3000-EXIT.
           IF EIBAID = KIKPF5
               EXEC KICKS
                   XCTL PROGRAM('LLSTPGM') COMMAREA(CA-ACCT)
                        LENGTH(10)
               END-EXEC.
           IF EIBAID = KIKENTER
               PERFORM 3100-ENTER THRU 3100-EXIT
               GO TO 3000-EXIT.
           IF EIBAID = KIKPF7 OR EIBAID = KIKPF8
               PERFORM 3200-PAGE THRU 3200-EXIT
               GO TO 3000-EXIT.
           PERFORM 3300-AGAIN THRU 3300-EXIT.
           MOVE 'THAT KEY IS NOT USED HERE' TO MSGO.
           MOVE 'Y' TO WS-ALARM.
       3000-EXIT.
           EXIT.

       3100-ENTER.
           EXEC KICKS
               RECEIVE MAP('LBALMAP') MAPSET('LBALSET')
                       INTO(LBALMAPI) RESP(WS-RESP)
           END-EXEC.
           MOVE SPACES TO WS-IN.
           IF WS-RESP = KIKRESP(NORMAL) AND ACCTL > ZERO
               MOVE ACCTI TO WS-IN.
           EXAMINE WS-IN REPLACING ALL LOW-VALUE BY SPACE.
           MOVE LOW-VALUE TO LBALMAPO.
           IF WS-IN-DASH = '-' AND WS-IN-CLS NUMERIC
                               AND WS-IN-SER NUMERIC
               MOVE WS-IN-CLS TO WS-ACCT-CLS
               MOVE WS-IN-SER TO WS-ACCT-SER
           ELSE
           IF WS-IN-10 NUMERIC AND WS-IN-LAST = SPACE
               MOVE WS-IN-10 TO WS-ACCT
           ELSE
               PERFORM 7000-ASOF THRU 7000-EXIT
               MOVE WS-IN TO ACCTO
               MOVE 'TYPE 10 DIGITS (1000000001) OR 100-0000001'
                   TO MSGO
               MOVE 'Y' TO WS-ALARM
               MOVE SPACES TO CA-ACCT
               GO TO 3100-EXIT.
           PERFORM 2000-NEW-ACCOUNT THRU 2000-EXIT.
       3100-EXIT.
           EXIT.

       3200-PAGE.
           IF CA-ACCT = SPACES
               PERFORM 1000-FIRST THRU 1000-EXIT
               MOVE 'Y' TO WS-ALARM
               GO TO 3200-EXIT.
           IF EIBAID = KIKPF7
               IF CA-PAGE > 1
                   SUBTRACT 1 FROM CA-PAGE
                   PERFORM 4000-SHOW THRU 4000-EXIT
               ELSE
                   PERFORM 4000-SHOW THRU 4000-EXIT
                   MOVE 'THIS IS THE NEWEST PAGE' TO MSGO
                   MOVE 'Y' TO WS-ALARM.
           IF EIBAID = KIKPF8
               IF CA-NEXT-KEY NOT = SPACES AND CA-PAGE < 50
                   ADD 1 TO CA-PAGE
                   MOVE CA-NEXT-KEY TO CA-TOP-KEY (CA-PAGE)
                   MOVE CA-NEXT-BAL TO CA-TOP-BAL (CA-PAGE)
                   PERFORM 4000-SHOW THRU 4000-EXIT
               ELSE
                   PERFORM 4000-SHOW THRU 4000-EXIT
                   MOVE 'NO OLDER ENTRIES' TO MSGO
                   MOVE 'Y' TO WS-ALARM.
       3200-EXIT.
           EXIT.

      *    ANY OTHER KEY: SHOW THE SAME PAGE AGAIN
       3300-AGAIN.
           IF CA-ACCT = SPACES
               PERFORM 1000-FIRST THRU 1000-EXIT
           ELSE
               PERFORM 4000-SHOW THRU 4000-EXIT.
       3300-EXIT.
           EXIT.

      *****************************************************************
      * SHOW PAGE CA-PAGE OF ACCOUNT CA-ACCT
      *****************************************************************
       4000-SHOW.
           PERFORM 7000-ASOF THRU 7000-EXIT.
           MOVE CA-ACCT TO WS-ACCT.
           EXEC KICKS
               READ FILE('INQMAST') INTO(IM-REC) RIDFLD(WS-ACCT)
                    RESP(WS-RESP)
           END-EXEC.
           IF WS-RESP NOT = KIKRESP(NORMAL)
               PERFORM 9000-FILE-ERROR THRU 9000-EXIT
               GO TO 4000-EXIT.
           PERFORM 4100-MASTER THRU 4100-EXIT.
           PERFORM 4200-JOURNAL THRU 4200-EXIT.
       4000-EXIT.
           EXIT.

       4100-MASTER.
           MOVE IM-ACCT-CLS TO WS-SHOW-CLS.
           MOVE IM-ACCT-SER TO WS-SHOW-SER.
           MOVE WS-ACCT-SHOW TO ACCTO.
           MOVE IM-CLASS TO CLASSO.
           MOVE IM-NAME TO NAMEO.
           MOVE IM-BAL TO WS-EDIT.
           MOVE WS-EDIT TO BALO.
           IF IM-BAL < ZERO
               MOVE '*** OVERDRAWN ***' TO OVERO.
           MOVE IM-ALLOC TO WS-EDIT.
           MOVE WS-EDIT TO ALLOCO.
           MOVE IM-USED TO WS-EDIT.
           MOVE WS-EDIT TO USEDO.
           MOVE IM-OPENED TO WS-YMD.
           PERFORM 7100-DATE THRU 7100-EXIT.
           MOVE WS-DATE TO OPENEDO.
           MOVE IM-LAST TO WS-YMD.
           PERFORM 7100-DATE THRU 7100-EXIT.
           MOVE WS-DATE TO LASTPO.
           IF IM-STATUS = 'A'
               MOVE 'A  DEFINED' TO STATO
           ELSE
           IF IM-STATUS = 'U'
               MOVE 'U  UNDEFINED' TO STATO
           ELSE
               MOVE IM-STATUS TO STATO.
           MOVE CA-PAGE TO PAGEO.
       4100-EXIT.
           EXIT.

      *    UP TO 10 ENTRIES FROM THE FIRST ENTRY OF THE PAGE, AND ONE
      *    MORE TO KNOW WHERE THE NEXT PAGE STARTS
       4200-JOURNAL.
           MOVE SPACES TO WS-LINES.
           MOVE SPACES TO CA-NEXT-KEY.
           MOVE CA-TOP-KEY (CA-PAGE) TO WS-KEY.
           MOVE CA-TOP-BAL (CA-PAGE) TO WS-RUNBAL.
           EXEC KICKS
               STARTBR FILE('INQJRNL') RIDFLD(WS-KEY) GTEQ
                       RESP(WS-RESP)
           END-EXEC.
      *    NOT OPEN OR DISABLED: THE FILE IS MISSING. ANY OTHER
      *    CONDITION (NOTFND, OR A KEY PAST THE LAST ONE): NO ENTRIES
           IF WS-RESP = KIKRESP(NOTOPEN) OR WS-RESP = KIKRESP(DISABLED)
               PERFORM 9000-FILE-ERROR THRU 9000-EXIT
               GO TO 4200-EXIT.
           IF WS-RESP NOT = KIKRESP(NORMAL)
               MOVE 'NO JOURNAL ENTRIES' TO MSGO
               GO TO 4200-SET.
           MOVE ZERO TO WS-I.
       4200-NEXT.
           EXEC KICKS
               READNEXT FILE('INQJRNL') INTO(IJ-REC) RIDFLD(WS-KEY)
                        RESP(WS-RESP)
           END-EXEC.
           IF WS-RESP NOT = KIKRESP(NORMAL) OR IJ-ACCT NOT = CA-ACCT
               GO TO 4200-END.
           IF WS-I = 10
               MOVE IJ-KEY TO CA-NEXT-KEY
               MOVE WS-RUNBAL TO CA-NEXT-BAL
               GO TO 4200-END.
           ADD 1 TO WS-I.
           PERFORM 4300-LINE THRU 4300-EXIT.
           GO TO 4200-NEXT.
       4200-END.
           EXEC KICKS
               ENDBR FILE('INQJRNL') RESP(WS-RESP)
           END-EXEC.
           IF WS-I = ZERO
               MOVE 'NO JOURNAL ENTRIES' TO MSGO
           ELSE
           IF CA-NEXT-KEY NOT = SPACES
               MOVE 'PF8 FOR OLDER ENTRIES' TO MSGO.
       4200-SET.
           MOVE WS-LINE (1) TO JL01O.
           MOVE WS-LINE (2) TO JL02O.
           MOVE WS-LINE (3) TO JL03O.
           MOVE WS-LINE (4) TO JL04O.
           MOVE WS-LINE (5) TO JL05O.
           MOVE WS-LINE (6) TO JL06O.
           MOVE WS-LINE (7) TO JL07O.
           MOVE WS-LINE (8) TO JL08O.
           MOVE WS-LINE (9) TO JL09O.
           MOVE WS-LINE (10) TO JL10O.
       4200-EXIT.
           EXIT.

      *    ONE JOURNAL LINE. THE BALANCE SHOWN IS AFTER THE ENTRY.
       4300-LINE.
           MOVE IJ-DATE TO WS-YMD.
           PERFORM 7100-DATE THRU 7100-EXIT.
           MOVE WS-DATE TO JL-DATE.
           MOVE IJ-SEQ TO JL-SEQ.
           IF IJ-TYPE = 'O'
               MOVE 'OPEN ALLOC' TO JL-TYPE
           ELSE
           IF IJ-TYPE = 'A'
               MOVE 'ALLOCATION' TO JL-TYPE
           ELSE
           IF IJ-TYPE = 'U'
               MOVE 'USAGE' TO JL-TYPE
           ELSE
               MOVE IJ-TYPE TO JL-TYPE.
           MOVE IJ-DEBIT TO JL-DEBIT.
           MOVE IJ-CREDIT TO JL-CREDIT.
           MOVE ZERO TO WS-EFFECT.
           IF IJ-CREDIT = '2100'
               MOVE IJ-AMOUNT TO WS-EFFECT.
           IF IJ-DEBIT = '2100'
               SUBTRACT IJ-AMOUNT FROM ZERO GIVING WS-EFFECT.
           MOVE WS-EFFECT TO JL-AMOUNT.
           MOVE WS-RUNBAL TO JL-BAL.
           MOVE JL-LINE TO WS-LINE (WS-I).
           SUBTRACT WS-EFFECT FROM WS-RUNBAL.
       4300-EXIT.
           EXIT.

      *****************************************************************
      * COMMON
      *****************************************************************
      *    AS OF WHICH CLOSING THE INQUIRY FILES ARE
       7000-ASOF.
           EXEC KICKS
               READ FILE('INQMAST') INTO(IC-REC) RIDFLD(WS-CTL-KEY)
                    RESP(WS-RESP)
           END-EXEC.
           IF WS-RESP = KIKRESP(NORMAL)
               MOVE IC-ASOF TO WS-YMD
               PERFORM 7100-DATE THRU 7100-EXIT
               MOVE WS-DATE TO ASOFO.
       7000-EXIT.
           EXIT.

       7100-DATE.
           MOVE WS-YMD-Y TO WS-DATE-Y.
           MOVE WS-YMD-M TO WS-DATE-M.
           MOVE WS-YMD-D TO WS-DATE-D.
       7100-EXIT.
           EXIT.

       8000-SEND.
           IF WS-ALARM = 'Y'
               EXEC KICKS
                   SEND MAP('LBALMAP') MAPSET('LBALSET')
                        FROM(LBALMAPO) ERASE ALARM
               END-EXEC
           ELSE
               EXEC KICKS
                   SEND MAP('LBALMAP') MAPSET('LBALSET')
                        FROM(LBALMAPO) ERASE
               END-EXEC.
       8000-EXIT.
           EXIT.

      *    THE INQUIRY FILES ARE MISSING, OR BEING MADE BY LEDGINQ
       9000-FILE-ERROR.
           MOVE 'INQUIRY FILES NOT READY. RUN LEDGINQ, THEN TRY AGAIN'
               TO MSGO.
           MOVE 'Y' TO WS-ALARM.
           MOVE SPACES TO CA-ACCT.
       9000-EXIT.
           EXIT.
