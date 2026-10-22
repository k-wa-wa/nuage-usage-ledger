       IDENTIFICATION DIVISION.
       PROGRAM-ID. LLSTPGM.
      *****************************************************************
      * LLSTPGM - ACCOUNT LIST (KICKS TRANSACTION LLST)               *
      *                                                               *
      * LISTS THE ACCOUNTS OF THE INQUIRY MASTER IN ACCOUNT ORDER,    *
      * 15 A PAGE, WITH THE BALANCE. OVERDRAWN ACCOUNTS ARE BRIGHT.   *
      * S BEFORE AN ACCOUNT SHOWS IT IN THE ACCOUNT INQUIRY (LBAL).   *
      * READ ONLY.                                                    *
      *                                                               *
      * FILE  : INQMAST - INQUIRY MASTER, KEY = ACCOUNT NO (10)       *
      *                   KEY 0000000000 IS THE CONTROL RECORD,       *
      *                   SO THE LIST STARTS AT 0000000001            *
      *                                                               *
      * PSEUDO-CONVERSATIONAL. THE COMMAREA KEEPS THE PAGE, THE FIRST *
      * KEY OF EVERY PAGE AND THE ACCOUNTS ON THE SCREEN. A COMMAREA  *
      * OF 10 BYTES IS AN ACCOUNT NO FROM THE ACCOUNT INQUIRY (LBAL): *
      * THE LIST STARTS THERE.                                        *
      *                                                               *
      * KEYS: ENTER = INQUIRE THE ACCOUNT MARKED S, OR START THE LIST *
      *       AT THE ACCOUNT TYPED, PF7 / PF8 = BACK / FORWARD,       *
      *       PF3 OR CLEAR = END                                      *
      *****************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-RESP                 PIC S9(8) COMP.
       77  WS-I                    PIC S9(4) COMP.
       77  WS-ALARM                PIC X     VALUE 'N'.
       77  WS-END                  PIC X.
       77  WS-SEL                  PIC S9(4) COMP.
      *    LENGTH OF CA-AREA. ANY OTHER COMMAREA (NONE, OR WHAT KICKS
      *    GIVES THE FIRST TRANSACTION AT STARTUP) IS A FIRST TIME.
       77  WS-CA-LEN               PIC S9(4) COMP VALUE +663.
       77  WS-ENDMSG               PIC X(60) VALUE
           'LLST ENDED. TYPE LLST, LBAL OR KSSF (SIGN OFF).'.
       77  WS-NOTREADY             PIC X(60) VALUE
           'INQUIRY FILES NOT READY. RUN LEDGINQ, THEN TRY AGAIN'.
      *
      * COMMAREA
      *
       01  CA-AREA.
           05  CA-PAGE             PIC 9(3).
           05  CA-NEXT-KEY         PIC X(10).
           05  CA-TOP-KEY          PIC X(10) OCCURS 50 TIMES.
           05  CA-ROW-ACCT         PIC X(10) OCCURS 15 TIMES.
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
       01  IC-REC.
           05  IC-KEY              PIC X(10).
           05  IC-TAG              PIC X(5).
           05  IC-ASOF.
               10  IC-ASOF-Y       PIC X(4).
               10  IC-ASOF-M       PIC X(2).
               10  IC-ASOF-D       PIC X(2).
           05  FILLER              PIC X(77).
       01  WS-CTL-KEY              PIC X(10) VALUE '0000000000'.
       01  WS-KEY                  PIC X(10).
       01  WS-DATE.
           05  WS-DATE-Y           PIC X(4).
           05  FILLER              PIC X     VALUE '-'.
           05  WS-DATE-M           PIC X(2).
           05  FILLER              PIC X     VALUE '-'.
           05  WS-DATE-D           PIC X(2).
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
      *
      * ONE ACCOUNT LINE ON THE SCREEN, 76 BYTES
      *
       01  LN-LINE.
           05  LN-ACCT-CLS         PIC X(3).
           05  FILLER              PIC X     VALUE '-'.
           05  LN-ACCT-SER         PIC X(7).
           05  FILLER              PIC X(2)  VALUE SPACES.
           05  LN-CLASS            PIC X(5).
           05  FILLER              PIC X(2)  VALUE SPACES.
           05  LN-NAME             PIC X(24).
           05  FILLER              PIC X(2)  VALUE SPACES.
           05  LN-BAL              PIC ---,---,---,--9.
           05  FILLER              PIC X(2)  VALUE SPACES.
           05  LN-ST               PIC X.
           05  FILLER              PIC X(2)  VALUE SPACES.
           05  LN-NOTE             PIC X(10).
       01  WS-ROWS.
           05  WS-ROW              OCCURS 15 TIMES.
               10  WS-ROW-LINE     PIC X(76).
               10  WS-ROW-OVER     PIC X.

       COPY LLSTSET.

       COPY KIKAID.

       COPY KIKBMSCA.

       LINKAGE SECTION.
       01  KIKCOMMAREA.
           05  LK-ACCT             PIC X(10).
           05  FILLER              PIC X(653).

       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE LOW-VALUE TO LLSTMAPO.
           MOVE 'N' TO WS-END.
           IF EIBCALEN = WS-CA-LEN
               MOVE KIKCOMMAREA TO CA-AREA
               PERFORM 3000-KEY THRU 3000-EXIT
           ELSE
           IF EIBCALEN = 10
               MOVE LK-ACCT TO WS-ACCT
               PERFORM 2000-NEW-LIST THRU 2000-EXIT
           ELSE
               MOVE SPACES TO WS-ACCT
               PERFORM 2000-NEW-LIST THRU 2000-EXIT.
           IF WS-END = 'Y'
               EXEC KICKS
                   SEND TEXT FROM(WS-ENDMSG) ERASE FREEKB
               END-EXEC
               EXEC KICKS
                   RETURN
               END-EXEC.
           PERFORM 8000-SEND THRU 8000-EXIT.
           EXEC KICKS
               RETURN TRANSID('LLST') COMMAREA(CA-AREA)
           END-EXEC.

      *****************************************************************
      * START THE LIST AT ACCOUNT WS-ACCT (NOT BEFORE 0000000001, THE
      * CONTROL RECORD IS NOT AN ACCOUNT)
      *****************************************************************
       2000-NEW-LIST.
           IF WS-ACCT = SPACES OR WS-ACCT < '0000000001'
               MOVE '0000000001' TO WS-ACCT.
           MOVE SPACES TO CA-AREA.
           MOVE 1 TO CA-PAGE.
           MOVE WS-ACCT TO CA-TOP-KEY (1).
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
               PERFORM 4000-SHOW THRU 4000-EXIT
               GO TO 3000-EXIT.
           IF EIBAID = KIKENTER
               PERFORM 3100-ENTER THRU 3100-EXIT
               GO TO 3000-EXIT.
           IF EIBAID = KIKPF7
               IF CA-PAGE > 1
                   SUBTRACT 1 FROM CA-PAGE
                   PERFORM 4000-SHOW THRU 4000-EXIT
               ELSE
                   PERFORM 4000-SHOW THRU 4000-EXIT
                   MOVE 'THIS IS THE FIRST PAGE' TO MSGO
                   MOVE 'Y' TO WS-ALARM.
           IF EIBAID = KIKPF8
               IF CA-NEXT-KEY NOT = SPACES AND CA-PAGE < 50
                   ADD 1 TO CA-PAGE
                   MOVE CA-NEXT-KEY TO CA-TOP-KEY (CA-PAGE)
                   PERFORM 4000-SHOW THRU 4000-EXIT
               ELSE
                   PERFORM 4000-SHOW THRU 4000-EXIT
                   MOVE 'THIS IS THE LAST PAGE' TO MSGO
                   MOVE 'Y' TO WS-ALARM.
           IF EIBAID NOT = KIKPF7 AND EIBAID NOT = KIKPF8
               PERFORM 4000-SHOW THRU 4000-EXIT
               MOVE 'THAT KEY IS NOT USED HERE' TO MSGO
               MOVE 'Y' TO WS-ALARM.
       3000-EXIT.
           EXIT.

      *    ENTER: AN S BEFORE AN ACCOUNT, ELSE THE START ACCOUNT TYPED
       3100-ENTER.
           EXEC KICKS
               RECEIVE MAP('LLSTMAP') MAPSET('LLSTSET')
                       INTO(LLSTMAPI) RESP(WS-RESP)
           END-EXEC.
           IF WS-RESP NOT = KIKRESP(NORMAL)
               MOVE LOW-VALUE TO LLSTMAPI.
           PERFORM 3200-SELECTED THRU 3200-EXIT.
           IF WS-SEL > ZERO
               MOVE CA-ROW-ACCT (WS-SEL) TO WS-ACCT
               EXEC KICKS
                   XCTL PROGRAM('LBALPGM') COMMAREA(WS-ACCT)
                        LENGTH(10)
               END-EXEC.
           MOVE SPACES TO WS-IN.
           IF ACCTL > ZERO
               MOVE ACCTI TO WS-IN.
           EXAMINE WS-IN REPLACING ALL LOW-VALUE BY SPACE.
           MOVE LOW-VALUE TO LLSTMAPO.
           IF WS-IN = SPACES
               PERFORM 4000-SHOW THRU 4000-EXIT
               GO TO 3100-EXIT.
           IF WS-IN-DASH = '-' AND WS-IN-CLS NUMERIC
                               AND WS-IN-SER NUMERIC
               MOVE WS-IN-CLS TO WS-ACCT-CLS
               MOVE WS-IN-SER TO WS-ACCT-SER
           ELSE
           IF WS-IN-10 NUMERIC AND WS-IN-LAST = SPACE
               MOVE WS-IN-10 TO WS-ACCT
           ELSE
               PERFORM 4000-SHOW THRU 4000-EXIT
               MOVE 'TYPE 10 DIGITS (1000000001) OR 100-0000001'
                   TO MSGO
               MOVE 'Y' TO WS-ALARM
               GO TO 3100-EXIT.
           PERFORM 2000-NEW-LIST THRU 2000-EXIT.
       3100-EXIT.
           EXIT.

      *    THE FIRST LINE MARKED S (WS-SEL = 0 IF NONE)
       3200-SELECTED.
           MOVE ZERO TO WS-SEL.
           IF SEL15I = 'S' OR SEL15I = 's' MOVE 15 TO WS-SEL.
           IF SEL14I = 'S' OR SEL14I = 's' MOVE 14 TO WS-SEL.
           IF SEL13I = 'S' OR SEL13I = 's' MOVE 13 TO WS-SEL.
           IF SEL12I = 'S' OR SEL12I = 's' MOVE 12 TO WS-SEL.
           IF SEL11I = 'S' OR SEL11I = 's' MOVE 11 TO WS-SEL.
           IF SEL10I = 'S' OR SEL10I = 's' MOVE 10 TO WS-SEL.
           IF SEL09I = 'S' OR SEL09I = 's' MOVE 9 TO WS-SEL.
           IF SEL08I = 'S' OR SEL08I = 's' MOVE 8 TO WS-SEL.
           IF SEL07I = 'S' OR SEL07I = 's' MOVE 7 TO WS-SEL.
           IF SEL06I = 'S' OR SEL06I = 's' MOVE 6 TO WS-SEL.
           IF SEL05I = 'S' OR SEL05I = 's' MOVE 5 TO WS-SEL.
           IF SEL04I = 'S' OR SEL04I = 's' MOVE 4 TO WS-SEL.
           IF SEL03I = 'S' OR SEL03I = 's' MOVE 3 TO WS-SEL.
           IF SEL02I = 'S' OR SEL02I = 's' MOVE 2 TO WS-SEL.
           IF SEL01I = 'S' OR SEL01I = 's' MOVE 1 TO WS-SEL.
           IF WS-SEL > ZERO
               IF CA-ROW-ACCT (WS-SEL) = SPACES
                   MOVE ZERO TO WS-SEL.
       3200-EXIT.
           EXIT.

      *****************************************************************
      * SHOW PAGE CA-PAGE: 15 ACCOUNTS FROM ITS FIRST KEY, AND ONE    *
      * MORE TO KNOW WHERE THE NEXT PAGE STARTS                       *
      *****************************************************************
       4000-SHOW.
           PERFORM 7000-ASOF THRU 7000-EXIT.
           MOVE SPACES TO WS-ROWS.
           MOVE SPACES TO CA-NEXT-KEY.
           MOVE ZERO TO WS-I.
           MOVE CA-TOP-KEY (CA-PAGE) TO WS-KEY.
           EXEC KICKS
               STARTBR FILE('INQMAST') RIDFLD(WS-KEY) GTEQ
                       RESP(WS-RESP)
           END-EXEC.
      *    NOT OPEN OR DISABLED: THE FILE IS MISSING. ANY OTHER
      *    CONDITION (NOTFND, OR A KEY PAST THE LAST ONE): NO ACCOUNTS
           IF WS-RESP = KIKRESP(NOTOPEN) OR WS-RESP = KIKRESP(DISABLED)
               MOVE WS-NOTREADY TO MSGO
               MOVE 'Y' TO WS-ALARM
               GO TO 4000-SET.
           IF WS-RESP NOT = KIKRESP(NORMAL)
               MOVE 'NO ACCOUNTS FROM HERE' TO MSGO
               GO TO 4000-SET.
       4000-NEXT.
           EXEC KICKS
               READNEXT FILE('INQMAST') INTO(IM-REC) RIDFLD(WS-KEY)
                        RESP(WS-RESP)
           END-EXEC.
           IF WS-RESP NOT = KIKRESP(NORMAL)
               GO TO 4000-END.
           IF WS-I = 15
               MOVE IM-ACCT TO CA-NEXT-KEY
               GO TO 4000-END.
           ADD 1 TO WS-I.
           PERFORM 4100-LINE THRU 4100-EXIT.
           GO TO 4000-NEXT.
       4000-END.
           EXEC KICKS
               ENDBR FILE('INQMAST') RESP(WS-RESP)
           END-EXEC.
           IF WS-I = ZERO
               MOVE 'NO ACCOUNTS FROM HERE' TO MSGO
           ELSE
           IF CA-NEXT-KEY NOT = SPACES
               MOVE 'PF8 FOR MORE ACCOUNTS' TO MSGO.
       4000-SET.
           PERFORM 4200-ROWS THRU 4200-EXIT.
       4000-EXIT.
           EXIT.

       4100-LINE.
           MOVE IM-ACCT TO CA-ROW-ACCT (WS-I).
           MOVE IM-ACCT-CLS TO LN-ACCT-CLS.
           MOVE IM-ACCT-SER TO LN-ACCT-SER.
           MOVE IM-CLASS TO LN-CLASS.
           MOVE IM-NAME TO LN-NAME.
           MOVE IM-BAL TO LN-BAL.
           MOVE IM-STATUS TO LN-ST.
           MOVE SPACES TO LN-NOTE.
           IF IM-BAL < ZERO
               MOVE 'OVERDRAWN' TO LN-NOTE
               MOVE 'Y' TO WS-ROW-OVER (WS-I).
           MOVE LN-LINE TO WS-ROW-LINE (WS-I).
       4100-EXIT.
           EXIT.

      *    THE ROWS INTO THE MAP. AN OVERDRAWN ROW IS BRIGHT.
       4200-ROWS.
           MOVE WS-ROW-LINE (1) TO LN01O.
           MOVE WS-ROW-LINE (2) TO LN02O.
           MOVE WS-ROW-LINE (3) TO LN03O.
           MOVE WS-ROW-LINE (4) TO LN04O.
           MOVE WS-ROW-LINE (5) TO LN05O.
           MOVE WS-ROW-LINE (6) TO LN06O.
           MOVE WS-ROW-LINE (7) TO LN07O.
           MOVE WS-ROW-LINE (8) TO LN08O.
           MOVE WS-ROW-LINE (9) TO LN09O.
           MOVE WS-ROW-LINE (10) TO LN10O.
           MOVE WS-ROW-LINE (11) TO LN11O.
           MOVE WS-ROW-LINE (12) TO LN12O.
           MOVE WS-ROW-LINE (13) TO LN13O.
           MOVE WS-ROW-LINE (14) TO LN14O.
           MOVE WS-ROW-LINE (15) TO LN15O.
           IF WS-ROW-OVER (1) = 'Y' MOVE KIKBMASB TO LN01A.
           IF WS-ROW-OVER (2) = 'Y' MOVE KIKBMASB TO LN02A.
           IF WS-ROW-OVER (3) = 'Y' MOVE KIKBMASB TO LN03A.
           IF WS-ROW-OVER (4) = 'Y' MOVE KIKBMASB TO LN04A.
           IF WS-ROW-OVER (5) = 'Y' MOVE KIKBMASB TO LN05A.
           IF WS-ROW-OVER (6) = 'Y' MOVE KIKBMASB TO LN06A.
           IF WS-ROW-OVER (7) = 'Y' MOVE KIKBMASB TO LN07A.
           IF WS-ROW-OVER (8) = 'Y' MOVE KIKBMASB TO LN08A.
           IF WS-ROW-OVER (9) = 'Y' MOVE KIKBMASB TO LN09A.
           IF WS-ROW-OVER (10) = 'Y' MOVE KIKBMASB TO LN10A.
           IF WS-ROW-OVER (11) = 'Y' MOVE KIKBMASB TO LN11A.
           IF WS-ROW-OVER (12) = 'Y' MOVE KIKBMASB TO LN12A.
           IF WS-ROW-OVER (13) = 'Y' MOVE KIKBMASB TO LN13A.
           IF WS-ROW-OVER (14) = 'Y' MOVE KIKBMASB TO LN14A.
           IF WS-ROW-OVER (15) = 'Y' MOVE KIKBMASB TO LN15A.
       4200-EXIT.
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
               MOVE IC-ASOF-Y TO WS-DATE-Y
               MOVE IC-ASOF-M TO WS-DATE-M
               MOVE IC-ASOF-D TO WS-DATE-D
               MOVE WS-DATE TO ASOFO.
       7000-EXIT.
           EXIT.

       8000-SEND.
           IF WS-ALARM = 'Y'
               EXEC KICKS
                   SEND MAP('LLSTMAP') MAPSET('LLSTSET')
                        FROM(LLSTMAPO) ERASE ALARM
               END-EXEC
           ELSE
               EXEC KICKS
                   SEND MAP('LLSTMAP') MAPSET('LLSTSET')
                        FROM(LLSTMAPO) ERASE
               END-EXEC.
       8000-EXIT.
           EXIT.
