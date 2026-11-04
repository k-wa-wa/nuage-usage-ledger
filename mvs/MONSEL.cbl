       IDENTIFICATION DIVISION.
       PROGRAM-ID. MONSEL.
      *****************************************************************
      * MONSEL - SELECT THE JOURNAL OF THE MONTH TO CLOSE (MONTHLY)   *
      *                                                               *
      * INPUT : BIZDATE  - BUSINESS DATES. 1-8 NEXT BUSINESS DATE,    *
      *                    19-24 LAST CLOSED MONTH (YYYYMM)           *
      *         JOURNAL  - JOURNAL, ALL GENERATIONS (GDG BASE NAME)   *
      *         LASTHIST - SAVED JOURNAL, NEWEST GENERATION (0)       *
      * OUTPUT: EXTRACT  - JOURNAL ENTRIES OF THE MONTH AND LATER     *
      *                    (NOT SORTED)                               *
      *         MONCTL   - THE MONTH, AND THE COUNT AND THE AMOUNT    *
      *                    OF ITS ENTRIES (CONTROL TOTALS), BY UNIT:  *
      *                    RU, AND USD FOR THE ACCOUNTS OF CLASS CODE *
      *                    300 (AGENT). AMOUNTS OF DIFFERENT UNITS    *
      *                    ARE NOT ADDED TOGETHER.                    *
      *                                                               *
      * THE MONTH TO CLOSE IS THE OLDEST MONTH IN THE JOURNAL AFTER   *
      * THE LAST CLOSED MONTH. IT IS CLOSED ONLY IF THE NEXT BUSINESS *
      * DATE IS IN A LATER MONTH, I.E. ALL ITS DAYS ARE CLOSED.       *
      * OTHERWISE THE RETURN CODE IS 4 AND THE JOB DOES NOTHING.      *
      *                                                               *
      * THE ENTRIES AFTER THE MONTH ARE ALSO SELECTED: ACCTLDG TAKES  *
      * THEM OFF THE BALANCE OF THE MASTER TO GET THE MONTH-END       *
      * BALANCE, AND CHECKS IT AGAINST THE LEDGER OF THE MONTH.       *
      *                                                               *
      * IF THE MONTH IS ALREADY IN THE SAVED JOURNAL, THE BUSINESS    *
      * DATES ARE BEHIND (A COMMIT STEP FAILED HALF WAY). THE RETURN  *
      * CODE IS 16 SO THAT THE MONTH IS NOT SAVED TWICE.              *
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BIZ-FILE     ASSIGN TO UT-S-BIZDATE.
           SELECT JRNL-FILE    ASSIGN TO UT-S-JOURNAL.
           SELECT HIST-FILE    ASSIGN TO UT-S-LASTHIST.
           SELECT EXT-FILE     ASSIGN TO UT-S-EXTRACT.
           SELECT CTL-FILE     ASSIGN TO UT-S-MONCTL.
       DATA DIVISION.
       FILE SECTION.
       FD  BIZ-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS BIZ-REC.
       01  BIZ-REC                 PIC X(80).
       FD  JRNL-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 10 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS J-REC.
      *    THE JOURNAL RECORD (SEE POSTING): 1-8 DATE, 9-14 SEQ,
      *    15-18 DEBIT, 19-22 CREDIT, 23-32 ACCOUNT, 33 TYPE,
      *    34-44 AMOUNT. THE CLASS CODE (23-25) GIVES THE UNIT.
       01  J-REC.
           05  J-YM                PIC X(6).
           05  FILLER              PIC X(16).
           05  J-ACCT-CLS          PIC X(3).
           05  FILLER              PIC X(8).
           05  J-AMOUNT            PIC 9(11).
           05  FILLER              PIC X(36).
       01  J-ALL                   PIC X(80).
       FD  HIST-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 10 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS H-REC.
       01  H-REC                   PIC X(80).
       FD  EXT-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 10 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS EXT-OUT.
       01  EXT-OUT                 PIC X(80).
       FD  CTL-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS CTL-OUT.
       01  CTL-OUT                 PIC X(80).
       WORKING-STORAGE SECTION.
       77  WS-EOF                  PIC X     VALUE 'N'.
       77  WS-TARGET               PIC X(6)  VALUE HIGH-VALUES.
       77  WS-READ-CNT             PIC 9(8)  VALUE ZERO.
      *    UNIT OF THE ENTRY: 1 = RU, 2 = USD
       77  WS-U                    PIC 9(4)  COMP.
       01  WS-BIZ.
           05  WS-NEXT-YM          PIC X(6).
           05  FILLER              PIC X(2).
           05  FILLER              PIC X.
           05  WS-LAST             PIC X(8).
           05  FILLER              PIC X.
           05  WS-MONTH            PIC X(6).
           05  FILLER              PIC X(56).
       01  WS-HIST.
           05  WS-HIST-YM          PIC X(6).
           05  FILLER              PIC X(74).
      *
      * CONTROL RECORD FOR ACCTLDG: THE MONTH, THEN FOR EACH UNIT
      * (1 = RU, 2 = USD) THE COUNT AND THE AMOUNT OF THE ENTRIES OF
      * THE MONTH AND THE COUNT OF THE ENTRIES AFTER IT. THE TOTALS
      * ARE COUNTED HERE DIRECTLY (NO VALUE CLAUSE IN AN OCCURS).
      *
       01  CTL-REC.
           05  C-TAG               PIC X(3).
           05  FILLER              PIC X.
           05  C-MONTH             PIC X(6).
           05  C-UNIT-TOTALS       OCCURS 2 TIMES.
               10  FILLER          PIC X.
               10  C-COUNT         PIC 9(8).
               10  FILLER          PIC X.
               10  C-AMOUNT        PIC 9(13).
               10  FILLER          PIC X.
               10  C-LATER         PIC 9(8).
           05  FILLER              PIC X(6).
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE SPACES TO WS-BIZ WS-HIST.
           OPEN INPUT BIZ-FILE.
           READ BIZ-FILE INTO WS-BIZ AT END MOVE SPACES TO WS-BIZ.
           CLOSE BIZ-FILE.
           DISPLAY 'MONSEL LAST CLOSED DATE  ' WS-LAST.
           DISPLAY 'MONSEL LAST CLOSED MONTH ' WS-MONTH.
      *    PASS 1: THE OLDEST MONTH AFTER THE LAST CLOSED MONTH. A
      *    BLANK LAST CLOSED MONTH (FIRST TIME) IS LOWER THAN ANY MONTH
           OPEN INPUT JRNL-FILE.
           PERFORM 1000-FIND THRU 1000-EXIT UNTIL WS-EOF = 'Y'.
           CLOSE JRNL-FILE.
           DISPLAY 'MONSEL JOURNAL READ      ' WS-READ-CNT.
           IF WS-TARGET = HIGH-VALUES
               DISPLAY 'MONSEL: NO JOURNAL AFTER THE LAST CLOSED MONTH'
               GO TO 0000-NOTHING.
           DISPLAY 'MONSEL CANDIDATE MONTH   ' WS-TARGET.
           IF WS-NEXT-YM = SPACES OR WS-TARGET NOT < WS-NEXT-YM
               DISPLAY 'MONSEL: THE MONTH IS NOT CLOSED TO ITS END'
               GO TO 0000-NOTHING.
      *    THE MONTH MUST NOT BE SAVED ALREADY
           OPEN INPUT HIST-FILE.
           READ HIST-FILE INTO WS-HIST AT END MOVE SPACES TO WS-HIST.
           CLOSE HIST-FILE.
           IF WS-HIST-YM NOT = SPACES AND WS-HIST-YM NOT < WS-TARGET
               DISPLAY 'MONSEL: MONTH ' WS-HIST-YM
                       ' IS ALREADY IN JRNLHIST(0)'
               DISPLAY 'MONSEL: THE LAST CLOSED MONTH IN BIZDATE IS '
                       'BEHIND. NOTHING IS DONE'
               MOVE 16 TO RETURN-CODE
               STOP RUN.
      *    PASS 2: THE ENTRIES OF THE MONTH AND LATER
           MOVE SPACES TO CTL-REC.
           MOVE 'MON' TO C-TAG.
           MOVE WS-TARGET TO C-MONTH.
           MOVE ZERO TO C-COUNT (1) C-AMOUNT (1) C-LATER (1)
                        C-COUNT (2) C-AMOUNT (2) C-LATER (2).
           OPEN INPUT JRNL-FILE
                OUTPUT EXT-FILE CTL-FILE.
           MOVE 'N' TO WS-EOF.
           PERFORM 2000-SELECT THRU 2000-EXIT UNTIL WS-EOF = 'Y'.
           WRITE CTL-OUT FROM CTL-REC.
           CLOSE JRNL-FILE EXT-FILE CTL-FILE.
           DISPLAY 'MONSEL MONTH TO CLOSE    ' WS-TARGET.
           DISPLAY 'MONSEL ENTRIES OF MONTH  ' C-COUNT (1) ' RU, '
                   C-COUNT (2) ' USD'.
           DISPLAY 'MONSEL AMOUNT OF MONTH   ' C-AMOUNT (1) ' RU, '
                   C-AMOUNT (2) ' USD (CENTS)'.
           DISPLAY 'MONSEL ENTRIES AFTER     ' C-LATER (1) ' RU, '
                   C-LATER (2) ' USD'.
           STOP RUN.
       0000-NOTHING.
           DISPLAY 'MONSEL: NO MONTH TO CLOSE (RC 4)'.
           MOVE 4 TO RETURN-CODE.
           STOP RUN.

       1000-FIND.
           READ JRNL-FILE AT END MOVE 'Y' TO WS-EOF GO TO 1000-EXIT.
           ADD 1 TO WS-READ-CNT.
           IF J-YM > WS-MONTH AND J-YM < WS-TARGET
               MOVE J-YM TO WS-TARGET.
       1000-EXIT.
           EXIT.

       2000-SELECT.
           READ JRNL-FILE AT END MOVE 'Y' TO WS-EOF GO TO 2000-EXIT.
           IF J-YM < WS-TARGET
               GO TO 2000-EXIT.
           WRITE EXT-OUT FROM J-ALL.
           IF J-ACCT-CLS = '300'
               MOVE 2 TO WS-U
           ELSE
               MOVE 1 TO WS-U.
           IF J-YM = WS-TARGET
               ADD 1 TO C-COUNT (WS-U)
               ADD J-AMOUNT TO C-AMOUNT (WS-U)
           ELSE
               ADD 1 TO C-LATER (WS-U).
       2000-EXIT.
           EXIT.
