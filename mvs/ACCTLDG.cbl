       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTLDG.
      *****************************************************************
      * ACCTLDG - ACCOUNT LEDGER OF THE MONTH (MONTHLY CLOSING)       *
      *                                                               *
      * INPUT : MONCTL   - THE MONTH AND ITS CONTROL TOTALS (MONSEL)  *
      *         OLDBAL   - MONTH-END BALANCES, GENERATION 0 (THE      *
      *                    MONTH BEFORE. EMPTY THE FIRST TIME)        *
      *         MASTER   - ACCOUNT MASTER, GENERATION 0               *
      *         GL       - GENERAL LEDGER, GENERATION 0               *
      *         JOURNAL  - ENTRIES OF THE MONTH AND LATER, SORTED BY  *
      *                    ACCOUNT, DATE AND SEQ                      *
      *         BIZDATE  - BUSINESS DATES                             *
      * OUTPUT: HIST     - ENTRIES OF THE MONTH (SAVED JOURNAL)       *
      *         NEWBAL   - MONTH-END BALANCES OF THE MONTH            *
      *         NEWBIZ   - BUSINESS DATES WITH THE LAST CLOSED MONTH  *
      *         REPORT   - ACCOUNT LEDGER AND MONTHLY BALANCES        *
      *                    (SYSOUT=M)                                 *
      *                                                               *
      * OLDBAL, MASTER AND JOURNAL ARE MATCHED BY ACCOUNT NUMBER. FOR *
      * EVERY ACCOUNT OPENED BY THE END OF THE MONTH:                 *
      *   BROUGHT FORWARD + INCREASE - DECREASE = CARRIED FORWARD     *
      * AN ENTRY CREDITING 2100 (ALLOCATION) IS AN INCREASE, ONE      *
      * DEBITING 2100 (USAGE) IS A DECREASE.                          *
      *                                                               *
      * CHECKS:                                                       *
      *   1. CARRIED FORWARD = THE BALANCE OF THE MASTER LESS THE     *
      *      ENTRIES AFTER THE MONTH (MONTH-END BALANCE FROM THE      *
      *      DAILY SUBSIDIARY LEDGER), FOR EVERY ACCOUNT              *
      *   2. CONTROL ACCOUNT 2100 OF THE GL LESS THE ENTRIES AFTER    *
      *      THE MONTH = THE TOTAL CARRIED FORWARD                    *
      *   3. ENTRIES SELECTED BY MONSEL = ENTRIES SAVED (COUNT AND    *
      *      AMOUNT)                                                  *
      * IF A CHECK FAILS, THE RETURN CODE IS 16 AND THE JOB COMMITS   *
      * NOTHING.                                                      *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           C01 IS TOP-OF-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CTL-FILE     ASSIGN TO UT-S-MONCTL.
           SELECT OLDBAL-FILE  ASSIGN TO UT-S-OLDBAL.
           SELECT MAST-FILE    ASSIGN TO UT-S-MASTER.
           SELECT GL-FILE      ASSIGN TO UT-S-GL.
           SELECT JRNL-FILE    ASSIGN TO UT-S-JOURNAL.
           SELECT BIZ-FILE     ASSIGN TO UT-S-BIZDATE.
           SELECT HIST-FILE    ASSIGN TO UT-S-HIST.
           SELECT NEWBAL-FILE  ASSIGN TO UT-S-NEWBAL.
           SELECT NEWBIZ-FILE  ASSIGN TO UT-S-NEWBIZ.
           SELECT REPORT-FILE  ASSIGN TO UT-S-REPORT.
       DATA DIVISION.
       FILE SECTION.
       FD  CTL-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS CTL-REC.
       01  CTL-REC.
           05  C-TAG               PIC X(3).
           05  FILLER              PIC X.
           05  C-MONTH             PIC X(6).
           05  FILLER              PIC X.
           05  C-COUNT             PIC 9(8).
           05  FILLER              PIC X.
           05  C-AMOUNT            PIC 9(13).
           05  FILLER              PIC X.
           05  C-LATER             PIC 9(8).
           05  FILLER              PIC X(38).
      *
      * MONTH-END BALANCE RECORD, 80 BYTES
      *
       FD  OLDBAL-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 10 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS OB-REC.
       01  OB-REC.
           05  OB-ACCT             PIC X(10).
           05  OB-MONTH            PIC X(6).
           05  OB-PRIOR            PIC S9(11) COMP-3.
           05  OB-INC              PIC S9(11) COMP-3.
           05  OB-DEC              PIC S9(11) COMP-3.
           05  OB-CLOSE            PIC S9(11) COMP-3.
           05  FILLER              PIC X(40).
      *
      * ACCOUNT MASTER RECORD (SEE POSTING), 100 BYTES
      *
       FD  MAST-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 10 RECORDS
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS M-REC.
       01  M-REC.
           05  M-ACCT              PIC X(10).
           05  M-CLASS             PIC X(5).
           05  M-NAME              PIC X(24).
           05  M-BAL               PIC S9(11) COMP-3.
           05  M-ALLOC             PIC S9(11) COMP-3.
           05  M-USED              PIC S9(11) COMP-3.
           05  M-OPENED.
               10  M-OPENED-YM     PIC X(6).
               10  FILLER          PIC X(2).
           05  M-LAST              PIC X(8).
           05  M-STATUS            PIC X.
           05  FILLER              PIC X(26).
       FD  GL-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 10 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS G-REC.
       01  G-REC.
           05  G-CODE              PIC X(4).
           05  G-NAME              PIC X(16).
           05  G-DEBIT             PIC S9(11) COMP-3.
           05  G-CREDIT            PIC S9(11) COMP-3.
           05  FILLER              PIC X(48).
      *
      * JOURNAL RECORD (SEE POSTING), 80 BYTES
      *
       FD  JRNL-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 10 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS J-REC.
       01  J-REC.
           05  J-DATE.
               10  J-YM.
                   15  J-Y         PIC X(4).
                   15  J-M         PIC X(2).
               10  J-D             PIC X(2).
           05  J-SEQ               PIC X(6).
           05  J-DEBIT             PIC X(4).
           05  J-CREDIT            PIC X(4).
           05  J-ACCT              PIC X(10).
           05  J-TYPE              PIC X.
           05  J-AMOUNT            PIC 9(11).
           05  FILLER              PIC X(36).
       01  J-ALL                   PIC X(80).
       FD  BIZ-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS BIZ-REC.
       01  BIZ-REC                 PIC X(80).
       FD  HIST-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 10 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS HIST-OUT.
       01  HIST-OUT                PIC X(80).
       FD  NEWBAL-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 10 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS NB-OUT.
       01  NB-OUT                  PIC X(80).
       FD  NEWBIZ-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS NBIZ-OUT.
       01  NBIZ-OUT                PIC X(80).
       FD  REPORT-FILE
           LABEL RECORDS ARE OMITTED
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS REPORT-REC.
       01  REPORT-REC              PIC X(133).
       WORKING-STORAGE SECTION.
       77  WS-ERROR                PIC X     VALUE 'N'.
       77  WS-HAS-OB               PIC X.
       77  WS-SHOW                 PIC X.
       77  WS-TARGET               PIC X(6).
       77  WS-OB-KEY               PIC X(10).
       77  WS-M-KEY                PIC X(10).
       77  WS-J-KEY                PIC X(10).
       77  WS-CUR                  PIC X(10).
       77  WS-I                    PIC 9(4)  COMP.
       77  WS-LINES                PIC 99    VALUE 99.
       77  WS-PAGE                 PIC 9(4)  VALUE ZERO.
       77  WS-ACCT-CNT             PIC 9(6)  VALUE ZERO.
       77  WS-MISMATCH             PIC 9(6)  VALUE ZERO.
       77  WS-HIST-CNT             PIC 9(8)  VALUE ZERO.
       77  WS-HIST-AMT             PIC 9(13) VALUE ZERO.
       77  WS-LATER-CNT            PIC 9(8)  VALUE ZERO.
       77  WS-EFFECT               PIC S9(11) COMP-3.
       77  WS-RUN                  PIC S9(11) COMP-3.
       77  WS-PRIOR                PIC S9(11) COMP-3.
       77  WS-INC                  PIC S9(11) COMP-3.
       77  WS-DEC                  PIC S9(11) COMP-3.
       77  WS-CLOSE                PIC S9(11) COMP-3.
       77  WS-AFTER                PIC S9(11) COMP-3.
       77  WS-MEND                 PIC S9(11) COMP-3.
       77  WS-GL2100               PIC S9(11) COMP-3 VALUE ZERO.
       77  WS-GL-MEND              PIC S9(11) COMP-3.
       77  TOT-PRIOR               PIC S9(11) COMP-3 VALUE ZERO.
       77  TOT-INC                 PIC S9(11) COMP-3 VALUE ZERO.
       77  TOT-DEC                 PIC S9(11) COMP-3 VALUE ZERO.
       77  TOT-CLOSE               PIC S9(11) COMP-3 VALUE ZERO.
       77  TOT-MEND                PIC S9(11) COMP-3 VALUE ZERO.
       77  TOT-AFTER               PIC S9(11) COMP-3 VALUE ZERO.
       01  WS-BIZ.
           05  FILLER              PIC X(18).
           05  WS-BIZ-MONTH        PIC X(6).
           05  FILLER              PIC X(56).
      *
      * MONTH-END BALANCE RECORD, BUILT HERE AND WRITTEN WITH FROM
      *
       01  NB-REC.
           05  NB-ACCT             PIC X(10).
           05  NB-MONTH            PIC X(6).
           05  NB-PRIOR            PIC S9(11) COMP-3.
           05  NB-INC              PIC S9(11) COMP-3.
           05  NB-DEC              PIC S9(11) COMP-3.
           05  NB-CLOSE            PIC S9(11) COMP-3.
           05  FILLER              PIC X(40) VALUE SPACES.
       01  WS-ACCT-SPLIT.
           05  WS-ACCT-CLS         PIC X(3).
           05  WS-ACCT-SER         PIC X(7).
       01  WS-OPENED.
           05  WS-OPENED-Y         PIC X(4).
           05  WS-OPENED-M         PIC X(2).
           05  WS-OPENED-D         PIC X(2).
       01  WS-TARGET-SPLIT.
           05  WS-TARGET-Y         PIC X(4).
           05  WS-TARGET-M         PIC X(2).
      *
      * MONTHLY BALANCES, KEPT FOR THE SUMMARY AFTER THE LEDGER
      *
       01  SM-TABLE.
           05  SM-COUNT            PIC 9(4)  COMP VALUE ZERO.
           05  SM-ENTRY            OCCURS 500 TIMES.
               10  SM-ACCT         PIC X(10).
               10  SM-CLASS        PIC X(5).
               10  SM-NAME         PIC X(24).
               10  SM-PRIOR        PIC S9(11) COMP-3.
               10  SM-INC          PIC S9(11) COMP-3.
               10  SM-DEC          PIC S9(11) COMP-3.
               10  SM-CLOSE        PIC S9(11) COMP-3.
               10  SM-MEND         PIC S9(11) COMP-3.
               10  SM-CHK          PIC X(3).
      *
      * CURRENT-DATE IS MM/DD/YY
      *
       01  WS-TODAY.
           05  WS-T-MM             PIC XX.
           05  FILLER              PIC X.
           05  WS-T-DD             PIC XX.
           05  FILLER              PIC X.
           05  WS-T-YY             PIC XX.
      *
      * REPORT LINES. EVERY LINE STARTS WITH 1 BYTE RESERVED FOR THE
      * CARRIAGE CONTROL CHARACTER SET BY WRITE ... ADVANCING.
      *
       01  HDR-1.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(33)
                   VALUE ' NUAGE USAGE LEDGER'.
           05  H1-TITLE            PIC X(40).
           05  FILLER              PIC X(16) VALUE SPACES.
           05  FILLER              PIC X(5)  VALUE 'PAGE '.
           05  H1-PAGE             PIC ZZZ9.
           05  FILLER              PIC X(34) VALUE SPACES.
       01  HDR-2.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(8)  VALUE ' MONTH: '.
           05  H2-Y                PIC X(4).
           05  FILLER              PIC X     VALUE '-'.
           05  H2-M                PIC X(2).
           05  FILLER              PIC X(66) VALUE SPACES.
           05  FILLER              PIC X(10) VALUE 'RUN DATE: '.
           05  FILLER              PIC XX    VALUE '20'.
           05  H2-YY               PIC XX.
           05  FILLER              PIC X     VALUE '-'.
           05  H2-MM               PIC XX.
           05  FILLER              PIC X     VALUE '-'.
           05  H2-DD               PIC XX.
           05  FILLER              PIC X(31) VALUE SPACES.
       01  AH-LINE.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(13) VALUE ' ACCOUNT NO: '.
           05  AH-ACCT-CLS         PIC X(3).
           05  FILLER              PIC X     VALUE '-'.
           05  AH-ACCT-SER         PIC X(7).
           05  FILLER              PIC X(10) VALUE '   CLASS: '.
           05  AH-CLASS            PIC X(5).
           05  FILLER              PIC X(9)  VALUE '   NAME: '.
           05  AH-NAME             PIC X(24).
           05  FILLER              PIC X(11) VALUE '   OPENED: '.
           05  AH-OPENED-Y         PIC X(4).
           05  FILLER              PIC X     VALUE '-'.
           05  AH-OPENED-M         PIC X(2).
           05  FILLER              PIC X     VALUE '-'.
           05  AH-OPENED-D         PIC X(2).
           05  FILLER              PIC X(11) VALUE '   STATUS: '.
           05  AH-STATUS           PIC X.
           05  FILLER              PIC X(3)  VALUE SPACES.
           05  AH-CONT             PIC X(11).
           05  FILLER              PIC X(13) VALUE SPACES.
       01  LH-1.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(13) VALUE ' DATE'.
           05  FILLER              PIC X(8)  VALUE 'SEQ'.
           05  FILLER              PIC X(22) VALUE 'DESCRIPTION'.
           05  FILLER              PIC X(11) VALUE 'DR/CR'.
           05  FILLER              PIC X(15) VALUE '       INCREASE'.
           05  FILLER              PIC X(2)  VALUE SPACES.
           05  FILLER              PIC X(15) VALUE '       DECREASE'.
           05  FILLER              PIC X(2)  VALUE SPACES.
           05  FILLER              PIC X(15) VALUE '        BALANCE'.
           05  FILLER              PIC X(29) VALUE SPACES.
       01  LH-2.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(13) VALUE ' ----------'.
           05  FILLER              PIC X(8)  VALUE '------'.
           05  FILLER              PIC X(22)
                   VALUE '--------------------'.
           05  FILLER              PIC X(11) VALUE '---------'.
           05  FILLER              PIC X(15) VALUE ALL '-'.
           05  FILLER              PIC X(2)  VALUE SPACES.
           05  FILLER              PIC X(15) VALUE ALL '-'.
           05  FILLER              PIC X(2)  VALUE SPACES.
           05  FILLER              PIC X(15) VALUE ALL '-'.
           05  FILLER              PIC X(29) VALUE SPACES.
      *    ONE LINE OF THE LEDGER. IT IS CLEARED WITH SPACES BEFORE
      *    EVERY USE, SO THE AMOUNT NOT USED ON THE LINE STAYS BLANK.
       01  LD-LINE.
           05  FILLER              PIC XX.
           05  LD-DATE.
               10  LD-Y            PIC X(4).
               10  LD-DASH1        PIC X.
               10  LD-M            PIC X(2).
               10  LD-DASH2        PIC X.
               10  LD-D            PIC X(2).
           05  FILLER              PIC X(2).
           05  LD-SEQ              PIC X(6).
           05  FILLER              PIC X(2).
           05  LD-DESC             PIC X(20).
           05  FILLER              PIC X(2).
           05  LD-DR               PIC X(4).
           05  LD-SLASH            PIC X.
           05  LD-CR               PIC X(4).
           05  FILLER              PIC X(2).
           05  LD-INC              PIC ZZZ,ZZZ,ZZZ,ZZ9.
           05  FILLER              PIC X(2).
           05  LD-DEC              PIC ZZZ,ZZZ,ZZZ,ZZ9.
           05  FILLER              PIC X(2).
           05  LD-BAL              PIC ---,---,---,--9.
           05  FILLER              PIC X(29).
       01  SH-1.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(14) VALUE ' ACCOUNT NO'.
           05  FILLER              PIC X(7)  VALUE 'CLASS'.
           05  FILLER              PIC X(26) VALUE 'NAME'.
           05  FILLER              PIC X(16) VALUE '    BROUGHT FWD'.
           05  FILLER              PIC X(16) VALUE '       INCREASE'.
           05  FILLER              PIC X(16) VALUE '       DECREASE'.
           05  FILLER              PIC X(16) VALUE '    CARRIED FWD'.
           05  FILLER              PIC X(17) VALUE '   MASTER M-END'.
           05  FILLER              PIC X(4)  VALUE 'CHK'.
       01  SH-2.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(14) VALUE ' -----------'.
           05  FILLER              PIC X(7)  VALUE '-----'.
           05  FILLER              PIC X(26)
                   VALUE '------------------------'.
           05  FILLER              PIC X(15) VALUE ALL '-'.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(15) VALUE ALL '-'.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(15) VALUE ALL '-'.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(15) VALUE ALL '-'.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(15) VALUE ALL '-'.
           05  FILLER              PIC X(2)  VALUE SPACES.
           05  FILLER              PIC X(4)  VALUE '---'.
       01  SM-LINE.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X     VALUE SPACE.
           05  S-ACCT.
               10  S-ACCT-CLS      PIC X(3).
               10  FILLER          PIC X     VALUE '-'.
               10  S-ACCT-SER      PIC X(7).
           05  FILLER              PIC XX    VALUE SPACES.
           05  S-CLASS             PIC X(5).
           05  FILLER              PIC XX    VALUE SPACES.
           05  S-NAME              PIC X(24).
           05  FILLER              PIC XX    VALUE SPACES.
           05  S-PRIOR             PIC ---,---,---,--9.
           05  FILLER              PIC X     VALUE SPACE.
           05  S-INC               PIC ---,---,---,--9.
           05  FILLER              PIC X     VALUE SPACE.
           05  S-DEC               PIC ---,---,---,--9.
           05  FILLER              PIC X     VALUE SPACE.
           05  S-CLOSE             PIC ---,---,---,--9.
           05  FILLER              PIC X     VALUE SPACE.
           05  S-MEND              PIC ---,---,---,--9.
           05  FILLER              PIC XX    VALUE SPACES.
           05  S-CHK               PIC X(4).
       01  CHECK-LINE.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X     VALUE SPACE.
           05  C-TEXT              PIC X(45).
      *    13 DIGITS: THE AMOUNT OF THE CONTROL TOTALS IS 9(13)
           05  C-LEFT              PIC --,---,---,---,--9.
           05  FILLER              PIC X(3)  VALUE ' : '.
           05  C-RIGHT             PIC --,---,---,---,--9.
           05  FILLER              PIC XX    VALUE SPACES.
           05  C-RESULT            PIC X(5).
           05  FILLER              PIC X(40) VALUE SPACES.
       01  MSG-LINE.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X     VALUE SPACE.
           05  M-TEXT              PIC X(80).
           05  FILLER              PIC X(51) VALUE SPACES.
       01  END-LINE.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(22)
                   VALUE ' *** END OF REPORT ***'.
           05  FILLER              PIC X(110) VALUE SPACES.
       01  BLANK-LINE.
           05  FILLER              PIC X(133) VALUE SPACES.
       PROCEDURE DIVISION.
       MAIN-LINE.
           OPEN INPUT CTL-FILE OLDBAL-FILE MAST-FILE GL-FILE JRNL-FILE
                      BIZ-FILE
                OUTPUT HIST-FILE NEWBAL-FILE NEWBIZ-FILE REPORT-FILE.
           PERFORM LOAD-CTL THRU LOAD-CTL-EXIT.
           PERFORM LOAD-GL THRU LOAD-GL-EXIT.
           PERFORM READ-OB THRU READ-OB-EXIT.
           PERFORM READ-M THRU READ-M-EXIT.
           PERFORM READ-J THRU READ-J-EXIT.
           PERFORM PROCESS-ACCOUNT THRU PROCESS-ACCOUNT-EXIT
               UNTIL WS-OB-KEY = HIGH-VALUES AND
                     WS-M-KEY = HIGH-VALUES AND
                     WS-J-KEY = HIGH-VALUES.
           PERFORM PRINT-SUMMARY THRU PRINT-SUMMARY-EXIT.
           PERFORM WRITE-BIZ THRU WRITE-BIZ-EXIT.
           CLOSE CTL-FILE OLDBAL-FILE MAST-FILE GL-FILE JRNL-FILE
                 BIZ-FILE HIST-FILE NEWBAL-FILE NEWBIZ-FILE
                 REPORT-FILE.
           DISPLAY 'ACCTLDG MONTH          ' WS-TARGET.
           DISPLAY 'ACCTLDG ACCOUNTS       ' WS-ACCT-CNT.
           DISPLAY 'ACCTLDG ENTRIES SAVED  ' WS-HIST-CNT.
           DISPLAY 'ACCTLDG ENTRIES AFTER  ' WS-LATER-CNT.
           IF WS-ERROR = 'Y'
               DISPLAY 'ACCTLDG: CHECK FAILED, NOTHING IS COMMITTED'
               MOVE 16 TO RETURN-CODE.
           STOP RUN.
      *
      * CONTROL RECORD FROM MONSEL
      *
       LOAD-CTL.
           READ CTL-FILE AT END MOVE SPACES TO CTL-REC.
           IF C-TAG NOT = 'MON'
               DISPLAY 'ACCTLDG: NO CONTROL RECORD IN MONCTL'
               MOVE 'Y' TO WS-ERROR
               MOVE ZERO TO C-COUNT C-AMOUNT C-LATER.
           MOVE C-MONTH TO WS-TARGET WS-TARGET-SPLIT.
           MOVE WS-TARGET-Y TO H2-Y.
           MOVE WS-TARGET-M TO H2-M.
           MOVE CURRENT-DATE TO WS-TODAY.
           MOVE WS-T-YY TO H2-YY.
           MOVE WS-T-MM TO H2-MM.
           MOVE WS-T-DD TO H2-DD.
       LOAD-CTL-EXIT.
           EXIT.
      *
      * CONTROL ACCOUNT 2100 OF THE GENERAL LEDGER (CREDIT BALANCE)
      *
       LOAD-GL.
           READ GL-FILE AT END GO TO LOAD-GL-EXIT.
           IF G-CODE = '2100'
               COMPUTE WS-GL2100 = G-CREDIT - G-DEBIT.
           GO TO LOAD-GL.
       LOAD-GL-EXIT.
           EXIT.
      *
      * THE THREE SORTED INPUTS. HIGH-VALUES MARKS THE END OF A FILE.
      *
       READ-OB.
           READ OLDBAL-FILE AT END
               MOVE HIGH-VALUES TO WS-OB-KEY
               GO TO READ-OB-EXIT.
           MOVE OB-ACCT TO WS-OB-KEY.
           IF OB-MONTH NOT < WS-TARGET
               DISPLAY 'ACCTLDG: OLDBAL IS NOT OF AN EARLIER MONTH '
                       OB-MONTH
               MOVE 'Y' TO WS-ERROR.
       READ-OB-EXIT.
           EXIT.
       READ-M.
           READ MAST-FILE AT END
               MOVE HIGH-VALUES TO WS-M-KEY
               GO TO READ-M-EXIT.
           MOVE M-ACCT TO WS-M-KEY.
       READ-M-EXIT.
           EXIT.
       READ-J.
           READ JRNL-FILE AT END
               MOVE HIGH-VALUES TO WS-J-KEY
               GO TO READ-J-EXIT.
           MOVE J-ACCT TO WS-J-KEY.
       READ-J-EXIT.
           EXIT.
      *
      * ONE ACCOUNT: THE LOWEST KEY OF THE THREE INPUTS. EVERY ACCOUNT
      * MUST BE IN THE MASTER (ACCOUNTS ARE NEVER REMOVED FROM IT).
      *
       PROCESS-ACCOUNT.
           MOVE WS-M-KEY TO WS-CUR.
           IF WS-OB-KEY < WS-CUR MOVE WS-OB-KEY TO WS-CUR.
           IF WS-J-KEY < WS-CUR MOVE WS-J-KEY TO WS-CUR.
           IF WS-M-KEY NOT = WS-CUR
               GO TO PROCESS-NO-MASTER.
           MOVE ZERO TO WS-PRIOR WS-INC WS-DEC WS-AFTER.
           MOVE 'N' TO WS-HAS-OB.
           IF WS-OB-KEY = WS-CUR
               MOVE 'Y' TO WS-HAS-OB
               MOVE OB-CLOSE TO WS-PRIOR.
      *    THE ACCOUNT IS IN THE LEDGER OF THE MONTH IF IT WAS OPENED BY
      *    THE END OF THE MONTH, OR HAS A BALANCE OR ENTRIES IN IT
           MOVE 'N' TO WS-SHOW.
           IF M-OPENED-YM NOT > WS-TARGET OR WS-HAS-OB = 'Y'
               MOVE 'Y' TO WS-SHOW.
           IF WS-J-KEY = WS-CUR AND J-YM = WS-TARGET
               MOVE 'Y' TO WS-SHOW.
           MOVE WS-PRIOR TO WS-RUN.
           IF WS-SHOW = 'Y'
               PERFORM LEDGER-HEAD THRU LEDGER-HEAD-EXIT
               PERFORM LEDGER-BF THRU LEDGER-BF-EXIT.
           PERFORM APPLY-J THRU APPLY-J-EXIT
               UNTIL WS-J-KEY NOT = WS-CUR.
           COMPUTE WS-CLOSE = WS-PRIOR + WS-INC - WS-DEC.
           COMPUTE WS-MEND = M-BAL - WS-AFTER.
           ADD WS-AFTER TO TOT-AFTER.
           IF WS-SHOW = 'Y'
               PERFORM LEDGER-END THRU LEDGER-END-EXIT
           ELSE
               PERFORM CHECK-NOT-OPEN THRU CHECK-NOT-OPEN-EXIT.
           PERFORM READ-M THRU READ-M-EXIT.
           IF WS-HAS-OB = 'Y'
               PERFORM READ-OB THRU READ-OB-EXIT.
           GO TO PROCESS-ACCOUNT-EXIT.
       PROCESS-NO-MASTER.
           DISPLAY 'ACCTLDG: ACCOUNT NOT IN THE MASTER ' WS-CUR.
           MOVE 'Y' TO WS-ERROR.
           PERFORM SKIP-J THRU SKIP-J-EXIT
               UNTIL WS-J-KEY NOT = WS-CUR.
           IF WS-OB-KEY = WS-CUR
               PERFORM READ-OB THRU READ-OB-EXIT.
       PROCESS-ACCOUNT-EXIT.
           EXIT.
       SKIP-J.
           PERFORM READ-J THRU READ-J-EXIT.
       SKIP-J-EXIT.
           EXIT.
      *
      * ONE ENTRY: AN ENTRY OF THE MONTH GOES TO THE LEDGER AND IS
      * SAVED, A LATER ONE ONLY COUNTS FOR THE MONTH-END BALANCE
      *
       APPLY-J.
           MOVE ZERO TO WS-EFFECT.
           IF J-CREDIT = '2100'
               MOVE J-AMOUNT TO WS-EFFECT.
           IF J-DEBIT = '2100'
               SUBTRACT J-AMOUNT FROM ZERO GIVING WS-EFFECT.
           IF J-YM = WS-TARGET
               PERFORM MONTH-ENTRY THRU MONTH-ENTRY-EXIT
           ELSE
               ADD WS-EFFECT TO WS-AFTER
               ADD 1 TO WS-LATER-CNT.
           PERFORM READ-J THRU READ-J-EXIT.
       APPLY-J-EXIT.
           EXIT.
       MONTH-ENTRY.
           ADD WS-EFFECT TO WS-RUN.
           IF WS-EFFECT < ZERO
               SUBTRACT WS-EFFECT FROM WS-DEC
           ELSE
               ADD WS-EFFECT TO WS-INC.
           WRITE HIST-OUT FROM J-ALL.
           ADD 1 TO WS-HIST-CNT.
           ADD J-AMOUNT TO WS-HIST-AMT.
           MOVE SPACES TO LD-LINE.
           MOVE J-Y TO LD-Y.
           MOVE J-M TO LD-M.
           MOVE J-D TO LD-D.
           MOVE '-' TO LD-DASH1 LD-DASH2.
           MOVE J-SEQ TO LD-SEQ.
           IF J-TYPE = 'O'
               MOVE 'OPENING ALLOCATION' TO LD-DESC
           ELSE
           IF J-TYPE = 'A'
               MOVE 'WEEKLY ALLOCATION' TO LD-DESC
           ELSE
           IF J-TYPE = 'U'
               MOVE 'USAGE' TO LD-DESC
           ELSE
               MOVE J-TYPE TO LD-DESC.
           MOVE J-DEBIT TO LD-DR.
           MOVE '/' TO LD-SLASH.
           MOVE J-CREDIT TO LD-CR.
           IF WS-EFFECT < ZERO
               MOVE J-AMOUNT TO LD-DEC
           ELSE
               MOVE J-AMOUNT TO LD-INC.
           MOVE WS-RUN TO LD-BAL.
           PERFORM LEDGER-LINE-OUT THRU LEDGER-LINE-OUT-EXIT.
       MONTH-ENTRY-EXIT.
           EXIT.
      *
      * REPORT: ACCOUNT LEDGER. EVERY ACCOUNT STARTS ON A NEW PAGE.
      *
       LEDGER-HEAD.
           MOVE SPACES TO AH-CONT.
           PERFORM LEDGER-PAGE THRU LEDGER-PAGE-EXIT.
       LEDGER-HEAD-EXIT.
           EXIT.
       LEDGER-PAGE.
           MOVE 'ACCOUNT LEDGER' TO H1-TITLE.
           ADD 1 TO WS-PAGE.
           MOVE WS-PAGE TO H1-PAGE.
           MOVE M-ACCT TO WS-ACCT-SPLIT.
           MOVE WS-ACCT-CLS TO AH-ACCT-CLS.
           MOVE WS-ACCT-SER TO AH-ACCT-SER.
           MOVE M-CLASS TO AH-CLASS.
           MOVE M-NAME TO AH-NAME.
           MOVE M-OPENED TO WS-OPENED.
           MOVE WS-OPENED-Y TO AH-OPENED-Y.
           MOVE WS-OPENED-M TO AH-OPENED-M.
           MOVE WS-OPENED-D TO AH-OPENED-D.
           MOVE M-STATUS TO AH-STATUS.
           WRITE REPORT-REC FROM HDR-1 AFTER ADVANCING TOP-OF-PAGE.
           WRITE REPORT-REC FROM HDR-2 AFTER ADVANCING 1 LINES.
           WRITE REPORT-REC FROM AH-LINE AFTER ADVANCING 2 LINES.
           WRITE REPORT-REC FROM LH-1 AFTER ADVANCING 2 LINES.
           WRITE REPORT-REC FROM LH-2 AFTER ADVANCING 1 LINES.
           MOVE 8 TO WS-LINES.
       LEDGER-PAGE-EXIT.
           EXIT.
       LEDGER-LINE-OUT.
           IF WS-LINES > 55
               MOVE '(CONTINUED)' TO AH-CONT
               PERFORM LEDGER-PAGE THRU LEDGER-PAGE-EXIT.
           WRITE REPORT-REC FROM LD-LINE AFTER ADVANCING 1 LINES.
           ADD 1 TO WS-LINES.
       LEDGER-LINE-OUT-EXIT.
           EXIT.
       LEDGER-BF.
           MOVE SPACES TO LD-LINE.
           MOVE 'BROUGHT FORWARD' TO LD-DESC.
           MOVE WS-PRIOR TO LD-BAL.
           PERFORM LEDGER-LINE-OUT THRU LEDGER-LINE-OUT-EXIT.
       LEDGER-BF-EXIT.
           EXIT.
       LEDGER-END.
           MOVE SPACES TO LD-LINE.
           PERFORM LEDGER-LINE-OUT THRU LEDGER-LINE-OUT-EXIT.
           MOVE 'MONTH TOTAL' TO LD-DESC.
           MOVE WS-INC TO LD-INC.
           MOVE WS-DEC TO LD-DEC.
           PERFORM LEDGER-LINE-OUT THRU LEDGER-LINE-OUT-EXIT.
           MOVE SPACES TO LD-LINE.
           MOVE 'CARRIED FORWARD' TO LD-DESC.
           MOVE WS-CLOSE TO LD-BAL.
           PERFORM LEDGER-LINE-OUT THRU LEDGER-LINE-OUT-EXIT.
      *    MONTH-END BALANCE FOR THE NEXT MONTH
           MOVE WS-CUR TO NB-ACCT.
           MOVE WS-TARGET TO NB-MONTH.
           MOVE WS-PRIOR TO NB-PRIOR.
           MOVE WS-INC TO NB-INC.
           MOVE WS-DEC TO NB-DEC.
           MOVE WS-CLOSE TO NB-CLOSE.
           WRITE NB-OUT FROM NB-REC.
           ADD 1 TO WS-ACCT-CNT.
           ADD WS-PRIOR TO TOT-PRIOR.
           ADD WS-INC TO TOT-INC.
           ADD WS-DEC TO TOT-DEC.
           ADD WS-CLOSE TO TOT-CLOSE.
           ADD WS-MEND TO TOT-MEND.
      *    CHECK 1 FOR THE ACCOUNT, KEPT FOR THE SUMMARY
           IF SM-COUNT NOT < 500
               DISPLAY 'ACCTLDG: MORE THAN 500 ACCOUNTS'
               MOVE 'Y' TO WS-ERROR
               GO TO LEDGER-END-EXIT.
           ADD 1 TO SM-COUNT.
           MOVE SM-COUNT TO WS-I.
           MOVE WS-CUR TO SM-ACCT (WS-I).
           MOVE M-CLASS TO SM-CLASS (WS-I).
           MOVE M-NAME TO SM-NAME (WS-I).
           MOVE WS-PRIOR TO SM-PRIOR (WS-I).
           MOVE WS-INC TO SM-INC (WS-I).
           MOVE WS-DEC TO SM-DEC (WS-I).
           MOVE WS-CLOSE TO SM-CLOSE (WS-I).
           MOVE WS-MEND TO SM-MEND (WS-I).
           IF WS-CLOSE = WS-MEND
               MOVE 'OK' TO SM-CHK (WS-I)
           ELSE
               MOVE 'ERR' TO SM-CHK (WS-I)
               ADD 1 TO WS-MISMATCH
               MOVE 'Y' TO WS-ERROR
               DISPLAY 'ACCTLDG: CARRIED FORWARD IS NOT THE MASTER '
                       WS-CUR.
       LEDGER-END-EXIT.
           EXIT.
      *    AN ACCOUNT OPENED AFTER THE MONTH: ALL ITS ENTRIES ARE AFTER
      *    THE MONTH, SO AT THE END OF THE MONTH ITS BALANCE WAS ZERO
       CHECK-NOT-OPEN.
           IF WS-MEND NOT = ZERO
               DISPLAY 'ACCTLDG: OPENED AFTER THE MONTH BUT HAS A '
                       'MONTH-END BALANCE ' WS-CUR
               ADD 1 TO WS-MISMATCH
               MOVE 'Y' TO WS-ERROR.
       CHECK-NOT-OPEN-EXIT.
           EXIT.
      *
      * REPORT: MONTHLY BALANCES AND THE CHECKS
      *
       PRINT-SUMMARY.
           PERFORM SUMMARY-HEAD THRU SUMMARY-HEAD-EXIT.
           PERFORM SUMMARY-LINE THRU SUMMARY-LINE-EXIT
               VARYING WS-I FROM 1 BY 1 UNTIL WS-I > SM-COUNT.
           MOVE SPACES TO S-ACCT S-CLASS S-CHK.
           MOVE 'TOTAL' TO S-NAME.
           MOVE TOT-PRIOR TO S-PRIOR.
           MOVE TOT-INC TO S-INC.
           MOVE TOT-DEC TO S-DEC.
           MOVE TOT-CLOSE TO S-CLOSE.
           MOVE TOT-MEND TO S-MEND.
           WRITE REPORT-REC FROM BLANK-LINE AFTER ADVANCING 1 LINES.
           WRITE REPORT-REC FROM SM-LINE AFTER ADVANCING 1 LINES.
           MOVE 'M-END = BALANCE OF THE MASTER LESS THE LATER ENTRIES'
               TO M-TEXT.
           WRITE REPORT-REC FROM MSG-LINE AFTER ADVANCING 2 LINES.
      *    CHECK 1: CARRIED FORWARD = MASTER AT THE MONTH END
           MOVE 'CARRIED FORWARD : MASTER AT MONTH END' TO C-TEXT.
           MOVE TOT-CLOSE TO C-LEFT.
           MOVE TOT-MEND TO C-RIGHT.
           IF WS-MISMATCH = ZERO AND TOT-CLOSE = TOT-MEND
               MOVE 'OK' TO C-RESULT
           ELSE
               MOVE 'ERROR' TO C-RESULT
               MOVE 'Y' TO WS-ERROR.
           WRITE REPORT-REC FROM CHECK-LINE AFTER ADVANCING 3 LINES.
      *    CHECK 2: CONTROL ACCOUNT 2100 AT THE MONTH END
           COMPUTE WS-GL-MEND = WS-GL2100 - TOT-AFTER.
           MOVE 'CONTROL ACCOUNT 2100 AT MONTH END : CARRIED' TO C-TEXT.
           MOVE WS-GL-MEND TO C-LEFT.
           MOVE TOT-CLOSE TO C-RIGHT.
           IF WS-GL-MEND = TOT-CLOSE
               MOVE 'OK' TO C-RESULT
           ELSE
               MOVE 'ERROR' TO C-RESULT
               MOVE 'Y' TO WS-ERROR.
           WRITE REPORT-REC FROM CHECK-LINE AFTER ADVANCING 1 LINES.
      *    CHECK 3: ENTRIES SELECTED = ENTRIES SAVED
           MOVE 'ENTRIES SELECTED : SAVED (COUNT)' TO C-TEXT.
           MOVE C-COUNT TO C-LEFT.
           MOVE WS-HIST-CNT TO C-RIGHT.
           IF C-COUNT = WS-HIST-CNT AND C-LATER = WS-LATER-CNT
               MOVE 'OK' TO C-RESULT
           ELSE
               MOVE 'ERROR' TO C-RESULT
               MOVE 'Y' TO WS-ERROR.
           WRITE REPORT-REC FROM CHECK-LINE AFTER ADVANCING 1 LINES.
           MOVE 'ENTRIES SELECTED : SAVED (AMOUNT)' TO C-TEXT.
           MOVE C-AMOUNT TO C-LEFT.
           MOVE WS-HIST-AMT TO C-RIGHT.
           IF C-AMOUNT = WS-HIST-AMT
               MOVE 'OK' TO C-RESULT
           ELSE
               MOVE 'ERROR' TO C-RESULT
               MOVE 'Y' TO WS-ERROR.
           WRITE REPORT-REC FROM CHECK-LINE AFTER ADVANCING 1 LINES.
           IF WS-ERROR = 'Y'
               MOVE '*** CHECK FAILED. THE MONTH IS NOT CLOSED ***'
                   TO M-TEXT
           ELSE
               MOVE 'ALL CHECKS PASSED' TO M-TEXT.
           WRITE REPORT-REC FROM MSG-LINE AFTER ADVANCING 2 LINES.
           WRITE REPORT-REC FROM END-LINE AFTER ADVANCING 3 LINES.
       PRINT-SUMMARY-EXIT.
           EXIT.
       SUMMARY-HEAD.
           MOVE 'MONTHLY BALANCES' TO H1-TITLE.
           ADD 1 TO WS-PAGE.
           MOVE WS-PAGE TO H1-PAGE.
           WRITE REPORT-REC FROM HDR-1 AFTER ADVANCING TOP-OF-PAGE.
           WRITE REPORT-REC FROM HDR-2 AFTER ADVANCING 1 LINES.
           WRITE REPORT-REC FROM SH-1 AFTER ADVANCING 2 LINES.
           WRITE REPORT-REC FROM SH-2 AFTER ADVANCING 1 LINES.
           MOVE 5 TO WS-LINES.
       SUMMARY-HEAD-EXIT.
           EXIT.
       SUMMARY-LINE.
           IF WS-LINES > 55
               PERFORM SUMMARY-HEAD THRU SUMMARY-HEAD-EXIT.
           MOVE SM-ACCT (WS-I) TO WS-ACCT-SPLIT.
           MOVE WS-ACCT-CLS TO S-ACCT-CLS.
           MOVE WS-ACCT-SER TO S-ACCT-SER.
           MOVE SM-CLASS (WS-I) TO S-CLASS.
           MOVE SM-NAME (WS-I) TO S-NAME.
           MOVE SM-PRIOR (WS-I) TO S-PRIOR.
           MOVE SM-INC (WS-I) TO S-INC.
           MOVE SM-DEC (WS-I) TO S-DEC.
           MOVE SM-CLOSE (WS-I) TO S-CLOSE.
           MOVE SM-MEND (WS-I) TO S-MEND.
           MOVE SM-CHK (WS-I) TO S-CHK.
           WRITE REPORT-REC FROM SM-LINE AFTER ADVANCING 1 LINES.
           ADD 1 TO WS-LINES.
       SUMMARY-LINE-EXIT.
           EXIT.
      *
      * BUSINESS DATES: ONLY THE LAST CLOSED MONTH (19-24) CHANGES
      *
       WRITE-BIZ.
           MOVE SPACES TO WS-BIZ.
           READ BIZ-FILE INTO WS-BIZ AT END MOVE SPACES TO WS-BIZ.
           MOVE WS-TARGET TO WS-BIZ-MONTH.
           WRITE NBIZ-OUT FROM WS-BIZ.
       WRITE-BIZ-EXIT.
           EXIT.
