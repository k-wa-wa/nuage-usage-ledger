       IDENTIFICATION DIVISION.
       PROGRAM-ID. POSTING.
      *****************************************************************
      * POSTING - POST THE TRANSACTIONS TO THE LEDGER (DAILY CLOSING) *
      *                                                               *
      * INPUT : CTLCARD  - RUN RECORD (FROM USAGEGET)                 *
      *         ACCTDEF  - ACCOUNT DEFINITIONS, SORTED BY ACCOUNT NO  *
      *         OLDMAST  - ACCOUNT MASTER (SUBSIDIARY LEDGER), GEN 0  *
      *         TRANS    - TRANSACTIONS, SORTED BY ACCOUNT, DATE, TYPE*
      *         OLDGL    - GENERAL LEDGER, GENERATION 0               *
      *         OLDBIZ   - BUSINESS DATES. COLUMNS 18-80 (THE LAST    *
      *                    MONTHLY CLOSING) ARE CARRIED OVER AS THEY  *
      *                    ARE                                        *
      * OUTPUT: NEWMAST  - ACCOUNT MASTER, NEXT GENERATION            *
      *         JOURNAL  - JOURNAL ENTRIES OF THIS RUN                *
      *         NEWGL    - GENERAL LEDGER, NEXT GENERATION            *
      *         NEWBIZ   - NEXT BUSINESS DATE                         *
      *         REPORT   - ACCOUNT BALANCES, OVERDRAWN ACCOUNTS AND   *
      *                    TRIAL BALANCE (SYSOUT=M)                   *
      *                                                               *
      * THE THREE SORTED INPUTS ACCTDEF, OLDMAST AND TRANS ARE        *
      * MATCHED BY ACCOUNT NUMBER (BALANCED LINE). A DEFINED ACCOUNT  *
      * WITHOUT A MASTER RECORD IS OPENED, AND ITS WEEKLY ALLOCATION  *
      * IS POSTED ONCE AS THE OPENING ALLOCATION.                     *
      *                                                               *
      * JOURNAL ENTRIES (DEBIT / CREDIT):                             *
      *   ALLOCATION (O, A) : 1100 ALLOCATION SOURCE / 2100 ACCOUNTS  *
      *   USAGE      (U)    : 2100 ACCOUNTS / 4100 USAGE              *
      *                                                               *
      * CHECKS: DEBIT TOTAL = CREDIT TOTAL, AND THE CONTROL ACCOUNT   *
      * 2100 = THE TOTAL OF THE SUBSIDIARY LEDGER. IF A CHECK FAILS,  *
      * THE RETURN CODE IS 16 AND THE JOB DOES NOT COMMIT THE NEW     *
      * GENERATIONS NOR THE NEXT BUSINESS DATE.                       *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           C01 IS TOP-OF-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CTL-FILE     ASSIGN TO UT-S-CTLCARD.
           SELECT ACCT-FILE    ASSIGN TO UT-S-ACCTDEF.
           SELECT OLDMAST-FILE ASSIGN TO UT-S-OLDMAST.
           SELECT TRAN-FILE    ASSIGN TO UT-S-TRANS.
           SELECT OLDGL-FILE   ASSIGN TO UT-S-OLDGL.
           SELECT NEWMAST-FILE ASSIGN TO UT-S-NEWMAST.
           SELECT JRNL-FILE    ASSIGN TO UT-S-JOURNAL.
           SELECT NEWGL-FILE   ASSIGN TO UT-S-NEWGL.
           SELECT OLDBIZ-FILE  ASSIGN TO UT-S-OLDBIZ.
           SELECT BIZ-FILE     ASSIGN TO UT-S-NEWBIZ.
           SELECT REPORT-FILE  ASSIGN TO UT-S-REPORT.
       DATA DIVISION.
       FILE SECTION.
       FD  CTL-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS CTL-REC.
       01  CTL-REC.
           05  CTL-TAG             PIC X(3).
           05  FILLER              PIC X.
           05  CTL-RUN-DATE        PIC X(10).
           05  FILLER              PIC X.
           05  CTL-FROM-DATE.
               10  CF-Y            PIC X(4).
               10  FILLER          PIC X.
               10  CF-M            PIC X(2).
               10  FILLER          PIC X.
               10  CF-D            PIC X(2).
           05  FILLER              PIC X.
           05  CTL-TO-DATE.
               10  CT-Y            PIC X(4).
               10  FILLER          PIC X.
               10  CT-M            PIC X(2).
               10  FILLER          PIC X.
               10  CT-D            PIC X(2).
           05  FILLER              PIC X.
           05  CTL-NEXT            PIC X(8).
           05  FILLER              PIC X(35).
       FD  ACCT-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS ACCT-REC.
       01  ACCT-REC.
           05  A-NO                PIC X(10).
           05  FILLER              PIC X.
           05  A-CLASS             PIC X(5).
           05  FILLER              PIC X.
           05  A-NAME              PIC X(24).
           05  FILLER              PIC X.
           05  A-ALLOT             PIC 9(9).
           05  FILLER              PIC X(29).
       01  ACCT-COMMENT.
           05  A-FLAG              PIC X.
           05  FILLER              PIC X(79).
       FD  OLDMAST-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 10 RECORDS
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS OM-REC.
       01  OM-REC.
           05  OM-ACCT             PIC X(10).
           05  FILLER              PIC X(90).
       FD  TRAN-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 10 RECORDS
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS TRAN-REC.
       01  TRAN-REC.
           05  T-ACCT              PIC X(10).
           05  T-DATE              PIC X(8).
           05  T-TYPE              PIC X.
           05  T-QTY               PIC 9(7)V99 OCCURS 4 TIMES.
           05  T-AMOUNT            PIC 9(9).
           05  T-NAME              PIC X(24).
           05  T-CLASS             PIC X(5).
           05  FILLER              PIC X(7).
       FD  OLDGL-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 10 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS OG-REC.
       01  OG-REC.
           05  OG-CODE             PIC X(4).
           05  OG-NAME             PIC X(16).
           05  OG-DEBIT            PIC S9(11) COMP-3.
           05  OG-CREDIT           PIC S9(11) COMP-3.
           05  FILLER              PIC X(48).
      *
      * ACCOUNT MASTER RECORD (SUBSIDIARY LEDGER), 100 BYTES
      *
       FD  NEWMAST-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 10 RECORDS
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS NM-OUT.
      *    AFTER A WRITE THE RECORD AREA OF A BLOCKED FILE MOVES TO THE
      *    NEXT RECORD, SO THE MASTER RECORD IS BUILT IN NM-REC
      *    (WORKING-STORAGE) AND WRITTEN WITH WRITE ... FROM.
       01  NM-OUT                  PIC X(100).
       FD  JRNL-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 10 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS J-REC.
       01  J-REC.
           05  J-DATE              PIC X(8).
           05  J-SEQ               PIC 9(6).
           05  J-DEBIT             PIC X(4).
           05  J-CREDIT            PIC X(4).
           05  J-ACCT              PIC X(10).
           05  J-TYPE              PIC X.
           05  J-AMOUNT            PIC 9(11).
           05  FILLER              PIC X(36).
       FD  NEWGL-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 10 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS NG-REC.
       01  NG-REC.
           05  NG-CODE             PIC X(4).
           05  NG-NAME             PIC X(16).
           05  NG-DEBIT            PIC S9(11) COMP-3.
           05  NG-CREDIT           PIC S9(11) COMP-3.
           05  FILLER              PIC X(48).
       FD  OLDBIZ-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS OB-REC.
       01  OB-REC.
           05  FILLER              PIC X(17).
           05  OB-REST             PIC X(63).
       FD  BIZ-FILE
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS BIZ-REC.
      *    1-8 NEXT BUSINESS DATE, 10-17 LAST CLOSED DATE (THIS PROGRAM)
      *    19-24 LAST CLOSED MONTH (MONTHLY CLOSING, ACCTLDG)
       01  BIZ-REC.
           05  B-NEXT              PIC X(8).
           05  FILLER              PIC X.
           05  B-LAST              PIC X(8).
           05  B-REST              PIC X(63).
       FD  REPORT-FILE
           LABEL RECORDS ARE OMITTED
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS REPORT-REC.
       01  REPORT-REC              PIC X(133).
       WORKING-STORAGE SECTION.
       77  WS-ERROR                PIC X     VALUE 'N'.
       77  WS-MATCH-DEF            PIC X.
       77  WS-MATCH-OM             PIC X.
       77  WS-DEF-KEY              PIC X(10).
       77  WS-PREV-DEF             PIC X(10) VALUE LOW-VALUES.
       77  WS-OM-KEY               PIC X(10).
       77  WS-TR-KEY               PIC X(10).
       77  WS-CUR-KEY              PIC X(10).
       77  WS-AMT                  PIC 9(9).
       77  WS-JDATE                PIC X(8).
       77  WS-JTYPE                PIC X.
       77  WS-JDR                  PIC X(4).
       77  WS-JCR                  PIC X(4).
       77  WS-SEQ                  PIC 9(6)  VALUE ZERO.
       77  WS-I                    PIC 9(4)  COMP.
       77  WS-LINES                PIC 99    VALUE 99.
       77  WS-PAGE                 PIC 9(4)  VALUE ZERO.
       77  WS-ACCT-CNT             PIC 9(6)  VALUE ZERO.
       77  WS-TRAN-CNT             PIC 9(6)  VALUE ZERO.
       77  WS-PRIOR                PIC S9(11) COMP-3.
       77  WS-PALLOC               PIC S9(11) COMP-3.
       77  WS-PUSED                PIC S9(11) COMP-3.
       77  TOT-PRIOR               PIC S9(11) COMP-3 VALUE ZERO.
       77  TOT-ALLOC               PIC S9(11) COMP-3 VALUE ZERO.
       77  TOT-USED                PIC S9(11) COMP-3 VALUE ZERO.
       77  SUB-TOTAL               PIC S9(11) COMP-3 VALUE ZERO.
       77  TOT-DEBIT               PIC S9(11) COMP-3 VALUE ZERO.
       77  TOT-CREDIT              PIC S9(11) COMP-3 VALUE ZERO.
       77  TOT-DR-BAL              PIC S9(11) COMP-3 VALUE ZERO.
       77  TOT-CR-BAL              PIC S9(11) COMP-3 VALUE ZERO.
       77  WS-CONTROL              PIC S9(11) COMP-3 VALUE ZERO.
       77  WS-DIFF                 PIC S9(11) COMP-3.
      *
      * ACCOUNT MASTER RECORD (SUBSIDIARY LEDGER), 100 BYTES
      *
       01  NM-REC.
           05  NM-ACCT             PIC X(10).
           05  NM-CLASS            PIC X(5).
           05  NM-NAME             PIC X(24).
           05  NM-BAL              PIC S9(11) COMP-3.
           05  NM-ALLOC            PIC S9(11) COMP-3.
           05  NM-USED             PIC S9(11) COMP-3.
           05  NM-OPENED           PIC X(8).
           05  NM-LAST             PIC X(8).
      *        A = DEFINED (ACTIVE), U = NOT IN ACCTDEF ANY MORE
           05  NM-STATUS           PIC X.
           05  FILLER              PIC X(26).
       01  WS-FROM-YMD.
           05  WF-Y                PIC X(4).
           05  WF-M                PIC X(2).
           05  WF-D                PIC X(2).
       01  WS-TO-YMD.
           05  WT-Y                PIC X(4).
           05  WT-M                PIC X(2).
           05  WT-D                PIC X(2).
       01  WS-ACCT-SPLIT.
           05  WS-ACCT-CLS         PIC X(3).
           05  WS-ACCT-SER         PIC X(7).
      *
      * GENERAL LEDGER: 1 = 1100 ALLOCATION SOURCE (DEBIT BALANCE),
      * 2 = 2100 ACCOUNTS (CONTROL ACCOUNT, CREDIT BALANCE),
      * 3 = 4100 USAGE (CREDIT BALANCE)
      *
       01  GL-INIT.
           05  FILLER              PIC X(20)
                   VALUE '1100ALLOCATION SRC  '.
           05  FILLER              PIC X(20)
                   VALUE '2100USER ACCOUNTS   '.
           05  FILLER              PIC X(20)
                   VALUE '4100USAGE           '.
       01  GL-INIT-TABLE REDEFINES GL-INIT.
           05  GLI-ENTRY           OCCURS 3 TIMES.
               10  GLI-CODE        PIC X(4).
               10  GLI-NAME        PIC X(16).
       01  GL-TABLE.
           05  GL-ENTRY            OCCURS 3 TIMES.
               10  GL-CODE         PIC X(4).
               10  GL-NAME         PIC X(16).
               10  GL-DEBIT        PIC S9(11) COMP-3.
               10  GL-CREDIT       PIC S9(11) COMP-3.
       01  OD-TABLE.
           05  OD-COUNT            PIC 9(4)  COMP VALUE ZERO.
           05  OD-ENTRY            OCCURS 500 TIMES.
               10  OD-ACCT         PIC X(10).
               10  OD-CLASS        PIC X(5).
               10  OD-NAME         PIC X(24).
               10  OD-BAL          PIC S9(11) COMP-3.
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
           05  FILLER              PIC X(16) VALUE ' BUSINESS DATE: '.
           05  H2-TO-DATE          PIC X(10).
           05  FILLER              PIC X(11) VALUE '  PERIOD: '.
           05  H2-FROM-DATE        PIC X(10).
           05  FILLER              PIC X(3)  VALUE ' - '.
           05  H2-PERIOD-TO        PIC X(10).
           05  FILLER              PIC X(19) VALUE SPACES.
           05  FILLER              PIC X(10) VALUE 'RUN DATE: '.
           05  H2-RUN-DATE         PIC X(10).
           05  FILLER              PIC X(33) VALUE SPACES.
       01  BAL-HDR-1.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(33) VALUE
               ' ACCOUNT NO   CLASS  NAME        '.
           05  FILLER              PIC X(33) VALUE
               '                PRIOR BALANCE    '.
           05  FILLER              PIC X(33) VALUE
               '    ALLOCATED            USED    '.
           05  FILLER              PIC X(33) VALUE
               '     BALANCE  ST                 '.
       01  BAL-HDR-2.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(33) VALUE
               ' -----------  -----  ------------'.
           05  FILLER              PIC X(33) VALUE
               '------------  ---------------  --'.
           05  FILLER              PIC X(33) VALUE
               '------------- --------------- ---'.
           05  FILLER              PIC X(33) VALUE
               '------------  --                 '.
       01  OD-HDR-1.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(33) VALUE
               ' ACCOUNT NO   CLASS  NAME        '.
           05  FILLER              PIC X(33) VALUE
               '                      BALANCE    '.
           05  FILLER              PIC X(33) VALUE
               '                                 '.
           05  FILLER              PIC X(33) VALUE
               '                                 '.
       01  OD-HDR-2.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(33) VALUE
               ' -----------  -----  ------------'.
           05  FILLER              PIC X(33) VALUE
               '------------  ---------------    '.
           05  FILLER              PIC X(33) VALUE
               '                                 '.
           05  FILLER              PIC X(33) VALUE
               '                                 '.
       01  TB-HDR-1.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(33) VALUE
               ' CODE  ACCOUNT               DEBI'.
           05  FILLER              PIC X(33) VALUE
               'T TOTAL     CREDIT TOTAL    DEBIT'.
           05  FILLER              PIC X(33) VALUE
               ' BALANCE   CREDIT BALANCE        '.
           05  FILLER              PIC X(33) VALUE
               '                                 '.
       01  TB-HDR-2.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(33) VALUE
               ' ----  ----------------  --------'.
           05  FILLER              PIC X(33) VALUE
               '-------  ---------------  -------'.
           05  FILLER              PIC X(33) VALUE
               '--------  ---------------        '.
           05  FILLER              PIC X(33) VALUE
               '                                 '.
       01  BAL-LINE.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X     VALUE SPACE.
           05  D-ACCT.
               10  D-ACCT-CLS      PIC X(3).
               10  FILLER          PIC X     VALUE '-'.
               10  D-ACCT-SER      PIC X(7).
           05  FILLER              PIC XX    VALUE SPACES.
           05  D-CLASS             PIC X(5).
           05  FILLER              PIC XX    VALUE SPACES.
           05  D-NAME              PIC X(24).
           05  FILLER              PIC XX    VALUE SPACES.
           05  D-PRIOR             PIC ---,---,---,--9.
           05  FILLER              PIC XX    VALUE SPACES.
           05  D-ALLOC             PIC ---,---,---,--9.
           05  FILLER              PIC X     VALUE SPACES.
           05  D-USED              PIC ---,---,---,--9.
           05  FILLER              PIC X     VALUE SPACES.
           05  D-BAL               PIC ---,---,---,--9.
           05  FILLER              PIC XX    VALUE SPACES.
           05  D-ST                PIC X(4).
           05  FILLER              PIC X(15) VALUE SPACES.
       01  OD-LINE.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X     VALUE SPACE.
           05  O-ACCT.
               10  O-ACCT-CLS      PIC X(3).
               10  FILLER          PIC X     VALUE '-'.
               10  O-ACCT-SER      PIC X(7).
           05  FILLER              PIC XX    VALUE SPACES.
           05  O-CLASS             PIC X(5).
           05  FILLER              PIC XX    VALUE SPACES.
           05  O-NAME              PIC X(24).
           05  FILLER              PIC XX    VALUE SPACES.
           05  O-BAL               PIC ---,---,---,--9.
           05  FILLER              PIC X(70) VALUE SPACES.
       01  TB-LINE.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X     VALUE SPACE.
           05  TB-CODE             PIC X(4).
           05  FILLER              PIC XX    VALUE SPACES.
           05  TB-NAME             PIC X(16).
           05  FILLER              PIC XX    VALUE SPACES.
           05  TB-DEBIT            PIC ---,---,---,--9.
           05  FILLER              PIC XX    VALUE SPACES.
           05  TB-CREDIT           PIC ---,---,---,--9.
           05  FILLER              PIC XX    VALUE SPACES.
           05  TB-DR-BAL           PIC ---,---,---,--9.
           05  FILLER              PIC XX    VALUE SPACES.
           05  TB-CR-BAL           PIC ---,---,---,--9.
           05  FILLER              PIC X(41) VALUE SPACES.
       01  CHECK-LINE.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X     VALUE SPACE.
           05  C-TEXT              PIC X(45).
           05  C-LEFT              PIC ---,---,---,--9.
           05  FILLER              PIC X(3)  VALUE ' : '.
           05  C-RIGHT             PIC ---,---,---,--9.
           05  FILLER              PIC XX    VALUE SPACES.
           05  C-RESULT            PIC X(5).
           05  FILLER              PIC X(46) VALUE SPACES.
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
           OPEN INPUT CTL-FILE ACCT-FILE OLDMAST-FILE TRAN-FILE
                      OLDGL-FILE OLDBIZ-FILE
                OUTPUT NEWMAST-FILE JRNL-FILE NEWGL-FILE BIZ-FILE
                       REPORT-FILE.
           PERFORM LOAD-CTL THRU LOAD-CTL-EXIT.
           PERFORM LOAD-GL THRU LOAD-GL-EXIT.
           PERFORM READ-DEF THRU READ-DEF-EXIT.
           PERFORM READ-OM THRU READ-OM-EXIT.
           PERFORM READ-TR THRU READ-TR-EXIT.
           MOVE 'ACCOUNT BALANCES' TO H1-TITLE.
           PERFORM BAL-HEADER THRU BAL-HEADER-EXIT.
           PERFORM PROCESS-ACCOUNT THRU PROCESS-ACCOUNT-EXIT
               UNTIL WS-DEF-KEY = HIGH-VALUES AND
                     WS-OM-KEY = HIGH-VALUES AND
                     WS-TR-KEY = HIGH-VALUES.
           PERFORM BAL-TOTAL THRU BAL-TOTAL-EXIT.
           PERFORM PRINT-OVERDRAWN THRU PRINT-OVERDRAWN-EXIT.
           PERFORM PRINT-TRIAL THRU PRINT-TRIAL-EXIT.
           PERFORM WRITE-GL THRU WRITE-GL-EXIT.
           PERFORM WRITE-BIZ THRU WRITE-BIZ-EXIT.
           CLOSE CTL-FILE ACCT-FILE OLDMAST-FILE TRAN-FILE OLDGL-FILE
                 OLDBIZ-FILE NEWMAST-FILE JRNL-FILE NEWGL-FILE BIZ-FILE
                 REPORT-FILE.
           DISPLAY 'POSTING ACCOUNTS     ' WS-ACCT-CNT.
           DISPLAY 'POSTING TRANSACTIONS ' WS-TRAN-CNT.
           DISPLAY 'POSTING JOURNAL      ' WS-SEQ.
           IF WS-ERROR = 'Y'
               DISPLAY 'POSTING: CHECK FAILED, NOTHING IS COMMITTED'
               MOVE 16 TO RETURN-CODE.
           STOP RUN.
      *
      * CONTROL CARD: ONLY THE RUN RECORD IS USED
      *
       LOAD-CTL.
           READ CTL-FILE AT END MOVE SPACES TO CTL-REC.
           IF CTL-TAG NOT = 'RUN'
               DISPLAY 'POSTING: NO RUN RECORD IN CTLCARD'
               MOVE 'Y' TO WS-ERROR.
           MOVE CF-Y TO WF-Y.
           MOVE CF-M TO WF-M.
           MOVE CF-D TO WF-D.
           MOVE CT-Y TO WT-Y.
           MOVE CT-M TO WT-M.
           MOVE CT-D TO WT-D.
           MOVE CTL-TO-DATE TO H2-TO-DATE H2-PERIOD-TO.
           MOVE CTL-FROM-DATE TO H2-FROM-DATE.
           MOVE CTL-RUN-DATE TO H2-RUN-DATE.
       LOAD-CTL-EXIT.
           EXIT.
      *
      * GENERAL LEDGER: START FROM ZERO, THEN TAKE THE TOTALS OF THE
      * PREVIOUS GENERATION (EMPTY ON THE FIRST RUN)
      *
       LOAD-GL.
           PERFORM INIT-GL THRU INIT-GL-EXIT
               VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3.
       LOAD-GL-READ.
           READ OLDGL-FILE AT END GO TO LOAD-GL-EXIT.
           PERFORM FIND-GL THRU FIND-GL-EXIT
               VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 3 OR GL-CODE (WS-I) = OG-CODE.
           IF WS-I > 3
               DISPLAY 'POSTING: UNKNOWN GL ACCOUNT ' OG-CODE
               MOVE 'Y' TO WS-ERROR
               GO TO LOAD-GL-READ.
           MOVE OG-DEBIT TO GL-DEBIT (WS-I).
           MOVE OG-CREDIT TO GL-CREDIT (WS-I).
           GO TO LOAD-GL-READ.
       LOAD-GL-EXIT.
           EXIT.
       INIT-GL.
           MOVE GLI-CODE (WS-I) TO GL-CODE (WS-I).
           MOVE GLI-NAME (WS-I) TO GL-NAME (WS-I).
           MOVE ZERO TO GL-DEBIT (WS-I) GL-CREDIT (WS-I).
       INIT-GL-EXIT.
           EXIT.
       FIND-GL.
           EXIT.
       FIND-GL-EXIT.
           EXIT.
      *
      * THE THREE SORTED INPUTS. HIGH-VALUES MARKS THE END OF A FILE.
      *
       READ-DEF.
           READ ACCT-FILE AT END
               MOVE HIGH-VALUES TO WS-DEF-KEY
               GO TO READ-DEF-EXIT.
           IF A-NO = SPACES OR A-FLAG = '*'
               GO TO READ-DEF.
           IF A-NO NOT > WS-PREV-DEF
               DISPLAY 'POSTING: ACCTDEF NOT IN ACCOUNT ORDER ' A-NO
               MOVE 'Y' TO WS-ERROR.
           MOVE A-NO TO WS-DEF-KEY WS-PREV-DEF.
       READ-DEF-EXIT.
           EXIT.
       READ-OM.
           READ OLDMAST-FILE AT END
               MOVE HIGH-VALUES TO WS-OM-KEY
               GO TO READ-OM-EXIT.
           MOVE OM-ACCT TO WS-OM-KEY.
       READ-OM-EXIT.
           EXIT.
       READ-TR.
           READ TRAN-FILE AT END
               MOVE HIGH-VALUES TO WS-TR-KEY
               GO TO READ-TR-EXIT.
           MOVE T-ACCT TO WS-TR-KEY.
       READ-TR-EXIT.
           EXIT.
      *
      * ONE ACCOUNT: THE LOWEST KEY OF THE THREE INPUTS
      *
       PROCESS-ACCOUNT.
           MOVE WS-DEF-KEY TO WS-CUR-KEY.
           IF WS-OM-KEY < WS-CUR-KEY MOVE WS-OM-KEY TO WS-CUR-KEY.
           IF WS-TR-KEY < WS-CUR-KEY MOVE WS-TR-KEY TO WS-CUR-KEY.
           MOVE 'N' TO WS-MATCH-DEF WS-MATCH-OM.
           IF WS-DEF-KEY = WS-CUR-KEY MOVE 'Y' TO WS-MATCH-DEF.
           IF WS-OM-KEY = WS-CUR-KEY MOVE 'Y' TO WS-MATCH-OM.
           IF WS-MATCH-OM = 'Y'
               MOVE OM-REC TO NM-REC
           ELSE
               MOVE SPACES TO NM-REC
               MOVE WS-CUR-KEY TO NM-ACCT
               MOVE ZERO TO NM-BAL NM-ALLOC NM-USED
               MOVE WS-FROM-YMD TO NM-OPENED NM-LAST
               MOVE T-NAME TO NM-NAME
               MOVE T-CLASS TO NM-CLASS.
           IF WS-MATCH-DEF = 'Y'
               MOVE A-CLASS TO NM-CLASS
               MOVE A-NAME TO NM-NAME
               MOVE 'A' TO NM-STATUS
           ELSE
               MOVE 'U' TO NM-STATUS.
           MOVE NM-BAL TO WS-PRIOR.
           MOVE ZERO TO WS-PALLOC WS-PUSED.
           IF WS-MATCH-OM = 'N' AND WS-MATCH-DEF = 'Y'
               MOVE A-ALLOT TO WS-AMT
               MOVE WS-FROM-YMD TO WS-JDATE
               MOVE 'O' TO WS-JTYPE
               PERFORM POST-ALLOC THRU POST-ALLOC-EXIT.
           PERFORM APPLY-TRAN THRU APPLY-TRAN-EXIT
               UNTIL WS-TR-KEY NOT = WS-CUR-KEY.
           WRITE NM-OUT FROM NM-REC.
           ADD 1 TO WS-ACCT-CNT.
           ADD NM-BAL TO SUB-TOTAL.
           PERFORM BAL-LINE-OUT THRU BAL-LINE-OUT-EXIT.
           IF WS-MATCH-DEF = 'Y'
               PERFORM READ-DEF THRU READ-DEF-EXIT.
           IF WS-MATCH-OM = 'Y'
               PERFORM READ-OM THRU READ-OM-EXIT.
       PROCESS-ACCOUNT-EXIT.
           EXIT.
       APPLY-TRAN.
           MOVE T-AMOUNT TO WS-AMT.
           MOVE T-DATE TO WS-JDATE NM-LAST.
           MOVE T-TYPE TO WS-JTYPE.
           IF T-TYPE = 'A'
               PERFORM POST-ALLOC THRU POST-ALLOC-EXIT
           ELSE
               PERFORM POST-USE THRU POST-USE-EXIT.
           ADD 1 TO WS-TRAN-CNT.
           PERFORM READ-TR THRU READ-TR-EXIT.
       APPLY-TRAN-EXIT.
           EXIT.
      *
      * ALLOCATION: DEBIT 1100 ALLOCATION SOURCE / CREDIT 2100 ACCOUNTS
      *
       POST-ALLOC.
           ADD WS-AMT TO NM-BAL NM-ALLOC WS-PALLOC.
           ADD WS-AMT TO GL-DEBIT (1) GL-CREDIT (2).
           MOVE '1100' TO WS-JDR.
           MOVE '2100' TO WS-JCR.
           PERFORM WRITE-JOURNAL THRU WRITE-JOURNAL-EXIT.
       POST-ALLOC-EXIT.
           EXIT.
      *
      * USAGE: DEBIT 2100 ACCOUNTS / CREDIT 4100 USAGE
      *
       POST-USE.
           SUBTRACT WS-AMT FROM NM-BAL.
           ADD WS-AMT TO NM-USED WS-PUSED.
           ADD WS-AMT TO GL-DEBIT (2) GL-CREDIT (3).
           MOVE '2100' TO WS-JDR.
           MOVE '4100' TO WS-JCR.
           PERFORM WRITE-JOURNAL THRU WRITE-JOURNAL-EXIT.
       POST-USE-EXIT.
           EXIT.
       WRITE-JOURNAL.
           ADD 1 TO WS-SEQ.
           MOVE SPACES TO J-REC.
           MOVE WS-JDR TO J-DEBIT.
           MOVE WS-JCR TO J-CREDIT.
           MOVE WS-JDATE TO J-DATE.
           MOVE WS-SEQ TO J-SEQ.
           MOVE WS-CUR-KEY TO J-ACCT.
           MOVE WS-JTYPE TO J-TYPE.
           MOVE WS-AMT TO J-AMOUNT.
           WRITE J-REC.
       WRITE-JOURNAL-EXIT.
           EXIT.
      *
      * REPORT: ACCOUNT BALANCES
      *
       BAL-HEADER.
           ADD 1 TO WS-PAGE.
           MOVE WS-PAGE TO H1-PAGE.
           WRITE REPORT-REC FROM HDR-1 AFTER ADVANCING TOP-OF-PAGE.
           WRITE REPORT-REC FROM HDR-2 AFTER ADVANCING 1 LINES.
           WRITE REPORT-REC FROM BAL-HDR-1 AFTER ADVANCING 2 LINES.
           WRITE REPORT-REC FROM BAL-HDR-2 AFTER ADVANCING 1 LINES.
           MOVE 5 TO WS-LINES.
       BAL-HEADER-EXIT.
           EXIT.
       BAL-LINE-OUT.
           IF WS-LINES > 55
               PERFORM BAL-HEADER THRU BAL-HEADER-EXIT.
           MOVE NM-ACCT TO WS-ACCT-SPLIT.
           MOVE WS-ACCT-CLS TO D-ACCT-CLS.
           MOVE WS-ACCT-SER TO D-ACCT-SER.
           MOVE NM-CLASS TO D-CLASS.
           MOVE NM-NAME TO D-NAME.
           MOVE WS-PRIOR TO D-PRIOR.
           MOVE WS-PALLOC TO D-ALLOC.
           MOVE WS-PUSED TO D-USED.
           MOVE NM-BAL TO D-BAL.
           MOVE SPACES TO D-ST.
           IF NM-STATUS = 'U'
               MOVE 'U' TO D-ST.
           IF NM-BAL < ZERO
               MOVE '*' TO D-ST
               PERFORM ADD-OVERDRAWN THRU ADD-OVERDRAWN-EXIT.
           WRITE REPORT-REC FROM BAL-LINE AFTER ADVANCING 1 LINES.
           ADD 1 TO WS-LINES.
           ADD WS-PRIOR TO TOT-PRIOR.
           ADD WS-PALLOC TO TOT-ALLOC.
           ADD WS-PUSED TO TOT-USED.
       BAL-LINE-OUT-EXIT.
           EXIT.
       ADD-OVERDRAWN.
           IF OD-COUNT < 500
               ADD 1 TO OD-COUNT
               MOVE NM-ACCT TO OD-ACCT (OD-COUNT)
               MOVE NM-CLASS TO OD-CLASS (OD-COUNT)
               MOVE NM-NAME TO OD-NAME (OD-COUNT)
               MOVE NM-BAL TO OD-BAL (OD-COUNT).
       ADD-OVERDRAWN-EXIT.
           EXIT.
       BAL-TOTAL.
           MOVE SPACES TO D-ACCT D-CLASS D-ST.
           MOVE 'TOTAL' TO D-NAME.
           MOVE TOT-PRIOR TO D-PRIOR.
           MOVE TOT-ALLOC TO D-ALLOC.
           MOVE TOT-USED TO D-USED.
           MOVE SUB-TOTAL TO D-BAL.
           WRITE REPORT-REC FROM BLANK-LINE AFTER ADVANCING 1 LINES.
           WRITE REPORT-REC FROM BAL-LINE AFTER ADVANCING 1 LINES.
           MOVE 'ST: * = OVERDRAWN (NEGATIVE BALANCE), U = NOT DEFINED'
               TO M-TEXT.
           WRITE REPORT-REC FROM MSG-LINE AFTER ADVANCING 2 LINES.
       BAL-TOTAL-EXIT.
           EXIT.
      *
      * REPORT: OVERDRAWN ACCOUNTS (NEGATIVE BALANCE)
      *
       PRINT-OVERDRAWN.
           MOVE 'OVERDRAWN ACCOUNTS' TO H1-TITLE.
           ADD 1 TO WS-PAGE.
           MOVE WS-PAGE TO H1-PAGE.
           WRITE REPORT-REC FROM HDR-1 AFTER ADVANCING TOP-OF-PAGE.
           WRITE REPORT-REC FROM HDR-2 AFTER ADVANCING 1 LINES.
           IF OD-COUNT = ZERO
               MOVE 'NO OVERDRAWN ACCOUNTS' TO M-TEXT
               WRITE REPORT-REC FROM MSG-LINE AFTER ADVANCING 2 LINES
               GO TO PRINT-OVERDRAWN-EXIT.
           WRITE REPORT-REC FROM OD-HDR-1 AFTER ADVANCING 2 LINES.
           WRITE REPORT-REC FROM OD-HDR-2 AFTER ADVANCING 1 LINES.
           PERFORM OD-LINE-OUT THRU OD-LINE-OUT-EXIT
               VARYING WS-I FROM 1 BY 1 UNTIL WS-I > OD-COUNT.
       PRINT-OVERDRAWN-EXIT.
           EXIT.
       OD-LINE-OUT.
           MOVE OD-ACCT (WS-I) TO WS-ACCT-SPLIT.
           MOVE WS-ACCT-CLS TO O-ACCT-CLS.
           MOVE WS-ACCT-SER TO O-ACCT-SER.
           MOVE OD-CLASS (WS-I) TO O-CLASS.
           MOVE OD-NAME (WS-I) TO O-NAME.
           MOVE OD-BAL (WS-I) TO O-BAL.
           WRITE REPORT-REC FROM OD-LINE AFTER ADVANCING 1 LINES.
       OD-LINE-OUT-EXIT.
           EXIT.
      *
      * REPORT: TRIAL BALANCE AND THE CHECKS
      *
       PRINT-TRIAL.
           MOVE 'TRIAL BALANCE' TO H1-TITLE.
           ADD 1 TO WS-PAGE.
           MOVE WS-PAGE TO H1-PAGE.
           WRITE REPORT-REC FROM HDR-1 AFTER ADVANCING TOP-OF-PAGE.
           WRITE REPORT-REC FROM HDR-2 AFTER ADVANCING 1 LINES.
           WRITE REPORT-REC FROM TB-HDR-1 AFTER ADVANCING 2 LINES.
           WRITE REPORT-REC FROM TB-HDR-2 AFTER ADVANCING 1 LINES.
           PERFORM TB-LINE-OUT THRU TB-LINE-OUT-EXIT
               VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3.
           MOVE SPACES TO TB-CODE.
           MOVE 'TOTAL' TO TB-NAME.
           MOVE TOT-DEBIT TO TB-DEBIT.
           MOVE TOT-CREDIT TO TB-CREDIT.
           MOVE TOT-DR-BAL TO TB-DR-BAL.
           MOVE TOT-CR-BAL TO TB-CR-BAL.
           WRITE REPORT-REC FROM BLANK-LINE AFTER ADVANCING 1 LINES.
           WRITE REPORT-REC FROM TB-LINE AFTER ADVANCING 1 LINES.
      *    CHECK 1: DEBIT TOTAL = CREDIT TOTAL
           MOVE 'DEBIT TOTAL : CREDIT TOTAL' TO C-TEXT.
           MOVE TOT-DEBIT TO C-LEFT.
           MOVE TOT-CREDIT TO C-RIGHT.
           IF TOT-DEBIT = TOT-CREDIT
               MOVE 'OK' TO C-RESULT
           ELSE
               MOVE 'ERROR' TO C-RESULT
               MOVE 'Y' TO WS-ERROR.
           WRITE REPORT-REC FROM CHECK-LINE AFTER ADVANCING 3 LINES.
      *    CHECK 2: CONTROL ACCOUNT 2100 = SUBSIDIARY LEDGER TOTAL
           COMPUTE WS-CONTROL = GL-CREDIT (2) - GL-DEBIT (2).
           MOVE 'CONTROL ACCOUNT 2100 : SUBSIDIARY LEDGER' TO C-TEXT.
           MOVE WS-CONTROL TO C-LEFT.
           MOVE SUB-TOTAL TO C-RIGHT.
           IF WS-CONTROL = SUB-TOTAL
               MOVE 'OK' TO C-RESULT
           ELSE
               MOVE 'ERROR' TO C-RESULT
               MOVE 'Y' TO WS-ERROR.
           WRITE REPORT-REC FROM CHECK-LINE AFTER ADVANCING 1 LINES.
           IF WS-ERROR = 'Y'
               MOVE '*** CHECK FAILED. THE CLOSING IS NOT COMMITTED ***'
                   TO M-TEXT
           ELSE
               MOVE 'ALL CHECKS PASSED' TO M-TEXT.
           WRITE REPORT-REC FROM MSG-LINE AFTER ADVANCING 2 LINES.
           WRITE REPORT-REC FROM END-LINE AFTER ADVANCING 3 LINES.
       PRINT-TRIAL-EXIT.
           EXIT.
       TB-LINE-OUT.
           MOVE GL-CODE (WS-I) TO TB-CODE.
           MOVE GL-NAME (WS-I) TO TB-NAME.
           MOVE GL-DEBIT (WS-I) TO TB-DEBIT.
           MOVE GL-CREDIT (WS-I) TO TB-CREDIT.
           ADD GL-DEBIT (WS-I) TO TOT-DEBIT.
           ADD GL-CREDIT (WS-I) TO TOT-CREDIT.
           COMPUTE WS-DIFF = GL-DEBIT (WS-I) - GL-CREDIT (WS-I).
           MOVE ZERO TO TB-DR-BAL TB-CR-BAL.
           IF WS-DIFF NOT < ZERO
               MOVE WS-DIFF TO TB-DR-BAL
               ADD WS-DIFF TO TOT-DR-BAL
           ELSE
               COMPUTE WS-DIFF = ZERO - WS-DIFF
               MOVE WS-DIFF TO TB-CR-BAL
               ADD WS-DIFF TO TOT-CR-BAL.
           WRITE REPORT-REC FROM TB-LINE AFTER ADVANCING 1 LINES.
       TB-LINE-OUT-EXIT.
           EXIT.
       WRITE-GL.
           PERFORM WRITE-GL-ONE THRU WRITE-GL-ONE-EXIT
               VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3.
       WRITE-GL-EXIT.
           EXIT.
       WRITE-GL-ONE.
           MOVE SPACES TO NG-REC.
           MOVE GL-CODE (WS-I) TO NG-CODE.
           MOVE GL-NAME (WS-I) TO NG-NAME.
           MOVE GL-DEBIT (WS-I) TO NG-DEBIT.
           MOVE GL-CREDIT (WS-I) TO NG-CREDIT.
           WRITE NG-REC.
       WRITE-GL-ONE-EXIT.
           EXIT.
       WRITE-BIZ.
           READ OLDBIZ-FILE AT END MOVE SPACES TO OB-REC.
           MOVE SPACES TO BIZ-REC.
           MOVE CTL-NEXT TO B-NEXT.
           MOVE WS-TO-YMD TO B-LAST.
           MOVE OB-REST TO B-REST.
           WRITE BIZ-REC.
       WRITE-BIZ-EXIT.
           EXIT.
