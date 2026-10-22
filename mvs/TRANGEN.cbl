       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANGEN.
      *****************************************************************
      * TRANGEN - GENERATE LEDGER TRANSACTIONS                        *
      *                                                               *
      * INPUT : CTLCARD  - RUN RECORD AND DAY RECORDS (FROM USAGEGET)  *
      *         RATES    - CONVERSION RATES TO RU                     *
      *         ACCTDEF  - ACCOUNT DEFINITIONS                        *
      *         USAGE    - USAGE RECORDS (DATE, CLASS, NAME, USAGE)   *
      * OUTPUT: TRANS    - TRANSACTIONS, ONE PER RECORD:              *
      *                    U = USAGE OF A DAY, CONVERTED TO RU        *
      *                    A = WEEKLY ALLOCATION (ON MONDAYS)         *
      *         REPORT   - UNREGISTERED USERS (SYSOUT=M)              *
      *                                                               *
      * CONVERSION: AMOUNT = USAGE * RATE, TRUNCATED TO A WHOLE RU    *
      * PER RESOURCE. THE RATE IS THE ONE WITH THE LATEST EFFECTIVE   *
      * DATE NOT AFTER THE USAGE DATE.                                *
      *                                                               *
      * RETURN CODE 16 IF A RATE OR A TABLE ENTRY IS MISSING.         *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           C01 IS TOP-OF-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CTL-FILE    ASSIGN TO UT-S-CTLCARD.
           SELECT RATE-FILE   ASSIGN TO UT-S-RATES.
           SELECT ACCT-FILE   ASSIGN TO UT-S-ACCTDEF.
           SELECT USAGE-FILE  ASSIGN TO UT-S-USAGE.
           SELECT TRAN-FILE   ASSIGN TO UT-S-TRANS.
           SELECT REPORT-FILE ASSIGN TO UT-S-REPORT.
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
           05  CTL-FROM-DATE       PIC X(10).
           05  FILLER              PIC X.
           05  CTL-TO-DATE         PIC X(10).
           05  FILLER              PIC X(44).
       01  CTL-DAY-REC.
           05  CD-TAG              PIC X(3).
           05  FILLER              PIC X.
           05  CD-DATE             PIC X(8).
           05  FILLER              PIC X.
           05  CD-MONDAY           PIC X.
           05  FILLER              PIC X(66).
       FD  RATE-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS RATE-REC.
       01  RATE-REC.
           05  R-DATE              PIC X(8).
           05  FILLER              PIC X.
           05  R-RES               PIC X(4).
           05  FILLER              PIC X.
           05  R-INT               PIC 9(5).
           05  FILLER              PIC X.
           05  R-DEC               PIC 9(4).
           05  FILLER              PIC X(56).
      *    THE RECORDS OF AN FD SHARE ONE AREA: THIS VIEWS COLUMN 1
       01  RATE-COMMENT.
           05  R-FLAG              PIC X.
           05  FILLER              PIC X(79).
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
       FD  USAGE-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 10 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS USAGE-REC.
       01  USAGE-REC.
           05  U-DATE              PIC X(8).
           05  U-CLASS             PIC X(5).
           05  U-NAME              PIC X(24).
           05  U-QTY               PIC 9(7)V99 OCCURS 4 TIMES.
           05  FILLER              PIC X(7).
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
       FD  REPORT-FILE
           LABEL RECORDS ARE OMITTED
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS REPORT-REC.
       01  REPORT-REC              PIC X(133).
       WORKING-STORAGE SECTION.
       77  WS-EOF                  PIC X     VALUE 'N'.
       77  WS-ERROR                PIC X     VALUE 'N'.
       77  WS-FOUND                PIC X     VALUE 'N'.
       77  WS-I                    PIC 9(4)  COMP.
       77  WS-J                    PIC 9(4)  COMP.
       77  WS-R                    PIC 9(4)  COMP.
       77  WS-BEST-DATE            PIC X(8).
       77  WS-RATE                 PIC 9(5)V9(4).
       77  WS-AMT                  PIC 9(9).
       77  WS-TOTAL                PIC 9(9).
       77  WS-USAGE-CNT            PIC 9(6)  VALUE ZERO.
       77  WS-ALLOT-CNT            PIC 9(6)  VALUE ZERO.
       77  WS-UNREG-CNT            PIC 9(6)  VALUE ZERO.
      *
      * RESOURCE CODES IN THE ORDER OF THE USAGE FIELDS
      *
       01  RES-CODES               PIC X(16) VALUE 'CPU MEM NET STOR'.
       01  RES-TABLE REDEFINES RES-CODES.
           05  RES-CODE            PIC X(4)  OCCURS 4 TIMES.
       01  RATE-TABLE.
           05  RT-COUNT            PIC 9(4)  COMP VALUE ZERO.
           05  RT-ENTRY            OCCURS 100 TIMES.
               10  RT-DATE         PIC X(8).
               10  RT-RES          PIC X(4).
               10  RT-RATE         PIC 9(5)V9(4).
       01  ACCT-TABLE.
           05  AC-COUNT            PIC 9(4)  COMP VALUE ZERO.
           05  AC-ENTRY            OCCURS 500 TIMES.
               10  AC-NO           PIC X(10).
               10  AC-CLASS        PIC X(5).
               10  AC-NAME         PIC X(24).
               10  AC-ALLOT        PIC 9(9).
       01  DAY-TABLE.
           05  DY-COUNT            PIC 9(4)  COMP VALUE ZERO.
           05  DY-ENTRY            OCCURS 400 TIMES.
               10  DY-DATE         PIC X(8).
               10  DY-MONDAY       PIC X.
       01  WS-RUN-DATE             PIC X(10).
       01  WS-FROM-DATE            PIC X(10).
       01  WS-TO-DATE              PIC X(10).
      *
      * REPORT LINES. EVERY LINE STARTS WITH 1 BYTE RESERVED FOR THE
      * CARRIAGE CONTROL CHARACTER SET BY WRITE ... ADVANCING.
      *
       01  HDR-1.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(33)
                   VALUE ' NUAGE USAGE LEDGER'.
           05  FILLER              PIC X(40)
                   VALUE 'UNREGISTERED USERS'.
           05  FILLER              PIC X(59) VALUE SPACES.
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
       01  HDR-3.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(45)
                   VALUE ' DATE      CLASS  NAME'.
           05  FILLER              PIC X(87) VALUE
               '    CPU-H       MEM-GBH        NET-GB       STOR-GB'.
       01  DETAIL-LINE.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X     VALUE SPACE.
           05  D-DATE              PIC X(8).
           05  FILLER              PIC XX    VALUE SPACES.
           05  D-CLASS             PIC X(5).
           05  FILLER              PIC XX    VALUE SPACES.
           05  D-NAME              PIC X(24).
           05  D-QTY               PIC Z,ZZZ,ZZ9.99 OCCURS 4 TIMES.
           05  FILLER              PIC X(42) VALUE SPACES.
       01  MSG-LINE.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X     VALUE SPACE.
           05  M-TEXT              PIC X(60).
           05  M-VALUE             PIC X(30).
           05  FILLER              PIC X(41) VALUE SPACES.
       01  END-LINE.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(22)
                   VALUE ' *** END OF REPORT ***'.
           05  FILLER              PIC X(5)  VALUE SPACES.
           05  E-COUNT             PIC ZZZ,ZZ9.
           05  FILLER              PIC X(98) VALUE ' UNREGISTERED'.
       PROCEDURE DIVISION.
       MAIN-LINE.
           OPEN INPUT CTL-FILE RATE-FILE ACCT-FILE USAGE-FILE
                OUTPUT TRAN-FILE REPORT-FILE.
           PERFORM LOAD-CTL THRU LOAD-CTL-EXIT.
           PERFORM LOAD-RATES THRU LOAD-RATES-EXIT.
           PERFORM LOAD-ACCTS THRU LOAD-ACCTS-EXIT.
           PERFORM PRINT-HEADER THRU PRINT-HEADER-EXIT.
           MOVE 'N' TO WS-EOF.
           PERFORM READ-USAGE THRU READ-USAGE-EXIT.
           PERFORM CONVERT-USAGE THRU CONVERT-USAGE-EXIT
               UNTIL WS-EOF = 'Y'.
           PERFORM MAKE-ALLOTS THRU MAKE-ALLOTS-EXIT.
           PERFORM PRINT-FOOTER THRU PRINT-FOOTER-EXIT.
           CLOSE CTL-FILE RATE-FILE ACCT-FILE USAGE-FILE
                 TRAN-FILE REPORT-FILE.
           DISPLAY 'TRANGEN USAGE TRANSACTIONS      ' WS-USAGE-CNT.
           DISPLAY 'TRANGEN ALLOCATION TRANSACTIONS ' WS-ALLOT-CNT.
           DISPLAY 'TRANGEN UNREGISTERED USAGE      ' WS-UNREG-CNT.
           IF WS-ERROR = 'Y' MOVE 16 TO RETURN-CODE.
           STOP RUN.
      *
      * CONTROL CARD: THE RUN RECORD, THEN ONE DAY RECORD PER DATE
      *
       LOAD-CTL.
           READ CTL-FILE AT END MOVE 'Y' TO WS-EOF.
           IF WS-EOF = 'Y' OR CTL-TAG NOT = 'RUN'
               DISPLAY 'TRANGEN: NO RUN RECORD IN CTLCARD'
               MOVE 'Y' TO WS-ERROR
               GO TO LOAD-CTL-EXIT.
           MOVE CTL-RUN-DATE TO WS-RUN-DATE.
           MOVE CTL-FROM-DATE TO WS-FROM-DATE.
           MOVE CTL-TO-DATE TO WS-TO-DATE.
       LOAD-CTL-DAY.
           READ CTL-FILE AT END GO TO LOAD-CTL-EXIT.
           IF CD-TAG = 'DAY' AND DY-COUNT < 400
               ADD 1 TO DY-COUNT
               MOVE CD-DATE TO DY-DATE (DY-COUNT)
               MOVE CD-MONDAY TO DY-MONDAY (DY-COUNT).
           GO TO LOAD-CTL-DAY.
       LOAD-CTL-EXIT.
           EXIT.
      *
      * RATES: SKIP COMMENT LINES (* IN COLUMN 1)
      *
       LOAD-RATES.
           READ RATE-FILE AT END GO TO LOAD-RATES-EXIT.
           IF R-DATE = SPACES OR R-FLAG = '*'
               GO TO LOAD-RATES.
           IF RT-COUNT NOT < 100
               DISPLAY 'TRANGEN: TOO MANY RATES'
               MOVE 'Y' TO WS-ERROR
               GO TO LOAD-RATES-EXIT.
           ADD 1 TO RT-COUNT.
           MOVE R-DATE TO RT-DATE (RT-COUNT).
           MOVE R-RES TO RT-RES (RT-COUNT).
           COMPUTE RT-RATE (RT-COUNT) = R-INT + R-DEC / 10000.
           GO TO LOAD-RATES.
       LOAD-RATES-EXIT.
           EXIT.
      *
      * ACCOUNT DEFINITIONS: SKIP COMMENT LINES (* IN COLUMN 1)
      *
       LOAD-ACCTS.
           READ ACCT-FILE AT END GO TO LOAD-ACCTS-EXIT.
           IF A-NO = SPACES OR A-FLAG = '*'
               GO TO LOAD-ACCTS.
           IF AC-COUNT NOT < 500
               DISPLAY 'TRANGEN: TOO MANY ACCOUNTS'
               MOVE 'Y' TO WS-ERROR
               GO TO LOAD-ACCTS-EXIT.
           ADD 1 TO AC-COUNT.
           MOVE A-NO TO AC-NO (AC-COUNT).
           MOVE A-CLASS TO AC-CLASS (AC-COUNT).
           MOVE A-NAME TO AC-NAME (AC-COUNT).
           MOVE A-ALLOT TO AC-ALLOT (AC-COUNT).
           GO TO LOAD-ACCTS.
       LOAD-ACCTS-EXIT.
           EXIT.
       READ-USAGE.
           READ USAGE-FILE AT END MOVE 'Y' TO WS-EOF.
       READ-USAGE-EXIT.
           EXIT.
      *
      * ONE USAGE RECORD: FIND THE ACCOUNT, CONVERT TO RU AND WRITE A
      * U TRANSACTION. A USER WITHOUT AN ACCOUNT GOES TO THE REPORT.
      *
       CONVERT-USAGE.
           MOVE 'N' TO WS-FOUND.
           PERFORM FIND-ACCT THRU FIND-ACCT-EXIT
               VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > AC-COUNT OR WS-FOUND = 'Y'.
           IF WS-FOUND = 'N'
               PERFORM PRINT-UNREG THRU PRINT-UNREG-EXIT
               GO TO CONVERT-USAGE-NEXT.
           SUBTRACT 1 FROM WS-I.
           MOVE AC-NO (WS-I) TO T-ACCT.
           MOVE U-DATE TO T-DATE.
           MOVE 'U' TO T-TYPE.
           MOVE AC-NAME (WS-I) TO T-NAME.
           MOVE AC-CLASS (WS-I) TO T-CLASS.
           MOVE ZERO TO WS-TOTAL.
           PERFORM CONVERT-RESOURCE THRU CONVERT-RESOURCE-EXIT
               VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4.
           MOVE WS-TOTAL TO T-AMOUNT.
           WRITE TRAN-REC.
           ADD 1 TO WS-USAGE-CNT.
       CONVERT-USAGE-NEXT.
           PERFORM READ-USAGE THRU READ-USAGE-EXIT.
       CONVERT-USAGE-EXIT.
           EXIT.
       FIND-ACCT.
           IF AC-CLASS (WS-I) = U-CLASS AND AC-NAME (WS-I) = U-NAME
               MOVE 'Y' TO WS-FOUND.
       FIND-ACCT-EXIT.
           EXIT.
      *
      * RESOURCE WS-J: PICK THE RATE AND ADD THE TRUNCATED AMOUNT
      *
       CONVERT-RESOURCE.
           MOVE U-QTY (WS-J) TO T-QTY (WS-J).
           MOVE LOW-VALUES TO WS-BEST-DATE.
           MOVE ZERO TO WS-RATE.
           PERFORM PICK-RATE THRU PICK-RATE-EXIT
               VARYING WS-R FROM 1 BY 1 UNTIL WS-R > RT-COUNT.
           IF WS-BEST-DATE = LOW-VALUES
               DISPLAY 'TRANGEN: NO RATE FOR ' RES-CODE (WS-J)
                       ' ON ' U-DATE
               MOVE 'Y' TO WS-ERROR
               GO TO CONVERT-RESOURCE-EXIT.
      *    NO ROUNDED PHRASE: THE FRACTION IS TRUNCATED
           COMPUTE WS-AMT = U-QTY (WS-J) * WS-RATE.
           ADD WS-AMT TO WS-TOTAL.
       CONVERT-RESOURCE-EXIT.
           EXIT.
       PICK-RATE.
           IF RT-RES (WS-R) = RES-CODE (WS-J) AND
              RT-DATE (WS-R) NOT > U-DATE AND
              RT-DATE (WS-R) > WS-BEST-DATE
               MOVE RT-DATE (WS-R) TO WS-BEST-DATE
               MOVE RT-RATE (WS-R) TO WS-RATE.
       PICK-RATE-EXIT.
           EXIT.
      *
      * WEEKLY ALLOCATION: AN A TRANSACTION FOR EVERY ACCOUNT ON EVERY
      * MONDAY OF THE PERIOD
      *
       MAKE-ALLOTS.
           PERFORM MAKE-ALLOTS-DAY THRU MAKE-ALLOTS-DAY-EXIT
               VARYING WS-J FROM 1 BY 1 UNTIL WS-J > DY-COUNT.
       MAKE-ALLOTS-EXIT.
           EXIT.
       MAKE-ALLOTS-DAY.
           IF DY-MONDAY (WS-J) NOT = 'M'
               GO TO MAKE-ALLOTS-DAY-EXIT.
           PERFORM MAKE-ALLOT THRU MAKE-ALLOT-EXIT
               VARYING WS-I FROM 1 BY 1 UNTIL WS-I > AC-COUNT.
       MAKE-ALLOTS-DAY-EXIT.
           EXIT.
       MAKE-ALLOT.
           MOVE AC-NO (WS-I) TO T-ACCT.
           MOVE DY-DATE (WS-J) TO T-DATE.
           MOVE 'A' TO T-TYPE.
           MOVE ZERO TO T-QTY (1) T-QTY (2) T-QTY (3) T-QTY (4).
           MOVE AC-ALLOT (WS-I) TO T-AMOUNT.
           MOVE AC-NAME (WS-I) TO T-NAME.
           MOVE AC-CLASS (WS-I) TO T-CLASS.
           WRITE TRAN-REC.
           ADD 1 TO WS-ALLOT-CNT.
       MAKE-ALLOT-EXIT.
           EXIT.
      *
      * REPORT: UNREGISTERED USERS
      *
       PRINT-HEADER.
           MOVE WS-TO-DATE TO H2-TO-DATE H2-PERIOD-TO.
           MOVE WS-FROM-DATE TO H2-FROM-DATE.
           MOVE WS-RUN-DATE TO H2-RUN-DATE.
           WRITE REPORT-REC FROM HDR-1 AFTER ADVANCING TOP-OF-PAGE.
           WRITE REPORT-REC FROM HDR-2 AFTER ADVANCING 1 LINES.
           WRITE REPORT-REC FROM HDR-3 AFTER ADVANCING 2 LINES.
       PRINT-HEADER-EXIT.
           EXIT.
       PRINT-UNREG.
           MOVE U-DATE TO D-DATE.
           MOVE U-CLASS TO D-CLASS.
           MOVE U-NAME TO D-NAME.
           MOVE U-QTY (1) TO D-QTY (1).
           MOVE U-QTY (2) TO D-QTY (2).
           MOVE U-QTY (3) TO D-QTY (3).
           MOVE U-QTY (4) TO D-QTY (4).
           WRITE REPORT-REC FROM DETAIL-LINE AFTER ADVANCING 1 LINES.
           ADD 1 TO WS-UNREG-CNT.
       PRINT-UNREG-EXIT.
           EXIT.
       PRINT-FOOTER.
           IF WS-UNREG-CNT = ZERO
               MOVE 'NO UNREGISTERED USERS' TO M-TEXT
               MOVE SPACES TO M-VALUE
               WRITE REPORT-REC FROM MSG-LINE AFTER ADVANCING 1 LINES.
           MOVE WS-UNREG-CNT TO E-COUNT.
           WRITE REPORT-REC FROM END-LINE AFTER ADVANCING 3 LINES.
       PRINT-FOOTER-EXIT.
           EXIT.
