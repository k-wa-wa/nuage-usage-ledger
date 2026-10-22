       IDENTIFICATION DIVISION.
       PROGRAM-ID. USAGERPT.
      *****************************************************************
      * NUAGE CLUSTER DAILY RESOURCE USAGE REPORT                     *
      *                                                               *
      * INPUT : CTLCARD  - RUN RECORD (RUN DATE, USAGE DATE) FROM     *
      *                    USAGEGET. DAY RECORDS ARE IGNORED.         *
      *         USAGE    - USAGE RECORDS OF ONE DAY, SORTED BY CLASS  *
      *                    AND NAME                                   *
      * OUTPUT: REPORT   - 132 COLUMN PRINTER REPORT (SYSOUT=M)       *
      *                                                               *
      * A SUBTOTAL IS PRINTED WHEN THE CLASS CHANGES (CONTROL BREAK). *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           C01 IS TOP-OF-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CTL-FILE    ASSIGN TO UT-S-CTLCARD.
           SELECT USAGE-FILE  ASSIGN TO UT-S-USAGE.
           SELECT REPORT-FILE ASSIGN TO UT-S-REPORT.
       DATA DIVISION.
       FILE SECTION.
       FD  CTL-FILE
           LABEL RECORDS ARE OMITTED
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS CTL-REC.
       01  CTL-REC.
           05  CTL-TAG             PIC X(3).
           05  FILLER              PIC X.
           05  CTL-RUN-DATE        PIC X(10).
           05  FILLER              PIC X.
           05  CTL-USAGE-DATE      PIC X(10).
           05  FILLER              PIC X(55).
       FD  USAGE-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 10 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS USAGE-REC.
       01  USAGE-REC.
           05  U-DATE              PIC X(8).
           05  U-CLASS             PIC X(5).
           05  U-NAME              PIC X(24).
           05  U-CPU               PIC 9(7)V99.
           05  U-MEM               PIC 9(7)V99.
           05  U-NET               PIC 9(7)V99.
           05  U-STOR              PIC 9(7)V99.
           05  FILLER              PIC X(7).
       FD  REPORT-FILE
           LABEL RECORDS ARE OMITTED
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS REPORT-REC.
       01  REPORT-REC              PIC X(133).
       WORKING-STORAGE SECTION.
       77  WS-EOF                  PIC X     VALUE 'N'.
       77  WS-PREV-CLASS           PIC X(5)  VALUE SPACES.
       77  WS-LINES                PIC 99    VALUE 99.
       77  WS-PAGE                 PIC 9(4)  VALUE ZERO.
       77  WS-COUNT                PIC 9(4)  VALUE ZERO.
       01  WS-CLASS-TOTAL.
           05  CT-CPU              PIC 9(9)V99 VALUE ZERO.
           05  CT-MEM              PIC 9(9)V99 VALUE ZERO.
           05  CT-NET              PIC 9(9)V99 VALUE ZERO.
           05  CT-STOR             PIC 9(9)V99 VALUE ZERO.
       01  WS-GRAND-TOTAL.
           05  GT-CPU              PIC 9(9)V99 VALUE ZERO.
           05  GT-MEM              PIC 9(9)V99 VALUE ZERO.
           05  GT-NET              PIC 9(9)V99 VALUE ZERO.
           05  GT-STOR             PIC 9(9)V99 VALUE ZERO.
      *
      * EVERY PRINT LINE STARTS WITH 1 BYTE RESERVED FOR THE
      * CARRIAGE CONTROL CHARACTER SET BY WRITE ... ADVANCING.
      *
       01  HDR-1.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(33) VALUE ' NUAGE CLUSTER'.
           05  FILLER              PIC X(40)
                   VALUE 'DAILY RESOURCE USAGE REPORT'.
           05  FILLER              PIC X(16) VALUE SPACES.
           05  FILLER              PIC X(5)  VALUE 'PAGE '.
           05  H1-PAGE             PIC ZZZ9.
           05  FILLER              PIC X(34) VALUE SPACES.
       01  HDR-2.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(13) VALUE ' USAGE DATE: '.
           05  H2-USAGE-DATE       PIC X(10).
           05  FILLER              PIC X(45) VALUE SPACES.
           05  FILLER              PIC X(10) VALUE 'RUN DATE: '.
           05  H2-RUN-DATE         PIC X(10).
           05  FILLER              PIC X(44) VALUE SPACES.
       01  HDR-3.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(33) VALUE
               ' CLASS  NAME                     '.
           05  FILLER              PIC X(33) VALUE
               '         CPU-H         MEM-GBH   '.
           05  FILLER              PIC X(33) VALUE
               '       NET-GB         STOR-GB    '.
           05  FILLER              PIC X(33) VALUE
               '                                 '.
       01  HDR-4.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(33) VALUE
               ' -----  ------------------------ '.
           05  FILLER              PIC X(33) VALUE
               ' -------------  --------------  -'.
           05  FILLER              PIC X(33) VALUE
               '-------------  --------------    '.
           05  FILLER              PIC X(33) VALUE
               '                                 '.
       01  DETAIL-LINE.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X     VALUE SPACE.
           05  D-CLASS             PIC X(5).
           05  FILLER              PIC XX    VALUE SPACES.
           05  D-NAME              PIC X(24).
           05  FILLER              PIC XX    VALUE SPACES.
           05  D-CPU               PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER              PIC XX    VALUE SPACES.
           05  D-MEM               PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER              PIC XX    VALUE SPACES.
           05  D-NET               PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER              PIC XX    VALUE SPACES.
           05  D-STOR              PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER              PIC X(37) VALUE SPACES.
       01  TOTAL-LINE.
           05  FILLER              PIC X     VALUE SPACE.
           05  T-LABEL             PIC X(33).
           05  T-CPU               PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER              PIC XX    VALUE SPACES.
           05  T-MEM               PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER              PIC XX    VALUE SPACES.
           05  T-NET               PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER              PIC XX    VALUE SPACES.
           05  T-STOR              PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER              PIC X(37) VALUE SPACES.
       01  END-LINE.
           05  FILLER              PIC X     VALUE SPACE.
           05  FILLER              PIC X(22)
                   VALUE ' *** END OF REPORT ***'.
           05  FILLER              PIC X(5)  VALUE SPACES.
           05  E-COUNT             PIC ZZZ9.
           05  FILLER              PIC X(101) VALUE ' RECORDS'.
       01  BLANK-LINE.
           05  FILLER              PIC X(133) VALUE SPACES.
       PROCEDURE DIVISION.
       MAIN-LINE.
           OPEN INPUT CTL-FILE USAGE-FILE
                OUTPUT REPORT-FILE.
           READ CTL-FILE AT END MOVE SPACES TO CTL-REC.
           MOVE CTL-USAGE-DATE TO H2-USAGE-DATE.
           MOVE CTL-RUN-DATE TO H2-RUN-DATE.
           PERFORM READ-USAGE THRU READ-USAGE-EXIT.
           PERFORM PROCESS-RECORD UNTIL WS-EOF = 'Y'.
           IF WS-PREV-CLASS NOT = SPACES PERFORM CLASS-BREAK.
           PERFORM GRAND-TOTAL.
           CLOSE CTL-FILE USAGE-FILE REPORT-FILE.
           STOP RUN.
       PROCESS-RECORD.
           IF U-CLASS NOT = WS-PREV-CLASS AND
              WS-PREV-CLASS NOT = SPACES
               PERFORM CLASS-BREAK.
           MOVE U-CLASS TO WS-PREV-CLASS.
           IF WS-LINES > 50 PERFORM PAGE-HEADER.
           MOVE U-CLASS TO D-CLASS.
           MOVE U-NAME TO D-NAME.
           MOVE U-CPU TO D-CPU.
           MOVE U-MEM TO D-MEM.
           MOVE U-NET TO D-NET.
           MOVE U-STOR TO D-STOR.
           WRITE REPORT-REC FROM DETAIL-LINE AFTER ADVANCING 1 LINES.
           ADD 1 TO WS-LINES.
           ADD 1 TO WS-COUNT.
           ADD U-CPU TO CT-CPU.
           ADD U-MEM TO CT-MEM.
           ADD U-NET TO CT-NET.
           ADD U-STOR TO CT-STOR.
           PERFORM READ-USAGE THRU READ-USAGE-EXIT.
       READ-USAGE.
           READ USAGE-FILE AT END MOVE 'Y' TO WS-EOF.
       READ-USAGE-EXIT.
           EXIT.
       PAGE-HEADER.
           ADD 1 TO WS-PAGE.
           MOVE WS-PAGE TO H1-PAGE.
           WRITE REPORT-REC FROM HDR-1 AFTER ADVANCING TOP-OF-PAGE.
           WRITE REPORT-REC FROM HDR-2 AFTER ADVANCING 1 LINES.
           WRITE REPORT-REC FROM HDR-3 AFTER ADVANCING 2 LINES.
           WRITE REPORT-REC FROM HDR-4 AFTER ADVANCING 1 LINES.
           MOVE 5 TO WS-LINES.
       CLASS-BREAK.
           MOVE SPACES TO T-LABEL.
           MOVE '                  * CLASS TOTAL ' TO T-LABEL.
           MOVE CT-CPU TO T-CPU.
           MOVE CT-MEM TO T-MEM.
           MOVE CT-NET TO T-NET.
           MOVE CT-STOR TO T-STOR.
           WRITE REPORT-REC FROM TOTAL-LINE AFTER ADVANCING 1 LINES.
           WRITE REPORT-REC FROM BLANK-LINE AFTER ADVANCING 1 LINES.
           ADD 2 TO WS-LINES.
           ADD CT-CPU TO GT-CPU.
           ADD CT-MEM TO GT-MEM.
           ADD CT-NET TO GT-NET.
           ADD CT-STOR TO GT-STOR.
           MOVE ZERO TO CT-CPU CT-MEM CT-NET CT-STOR.
       GRAND-TOTAL.
           IF WS-LINES > 50 PERFORM PAGE-HEADER.
           MOVE '                 ** GRAND TOTAL ' TO T-LABEL.
           MOVE GT-CPU TO T-CPU.
           MOVE GT-MEM TO T-MEM.
           MOVE GT-NET TO T-NET.
           MOVE GT-STOR TO T-STOR.
           WRITE REPORT-REC FROM TOTAL-LINE AFTER ADVANCING 1 LINES.
           MOVE WS-COUNT TO E-COUNT.
           WRITE REPORT-REC FROM END-LINE AFTER ADVANCING 3 LINES.
