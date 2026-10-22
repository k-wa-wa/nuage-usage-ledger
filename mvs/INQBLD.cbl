       IDENTIFICATION DIVISION.
       PROGRAM-ID. INQBLD.
      *****************************************************************
      * INQBLD - BUILD THE INQUIRY FILES FOR THE ONLINE (KICKS)       *
      *                                                               *
      * THE ONLINE ONLY READS. IT NEVER UPDATES THE LEDGER. AFTER     *
      * EVERY CLOSING THE BATCH COPIES THE LEDGER INTO KEYED FILES    *
      * THAT THE ONLINE CAN READ BY ACCOUNT NUMBER.                   *
      *                                                               *
      * INPUT : BIZDATE  - BUSINESS DATES (LAST CLOSED DATE)          *
      *         MASTER   - ACCOUNT MASTER, GENERATION 0               *
      *         JOURNAL  - JOURNAL, ALL GENERATIONS (GDG BASE NAME)   *
      * OUTPUT: IMAST    - INQUIRY MASTER, 100 BYTES, KEY 1-10        *
      *                    A CONTROL RECORD (KEY 0000000000) FIRST,   *
      *                    THEN THE MASTER RECORDS AS THEY ARE        *
      *         IJRN     - INQUIRY JOURNAL, 104 BYTES, KEY 1-24       *
      *                    ACCOUNT NO + INVERTED DATE + INVERTED SEQ  *
      *                    + THE JOURNAL RECORD AS IT IS. NOT SORTED  *
      *                                                               *
      * THE DATE AND THE SEQUENCE NUMBER ARE INVERTED (99999999 -     *
      * DATE) SO THAT THE NEWEST ENTRY OF AN ACCOUNT COMES FIRST IN   *
      * KEY ORDER. THE ONLINE READS FORWARD ONLY (READNEXT).          *
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BIZ-FILE     ASSIGN TO UT-S-BIZDATE.
           SELECT MAST-FILE    ASSIGN TO UT-S-MASTER.
           SELECT JRNL-FILE    ASSIGN TO UT-S-JOURNAL.
           SELECT IMAST-FILE   ASSIGN TO UT-S-IMAST.
           SELECT IJRN-FILE    ASSIGN TO UT-S-IJRN.
       DATA DIVISION.
       FILE SECTION.
       FD  BIZ-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS BIZ-REC.
       01  BIZ-REC.
           05  B-NEXT              PIC X(8).
           05  FILLER              PIC X.
           05  B-LAST              PIC X(8).
           05  FILLER              PIC X(63).
       FD  MAST-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 10 RECORDS
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS M-REC.
       01  M-REC                   PIC X(100).
       FD  JRNL-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 10 RECORDS
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS J-REC.
       01  J-REC.
           05  J-DATE              PIC 9(8).
           05  J-SEQ               PIC 9(6).
           05  J-DEBIT             PIC X(4).
           05  J-CREDIT            PIC X(4).
           05  J-ACCT              PIC X(10).
           05  J-TYPE              PIC X.
           05  J-AMOUNT            PIC 9(11).
           05  FILLER              PIC X(36).
       01  J-ALL                   PIC X(80).
       FD  IMAST-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 10 RECORDS
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS IM-OUT.
       01  IM-OUT                  PIC X(100).
       FD  IJRN-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 10 RECORDS
           RECORD CONTAINS 104 CHARACTERS
           DATA RECORD IS IJ-OUT.
       01  IJ-OUT                  PIC X(104).
       WORKING-STORAGE SECTION.
       77  WS-EOF                  PIC X     VALUE 'N'.
       77  WS-ACCT-CNT             PIC 9(6)  VALUE ZERO.
       77  WS-JRNL-CNT             PIC 9(8)  VALUE ZERO.
      *
      * CONTROL RECORD OF THE INQUIRY MASTER (KEY 0000000000).
      * IT TELLS THE ONLINE AS OF WHICH CLOSING THE DATA IS.
      *
       01  CTL-REC.
           05  C-KEY               PIC X(10) VALUE '0000000000'.
           05  C-TAG               PIC X(5)  VALUE '*CTL*'.
           05  C-ASOF              PIC X(8).
           05  C-BUILT-DATE        PIC X(8).
           05  C-BUILT-TIME        PIC X(6).
           05  C-ACCT-CNT          PIC 9(6).
           05  C-JRNL-CNT          PIC 9(8).
           05  FILLER              PIC X(49) VALUE SPACES.
      *
      * INQUIRY JOURNAL RECORD, 104 BYTES
      *
       01  IJ-REC.
           05  IJ-ACCT             PIC X(10).
           05  IJ-RDATE            PIC 9(8).
           05  IJ-RSEQ             PIC 9(6).
           05  IJ-JREC             PIC X(80).
      *
      * CURRENT-DATE IS MM/DD/YY, TIME-OF-DAY IS HHMMSS
      *
       01  WS-TODAY.
           05  WS-T-MM             PIC XX.
           05  FILLER              PIC X.
           05  WS-T-DD             PIC XX.
           05  FILLER              PIC X.
           05  WS-T-YY             PIC XX.
       01  WS-BUILT.
           05  FILLER              PIC XX    VALUE '20'.
           05  WS-B-YY             PIC XX.
           05  WS-B-MM             PIC XX.
           05  WS-B-DD             PIC XX.
       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN INPUT  BIZ-FILE MAST-FILE JRNL-FILE
                OUTPUT IMAST-FILE IJRN-FILE.
           MOVE SPACES TO C-ASOF.
           READ BIZ-FILE AT END MOVE SPACES TO BIZ-REC.
           MOVE B-LAST TO C-ASOF.
      *    THE MASTER IS IN ACCOUNT ORDER, SO IT IS COPIED AS IT IS.
      *    IT IS READ TWICE: FIRST FOR THE COUNT IN THE CONTROL
      *    RECORD, WHICH MUST COME FIRST IN KEY ORDER.
           PERFORM 1000-COUNT-MASTER THRU 1000-EXIT
               UNTIL WS-EOF = 'Y'.
           CLOSE MAST-FILE.
           MOVE 'N' TO WS-EOF.
           PERFORM 2000-BUILD-JOURNAL THRU 2000-EXIT
               UNTIL WS-EOF = 'Y'.
           MOVE CURRENT-DATE TO WS-TODAY.
           MOVE WS-T-YY TO WS-B-YY.
           MOVE WS-T-MM TO WS-B-MM.
           MOVE WS-T-DD TO WS-B-DD.
           MOVE WS-BUILT TO C-BUILT-DATE.
           MOVE TIME-OF-DAY TO C-BUILT-TIME.
           MOVE WS-ACCT-CNT TO C-ACCT-CNT.
           MOVE WS-JRNL-CNT TO C-JRNL-CNT.
           WRITE IM-OUT FROM CTL-REC.
           OPEN INPUT MAST-FILE.
           MOVE 'N' TO WS-EOF.
           PERFORM 3000-COPY-MASTER THRU 3000-EXIT
               UNTIL WS-EOF = 'Y'.
           CLOSE BIZ-FILE MAST-FILE JRNL-FILE IMAST-FILE IJRN-FILE.
           DISPLAY 'INQBLD AS OF CLOSING  ' C-ASOF.
           DISPLAY 'INQBLD ACCOUNTS       ' WS-ACCT-CNT.
           DISPLAY 'INQBLD JOURNAL        ' WS-JRNL-CNT.
           STOP RUN.

       1000-COUNT-MASTER.
           READ MAST-FILE AT END MOVE 'Y' TO WS-EOF.
           IF WS-EOF = 'N' ADD 1 TO WS-ACCT-CNT.
       1000-EXIT.
           EXIT.

       2000-BUILD-JOURNAL.
           READ JRNL-FILE AT END MOVE 'Y' TO WS-EOF.
           IF WS-EOF = 'Y' GO TO 2000-EXIT.
           MOVE J-ACCT TO IJ-ACCT.
           SUBTRACT J-DATE FROM 99999999 GIVING IJ-RDATE.
           SUBTRACT J-SEQ FROM 999999 GIVING IJ-RSEQ.
           MOVE J-ALL TO IJ-JREC.
           WRITE IJ-OUT FROM IJ-REC.
           ADD 1 TO WS-JRNL-CNT.
       2000-EXIT.
           EXIT.

       3000-COPY-MASTER.
           READ MAST-FILE AT END MOVE 'Y' TO WS-EOF.
           IF WS-EOF = 'Y' GO TO 3000-EXIT.
           WRITE IM-OUT FROM M-REC.
       3000-EXIT.
           EXIT.
