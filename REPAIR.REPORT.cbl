       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RPRREPORT.
       AUTHOR.        Grant Heath.
       DATE-WRITTEN.  2026-4-23.

      * Description:
      * This script generates a formatted report of records using
      * the previously defined REPAIR data file from repo 
      *	"COBOL-Split-Valid-Insurance-Claim"
      *
      * Each page has a maximum of 20 records
      *
      * Files:
      * Input file: A7.REPAIR.OUT (REPL-DATA-IN)
      * Output file: A8.SUMMARY.RPT (REPL-REPORT)
      

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPR-DATA-IN   ASSIGN TO REPIN
                                ORGANIZATION IS SEQUENTIAL.
           SELECT RPR-REPORT    ASSIGN TO REPOUT
                                ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  RPR-DATA-IN
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
       01 RPR-REC-IN.
          05 IR-POLICY-NO           PIC X(10).
          05 IR-CUST-NAME           PIC X(20).
          05 IR-PROD-CODE           PIC X(3).
          05 IR-CLAIM-TYPE          PIC X(7).
          05 IR-AMOUNT              PIC 9(4)V99.
          05 IR-JUSTIFICATION       PIC X(30).
          05 FILLER                 PIC X(4).

       FD  RPR-REPORT
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS.
       01 REPORT-LINE             PIC X(132).

       WORKING-STORAGE SECTION.

       77 WS-DEDUCT-RATE          PIC V99         VALUE .08.
       77 WS-LINES-PER-PAGE       PIC 9(2)        VALUE 20.

       01 WS-FLAGS.
          05 WS-EOF               PIC X           VALUE 'N'.
             88 EOF-YES                           VALUE 'Y'.

       01 WS-COUNTERS.
          05 WS-PAGE-COUNT        PIC 9(3)        VALUE 0.
          05 WS-LINE-COUNT        PIC 9(2)        VALUE 99.
          05 WS-RPR-TOTAL-COUNT   PIC 9(5)        VALUE 0.
          05 WS-TOTAL-AMT         PIC 9(7)V99     VALUE 0.
          05 WS-TOTAL-DEDUCT      PIC 9(7)V99     VALUE 0.

       01 WS-REGION-TABLE.
          05 WS-REGION-ENTRY OCCURS 4 TIMES INDEXED BY REG-IDX.
             10 WS-REGION-COUNT   PIC 9(4)        VALUE 0.

       01 WS-CALCS.
          05 WS-DEDUCTIBLE-OWING  PIC 9(4)V99     VALUE 0.
          05 WS-CURRENT-REGION    PIC 9           VALUE 0.
          05 WS-REG-NAME-TEMP     PIC X(15)       VALUE SPACES.

       01 WS-BLANK-LINE           PIC X(132)      VALUE SPACES.

       01 WS-HEADING-1.
          05 FILLER               PIC X(53)       VALUE SPACES.
          05 FILLER               PIC X(25)       VALUE
                                  "REPAIR CLAIMS REPORT     ".
          05 FILLER               PIC X(44)       VALUE SPACES.
          05 FILLER               PIC X(7)        VALUE "PAGE:  ".
          05 WS-RPT-PAGE          PIC ZZ9.

       01 WS-HEADING-2.
          05 FILLER               PIC X(15)       VALUE "POLICY #".
          05 FILLER               PIC X(25)       VALUE "CUSTOMER NAME".
          05 FILLER               PIC X(8)        VALUE "PRD".
          05 FILLER               PIC X(12)       VALUE "CLAIM".
          05 FILLER               PIC X(15)       VALUE "AMOUNT".
          05 FILLER               PIC X(15)       VALUE "DEDUCTIBLE".
          05 FILLER               PIC X(33)       VALUE
                                  " JUSTIFICATION".

       01 WS-DETAIL-LINE.
          05 DET-POLICY           PIC X(10).
          05 FILLER               PIC X(5)        VALUE SPACES.
          05 DET-NAME             PIC X(20).
          05 FILLER               PIC X(5)        VALUE SPACES.
          05 DET-PROD             PIC X(3).
          05 FILLER               PIC X(5)        VALUE SPACES.
          05 DET-TYPE             PIC X(7).
          05 FILLER               PIC X(5)        VALUE SPACES.
          05 DET-AMT              PIC Z,ZZ9.99.
          05 FILLER               PIC X(8)        VALUE SPACES.
          05 DET-DEDUCT           PIC Z,ZZ9.99.
          05 FILLER               PIC X(8)        VALUE SPACES.
          05 DET-JUST             PIC X(30).

       01 WS-SUM-LINE-ONE         PIC X(132)      VALUE ALL "-".

       01 WS-SUM-TITLE.
          05 FILLER               PIC X(56)       VALUE SPACES.
          05 FILLER               PIC X(20)       VALUE
                                  "REPAIR SUMMARY".

       01 WS-SUM-TOTALS.
          05 FILLER               PIC X(25)       VALUE
                                  "TOTAL REPAIR RECORDS: ".
          05 SUM-COUNT            PIC ZZ,ZZ9.
          05 FILLER               PIC X(10)       VALUE SPACES.
          05 FILLER               PIC X(15)       VALUE
                                  "TOTAL AMOUNT:  ".
          05 SUM-AMT              PIC $ZZZ,ZZ9.99.

       01 WS-SUM-DEDUCT.
          05 FILLER               PIC X(25)       VALUE
                                  "TOTAL DEDUCTIBLE OWING:".
          05 SUM-DEDUCT-TOT       PIC $ZZZ,ZZ9.99.

       01 WS-REG-COUNT-LINE.
          05 FILLER               PIC X(15)       VALUE "REGION: ".
          05 SUM-REG-NAME         PIC X(15).
          05 FILLER               PIC X(15)       VALUE "COUNT: ".
          05 SUM-REG-QTY          PIC ZZ9.

       PROCEDURE DIVISION.

       000-MAIN.
           PERFORM 100-INITIALIZE.
           PERFORM 200-PROCESS-LOOP UNTIL EOF-YES.
           PERFORM 300-FINAL.
           GOBACK.

       100-INITIALIZE.
           OPEN INPUT RPR-DATA-IN
                OUTPUT RPR-REPORT.
           PERFORM 210-READ-RECORD.

       200-PROCESS-LOOP.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 220-PRINT-HEADERS
           END-IF.

           COMPUTE WS-DEDUCTIBLE-OWING ROUNDED =
              IR-AMOUNT * WS-DEDUCT-RATE.

           ADD 1 TO WS-RPR-TOTAL-COUNT.
           ADD IR-AMOUNT TO WS-TOTAL-AMT.
           ADD WS-DEDUCTIBLE-OWING TO WS-TOTAL-DEDUCT.

           EVALUATE IR-POLICY-NO(1:1)
              WHEN 'O' MOVE 1 TO WS-CURRENT-REGION
              WHEN 'Q' MOVE 2 TO WS-CURRENT-REGION
              WHEN 'M' MOVE 3 TO WS-CURRENT-REGION
              WHEN 'A' MOVE 4 TO WS-CURRENT-REGION
              WHEN OTHER MOVE 1 TO WS-CURRENT-REGION
           END-EVALUATE.

           ADD 1 TO WS-REGION-COUNT(WS-CURRENT-REGION).

           MOVE IR-POLICY-NO TO DET-POLICY.
           MOVE IR-CUST-NAME TO DET-NAME.
           MOVE IR-PROD-CODE TO DET-PROD.
           MOVE IR-CLAIM-TYPE TO DET-TYPE.
           MOVE IR-AMOUNT TO DET-AMT.
           MOVE WS-DEDUCTIBLE-OWING TO DET-DEDUCT.
           MOVE IR-JUSTIFICATION TO DET-JUST.

           WRITE REPORT-LINE FROM WS-DETAIL-LINE.
           ADD 1 TO WS-LINE-COUNT.

           PERFORM 210-READ-RECORD.

       210-READ-RECORD.
           READ RPR-DATA-IN
           AT END
              SET EOF-YES TO TRUE
           END-READ.

       220-PRINT-HEADERS.
           ADD 1 TO WS-PAGE-COUNT.
           MOVE WS-PAGE-COUNT TO WS-RPT-PAGE.
           IF WS-PAGE-COUNT > 1
              WRITE REPORT-LINE FROM WS-BLANK-LINE
              WRITE REPORT-LINE FROM WS-BLANK-LINE
           END-IF.
           WRITE REPORT-LINE FROM WS-HEADING-1.
           WRITE REPORT-LINE FROM WS-BLANK-LINE.
           WRITE REPORT-LINE FROM WS-HEADING-2.
           WRITE REPORT-LINE FROM WS-BLANK-LINE.
           MOVE 0 TO WS-LINE-COUNT.

       300-FINAL.
           PERFORM 310-SUMMARY-TOTALS.
           CLOSE RPR-DATA-IN RPR-REPORT.

       310-SUMMARY-TOTALS.
           WRITE REPORT-LINE FROM WS-BLANK-LINE.
           WRITE REPORT-LINE FROM WS-SUM-LINE-ONE.
           WRITE REPORT-LINE FROM WS-SUM-TITLE.
           WRITE REPORT-LINE FROM WS-BLANK-LINE.

           MOVE WS-RPR-TOTAL-COUNT TO SUM-COUNT.
           MOVE WS-TOTAL-AMT TO SUM-AMT.
           WRITE REPORT-LINE FROM WS-SUM-TOTALS.

           MOVE WS-TOTAL-DEDUCT TO SUM-DEDUCT-TOT.
           WRITE REPORT-LINE FROM WS-SUM-DEDUCT.
           WRITE REPORT-LINE FROM WS-BLANK-LINE.

           PERFORM VARYING REG-IDX FROM 1 BY 1 UNTIL REG-IDX > 4
               SET WS-CURRENT-REGION TO REG-IDX
               PERFORM 320-GET-REG-NAME
               MOVE WS-REG-NAME-TEMP TO SUM-REG-NAME
               MOVE WS-REGION-COUNT(REG-IDX) TO SUM-REG-QTY
               WRITE REPORT-LINE FROM WS-REG-COUNT-LINE
           END-PERFORM.

       320-GET-REG-NAME.
           EVALUATE WS-CURRENT-REGION
                WHEN 1 MOVE "ONTARIO"   TO WS-REG-NAME-TEMP
                WHEN 2 MOVE "QUEBEC"    TO WS-REG-NAME-TEMP
                WHEN 3 MOVE "MARITIMES" TO WS-REG-NAME-TEMP
                WHEN 4 MOVE "ALBERTA"   TO WS-REG-NAME-TEMP
           END-EVALUATE.

       END PROGRAM RPRREPORT.
