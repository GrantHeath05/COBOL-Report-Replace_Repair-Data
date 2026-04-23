       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RPLREPORT.
       AUTHOR.        Grant Heath.
       DATE-WRITTEN.  2026-04-12.
      *****************************************************************
      * Description:
      * This script generates a formatted report of records using
      * the previously defined REPLACE data file from
      *	"COBOL-Split-Valid-Insurance-Claim"
      *
      * Each page has a maximum of 20 records
      *
      * Files:
      * Input file: RAW.REPLACE.OUT (REPL-DATA-IN)
      * Output file: REPLACE.SUMMARY.OUT (REPL-REPORT)
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPL-DATA-IN  ASSIGN TO REPIN
                                ORGANIZATION IS SEQUENTIAL.
           SELECT REPL-REPORT   ASSIGN TO REPOUT
                                ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

      * REPLACE DATA FILE
       FD  REPL-DATA-IN
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
       01 REPL-REC-IN.
          05 IR-POLICY-NO          PIC X(10).
          05 IR-CUST-NAME          PIC X(20).
          05 IR-PROD-CODE          PIC X(3).
          05 IR-CLAIM-TYPE         PIC X(7).
          05 IR-AMOUNT             PIC 9(4)V99.
          05 IR-JUSTIFICATION      PIC X(30).
          05 FILLER                PIC X(4).

       FD  REPL-REPORT
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS.
       01 REPORT-LINE             PIC X(132).

       WORKING-STORAGE SECTION.

      *---------------------------------------------------------------*
      * 77 LEVEL NAMED CONSTANTS
      *---------------------------------------------------------------*
       77 WS-DEDUCT-RATE          PIC V99         VALUE .08.
       77 WS-LINES-PER-PAGE       PIC 9(2)        VALUE 20.
       77 WS-REG-ONT              PIC X           VALUE 'O'.
       77 WS-REG-QUE              PIC X           VALUE 'Q'.
       77 WS-REG-MAR              PIC X           VALUE 'M'.
       77 WS-REG-ALB              PIC X           VALUE 'A'.

      *---------------------------------------------------------------*
      * VARIABLE STRUCTURES
      *---------------------------------------------------------------*
       01 WS-FLAGS.
          05 WS-EOF               PIC X           VALUE 'N'.
             88 EOF-YES                           VALUE 'Y'.

       01 WS-COUNTERS.
          05 WS-PAGE-COUNT        PIC 9(3)        VALUE 0.
          05 WS-LINE-COUNT        PIC 9(2)        VALUE 99.
          05 WS-REPLACE-COUNT     PIC 9(5)        VALUE 0.
          05 WS-TOTAL-AMT         PIC 9(7)V99     VALUE 0.
          05 WS-TOTAL-DEDUCT      PIC 9(7)V99     VALUE 0.

       01 WS-PRODUCT-COUNTS.
          05 WS-FRIDGE-CT         PIC 9(4)        VALUE 0.
          05 WS-STOVE-CT          PIC 9(4)        VALUE 0.
          05 WS-WASHER-CT         PIC 9(4)        VALUE 0.
          05 WS-AC-CT             PIC 9(4)        VALUE 0.
          05 WS-OTHER-CT          PIC 9(4)        VALUE 0.

       01 WS-REGION-TABLE.
          05 WS-REGION-ENTRY OCCURS 4 TIMES INDEXED BY REG-IDX.
             10 WS-REG-TOTAL      PIC 9(7)V99     VALUE 0.

       01 WS-REG-HI-LO.
          05 WS-HI-AMT            PIC 9(7)V99     VALUE 0.
          05 WS-LO-AMT            PIC 9(7)V99     VALUE 9999999.99.
          05 WS-HI-REG            PIC 9           VALUE 0.
          05 WS-LO-REG            PIC 9           VALUE 0.
          05 WS-REG-NAME-TEMP     PIC X(15)       VALUE SPACES.

       01 WS-CALCS.
          05 WS-DEDUCTIBLE-OWING  PIC 9(4)V99     VALUE 0.
          05 WS-CURRENT-REGION    PIC 9           VALUE 0.
          05 WS-PCT               PIC 9(3)V99     VALUE 0.

       01 WS-BLANK-LINE           PIC X(132)      VALUE SPACES.

      *---------------------------------------------------------------*
      * REPORT LAYOUTS
      *---------------------------------------------------------------*
       01 WS-HEADING-1.
          05 FILLER               PIC X(53)       VALUE SPACES.
          05 FILLER               PIC X(25)       VALUE
                "REPLACEMENT CLAIMS REPORT".
          05 FILLER               PIC X(44)       VALUE SPACES.
          05 FILLER               PIC X(7)        VALUE "PAGE:  ".
          05 WS-RPT-PAGE          PIC ZZ9.

       01 WS-HEADING-2.
          05 FILLER               PIC X(15)       VALUE
                "POLICY #".
          05 FILLER               PIC X(25)       VALUE
                "CUSTOMER NAME".
          05 FILLER               PIC X(8)        VALUE
                "PRD".
          05 FILLER               PIC X(12)       VALUE
                "CLAIM".
          05 FILLER               PIC X(15)       VALUE
                "AMOUNT".
          05 FILLER               PIC X(15)       VALUE
                "DEDUCTIBLE".
          05 FILLER               PIC X(33)       VALUE
                " JUSTIFICATION".

      * Report details
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

      * Summary Header Line 1 (line)
       01 WS-SUM-LINE-ONE.
          05 FILLER               PIC X(132)      VALUE ALL "-".

      * Summary Header Line 2 (Text)
       01 WS-SUM-LINE-TWO.
          05 FILLER               PIC X(56)       VALUE SPACES.
          05 FILLER               PIC X(20)       VALUE
                "SUMMARY STATISTICS".
          05 FILLER               PIC X(56)       VALUE SPACES.

       01 WS-SUM-TOTALS.
          05 FILLER               PIC X(25)       VALUE
                "TOTAL REPLACE RECORDS:".
          05 SUM-COUNT            PIC ZZ,ZZ9.
          05 FILLER               PIC X(10)       VALUE SPACES.
          05 FILLER               PIC X(15)       VALUE
                "TOTAL AMOUNT:".
          05 SUM-AMT              PIC $ZZZ,ZZ9.99.

       01 WS-SUM-DEDUCT.
          05 FILLER               PIC X(25)       VALUE
                "TOTAL DEDUCTIBLE OWING:".
          05 SUM-DEDUCT-TOT       PIC $ZZZ,ZZ9.99.

       01 WS-PROD-LINE.
          05 FILLER               PIC X(10)       VALUE
                "PRODUCT:".
          05 SUM-PROD-NAME        PIC X(10).
          05 FILLER               PIC X(10)       VALUE
                "Quantity:".
          05 SUM-PROD-QTY         PIC ZZ9.
          05 FILLER               PIC X(15)       VALUE
                "   Percentage:".
          05 SUM-PROD-PCT         PIC ZZ9.9.
          05 FILLER               PIC X(1)        VALUE
                "%".

       01 WS-REG-LINE.
          05 SUM-REG-LABEL        PIC X(20).
          05 SUM-REG-NAME         PIC X(12).
          05 FILLER               PIC X(8)        VALUE
                " (ID: ".
          05 SUM-REG-ID           PIC 9.
          05 FILLER               PIC X(2)        VALUE ")".
          05 FILLER               PIC X(10)       VALUE
                "  Amount:".
          05 SUM-REG-AMT          PIC $ZZZ,ZZ9.99.

       PROCEDURE DIVISION.

       000-MAIN.
           PERFORM 100-INITIALIZE.
           PERFORM 200-PROCESS-LOOP UNTIL EOF-YES.
           PERFORM 300-FINAL.
           GOBACK.

      * Open Files & Read First Record
       100-INITIALIZE.
           OPEN INPUT REPL-DATA-IN
                OUTPUT REPL-REPORT.
           PERFORM 210-READ-RECORD.

      * Process a record
       200-PROCESS-LOOP.
           IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
              PERFORM 220-PRINT-HEADERS
           END-IF.

           COMPUTE WS-DEDUCTIBLE-OWING ROUNDED =
              IR-AMOUNT * WS-DEDUCT-RATE.

           ADD 1 TO WS-REPLACE-COUNT.
           ADD IR-AMOUNT TO WS-TOTAL-AMT.
           ADD WS-DEDUCTIBLE-OWING TO WS-TOTAL-DEDUCT.

           EVALUATE IR-POLICY-NO(1:1)
           WHEN WS-REG-ONT
                MOVE 1 TO WS-CURRENT-REGION
           WHEN WS-REG-QUE
                MOVE 2 TO WS-CURRENT-REGION
           WHEN WS-REG-MAR
                MOVE 3 TO WS-CURRENT-REGION
           WHEN WS-REG-ALB
                MOVE 4 TO WS-CURRENT-REGION
           WHEN OTHER
                MOVE 1 TO WS-CURRENT-REGION
           END-EVALUATE.

           ADD IR-AMOUNT TO WS-REG-TOTAL(WS-CURRENT-REGION).

           EVALUATE IR-PROD-CODE
           WHEN "FRG"
                ADD 1 TO WS-FRIDGE-CT
           WHEN "STV"
                ADD 1 TO WS-STOVE-CT
           WHEN "WAS"
                ADD 1 TO WS-WASHER-CT
           WHEN "ACO"
                ADD 1 TO WS-AC-CT
           WHEN OTHER
                ADD 1 TO WS-OTHER-CT
           END-EVALUATE.

           MOVE IR-POLICY-NO TO DET-POLICY.
           MOVE IR-CUST-NAME TO DET-NAME.
           MOVE IR-PROD-CODE TO DET-PROD.
           MOVE IR-CLAIM-TYPE TO DET-TYPE.
           MOVE IR-AMOUNT TO DET-AMT.
           MOVE WS-DEDUCTIBLE-OWING TO DET-DEDUCT.
           MOVE IR-JUSTIFICATION TO DET-JUST.

           WRITE REPORT-LINE FROM WS-DETAIL-LINE.

           *> Single space: incrementing by 1 per record
           ADD 1 TO WS-LINE-COUNT.

      * Read Next Record
           PERFORM 210-READ-RECORD.

       210-READ-RECORD.
           READ REPL-DATA-IN
           AT END
              SET EOF-YES TO TRUE
           END-READ.

      * Prints page header
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

           *> Headers take up 4 lines of vertical space
           MOVE 0 TO WS-LINE-COUNT.

      * Finalize report (close files and run 310)
       300-FINAL.
           PERFORM 310-SUMMARY-TOTALS.
           CLOSE REPL-DATA-IN REPL-REPORT.

      * Calculate and print the summary section
       310-SUMMARY-TOTALS.
           WRITE REPORT-LINE FROM WS-BLANK-LINE.
           WRITE REPORT-LINE FROM WS-SUM-LINE-ONE.
           WRITE REPORT-LINE FROM WS-SUM-LINE-TWO.
           WRITE REPORT-LINE FROM WS-BLANK-LINE.

           MOVE WS-REPLACE-COUNT TO SUM-COUNT.
           MOVE WS-TOTAL-AMT TO SUM-AMT.
           WRITE REPORT-LINE FROM WS-SUM-TOTALS.

           MOVE WS-TOTAL-DEDUCT TO SUM-DEDUCT-TOT.
           WRITE REPORT-LINE FROM WS-SUM-DEDUCT.
           WRITE REPORT-LINE FROM WS-BLANK-LINE.

      * Calculates and prints product percentages
           MOVE "FRIDGE" TO SUM-PROD-NAME.
           MOVE WS-FRIDGE-CT TO SUM-PROD-QTY.
           COMPUTE WS-PCT =(WS-FRIDGE-CT / WS-REPLACE-COUNT) * 100.
           MOVE WS-PCT TO SUM-PROD-PCT.
           WRITE REPORT-LINE FROM WS-PROD-LINE.

           MOVE "STOVE" TO SUM-PROD-NAME.
           MOVE WS-STOVE-CT TO SUM-PROD-QTY.
           COMPUTE WS-PCT =(WS-STOVE-CT / WS-REPLACE-COUNT) * 100.
           MOVE WS-PCT TO SUM-PROD-PCT.
           WRITE REPORT-LINE FROM WS-PROD-LINE.

           MOVE "WASHER" TO SUM-PROD-NAME.
           MOVE WS-WASHER-CT TO SUM-PROD-QTY.
           COMPUTE WS-PCT =(WS-WASHER-CT / WS-REPLACE-COUNT) * 100.
           MOVE WS-PCT TO SUM-PROD-PCT.
           WRITE REPORT-LINE FROM WS-PROD-LINE.

           MOVE "AC UNIT" TO SUM-PROD-NAME.
           MOVE WS-AC-CT TO SUM-PROD-QTY.
           COMPUTE WS-PCT =(WS-AC-CT / WS-REPLACE-COUNT) * 100.
           MOVE WS-PCT TO SUM-PROD-PCT.
           WRITE REPORT-LINE FROM WS-PROD-LINE.

           MOVE "OTHER" TO SUM-PROD-NAME.
           MOVE WS-OTHER-CT TO SUM-PROD-QTY.
           COMPUTE WS-PCT =(WS-OTHER-CT / WS-REPLACE-COUNT) * 100.
           MOVE WS-PCT TO SUM-PROD-PCT.
           WRITE REPORT-LINE FROM WS-PROD-LINE.
           WRITE REPORT-LINE FROM WS-BLANK-LINE.

           PERFORM VARYING REG-IDX FROM 1 BY 1 UNTIL REG-IDX > 4
                   IF WS-REG-TOTAL(REG-IDX) > WS-HI-AMT
                      MOVE WS-REG-TOTAL(REG-IDX) TO WS-HI-AMT
                      SET WS-HI-REG TO REG-IDX
                   END-IF
                   IF WS-REG-TOTAL(REG-IDX) < WS-LO-AMT
                      MOVE WS-REG-TOTAL(REG-IDX) TO WS-LO-AMT
                      SET WS-LO-REG TO REG-IDX
                   END-IF
           END-PERFORM.

           MOVE "HIGHEST REGION:" TO SUM-REG-LABEL.
           MOVE WS-HI-REG TO WS-CURRENT-REGION.
           PERFORM 320-GET-REG-NAME.
           MOVE WS-REG-NAME-TEMP TO SUM-REG-NAME.
           MOVE WS-HI-REG TO SUM-REG-ID.
           MOVE WS-HI-AMT TO SUM-REG-AMT.
           WRITE REPORT-LINE FROM WS-REG-LINE.

           MOVE "LOWEST REGION:" TO SUM-REG-LABEL.
           MOVE WS-LO-REG TO WS-CURRENT-REGION.
           PERFORM 320-GET-REG-NAME.
           MOVE WS-REG-NAME-TEMP TO SUM-REG-NAME.
           MOVE WS-LO-REG TO SUM-REG-ID.
           MOVE WS-LO-AMT TO SUM-REG-AMT.
           WRITE REPORT-LINE FROM WS-REG-LINE.

      * Translates ID to region name for better readability
       320-GET-REG-NAME.
           EVALUATE WS-CURRENT-REGION
               WHEN 1 MOVE "ONTARIO" TO WS-REG-NAME-TEMP
               WHEN 2 MOVE "QUEBEC"  TO WS-REG-NAME-TEMP
               WHEN 3 MOVE "MANITOBA" TO WS-REG-NAME-TEMP
               WHEN 4 MOVE "ALBERTA" TO WS-REG-NAME-TEMP
               WHEN OTHER MOVE "UNKNOWN" TO WS-REG-NAME-TEMP
           END-EVALUATE.

       END PROGRAM RPLREPORT.
