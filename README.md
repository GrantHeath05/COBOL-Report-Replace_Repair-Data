# COBOL Insurance Claim Reporting – REPAIR & REPLACE Reports

This repository contains two COBOL batch reporting programs:

- **RPLREPORT (RAW.REPLACE)** – Generates the **Replacement Claims Report**
- **RPRREPORT (RAW.REPAIR)** – Generates the **Repair Claims Report**

Both programs read the output files produced by the earlier **RAW Split Program**, which separates valid insurance claim records into REPAIR and REPLACE data sets.

Each report program:
- Assumes **all input data is valid**
- Reads a sequential file of claim records
- Prints a **formatted, paginated report** (20 records per page)
- Calculates totals, counts, and summary statistics
- Outputs a human‑readable `.RPT` file

---

# 1. Replacement Claims Report (A8RPL)

## Overview
`RPLREPORT` reads the **RAW.REPLACE.OUT** file and produces a detailed, multi‑page **Replacement Claims Report** followed by a full statistical summary.

This program includes:
- Detailed line‑by‑line listing of all REPLACE claims
- Deductible calculation (8% of claim amount)
- Product code breakdown (FRG, STV, WAS, ACO, OTHER)
- Region totals and highest/lowest region analysis
- Total amounts and total deductible owing

---

## Input Assumptions
The program assumes:
- All records are valid and correctly formatted  
- No validation is required for policy numbers, product codes, claim types, or amounts  
- The input file layout matches the RAW output structure  

---

## Processing Summary

### Detail Processing
For each record:
- Deductible = `Amount * 0.08`
- Totals updated:
  - Replace record count
  - Total amount
  - Total deductible
  - Product code counters
  - Region totals (Ontario, Quebec, Manitoba, Alberta)

### Pagination
- Each page prints:
  - Page header
  - Column headings
  - Up to **20 detail lines**

### Summary Section
At end of file, the program prints:
- Total REPLACE records  
- Total amount  
- Total deductible owing  
- Product code counts + percentages  
- Highest‑earning region  
- Lowest‑earning region  

---

# 2. Repair Claims Report (A8RPR)

## Overview
`RPRREPORT` reads the **RAW.REPAIR.OUT** file and produces a formatted **Repair Claims Report** with a simpler summary section compared to the REPLACE report.

This program includes:
- Detailed listing of all REPAIR claims
- Deductible calculation (8% of claim amount)
- Region count totals (Ontario, Quebec, Maritimes, Alberta)
- Total amount and total deductible owing

---

## Input Assumptions
- All records are valid and correctly structured  
- No validation logic is required  
- Input file layout matches the RAW.REPAIR output  

---

## Processing Summary

### Detail Processing
For each record:
- Deductible = `Amount * 0.08`
- Totals updated:
  - Repair record count
  - Total amount
  - Total deductible
  - Region count (not dollar totals)

### Pagination
- Same as REPLACE report: 20 lines per page with headers

### Summary Section
At end of file, the program prints:
- Total REPAIR records  
- Total amount  
- Total deductible owing  
- Region counts for:
  - Ontario  
  - Quebec  
  - Maritimes  
  - Alberta  

---

# 3. File Inputs & Outputs

| Program      | Input File          | Output File           | Description |
|--------------|---------------------|------------------------|-------------|
| **A8RPL**    | `RAW.REPLACE.OUT`    | `REPLACE.SUMMARY.RPT`      | Replacement Claims Report |
| **A8RPR**    | `RAW.REPAIR.OUT`     | `REPAIR.SUMMARY.RPT`      | Repair Claims Report |

> Note: Both programs write to a `.RPT` file but are typically run separately, producing different report outputs.

---

# 4. Purpose of These Programs

These reporting programs demonstrate:
- Sequential file reading  
- Conditional logic and record classification  
- Paginated COBOL report formatting  
- Totals, percentages, and summary calculations  
- Use of OCCURS tables and working‑storage accumulators  

They serve as the final reporting stage of the insurance claim processing workflow.

---

# 5. Related Program (RAW Split Program)

Both reports depend on the earlier program:

**COBOL‑Split‑Valid‑Insurance‑Claim**

Which:
- Reads the raw input file  
- Splits records into REPAIR and REPLACE output files  
- Performs no validation (valid‑data‑only assumption)  

The report programs consume those files to produce final formatted reports.

---

# End of README
