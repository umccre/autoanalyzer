# Automated Survey Analyzer

**Version:** 1.01  
**Release Date:** January 10, 2024

https://ccre.shinyapps.io/automatedsurveyanalyzer/

## Overview

Welcome to the Automated Survey Analyzer! This tool is designed to quickly process and analyze survey data (or similar datasets), providing:

- Frequencies
- Percentages
- Basic summary statistics

It’s ideal for simple analyses of **one-time surveys**, but also supports **pre-post surveys** and **multi-time-point surveys**—as long as each individual assessment is recorded on a separate line. For longitudinal analyses, be sure to include a variable that identifies the time point.

---

## How to Use

1. **Prepare Your Data:**
   - Upload a CSV file.
   - The **first row** must contain variable names.
   - Data should begin in the **second row**.
   - **Missing data:** Leave cells blank.
   - **File size limit:** 30 MB.

2. **Variable Types:**
   - By default, variables are treated as **categorical** unless **all data points** in a variable are **perfectly numeric** (no text in any cell).
   - You can select an option to force all data to be treated as **categorical**, regardless of content.

---

## Features & Limitations

✅ Handles:
- Basic descriptive statistics
- Multi-time-point (e.g., pre-post) survey data

🚫 Does NOT handle:
- Tests of statistical significance
- Cross-tabulations (although you can approximate crosstabs by using the column variable as the pre/post variable)

---

## Disclaimer

This software is provided **as is** and comes with **no warranty**. Users are responsible for verifying results.

---

## Support

For technical assistance, please contact:

**Jonathan Bennett**  
Center for Community Research and Evaluation  
University of Memphis  
📞 (662) 686-3945  
✉️ jrbnnett@memphis.edu

---

## Version History

| Version | Description                                   |
|---------|-----------------------------------------------|
| v0.1    | Initial release                               |
| v0.11   | Fixed bug related to file encoding            |
| v1.01   | Added filter feature                          |
