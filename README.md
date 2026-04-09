# Data and Code for: Costly Regulation, Minimal Results: The EU's Deforestation Regulation Effect on Global Soy Trade

**Principal Investigator(s):**
*   **Manoj Sharma**, PhD Candidate, Department of Agricultural Economics, Kansas State University
*   **Nelson Villoria**, Professor, Department of Agricultural Economics, Kansas State University

**Version:** V1

---

## Repository Contents

| Name | File Type | Description |
| :--- | :--- | :--- |
| **data/** | Folder | Contains analysis-ready `.rds` and `.csv` data files used by the R scripts. |
| **images/** | Folder | Contains input images (Sankey plots) and generated output figures. |
| **R/** | Folder | Contains R scripts for analysis, table generation, and figure creation. |
| **March 22 Essay 1.qmd** | QMD | Main Quarto document that generates the paper PDF. |
| **references.bib** | BibTeX | Bibliography file. |
| **ajae.csl** | CSL | Citation style language file for AJAE. |

---

## Project Citation

Sharma, Manoj, and Villoria, Nelson. "Data and Code for: Costly Regulation, Minimal Results: The EU's Deforestation Regulation Effect on Global Soy Trade." *European Review of Agricultural Economics* (forthcoming).

---

## Project Description

**Summary:**
The European Union Deforestation Regulation (EUDR) aims to reduce deforestation by restricting market access to soy products that are deforestation-embodied in the EU market. The primary concern is that such restrictions could shift the soy trade to unregulated markets. To shed light on this issue, we employ a gravity model, treating the EUDR compliance costs as additional trade costs for exports to the EU. We find that stricter compliance costs reallocate soy exports from South America toward non-EU markets, mainly China, and divert the EU imports towards North America. Under the counterfactual scenario in which South America does not comply with the regulation, EU consumers face even larger price increases, while South American countries experience minimal terms-of-trade losses. Our analysis suggests that trade reallocation to an unregulated market could potentially dilute the impact of the EUDR, making it less effective in directly reducing deforestation linked to soy production.

---

## Scope of Project

**Subject Terms:**
EUDR; Structural gravity model; Deforestation; Soy; Tariffs

**JEL Classification:**
*   **Q17:** Agriculture in International Trade
*   **Q56:** Environment and Development; Environment and Trade; Sustainability; Environmental Accounts and Accounting; Environmental Equity; Population Growth
*   **F18:** Trade and Environment

**Geographic Coverage:**
Global (Panel of 90 countries)

**Time Period(s):**
2007 – 2022

**Data Type(s):**
Panel data; International trade flows; Intra-national trade flows; Effectively applied tariffs; Gravity variables.

---

## Related Publications

*   Sharma, Manoj, and Villoria, Nelson. "Costly Regulation, Minimal Results: The EU's Deforestation Regulation Effect on Global Soy Trade." *European Review of Agricultural Economics* (forthcoming).

---

## License

**Creative Commons Attribution 4.0 International (CC BY 4.0) License**

This work is licensed under a Creative Commons Attribution 4.0 International (CC BY 4.0) License.

---

## Instructions for Replication

1.  **Software Requirements:** R (version 4.0 or higher), Quarto.
2.  **R Packages:** `flextable`, `tidyverse`, `fixest`, `readxl`, `tradepolicy`, `ggplot2`, `alpaca`, `modelsummary`, `ggsci`, `gridExtra`, `cowplot`, `magick`, `ggtext`.
3.  **Running the Code:**
    *   The `R/` folder contains numbered scripts. Run them in order:
        1.  `01_create_summary_table.R`: Generates summary statistics and saves `data/summary_table.rds`.
        2.  `02_estimate_gravity_ppml.R`: estimates the gravity model and saves results to `data/`.
        3.  `03_create_major_figures.R`: Generates the main figures of the paper.
        4.  `04_create_sensitivity_figures.R`: Generates sensitivity analysis figures.
    *   Render `March 22 Essay 1.qmd` using Quarto to generate the final PDF.
