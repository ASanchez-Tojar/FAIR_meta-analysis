## **FAIR_meta-analysis**

Welcome to the Repository of the project that contains material related to the study:

# **Meta-analysts must lead by example and embrace all FAIR principles**

Meta-analyses rely on comprehensive reporting of results and data in primary studies. They should therefore also strive for exemplary reporting and follow FAIR principles (Findable, Accessible, Interoperable, Reusable) for data sharing. Yet, despite growing calls for open, reliable, and transparent science, the quality, reporting and FAIRness of meta-analyses in ecology, evolution, and environmental sciences remain poorly characterised. Here, we evaluate features of methodological quality, reporting and data and code sharing practices of 81 meta-analyses published between 2016 and 2020 in ecology, evolution, and environmental sciences. Whilst we found data sharing was relatively common (68%), code sharing was rare (15%). We found low uptake of reporting guidelines (22%), a small proportion of unweighted approaches (17%) and no risk of bias assessments (0%). The FAIRness of data requires urgent attention. Most openly available meta-analytic datasets were Findable and Accessible, but less often Interoperable or Reusable. Low Interoperability and Reusability prevent verification and may explain why data reuse remains uncommon, and ultimately contributes to research waste. We found no evidence of higher FAIRness scores when studies reported having used guidelines, nor that FAIRness associated with citation rates, suggesting a lack of incentives for adopting FAIR principles. Lastly, we provide recommendations for readily implementable methodological improvements applicable to meta-analyses in any field.

# Repository structure

The repository is organized into separate folders for

`code/`: contains all R scripts used for data cleaning, analysis and creating figures.

`data/`: contains all datasets raw and processed, used for the study.

`figures/`: contains the figures generated for the manuscript and supplementary materials of this project.

`README.md`: provides an overview of the repository structure, workflow, code files, and some instructions for reproducing the analyses.

`meta-analysis_quality_and_reproducibility.Rproj` This is an R Project file for our project. We recommend using this .Rproj file after forking/downloading the repository. Open this file (in RStudio) before running the workflow so that relative file paths resolve correctly. If you are using Visual Studio or Positron IDE, please open the entire folder. It sets the working directory correctly and makes folder paths in R script more accessible.

### **CODE USED IN THE STUDY**

`code/` This folder contains all the R scripts associated with this project

### **How to run the code**

The code has been arranged in numerical order of how it was run to produce the results. If you want to reproduce the results from the manuscript, please run the code in numerical order. Each script assumes that the output files from the previous step are available in the corresponding `data/` folders.

**Package versions and session information are commented at the end of the respective code files to document the computational environment used when the scripts were checked.**

This project largely maintains the actual workflow of the work done in this study:

- **001_hackathon_data_cleaning.R :** imports and merges data collected during the two hackathons, standardises variable names and response. It performs consistency and observer-disagreement checks, incorporates documented manual corrections, and exports the cleaned variables required for subsequent processing and analysis.

- **002_manual_data_addition.R :** merges the cleaned hackathon assessments with the independently extracted manual dataset and incorporates the eight additional meta-analyses assessed by AST. It standardises selected reporting variables and exports the final combined dataset used in next scripts.

- **003_FAIR_assessment_data_cleaning.R :** imports and cleans the FAIR data assessments for the 55 meta-analyses that shared data, standardises assessor responses and incorporates documented post-assessment corrections and fixes any disagreements. It prepares the final FAIR assessment dataset for further analysis.

- **004_citation_peaking.R :** cleans and combines bibliographic information, Google Scholar citation counts, journal data-sharing policies, and 2018 five-year journal impact factors for the 81 meta-analyses. It calculates total and first-six-year citation counts, identifies the observed annual citation peak for each meta-analysis, summarizes citation-peak timing across studies, and exports the combined dataset.

- **005_figures.R :** merges the final meta-analysis, FAIR-assessment, citation, and journal-policy datasets; derives the eight-item FAIR score and associated FAIR-principle components; calculates descriptive proportions and Wilson 95% confidence intervals and produces the figures and statistical summaries reported in the manuscript. These include data and code sharing (Figure 1), the FAIRness heatmap across the 55 meta-analyses that shared data (Figure 2), and associations of FAIRness with reporting-guideline use and first-six-year citation counts (Figure 3).

### **DATA DESCRIPTION**

The repository contains several intermediate data files generated during the workflow. These intermediate files are retained for transparency and reproducibility. Because many files share similar columns and several are subsets of later datasets, we highlight below the key files that are most useful for navigating and reproducing the project.

### Key files

- `data/00_literature-search/ISSNJournalList.xlsx`: journal and ISSN information with the respective journal name for creating the search string.

- `data/00_literature-search/search_string`: final search string used to identify potentially eligible meta-analyses.

- `data/00_literature-search/wos.all.csv`: Web of Science records retrieved from the literature search.

- `data/01_raw_data/hackathons/2016-2020/`: original methodological and reporting assessments collected during the hackathons, including master copies and files used to document and resolve extraction errors.

- `data/01_raw_data/FAIR_assessment/`: original responses from the FAIRness assessment of the 55 meta-analyses that shared data.

- `data/02_processed_data/combined/meta-research_meta-analysis_final_2016-2020.csv`: final methodological and reporting dataset for the 81 meta-analyses included in the study.

- `data/02_processed_data/FAIR_assessment/FAIR_data_assessment_final.csv`: cleaned and adjudicated FAIRness assessment dataset containing one final assessment for each of the 55 meta-analyses that shared data.

- `data/02_processed_data/combined/meta-analysis_2016-2020_citations_and_policies_summary.csv`: bibliographic dataset containing citation information and journal data-sharing policy classifications used in the final analyses.

- `data/02_processed_data/combined/final_FAIR.csv`: analysis-ready FAIRness dataset generated for the final results and figures.

- `data/02_processed_data/combined/final_MR_MA.csv`: complete analysis-ready dataset combining methodological and reporting assessments, FAIRness variables, bibliographic information, citation counts, and journal-policy information.

- `data/03_journal_policies/`: source data used to classify journal data-sharing policies and their strength.

### **LICENSE**

This repository contains both analysis code and research materials. The code may be reused, modified, and redistributed under the terms of the MIT License, while the research materials may be reused and adapted with appropriate attribution according to **CC BY-NC 4.0**.

**Code** in the \`code/\` and \`functions/\` folders is released under the **MIT License**.\
\
**Data, data dictionaries, figures, tables, and documentation** are released under the Creative Commons Attribution-NonCommercial 4.0 International License (**CC BY-NC 4.0**), unless otherwise stated.\
\
**Additional notice**: No permission is granted for AI training or model development.

### **AUTHOR CONTRIBUTIONS**

This repository was created and is maintained by **Alfredo Sánchez-Tójar**. Alfredo organized the repository structure, curated the data files, prepared the analysis scripts.

**Shreya Dimri** validated the repository code by checking and running the analysis workflow and reviewing code reproducibility, and wrote the repository documentation.

For author contributions to the associated manuscript, see the complete CRediT statement in the manuscript

### **CONTACT**

Please feel free to contact **Alfredo Sánchez-Tójar(** alfredo.tojar\@gmail.com) **or Shreya Dimri (shreyadimriofficial\@gmail.com)** for any feedback or questions about the data or code in this repository.
