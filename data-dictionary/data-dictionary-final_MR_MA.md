# Data dictionary: `final_MR_MA.csv`

This file provides short descriptions for variables in `data/02_processed_data/combined/final_MR_MA.csv`. This is the complete analysis-ready dataset combining methodological and reporting assessments, data- and code-sharing variables, FAIRness assessments, bibliographic and citation information, journal-policy information, and derived FAIR scores.

`final_MR_MA.csv` contains one row per meta-analysis (`n = 81`) and 94 variables (columns). Variables from the FAIRness assessment are available only for the 55 meta-analyses that shared data. Missing values therefore often indicate that a variable was not applicable or not assessed for that study.

| **Variable** | **Description** | **Unique values (n)** | **Missing** |
|----------------|------------------------|---------------:|---------------:|
| `MA_ID` | Unique identifier assigned to each meta-analysis. | 81 | 0.00% |
| `title` | Title of the meta-analysis article. | 81 | 0.00% |
| `number_of_assessments` | Number of hackathon assessments completed for the meta-analysis. | 2 | 0.00% |
| `participants_names` | Names of hackathon participants who assessed the meta-analysis; multiple assessors are separated by semicolons. | 57 | 0.00% |
| `forms_versions` | Version(s) of the assessment form used during the hackathon assessment. | `v1; v2` (45); `v2; v2` (15); `v1` (8); `v2` (4) | 11.11% |
| `effect_size_sign_accounted_revised` | Whether the meta-analysis accounted for the direction/sign of effect sizes where relevant. | `1` (21); `2` (60) | 77.78% |
| `effect_size_used` | Effect-size measure used in the meta-analysis, as extracted from the article. | 28 | 0.00% |
| `effect_size_category` | Recategorised effect-size type: response ratio (RR), correlation (r), odds ratio (OR), standardised mean difference (SMD), or Others. | `RR` (31); `Others` (23); `SMD` (17); `r` (8); `OR` (2) | 0.00% |
| `effect_size_eqn_quote` | Text from the article documenting the effect-size calculation or definition. | 81 | 0.00% |
| `inferential_statistics_revised` | Whether inferential statistics were reported being used to calculate effect size | `Yes` (3); `NA` (78) | 0.00% |
| `processed_data_shared_participants_conc` | Hackathon-participant assessments of whether processed data were shared; multiple assessments are separated by semicolons. | `No; No` (27); `Yes; Yes` (13); `Yes; No` (10); `No; Yes` (10); `No` (8); `Yes` (4) | 11.11% |
| `processed_data_shared_AST` | Assessment of processed-data sharing by AST when disagreement occured. | `No` (18); `Yes` (9) | 66.67% |
| `processed_data_shared_SD` | Independent assessment of processed-data sharing by SD. | `Yes` (39); `No` (34) | 9.88% |
| `processed_data_shared_final` | Final processed-data-sharing classification, prioritising the decisions by AST assessment and otherwise using the SD assessment; when disagreements occured. | `No` (46); `Yes` (35) | 0.00% |
| `raw_data_shared_participants_conc` | Hackathon-participant assessments of whether raw data were shared; multiple assessments are separated by semicolons. | `No; No` (23); `Yes; Yes` (21); `Yes; No` (9); `Yes` (8); `No; Yes` (7); `No` (4) | 11.11% |
| `raw_data_shared_AST` | Assessment of raw-data sharing by AST when disagreement occured. | `Yes` (17); `No` (10) | 66.67% |
| `raw_data_shared_SD` | Independent assessment of raw-data sharing by SD. | `Yes` (38); `No` (35) | 9.88% |
| `raw_data_shared_final` | Final raw-data-sharing classification, prioritising the decisions by AST assessment and otherwise using the SD assessment; when disagreements occured. | `Yes` (44); `No` (37) | 0.00% |
| `data_shared_AST` | Overall data-sharing classification derived from AST assessments of processed and raw data. | `Yes` (20); `No` (7) | 66.67% |
| `data_shared_SD` | Overall data-sharing classification derived from SD assessments of processed and raw data. | `Yes` (52); `No` (21) | 9.88% |
| `data_shared_final` | Final classification of whether any processed or raw data were shared. | `Yes` (55); `No` (26) | 0.00% |
| `code_shared_participants_conc` | Hackathon-participant assessments of whether analysis code was shared; multiple assessments are separated by semicolons. | `No; No` (49); `No` (11); `No; Yes` (5); `Yes; Yes` (5); `Yes` (1); `Yes; No` (1) | 11.11% |
| `code_shared_AST` | Assessment of code sharing by AST when disagreement occured. | `No` (23); `Yes` (4) | 66.67% |
| `code_shared_SD` | Independent assessment of code sharing by SD. | `No` (62); `Yes` (11) | 9.88% |
| `code_shared_final` | Final code-sharing classification, prioritising the decisions by AST assessment and otherwise using the SD assessment; when disagreements occured. | `No` (69); `Yes` (12) | 0.00% |
| `data_and_code_sharing_comments_participants_conc` | Participant comments concerning data and code sharing. | 35 | 19.75% |
| `data_and_code_sharing_comments_AST` | Comments by AST during reassessment of data and code sharing. | 27 | 65.43% |
| `software` | Whether statistical software was reported. | `Yes` (77); `No` (4) | 0.00% |
| `software_used` | Statistical software reported in the article. | `R` (57); `SPSS` (8); `Metawin` (7); `STATA` (2); `RevMan` (1); `WinBUGS` (1); `GenStat` (1); `OpenMEE` (1) | 3.70% |
| `software_used_category` | Recategorised software: R, MetaWin, SPSS, or Others. | `R` (57); `SPSS` (8); `MetaWin` (7); `Others` (6) | 3.70% |
| `software_quote` | Text documenting the software used. | 80 | 1.23% |
| `RoB_assessment_revised_merged_final` | Final assessment of whether a formal risk-of-bias assessment was conducted. | `No` (81) | 0.00% |
| `RoB_assessment_alternative_revised_merged_final` | Alternative classification capturing methodological or experimental-design comparisons that could be interpreted as addressing risk of bias. | `Experimental_design_Method_comparison` (12) | 85.19% |
| `guidelines` | Whether the article reported following systematic-review or meta-analysis reporting guidelines. | `No` (63); `Yes` (18) | 0.00% |
| `guidelines_quote` | Text from the article supporting the guideline classification. | 18 | 77.78% |
| `guidelines_name` | Name or citation of the guideline reported by the article. | `PRISMA` (11); `Cooper H, Hedges LV, Valentine JC. 1994. The handbook of research synthesis and meta-analysis. Russell Sage Foundation, New York.` (1); `Pullin and Stewart 2006` (1); `CEE` (1); `Koricheva and Gurevitch 2014` (1); `Pullin and Stewart 2006, Koricheva et al. (2013)` (1); `Nakagawa, Noble, Senior, and Lagisz (2017)` (1); `Koricheva et al. (2013)` (1) | 77.78% |
| `guidelines_name_category` | Recategorised guideline: PRISMA, CEE, or Others. | `PRISMA` (11); `Others` (6); `CEE` (1) | 77.78% |
| `registration` | Whether a review protocol was registered or otherwise prospectively documented. | `No` (80); `Yes` (1) | 0.00% |
| `registration_quote` | Text supporting the protocol-registration classification. | `Yes` (1) | 98.77% |
| `meta_analytic_model` | Extracted description or classification of the meta-analytic model used. | 1 | 0.00% |
| `meta_analytic_model_quote` | Text describing the meta-analytic model. | 81 | 0.00% |
| `sampling_variance` | Whether effect sizes were weighted using sampling variance or another precision-related weighting. | `Yes` (67); `No` (14) | 0.00% |
| `sampling_variance_quote` | Text supporting the sampling-variance or weighting classification. | 72 | 11.11% |
| `data_and_code` | Whether both data and analysis code were shared. | `No` (69); `Yes` (12) | 0.00% |
| `number_of_FAIR_assessments` | Number of FAIRness assessments completed for the shared dataset; applicable to meta-analyses that shared data. | `1` (44); `2` (11) | 32.10% |
| `FAIR_assessment_names_conc` | Names of assessors who performed the FAIRness assessment; multiple names are separated by semicolons. | 22 | 32.10% |
| `data_GUPID_presence` | Type of globally unique persistent identifier associated with the shared data, such as a DOI or Handle, or indication that none was present. | `Digital Object Identifier (DOI)` (33); `No unique and persistent identifier associated with the data` (21); `Handle (link has hdl or handle.net)` (1) | 32.10% |
| `data_GUPID_presence_recat` | Classification of whether the shared data had a globally unique persistent identifier. | `Yes` (34); `No` (21) | 32.10% |
| `data_GUPID` | Globally unique persistent identifier assigned to the shared data, where available. | 34 | 58.02% |
| `data_sharing_location` | Location where the data were shared, e.g. repository, supplementary material, GitHub, or within the article. | `Dryad` (20); `Supplementary materials` (17); `Figshare` (9); `DataVerse` (2); `Table within article` (2); `Institutional repository` (1); `Dryad;GitHub or GitLab` (1); `Zenodo` (1); `Dryad;Supplementary materials` (1); `NCBI GenBank;Supplementary materials` (1) | 32.10% |
| `data_sharing_location_recat` | Recategorisation of the data-sharing location. | `Repository` (35); `Supplements` (18); `Within_article` (2) | 32.10% |
| `data_sharing_location_recat_bin` | Whether data were deposited in an external/searchable repository. | `Yes` (35); `No` (20) | 32.10% |
| `data_metadata_presence` | Whether metadata other than a data dictionary/codebook were provided. | `Yes` (38); `No` (17) | 32.10% |
| `data_metadata_type` | Location or form of the accompanying metadata, such as repository metadata, README/text file, spreadsheet tab, or article. | `As part of the data repository` (32); `Meta-data is not shared` (17); `As README file/ .txt file; As part of the data repository` (2); `As .xlsx tab` (2); `As part of the article` (2) | 32.10% |
| `data_downloadable` | Whether the shared data could be downloaded. | `Yes` (55) | 32.10% |
| `data_licence_presence` | Licence associated with the data, e.g. CC0, CC BY, CC BY-NC, GNU, or publisher copyright/rights reserved. | `CC0 (Public Domain Dedication)` (25); `Publisher copyright/Rights reserved` (20); `CC BY (Attribution)` (8); `CC BY-NC (Attribution, NonCommercial)` (1); `CC0 (Public Domain Dedication);GNU` (1) | 32.10% |
| `data_licence_presence_recat` | Whether the licence permitted data reuse. | `Yes` (35); `No` (20) | 32.10% |
| `data_dictionary_presence` | Whether a data dictionary or codebook explaining variables was provided. | `No` (30); `Yes` (25) | 32.10% |
| `data_type` | File format(s) in which data were shared. | `.xlsx` (19); `.csv/tsv` (17); `.pdf` (5); `.xls` (3); `.txt` (2); `.doc` (2); `.docx` (2); `.csv/tsv;.tre` (1); `.csv/tsv;.txt` (1); `.csv/tsv;.xlsx` (1); `.docx;.pdf` (1); `.xlsx;.txt` (1) | 32.10% |
| `data_type_FAIR` | FAIR interoperability classification of the shared data format: Yes = fully interoperable; Partial = lower-interoperability or mixed formats; No = document-only/non-interoperable formats. | `Partial` (24); `Yes` (21); `No` (10) | 32.10% |
| `data_metadata_comments_conc` | Assessor comments concerning metadata. | 26 | 67.90% |
| `data_downloadable_comments_conc` | Assessor comments concerning data accessibility/downloadability. | 13 | 82.72% |
| `data_type_comments_conc` | Assessor comments concerning file formats. | 12 | 83.95% |
| `data_dictionary_comments_conc` | Assessor comments concerning data dictionaries/codebooks. | 30 | 58.02% |
| `article_links2data` | Whether the article directly mentioned or linked to the available underlying data, including through a link, PID, or accession number. | `Yes` (54); `No` (1) | 32.10% |
| `year` | Publication year of the meta-analysis. | `2018` (20); `2020` (17); `2019` (16); `2016` (14); `2017` (14) | 0.00% |
| `DOI` | Digital Object Identifier of the meta-analysis article. | 81 | 0.00% |
| `journal_clean` | Standardised journal name. | 35 | 0.00% |
| `authors_simple` | Simplified first-author identifier used for bibliographic matching. | 77 | 0.00% |
| `five_year_impact_factor_2018` | Journal five-year Impact Factor for 2018, used as a common journal-level reference metric. | 35 | 0.00% |
| `policy` | Journal data-sharing policy strength classification. | `strong` (63); `weak` (15); `none` (3) | 0.00% |
| `policyYYYY` | Year in which the relevant journal data-sharing policy was introduced, where known. | `2019` (20); `2014` (13); `2016` (11); `2018` (10); `2017` (7); `2011` (5); `2021` (3); `2022` (3); `2013` (2); `2020` (1) | 7.41% |
| `policy_in_place` | Whether the journal's data-sharing policy was already in place when the focal meta-analysis was published. | `TRUE` (46); `FALSE` (32) | 3.70% |
| `GS_date_citations` | Date on which Google Scholar citation counts were retrieved. | `20260112` (80); `20251001` (1) | 0.00% |
| `GS_total_citations` | Total Google Scholar citations accumulated by the article at the citation-extraction date. | 69 | 0.00% |
| `GS_6_first_years` | Total Google Scholar citations accumulated during the first six calendar years from publication, including the publication year. | 69 | 0.00% |
| `GS_6_first_years_per_year` | Mean annual citation count over the first six years. | 69 | 0.00% |
| `policy_in_place_type` | Journal-policy category applicable at the time the article was published, before contacting the journals | `strong` (39); `none` (29); `weak` (7) | 7.41% |
| `article_links2data_num` | Numeric FAIR score for article-to-data linking: Yes = 1, No = 0. | `2` | 32.10% |
| `data_sharing_location_recat_bin_num` | Numeric FAIR score for repository-based data sharing: Yes = 1, No = 0. | `2` | 32.10% |
| `data_metadata_presence_num` | Numeric FAIR score for metadata presence: Yes = 1, No = 0. | 2 | 32.10% |
| `data_downloadable_num` | Numeric FAIR score for downloadability: Yes = 1, No = 0. | 1 | 32.10% |
| `data_GUPID_presence_recat_num` | Numeric FAIR score for presence of a globally unique persistent identifier: Yes = 1, No = 0. | 2 | 32.10% |
| `data_type_FAIR_num` | Numeric FAIR score for data format: Yes = 1, Partial = 0.5, No = 0. | 3 | 32.10% |
| `data_dictionary_presence_num` | Numeric FAIR score for data-dictionary presence: Yes = 1, No = 0. | 2 | 32.10% |
| `data_licence_presence_recat_num` | Numeric FAIR score for a reuse-permitting licence: Yes = 1, No = 0. | 2 | 32.10% |
| `FAIR_score` | Sum of the eight numerical FAIR indicators; maximum possible score = 8. | `8` (8); `7.5` (8); `6.5` (8); `7` (7); `3` (5); `2` (4); `2.5` (4); `4` (3); `6` (3); `4.5` (2); `3.5` (2); `5` (1) | 32.10% |
| `FAIR_score_perc` | Overall FAIR score expressed as a percentage of the maximum possible score. | `100` (8); `93.75` (8); `81.25` (8); `87.5` (7); `37.5` (5); `25` (4); `31.25` (4); `50` (3); `75` (3); `56.25` (2); `43.75` (2); `62.5` (1) | 32.10% |
| `Findable_score` | Mean score across the three Findability indicators; range 0–1. | `1` (33); `0.3333` (16); `0.6667` (6) | 32.10% |
| `Accessible_score` | Mean score across the two Accessibility indicators; range 0–1. | `1` (34); `0.5` (21) | 32.10% |
| `Interoperable_score` | Score for data-file interoperability; range 0–1, with 0.5 representing partial interoperability. | `0.5` (24); `1` (21); `0` (10) | 32.10% |
| `Reusable_score` | Mean score across the two Reusability indicators; range 0–1. | `0.5` (28); `1` (16); `0` (11) | 32.10% |
| `policy_any_1` | Simplified journal-policy variable distinguishing any policy from none. | `any` (46); `none` (29) | 7.41% |

## Notes

- FAIRness variables apply only to the 55 meta-analyses that shared data; **32.10% missingness** across all FAIR variables is therefore expected for meta-analyses that did not share data.
- The overall FAIR score consists of eight indicators: three Findability indicators, two Accessibility indicators, one Interoperability indicator, and two Reusability indicators.
- For numerical FAIR indicators, `Yes = 1`, `Partial = 0.5` where applicable, and `No = 0`.
