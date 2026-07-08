################################################################################
# Authors: 
#
# Alfredo Sanchez-Tojar (alfredo.tojar@gmail.com): original code

# Script first created in September 2025

################################################################################
# Description of script and Instructions
################################################################################

# This script is add the data obtained by manual extraction from the same 73
# meta-analysis that were used in two hackathons aimed to assess the 
# transparency, reliability and reproducibility of meta-analyses published in 
# ecology and evolution

# Hackathon 1: ESMARConf2025 June 13th 2025
# Hackathon 2: unofficial August 20th 2025

# The manual data was originally prepared by Vedant and Marit, and later on
# verified and cleaned by Shreya Dimri. This manual extraction include data
# for 3 overlapping variables (processed and raw data sharing, and code sharing)
# as well as for additional variables important to understand the transparency, 
# reliability and reproducibility of those meta-analyses

################################################################################
# Packages needed
################################################################################

# install.packages("pacman")
# load packages
pacman::p_load(dplyr,
               tidyverse,
               stringr,
               writexl,
               readxl)

# cleaning up
rm(list = ls())

################################################################################
# Functions needed
################################################################################

# none

################################################################################
# Data
################################################################################

# importing cleaned hackathondata from script 001
hackathon_cleaned <- read.csv("data/processed_data/hackathons/2016-2020/01_hackathon_data_cleaned.csv", header=T)


# importing cleaned manual data prepared by Shreya Dimri
manual_cleaned <- read.csv("data/processed_data/manual_assessment/2016-2020/manual_extraction_master_MRMA_SD.csv", header=T)

################################################################################
# revising column names
################################################################################

# hackathon
names(hackathon_cleaned)

hackathon_cleaned_1 <- hackathon_cleaned %>%
  select(-participant_email,-Timestamp) %>%
  rename(processed_data_shared_participants = processed_data_shared,
         processed_data_shared_AST = processed_data_shared_PDF,
         raw_data_shared_participants = raw_data_shared,
         raw_data_shared_AST = raw_data_shared_PDF,
         code_shared_participants = code_shared,
         code_shared_AST = code_shared_PDF) %>%
  as.data.frame()

# manual
names(manual_cleaned)

manual_cleaned_1 <- manual_cleaned %>%
  select(-paperID, -title) %>%
  rename(processed_data_shared_SD = processed_data_shared,
         raw_data_shared_SD = raw_data_shared,
         code_shared_SD = code_shared) %>%
  as.data.frame()

# Check result
nrow(manual_cleaned_1)
head(manual_cleaned_1)
table(manual_cleaned_1=="NA")
table(manual_cleaned_1=="")
table(is.na(manual_cleaned_1))


################################################################################
# make sure no conflicts are left in the columns that are fully standardised
################################################################################

# This does not include the participants extractions for data and code, and
# it only applies to the hackathon dataset

# Columns that should have been standardized (no conflicts)
standardized_cols <- c(
  "title",
  "effect_size_sign_accounted_revised",
  "inferential_statistics_revised",
  "processed_data_shared_AST",
  "raw_data_shared_AST",
  "code_shared_AST",
  "RoB_assessment_revised_merged_final",
  "RoB_assessment_alternative_revised_merged_final"
)

# Check for conflicts: count unique values per MA_ID for each standardized column
conflict_check <- hackathon_cleaned_1 %>%
  group_by(MA_ID) %>%
  summarise(across(all_of(standardized_cols),
                   ~ n_distinct(.x, na.rm = TRUE)), .groups = "drop")

# conflicted_articles: any standardized column with more than 1 unique value
conflicted_articles <- conflict_check %>%
  filter(apply(select(., all_of(standardized_cols)), 1, function(x) any(x > 1)))

# Print result
if (nrow(conflicted_articles) == 0) {
  message("✅ All conflicts resolved: ready to collapse dataset.")
} else {
  stop("⚠️ Conflicts remain in standardized columns for the following MA_ID(s):\n",
       paste(conflicted_articles$MA_ID, collapse = ", "))
}

################################################################################
# Generating a dataset with one row per MA_ID after having resolved all 
# conflicts between entries of the same MA_ID for the variables that apply
# see section above
################################################################################

# Title: Collapse Hackathon Dataset to One Row per Article (MA_ID)
# Description:
# This script takes the final cleaned hackathon dataset (hackathon_cleaned),
# which contains multiple entries per article (MA_ID), and collapses it so
# that there is one row per article. Standardized columns are preserved using
# first(), while participant names and form versions are concatenated using
# paste() with unique() to avoid duplicates. This only work after all conflicts
# between entries of the same MA_ID have been resolved.

################################################################################
# One row per meta-analysis with the variables of interest

names(hackathon_cleaned_1)

# Collapse dataset to one row per MA_ID
hackathon_cleaned_2 <- hackathon_cleaned_1 %>%
  
  # Group by article identifier
  group_by(MA_ID) %>%
  
  # Summarise each column: take first() for standardized variables
  summarise(
    
    # Article metadata
    title = first(title), # take first title
    number_of_assessments = first(number_of_assessments), # number of entries for this article
    
    # Concatenate participant names with ; separator
    participants_names = paste((participant_name), collapse = "; "),
    
    # Concatenate form versions with ; separator
    forms_versions = paste((form_version), collapse = "; "),
    
    # Effect size / inferential statistics
    effect_size_sign_accounted_revised = first(effect_size_sign_accounted_revised),
    inferential_statistics_revised = first(inferential_statistics_revised),
    
    # Concatenate participants entry with ; separator
    processed_data_shared_participants_conc = paste((processed_data_shared_participants), collapse = "; "),
    
    # using priority AST
    processed_data_shared_AST = first(processed_data_shared_AST),
    
    # Concatenate participants entry with ; separator
    raw_data_shared_participants_conc = paste((raw_data_shared_participants), collapse = "; "),
    
    # using priority AST
    raw_data_shared_AST = first(raw_data_shared_AST),
    
    # Concatenate participants entry with ; separator
    code_shared_participants_conc = paste((code_shared_participants), collapse = "; "),
    
    # using priority AST
    code_shared_AST = first(code_shared_AST),
    
    # Concatenate participants entry with ; separator
    data_and_code_sharing_comments_participants_conc = paste((data_and_code_sharing_comments), collapse = "; "),
    
    # using priority AST
    data_and_code_sharing_comments_AST = first(data_and_code_sharing_comments_PDF), 
    
    # Risk-of-bias assessments
    RoB_assessment_revised_merged_final = first(RoB_assessment_revised_merged_final),
    RoB_assessment_alternative_revised_merged_final = first(RoB_assessment_alternative_revised_merged_final),
    
    # Ungroup after summarise to return a plain data frame
    .groups = "drop"
    
  ) %>%
  
  # converting all "NA" string to real NA's from now on
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  # converting all "" string to real NA's from now on
  mutate(across(where(is.character), ~ na_if(.x, ""))) %>%
  
  # Convert to base R data.frame for compatibility with other code
  as.data.frame()

# Check result
nrow(hackathon_cleaned_2)
head(hackathon_cleaned_2)
table(hackathon_cleaned_2=="NA")
table(hackathon_cleaned_2=="")
table(is.na(hackathon_cleaned_2))

################################################################################
# Merging hackathon and manual dataset
################################################################################

full_cleaned <- hackathon_cleaned_2 %>%
  left_join(manual_cleaned_1, by = "MA_ID") %>% 
  relocate(processed_data_shared_SD,
           .after = processed_data_shared_AST) %>%
  relocate(raw_data_shared_SD,
           .after = raw_data_shared_AST) %>%
  relocate(metadata,
           .after = raw_data_shared_SD) %>%
  relocate(code_shared_SD,
           .after = code_shared_AST) %>%
  relocate(software,
           .after = data_and_code_sharing_comments_AST) %>%
  relocate(software_used,
           .after = software) %>%
  relocate(software_quote,
           .after = software_used) %>%
  relocate(effect_size_used,
           .after = effect_size_sign_accounted_revised) %>%
  relocate(effect_size_eqn_quote,
           .after = effect_size_used) %>%
  relocate(software_quote,
           .after = software_used) %>%
  mutate(number_of_assessments = coalesce(number_of_assessments, 1)) 

names(full_cleaned)
nrow(full_cleaned)
summary(full_cleaned)


################################################################################
# Finilising processed_data_shared, raw_data_shared and code_shared
################################################################################

# For this, we are treating the _AST as the most reliable source. However, AST
# only fully extracted data for these three variables for a total of 27 studies.
# Therefore, for the remaining studies, we are going to use the data extraction
# performed by SD. Eventually, we will also compare these two extractions as 
# well as the participants extractions to better understand the process. It is
# expected that disagreements for data sharing come mostly from slight
# differences in what was thought to be processed vs raw data, as that was 
# often difficult to know, particularly without any metadata being provided by 
# the authors

# first, let's create a variable that simplifies processed vs raw into simply
# data shared

full_cleaned <- full_cleaned %>%
  # we need to account for the presence of NA's
  mutate(data_shared_AST = case_when(
    processed_data_shared_AST == "Yes" | raw_data_shared_AST == "Yes" ~ "Yes",
    is.na(processed_data_shared_AST) & is.na(raw_data_shared_AST) ~ NA_character_,
    TRUE ~ "No"
  )) %>%
  # the _SD does not need to account for NA's since the dataset is full
  mutate(data_shared_SD = case_when(
    processed_data_shared_SD == "Yes" | raw_data_shared_SD == "Yes" ~ "Yes",
    TRUE ~ "No"))

# are there disagreements between AST and SD for those 27 cases for which AST
# extracted data? DATA
full_cleaned %>%
  filter(!is.na(data_shared_AST)) %>%
  count(data_shared_AST, data_shared_SD)

# print the disagreements
full_cleaned %>%
  filter(!is.na(data_shared_AST)) %>%   # only keep rows where AST is not NA
  filter(data_shared_AST != data_shared_SD)  # disagreements

# There is one single disagreement, which is a special case.

# MA_067: AST says No, SD says Yes (to processed data)
# Participants & SD say raw_data_shared No, No, No
# Participants say processed_data_shared No, Yes
# What AST found about this paper is that "Some processed data available in 
# Tables S1 & S2, but not all"

# We can repeat the same for processed and raw data, for which there are more 
# disagreements. Ignore for the time being.


# Processed data
full_cleaned %>%
  filter(!is.na(processed_data_shared_AST)) %>%
  count(processed_data_shared_AST, processed_data_shared_SD)

# print the disagreements
full_cleaned %>%
  filter(!is.na(processed_data_shared_AST)) %>%   # only keep rows where AST is not NA
  filter(processed_data_shared_AST != processed_data_shared_SD)  # disagreements

# MA_ID: AST says No, SD says Yes
# MA_002
# MA_005
# MA_010
# MA_036
# MA_043
# MA_053


# Raw data
full_cleaned %>%
  filter(!is.na(raw_data_shared_AST)) %>%
  count(raw_data_shared_AST, raw_data_shared_SD)

# print the disagreements
full_cleaned %>%
  filter(!is.na(raw_data_shared_AST)) %>%   # only keep rows where AST is not NA
  filter(raw_data_shared_AST != raw_data_shared_SD)  # disagreements

# MA_ID: AST says Yes, SD says No
# MA_012
# MA_064


# are there disagreements between AST and SD for those 27 cases for which AST
# extracted data? CODE
full_cleaned %>%
  filter(!is.na(code_shared_AST)) %>%
  count(code_shared_AST, code_shared_SD)

# print the disagreements
full_cleaned %>%
  filter(!is.na(code_shared_AST)) %>%   # only keep rows where AST is not NA
  filter(code_shared_AST != code_shared_SD)  # disagreements


# checking that metadata agrees with the final call using AST prioritisation
# which it does not because there is the case above in which AST considers there
# is no data, and therefore, we have to adjust its metadata column accordingly
full_cleaned %>%
  filter(data_shared_AST == "No" & !is.na(metadata))

full_cleaned <- full_cleaned %>%
  mutate(metadata = ifelse(!is.na(data_shared_AST) & data_shared_AST == "No",
                           NA_character_, metadata))


################################################################################
# Prioritising AST extractions, filling in SD
################################################################################

names(full_cleaned)

full_cleaned_final <- full_cleaned %>%
  mutate(
    data_shared_final = 
      ifelse(!is.na(data_shared_AST),
             data_shared_AST, 
             data_shared_SD),
    processed_data_shared_final = 
      ifelse(!is.na(processed_data_shared_AST),
             processed_data_shared_AST, 
             processed_data_shared_SD),
    raw_data_shared_final = 
      ifelse(!is.na(raw_data_shared_AST),
             raw_data_shared_AST, 
             raw_data_shared_SD),
    code_shared_final = 
      ifelse(!is.na(code_shared_AST),
             code_shared_AST, 
             code_shared_SD)
  ) %>% 
  relocate(data_shared_AST,
           .after = raw_data_shared_SD) %>% 
  relocate(data_shared_SD,
           .after = data_shared_AST) %>% 
  relocate(data_shared_final,
           .after = data_shared_SD) %>% 
  relocate(processed_data_shared_final,
           .after = processed_data_shared_SD) %>% 
  relocate(raw_data_shared_final,
           .after = raw_data_shared_SD) %>% 
  relocate(code_shared_final,
           .after = code_shared_SD)

names(full_cleaned_final)
table(full_cleaned_final$number_of_assessments)
table(full_cleaned_final$effect_size_sign_accounted_revised)
table(full_cleaned_final$processed_data_shared_final)
table(full_cleaned_final$raw_data_shared_final)
table(full_cleaned_final$data_shared_final)
table(full_cleaned_final$code_shared_final)

################################################################################
# Recoding variables
################################################################################

################################################################################
# recategorising effect_size_used
table(full_cleaned_final$effect_size_used)
sum(table(full_cleaned_final$effect_size_used))
# response ratio, correlation, odds ratio, SMD, others

full_cleaned_final <- full_cleaned_final %>%
  mutate(effect_size_category = case_when(
    grepl("response ratio", effect_size_used, ignore.case = TRUE) ~ "RR",
    grepl("correlation", effect_size_used, ignore.case = TRUE) ~ "r",
    grepl("odds ratio", effect_size_used, ignore.case = TRUE) ~ "OR",
    grepl("^SMD$", effect_size_used, ignore.case = TRUE) ~ "SMD",
    TRUE ~ "Others"
  ))

sort(table(full_cleaned_final$effect_size_category))
sum(table(full_cleaned_final$effect_size_category))

################################################################################
# recategorising software_used
# R, Metawin, SPSS, Others

names(full_cleaned_final)
table(full_cleaned_final$software)
table(full_cleaned_final$software_used)
sum(table(full_cleaned_final$software_used))

full_cleaned_final <- full_cleaned_final %>%
  mutate(software_used_category = case_when(
    is.na(software_used) ~ NA_character_,   # preserve missing values
    grepl("^R$", software_used, ignore.case = TRUE) ~ "R",
    grepl("Metawin", software_used, ignore.case = TRUE) ~ "MetaWin",
    grepl("SPSS", software_used, ignore.case = TRUE) ~ "SPSS",
    TRUE ~ "Others"
  ))

sort(table(full_cleaned_final$software_used_category))
sum(table(full_cleaned_final$software_used_category))


################################################################################
# recategorising guidelines_name
table(full_cleaned_final$guidelines)
table(full_cleaned_final$guidelines_name)
sum(table(full_cleaned_final$guidelines_name))

# PRISMA, CEE, Others
full_cleaned_final <- full_cleaned_final %>%
  mutate(guidelines_name_category = case_when(
    is.na(guidelines_name) ~ NA_character_,   # preserve missing values
    grepl("PRISMA", guidelines_name, ignore.case = TRUE) ~ "PRISMA",
    grepl("CEE", guidelines_name, ignore.case = TRUE) ~ "CEE",
    TRUE ~ "Others"
  ))

sort(table(full_cleaned_final$guidelines_name_category))
sum(table(full_cleaned_final$guidelines_name_category))



# ################################################################################
# # recategorising meta_analytic_model
# table(full_cleaned_final$meta_analytic_model)
# # needs lots of cleaning

################################################################################
# sampling_variance ready to be used
table(full_cleaned_final$sampling_variance)


################################################################################
# Computational reproducibility
################################################################################

# coding those studies providing both data and code
full_cleaned_final <- full_cleaned_final %>%
  mutate(data_and_code = case_when(
    data_shared_final == "Yes" & code_shared_final == "Yes" ~ "Yes",
    TRUE ~ "No"))

table(full_cleaned_final$data_and_code)
#table(full_cleaned_final$code_shared_final)

# and those that provide metadata
full_cleaned_final <- full_cleaned_final %>%
  mutate(data_code_and_metadata = case_when(
    data_and_code == "Yes" & metadata == "Yes" ~ "Yes",
    TRUE ~ "No"))

table(full_cleaned_final$data_code_and_metadata)


################################################################################
# Getting ready to save data, including adding the final set of 8 meta-analyses
################################################################################

full_cleaned_final <- full_cleaned_final %>%
  relocate(effect_size_category,
           .after = effect_size_used) %>% 
  relocate(software_used_category,
           .after = software_used) %>% 
  relocate(guidelines_name_category,
           .after = guidelines_name)

# importing extra 8 meta-analysis
extra_ma_cleaned <- read.csv("data/processed_data/combined/meta-research_meta-analysis_final_2016-2020_extra_papers.csv", header=T)

# table(extra_ma_cleaned$effect_size_sign_accounted_revised)
# table(extra_ma_cleaned$effect_size_category)
# table(extra_ma_cleaned$inferential_statistics_revised)
# table(extra_ma_cleaned$processed_data_shared_final)
# table(extra_ma_cleaned$raw_data_shared_final)
# table(extra_ma_cleaned$data_shared_final)
# table(extra_ma_cleaned$code_shared_final)
# table(extra_ma_cleaned$software)
# table(extra_ma_cleaned$software_used_category)
# table(extra_ma_cleaned$RoB_assessment_revised_merged_final)
# table(extra_ma_cleaned$RoB_assessment_alternative_revised_merged_final)
# table(extra_ma_cleaned$guidelines)
# table(extra_ma_cleaned$registration)
# table(extra_ma_cleaned$sampling_variance)
# table(extra_ma_cleaned$data_and_code)
# table(extra_ma_cleaned$data_code_and_metadata)

# adding them to our dataset
full_cleaned_final_extra <- rbind(full_cleaned_final,extra_ma_cleaned)

# table(full_cleaned_final_extra$effect_size_sign_accounted_revised)
# table(full_cleaned_final_extra$effect_size_category)
# table(full_cleaned_final_extra$inferential_statistics_revised)
# table(full_cleaned_final_extra$processed_data_shared_final)
# table(full_cleaned_final_extra$raw_data_shared_final)
# table(full_cleaned_final_extra$data_shared_final)
# table(full_cleaned_final_extra$code_shared_final)
# table(full_cleaned_final_extra$software)
# table(full_cleaned_final_extra$software_used_category)
# table(full_cleaned_final_extra$RoB_assessment_revised_merged_final)
# table(full_cleaned_final_extra$RoB_assessment_alternative_revised_merged_final)
# table(full_cleaned_final_extra$guidelines)
# table(full_cleaned_final_extra$registration)
# table(full_cleaned_final_extra$sampling_variance)
# table(full_cleaned_final_extra$data_and_code)
# table(full_cleaned_final_extra$data_code_and_metadata)


################################################################################
# Saving data
################################################################################

# UPDATE: After having processed the FAIR data assessment dataset, I decided to
# remove some variables from this dataset before saving for final use in the 
# remaining scripts. This includes, very importantly, removing the variables
# metadata and data_code_and_metadata, which no longer apply since during the 
# FAIR data assssment we standardised the data extraction regarding metadata
# and data dictionary, and our standardised and more granular approach may no 
# longer agree with what we generally called "metadata" before.

full_cleaned_final_extra_red <- full_cleaned_final_extra %>%
  select(-metadata, -data_code_and_metadata) %>%
  as.data.frame()

write.csv(full_cleaned_final_extra_red,
          "data/processed_data/combined/meta-research_meta-analysis_final_2016-2020.csv",
          row.names=FALSE)

