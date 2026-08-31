################################################################################
# Authors: 
#
# Alfredo Sanchez-Tojar (alfredo.tojar@gmail.com): original code

# Script first created in September 2025

################################################################################
# Description of script and Instructions
################################################################################

# This script is to add bibliographic and annual citation count to all 
# meta-analyses in our list.

# Each meta-analysis is identified by a unique article identifier ("MA_ID") and
# is linked to its title and year of publication. The variable
# "year_of_publication" refers to the year in which the article was assigned to
# a journal issue. In some cases, the article may have been published online in
# the preceding year.
# Annual citation counts were extracted from Google Scholar. The variable
# "GS_date_citations" records the date on which the citation data were collected.
# Citation counts are stored in ten year-specific variables:
# "GS_citations_2016" to "GS_citations_2025".
# Missing values in these variables generally indicate that the meta-analysis
# had not yet been published in the corresponding year. However, some articles
# received citations before their formal issue year, likely because they had
# already been published online.

# This script
# 1.identifies the formal year of publication
# 2.calculates the number of years since publication as:
# year_since_publication = citation_year - year_of_publication
# 3.identifies the year since publication with the highest annual citation count
# 4.calculates the average citation-peak year across all meta-analyses.

################################################################################
# Packages and data needed
################################################################################

# install.packages("pacman")
# load packages
pacman::p_load(tidyverse,
               purrr)

# cleaning up
rm(list = ls())

# Load data for citations
citations_df <- read.csv("data/02_processed_data/combined/meta-analysis_2016-2020_citations.csv")

# Load data with bibligraphic information
biblio <- read.csv("data/02_processed_data/combined/meta-analysis_2016-2020_bibliographic_info.csv")

################################################################################
# Bibliographic information (Cleaning Journal names and Author Names)
################################################################################

biblio_clean <- biblio %>%
  # Keeping only articles that were included in the study
  filter(Include_exclude == "include") %>% 
  # Remove variables that are not needed 
  select(-paperID,
         -exclusion_explanation, 
         -exclusion_category, 
         -Include_exclude, 
         -Clarification) %>%
  # Standardise journal names
  mutate(
    journal_clean = journal %>%
      str_to_lower() %>%                      # all lowercase
      str_replace_all("\\\\&|\\b(?:and|ad)\\b", "&") %>% 
      str_squish()                            # trim + remove double spaces
  ) %>%
# Manual harmonization for known inconsistencies
mutate(
  journal_clean = case_when(
    journal_clean == "agricultural ecosystems & environment" ~ "agriculture ecosystems & environment",
    journal_clean == "biological conversation" ~ "biological conservation",
    journal_clean == "evolution & ecology of microbiomes" ~ "functional ecology", #this was a typo!
    TRUE ~ journal_clean
  ))
 

head(biblio_clean)
table(biblio_clean$journal_clean)
biblio_clean %>% count(journal_clean, sort = TRUE)


# Now cleaning the author names. For cleaning this messy variable, 
# I worked together with ChatGPT to make the helper function.

# Helper function to extract first author surname
extract_first_surname <- function(authors) {
  if (is.na(authors) || stringr::str_squish(authors) == "") {
    return(NA_character_)
  }
  authors <- stringr::str_squish(authors)
  n_commas <- stringr::str_count(authors, ",")
  # Identify whether the record contains multiple authors
  multiple <- stringr::str_detect(
    authors,
    stringr::regex("\\band\\b", ignore_case = TRUE)
  ) || n_commas >= 2
  # Extract the first author
  first_author <- if (stringr::str_detect(
    authors,
    stringr::regex("\\band\\b", ignore_case = TRUE)
  )) {
    stringr::str_split(
      authors,
      stringr::regex("\\band\\b", ignore_case = TRUE),
      simplify = TRUE
    )[1]
  } else if (n_commas >= 2) {
    # Handles "First Last, First Last, First Last"
    stringr::str_split(authors, ",", simplify = TRUE)[1]
  } else {
    authors
  }
  first_author <- stringr::str_squish(first_author)
  # Extract surname
  surname <- if (stringr::str_detect(first_author, ",")) {
    # Handles "Surname, Given name"
    stringr::str_extract(first_author, "^[^,]+")
  } else {
    # Handles "Given name Surname", including prefixes such as Van Eeden
    stringr::str_extract(
      first_author,
      stringr::regex(
        "(?:(?:van|von|de|del|di|da|la|le|du|st)\\s+)*\\S+$",
        ignore_case = TRUE)
    )
    }
  
  surname <- surname |>
    stringr::str_remove_all("\\.") |>
    stringr::str_squish() |>
    stringr::str_replace_all("\\s+", "_")
  
  if (multiple) paste0(surname, "_et_al") else surname
}

# Apply to dataset
biblio_clean <- biblio_clean %>%
  mutate(authors_simple = map_chr(authors, extract_first_surname)) %>%
  mutate(
    authors_simple = case_when(
      authors_simple %in% c("Luis_Vicente-Vicente_et_al") ~ "Vicente-Vicente_et_al",
      authors_simple %in% c("Lee_et_al") ~ "Lee",
      authors_simple %in% c("Branoff_et_al") ~ "Branoff",
      TRUE ~ authors_simple
    )
  )

biblio_clean %>%
  select(authors,authors_simple) %>%
  mutate(authors = str_trunc(authors, 50)) %>%
  print()  # Adjust n to show more or fewer matches

# it works and it does not generate problems with uniqueness
length(biblio_clean$authors_simple)
unique(length(biblio_clean$authors_simple))

# removing the variables that we do not need anymore
biblio_clean <- biblio_clean %>%
  # Remove unneeded variables
  select(-authors,-journal)

head(biblio_clean)
biblio_clean %>% count(year, sort = TRUE)

################################################################################
# Journal policies
################################################################################

# Load data with bibligraphic information 
# from Berberi_and_Roche_OSF_Living_dataset_2025

journal.policies <- read.csv("data/03_journal_policies/Berberi_and_Roche_OSF_Living_dataset_20251003/DataPolicies_DATA.csv")
head(journal.policies)

journal.policies_clean <- journal.policies %>%
  mutate(
    Journal_clean = Journal %>%
      str_to_lower() %>%
      str_replace_all("\\band\\b", "&") %>%
      str_replace_all("\\s*&\\s*", " & ") %>%
      str_squish(),
    # manually add missing policy year
    policyYYYY = case_when(
      Journal_clean == "landscape ecology" ~ 2014, # added after communication with the journal
      Journal_clean == "isme journal" ~ 2022, # added after communication with the journal
      Journal_clean == "journal of wildlife management" ~ 2022, # added after communication with the journal
      Journal_clean == "ecohydrology" ~ 2017, # added after communication with the journal
      Journal_clean == "biological conservation" ~ 2020, # added after communication with the journal
      Journal_clean == "agriculture ecosystems & environment" ~ 2017, # added after communication with the journal
      TRUE ~ policyYYYY
    )
  )

# Check matches
biblio_clean %>%
  mutate(in_policy_dataset = journal_clean %in% journal.policies_clean$Journal_clean)
 

# There is no  journal that is not in the living dataset
biblio_clean %>%
  mutate(in_policy_dataset = journal_clean %in% journal.policies_clean$Journal_clean) %>%
  filter(!in_policy_dataset) %>%
  select(journal_clean)

# checking journals and counts
biblio_clean %>% count(journal_clean)

################################################################################
# Impact Factor for Journals
################################################################################

# -----------------------------------------------------------------------------
# Add five-year impact factor (2018) manually
# 2018 is the year in the middle between 2016 and 2020, thought as most representative
# IF obtained from https://jcr.clarivate.com/jcr/home (03.10.2025)
# -----------------------------------------------------------------------------

# Create a lookup table
journal_impact <- tibble(
  journal_clean = c(
    "agriculture ecosystems & environment",
    "applied ecology & environmental research",
    "austral ecology",
    "biogeosciences",
    "biological conservation",
    "biology letters",
    "conservation biology",
    "ecography",
    "ecohydrology",
    "ecological applications",
    "ecological engineering",
    "ecology",
    "ecology & evolution",
    "ecology letters",
    "ecosphere",
    "evolution",
    "evolutionary applications",
    "freshwater biology",
    "functional ecology",
    "global change biology",
    "global ecology & biogeography",
    "isme journal",
    "journal of animal ecology",
    "journal of applied ecology",
    "journal of biogeography",
    "journal of heredity",
    "journal of wildlife management",
    "landscape ecology",
    "marine ecology progress series",
    "methods in ecology & evolution",
    "molecular biology & evolution",
    "molecular ecology",
    "oikos",
    "proceedings of the royal society b-biological sciences",
    "wildlife research"
  ),
  five_year_impact_factor_2018 = c(
    3.954,  # agriculture ecosystems & environment
    0.689,    # applied ecology & environmental research
    1.403,    # austral ecology
    3.951,    # biogeosciences
    4.451,    # biological conservation
    3.323,    # biology letters
    6.194,    # conservation biology
    5.946,    # ecography
    2.564,    # ecohydrology
    4.378,    # ecological applications
    3.406,    # ecological engineering
    4.285,    # ecology
    2.415,    # ecology & evolution
    8.699,    # ecology letters
    2.746,    # ecosphere
    3.573,    # evolution
    5.038,    # evolutionary applications
    3.404,    # freshwater biology
    5.037,    # functional ecology
    8.880,    # global change biology
    5.667,    # global ecology & biogeography
    9.493,   # isme journal
    4.364,    # journal of animal ecology
    5.782,    # journal of applied ecology
    3.884,    # journal of biogeography
    2.618,    # journal of heredity
    1.881,    # journal of wildlife management
    4.349,    # landscape ecology
    2.359,    # marine ecology progress series
    7.099,    # methods in ecology & evolution
    14.797,    # molecular biology & evolution
    5.855,   # molecular ecology
    3.468,    # oikos
    4.304,   # proceedings of the royal society b-biological sciences
    1.244     # wildlife research
  )
)

# Join to biblio_clean
biblio_clean <- biblio_clean %>%
  left_join(journal_impact, by = "journal_clean")

# Check
biblio_clean %>%
  select(journal_clean, five_year_impact_factor_2018) %>%
  distinct() %>%
  arrange(desc(five_year_impact_factor_2018))

# trying to understand the time of policy implementation
journal.policies_clean <- journal.policies_clean %>%
  mutate(policyYYYY = as.numeric(policyYYYY))

# Join policy info to the bibliographic dataset
biblio_with_policy <- biblio_clean %>%
  left_join(
    journal.policies_clean %>%
      select(Journal_clean, policy, policyYYYY),
    by = c("journal_clean" = "Journal_clean")
  )


# Are journals represented by more meta-analyses in our sample associated
# with higher 2018 Journal Impact Factors?

journal_df <- biblio_clean %>%
  count(journal_clean,
        five_year_impact_factor_2018,
        name = "n")

journal_df %>% ggplot(aes(n, five_year_impact_factor_2018)) +
  geom_point(
    alpha = 0.7,
    size = 4,
    color = "grey30"
  ) +
  geom_smooth(
    method = "lm",
    color = "grey20",
    fill = "grey70",
    linewidth = 1
  ) +
  labs(
    x = "Number of sampled meta-analyses",
    y = "Five-year impact factor (2018)"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid.major = element_line(
      linetype = "dashed",
      color = "grey80"
    ),
    panel.grid.minor = element_blank(),
    axis.line = element_line(
      color = "grey30",
      linewidth = 0.8
    ),
    axis.title = element_text(size = 20),
    axis.title.x = element_text(
      margin = margin(t = 12)
    ),
    axis.title.y = element_text(
      margin = margin(r = 12)
    ),
    axis.text = element_text(
      color = "grey20"
    )
  )

################################################################################
# Journal policies implementation year
################################################################################

# -----------------------------------------------------------------------------
# Identify journals with articles published both before and after policy
# implementation
# -----------------------------------------------------------------------------
# Articles are classified according to whether they were published before or
# after the implementation of a weak or strong journal data-sharing policy.
# Journals that never had a policy are retained as a separate category and are
# not treated as part of a before-after comparison. Journals with articles on
# both sides of policy implementation may support within-journal comparisons.

# Determine if policy existed at the time of article publication
biblio_with_policy <- biblio_with_policy %>%
  mutate(
    policy_in_place = case_when(
      policy == "none" ~ FALSE,                              # no policy ever, treat as TRUE
      !is.na(policyYYYY) & year >= policyYYYY ~ TRUE,       # policy existed when article published
      !is.na(policyYYYY) & year < policyYYYY  ~ FALSE,      # policy not yet in place
      TRUE ~ NA                                             # unknown/missing policy year
    )
  )

biblio_with_policy %>%
  count(
    policy,
    policy_in_place,
    .drop = FALSE
  )

# Identify articles where the policy year is missing, excluding 'none' policies
journals_missing_policy_year <- biblio_with_policy %>%
  filter(policy %in% c("weak", "strong"),is.na(policyYYYY)) %>%
  distinct(journal_clean,policy,policyYYYY)

journals_missing_policy_year

# > unique_journals_missing_year
# journal_clean policy policyYYYY
# 1 agriculture ecosystems & environment strong         NA
# 2                 conservation biology   weak         NA https://www.topfactor.org/journals/conservation-biology "Requires disclosure of data availability through transparency checklist" Last Updated 2022-11-01
# 3                    landscape ecology   weak         NA https://www.topfactor.org/journals/landscape-ecology "Data sharing and evidence of data sharing encouraged" Last Updated 2023-07-06
# 4              biological conservation strong         NA https://www.topfactor.org/journals/biological-conservation "Data must be posted to a trusted repository" Last Updated 2023-10-19
# 5       journal of wildlife management   weak         NA
# 6               ecological engineering strong         NA
# 7                         ecohydrology   weak         NA https://www.topfactor.org/journals/ecohydrology "Journal encourages data sharing, or says nothing" Last Updated 2023-03-20
# 8                      austral ecology   weak         NA
# 9                         isme journal   weak         NA

# I contacted the journals asking for information on the year of implementation

journal_policy_summary <- biblio_with_policy %>%
  group_by(journal_clean) %>%
  summarise(
    n_articles = n(),
    n_before_policy = sum(policy_in_place == FALSE, na.rm = TRUE),
    n_after_policy  = sum(policy_in_place == TRUE, na.rm = TRUE),
    n_unknown       = sum(is.na(policy_in_place)),
    policy_year = unique(policyYYYY),
    policy_type = unique(policy)
  ) %>%
  ungroup()

journals_before_after <- journal_policy_summary %>%
  filter(n_before_policy > 0 & n_after_policy > 0)

journals_before_after

################################################################################
# Citations
################################################################################

# Adding a total number of citations for each article

citations_df <- citations_df %>%
  mutate(
    GS_total_citations = rowSums(
      across(starts_with("GS_citations_")),
      na.rm = TRUE
    )
  )

# and the first 6 years (including the publication year, but excluding any
# citations that happened before the actual publication of the article)

# Identify citation columns and extract their years
citation_cols  <- grep("^GS_citations_", names(citations_df), value = TRUE)
citation_years <- as.integer(str_remove(citation_cols, "GS_citations_"))

citations_df <- citations_df %>%
  rowwise() %>%
  mutate(
    GS_6_first_years = sum(
      c_across(all_of(citation_cols)) *
        (citation_years >= year_of_publication &
           citation_years <= year_of_publication + 5),
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  as.data.frame()

citations_df$GS_6_first_years_per_year <- citations_df$GS_6_first_years/6

head(citations_df)
summary(citations_df)

df.total.citations <- citations_df[,c("MA_ID","GS_date_citations","GS_total_citations",
                            "GS_6_first_years","GS_6_first_years_per_year")]

################################################################################
# Saving data
################################################################################

# adding the citation information to the dataset, and exporting for plotting

biblio_with_policy_citations <- biblio_with_policy %>%
  left_join(df.total.citations, by = "MA_ID") %>%
  as.data.frame()

write.csv(biblio_with_policy_citations,
          "data/02_processed_data/combined/meta-analysis_2016-2020_citations_and_policies_summary.csv",
          row.names=FALSE)


################################################################################

# From wide to long data
df_long <- citations_df %>%
  pivot_longer(
    cols = starts_with("GS_citations_"),
    names_to = "citation_year",
    values_to = "citations"
  ) %>%
  mutate(
    citation_year = as.numeric(gsub("GS_citations_", "", citation_year))
  )

# Compute years since publication
df_long <- df_long %>%
  mutate(years_since_pub = citation_year - year_of_publication)

# Remove NA citations
df_long <- df_long %>% filter(!is.na(citations))

# Identify peak citation year per meta-analysis
peaks <- df_long %>%
  group_by(MA_ID) %>%
  filter(citations == max(citations, na.rm = TRUE)) %>%
  # If multiple peaks, pick earliest one
  slice_min(years_since_pub, with_ties = FALSE) %>%
  ungroup()

# Computing mean and 95% CI for years_since_pub
summary_stats <- peaks %>%
  summarise(
    mean_peak_years = mean(years_since_pub, na.rm = TRUE),
    sd_peak_years = sd(years_since_pub, na.rm = TRUE),
    n = n(),
    se = sd_peak_years / sqrt(n),
    ci_lower = mean_peak_years - qt(0.975, df = n - 1) * se,
    ci_upper = mean_peak_years + qt(0.975, df = n - 1) * se
  )

summary_stats

# visualisation
citation_peak_plot<-ggplot(peaks, aes(x = years_since_pub)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  geom_vline(xintercept = summary_stats$mean_peak_years, color = "red", linetype = "dashed") +
  labs(
    x = "Years since publication (at citation peak)",
    y = "Number of meta-analyses",
    title = "Distribution of citation peak years since publication"
  ) +
  theme_minimal()


ggsave(
  filename = "figures/Supplementary_Figure_citation_peak_years.png",
  plot = citation_peak_plot,
  width = 18,
  height = 14,
  units = "cm",
)


# sessionInfo()
# R version 4.5.2 (2025-10-31)
# Platform: aarch64-apple-darwin20
# Running under: macOS Tahoe 26.3.1
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
# LAPACK: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# time zone: Europe/Berlin
# tzcode source: internal
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] lme4_2.0-1      Matrix_1.7-5    readxl_1.5.0    writexl_1.5.4   lubridate_1.9.5 forcats_1.0.1   stringr_1.6.0  
# [8] purrr_1.2.2     readr_2.2.0     tidyr_1.3.2     tibble_3.3.1    ggplot2_4.0.3   tidyverse_2.0.0 dplyr_1.2.1    
# 
# loaded via a namespace (and not attached):
#   [1] gtable_0.3.6       xfun_0.58          lattice_0.22-9     tzdb_0.5.0         vctrs_0.7.3        tools_4.5.2       
# [7] Rdpack_2.6.6       generics_0.1.4     pacman_0.5.1       pkgconfig_2.0.3    RColorBrewer_1.1-3 S7_0.2.2          
# [13] lifecycle_1.0.5    compiler_4.5.2     farver_2.1.2       textshaping_1.0.5  htmltools_0.5.9    yaml_2.3.12       
# [19] pillar_1.11.1      nloptr_2.2.1       MASS_7.3-65        reformulas_0.4.4   viridis_0.6.5      boot_1.3-32       
# [25] nlme_3.1-169       tidyselect_1.2.1   digest_0.6.39      stringi_1.8.7      labeling_0.4.3     splines_4.5.2     
# [31] rprojroot_2.1.1    fastmap_1.2.0      grid_4.5.2         here_1.0.2         cli_3.6.6          magrittr_2.0.5    
# [37] patchwork_1.3.2    utf8_1.2.6         withr_3.0.3        scales_1.4.0       timechange_0.4.0   rmarkdown_2.31    
# [43] otel_0.2.0         gridExtra_2.3      cellranger_1.1.0   ragg_1.5.2         hms_1.1.4          evaluate_1.0.5    
# [49] knitr_1.51         rbibutils_2.4.1    viridisLite_0.4.3  mgcv_1.9-4         rlang_1.3.0        Rcpp_1.1.2        
# [55] glue_1.8.1         rstudioapi_0.18.0  minqa_1.2.8        R6_2.6.1           systemfonts_1.3.2 