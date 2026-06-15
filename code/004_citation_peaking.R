################################################################################
# Authors: 
#
# Alfredo Sanchez-Tojar (alfredo.tojar@gmail.com): original code

# Script first created in September 2025

################################################################################
# Description of script and Instructions
################################################################################

# This script is to add bibliographic and citation details to all meta-analyses
# in our list.

# Here is a dataset that I have collected in which "MA_ID" is the id for each of
# the 81 meta-analyses assessed, with their title ("title") and year of 
# publication ("year_of_publication"; which refers to the year they where 
# included in an issue, but notice that sometimes their corresponding date of 
# first online publication by the journal could be the year before; range: 2016 
# to 2020). For these 81 meta-analyses, I extracted their citations per year 
# using Google Scholar. "GS_date_citations" is the date of extraction. There are
# 10 columns containing the citations for each of those years (GS_citations_2016, 
# GS_citations_2017, ... , GS_citations_2025). In the latter, NA indicates that 
# the meta-analysis was not yet published. Notice, however, that for a few cases, 
# there might be citations in the year before they were published, which might 
# happen if the article was published online in the year before they were finally 
# included in the journal issue. What I would like to know using this dataset is 
# what is the average number of years after publication where citations peak. 

# For each meta-analysis (row):
# Identify its publication year (year_of_publication).
# Gather its citations per year (2016–2025).
# Compute the citations per year since publication, i.e. 
#       year_since_pub = citation_year - year_of_publication.
# Find the year_since_pub with the maximum number of citations for that 
#       meta-analysis (its “peak year”).
# Average those peak years since publication across all meta-analyses.


################################################################################
# Packages needed
################################################################################

# install.packages("pacman")
# load packages
pacman::p_load(dplyr,
               tidyr,
               ggplot2,
               stringr,
               purrr)


# cleaning up
rm(list = ls())

setwd("C:/Users/localadmin/Dropbox/EXCELSiOR/projects/meta-research/meta-analysis_quality_and_reproducibility")

# Load data for citations
df <- read.csv("data/processed_data/combined/meta-analysis_2016-2020_citations.csv")

# Load data with bibligraphic information
biblio <- read.csv("data/processed_data/combined/meta-analysis_2016-2020_bibliographic_info.csv")

################################################################################
# Bibliographic information
################################################################################

# excluding excluded articles
biblio.included <- biblio %>% filter(Include_exclude=="include")

# cleaning the data, starting with the journal
biblio_clean <- biblio.included %>%
  # Remove unneeded variables
  select(-paperID,-exclusion_explanation, -exclusion_category, -Include_exclude, -Clarification) %>%
  
  # Clean and harmonize journal names
  mutate(
    journal_clean = journal %>%
      str_to_lower() %>%                      # all lowercase
      str_replace_all("\\\\&", "&") %>%       # fix escaped ampersand
      str_replace_all("\\band\\b", "&") %>%   # replace "and" with "&"
      str_replace_all("\\bad\\b", "&") %>%    # fix "ad" typos to "&"
      str_squish()                            # trim + remove double spaces
  ) %>%
  # Manual harmonization for known inconsistencies
  mutate(
    journal_clean = case_when(
      journal_clean == "agricultural ecosystems & environment" ~ "agriculture ecosystems & environment",
      journal_clean == "global ecology ad biogeography" ~ "global ecology & biogeography",
      journal_clean == "biological conversation" ~ "biological conservation",
      journal_clean == "evolution & ecology of microbiomes" ~ "functional ecology", #this was a typo!
      TRUE ~ journal_clean
    )
  )

head(biblio_clean)
table(biblio_clean$journal_clean)
biblio_clean %>% count(journal_clean, sort = TRUE)


# cleaning the data, going for authors. For cleaning this messy variable, I 
# worked together with ChatGPT. The code is a bit of an overkilled but it works
# so I am keepin it here

# Helper function to extract first author surname
extract_first_surname <- function(a) {
  if (is.na(a) || str_trim(a) == "") return(NA_character_)
  s <- str_trim(a)
  s2 <- str_replace_all(s, "\\s*&\\s*", " and ")  # unify ampersands
  has_and <- str_detect(s2, regex("\\band\\b", ignore_case = TRUE))
  comma_count <- str_count(s2, ",")
  multiple <- FALSE
  
  if (has_and) {
    first_author <- str_trim(str_split(s2, regex("\\band\\b", ignore_case = TRUE))[[1]][1])
    multiple <- TRUE
  } else if (comma_count >= 2) {
    first_author <- str_trim(str_split(s2, ",")[[1]][1])
    multiple <- TRUE
  } else if (comma_count == 1) {
    parts <- str_split(s2, ",")[[1]]
    part_after <- str_trim(parts[2])
    if (str_detect(part_after, "\\s")) {
      first_author <- str_trim(parts[1])
      multiple <- TRUE
    } else {
      first_author <- str_trim(parts[1])
      multiple <- FALSE
    }
  } else {
    first_author <- s2
    multiple <- FALSE
  }
  
  fa <- first_author
  # If format "Last, First"
  if (str_detect(fa, ",")) {
    surname <- str_trim(str_split(fa, ",")[[1]][1])
  } else {
    # keep prefixes (van, de, etc.)
    if (str_detect(fa, regex("van\\s+der\\s+\\w+$", ignore_case = TRUE))) {
      surname <- str_extract(fa, regex("van\\s+der\\s+\\w+$", ignore_case = TRUE))
    } else if (str_detect(fa, regex("van\\s+de\\s+\\w+$", ignore_case = TRUE))) {
      surname <- str_extract(fa, regex("van\\s+de\\s+\\w+$", ignore_case = TRUE))
    } else if (str_detect(fa, regex("van\\s+den\\s+\\w+$", ignore_case = TRUE))) {
      surname <- str_extract(fa, regex("van\\s+den\\s+\\w+$", ignore_case = TRUE))
    } else if (str_detect(fa, regex("(van|von|de|del|di|da|la|le|du|st)\\s+\\w+$", ignore_case = TRUE))) {
      surname <- str_extract(fa, regex("(van|von|de|del|di|da|la|le|du|st)\\s+\\w+$", ignore_case = TRUE))
    } else {
      surname <- str_trim(str_extract(fa, "[^\\s]+$"))
    }
  }
  
  surname_clean <- surname %>%
    str_replace_all("\\.", "") %>%
    str_squish()
  
  surname_final <- str_replace_all(surname_clean, "\\s+", "_")
  
  if (multiple) paste0(surname_final, "_et_al") else surname_final
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
journal.policies <- read.csv("data/journal_policies/Berberi_and_Roche_OSF_Living_dataset_20251003/DataPolicies_DATA.csv")
head(journal.policies)

journal.policies_clean <- journal.policies %>%
  mutate(
    Journal_clean = Journal %>%
      str_to_lower() %>%                      # all lowercase
      str_replace_all("\\band\\b", "&") %>%   # replace "and" with &
      str_replace_all("\\s*&\\s*", " & ") %>% # unify spacing around &
      str_squish()                            # trim + remove extra spaces
  ) %>%
  # manual harmonization for known mismatches
  mutate(
    Journal_clean = case_when(
      Journal_clean == "agricultural ecosystems & environment" ~ "agriculture ecosystems & environment",
      Journal_clean == "global ecology ad biogeography" ~ "global ecology & biogeography",
      TRUE ~ Journal_clean
    ),
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

# Join policy info to your bibliographic dataset
biblio_with_policy <- biblio_clean %>%
  left_join(
    journal.policies_clean %>%
      select(Journal_clean, policy, policyYYYY),
    by = c("journal_clean" = "Journal_clean")
  )


# Larger journals have a higher IF? Just plotting.
journal_df <- biblio_clean %>%
  count(journal_clean, name = "n") %>%
  left_join(
    biblio_clean %>%
      select(journal_clean, five_year_impact_factor_2018) %>%
      distinct(),
    by = "journal_clean"
  )

journal_df %>%
  ggplot(aes(x = n, y = five_year_impact_factor_2018)) +
  #ggplot(aes(x = log(n), y = log(five_year_impact_factor_2018))) +
  
  # points
  geom_point(
    alpha = 0.7,
    size = 4,
    color = "grey30"
  ) +
  
  # linear trend with 95% CI
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "grey20",
    fill = "grey70",
    linewidth = 1
  ) +
  
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 6),
    expand = expansion(mult = c(0.05, 0.1))
  ) +
  
  # scale_x_log10(
  #   breaks = c(1, 2, 5, 10, 20),
  #   expand = expansion(mult = c(0.05, 0.1))
  # ) +
  
  labs(
    x = "Number of meta-analyses (n)",
    y = "Five-year impact factor (2018) [log]"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(linetype = "dashed", color = "grey80"),
    panel.grid.major.x = element_line(linetype = "dashed", color = "grey80"),
    panel.grid.minor = element_blank(),
    
    axis.line = element_line(color = "grey30", linewidth = 0.8),
    
    axis.title.x = element_text(
      size = 28,
      face = "bold",
      margin = margin(t = 12)
    ),
    axis.title.y = element_text(
      size = 28,
      face = "bold",
      margin = margin(r = 12)
    ),
    
    axis.text = element_text(size = 24, color = "grey20"),
    plot.title = element_text(size = 28, hjust = 0.5)
  )


# -----------------------------------------------------------------------------
# Identify journals with articles published both before and after policy
# implementation
# -----------------------------------------------------------------------------
# This section summarizes the bibliographic dataset by journal to determine
# how many articles were published before and after the journal's policy year.
# For journals with no policy ("none"), all articles are considered as having
# the policy in place. This allows us to identify journals that provide 
# "natural experiments" where the same journal has publications on both sides 
# of policy implementation.

# Determine if policy existed at the time of article publication
biblio_with_policy <- biblio_with_policy %>%
  mutate(
    policy_in_place = case_when(
      policy == "none" ~ TRUE,                               # no policy ever, treat as TRUE
      !is.na(policyYYYY) & year >= policyYYYY ~ TRUE,       # policy existed when article published
      !is.na(policyYYYY) & year < policyYYYY  ~ FALSE,      # policy not yet in place
      TRUE ~ NA                                             # unknown/missing policy year
    )
  )

# biblio_with_policy %>%
#   count(policy)

biblio_with_policy %>%
  count(policy_in_place)

biblio_with_policy %>%
  count(policy)

biblio_with_policy %>%
  filter(policy_in_place == TRUE) %>%
  count(policy)

# Identify articles where the policy year is missing, excluding 'none' policies
biblio_missing_policy_year <- biblio_with_policy %>%
  filter(is.na(policyYYYY) & policy != "none") %>%
  select(MA_ID, journal_clean, year, policy, policyYYYY)

# View the unique journals with missing policy year (excluding 'none')
unique_journals_missing_year <- biblio_missing_policy_year %>%
  distinct(journal_clean, policy,policyYYYY)

# Print results
unique_journals_missing_year

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

# -----------------------------------------------------------------------------
# Identify journals with articles published both before and after policy
# implementation
# -----------------------------------------------------------------------------
# This section summarizes the bibliographic dataset by journal to determine
# how many articles were published before and after the journal's policy year.
# It allows us to identify journals that provide "natural experiments" where
# the same journal has publications on both sides of policy implementation,
# which can be useful for comparing practices or outcomes pre- vs post-policy.
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

#
# 0. Adding a total number of citations for each article



df <- df %>%
  mutate(
    GS_total_citations = rowSums(
      dplyr::select(., starts_with("GS_citations_")),
      na.rm = TRUE
    )
  )



# and the first 6 years (including the publication year, but excluding any
# citations that happened before the actual publication of the article)

# Identify citation columns and extract their years
citation_cols  <- grep("^GS_citations_", names(df), value = TRUE)
citation_years <- as.integer(str_remove(citation_cols, "GS_citations_"))

df <- df %>%
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

df$GS_6_first_years_per_year <- df$GS_6_first_years/6

head(df)
summary(df)

df.total.citations <- df[,c("MA_ID","GS_date_citations","GS_total_citations",
                            "GS_6_first_years","GS_6_first_years_per_year")]

################################################################################
# Saving data
################################################################################

# adding the citation information to the dataset, and exporting for plotting

biblio_with_policy_citations <- biblio_with_policy %>%
  left_join(df.total.citations, by = "MA_ID") %>%
  as.data.frame()

write.csv(biblio_with_policy_citations,
          "data/processed_data/combined/meta-analysis_2016-2020_citations_and_policies_summary.csv",
          row.names=FALSE)


################################################################################
# 1. Reshape from wide to long
df_long <- df %>%
  pivot_longer(
    cols = starts_with("GS_citations_"),
    names_to = "citation_year",
    values_to = "citations"
  ) %>%
  mutate(
    citation_year = as.numeric(gsub("GS_citations_", "", citation_year))
  )

################################################################################
# 2. Compute years since publication
df_long <- df_long %>%
  mutate(years_since_pub = citation_year - year_of_publication)

################################################################################
# 3. Remove NA citations
df_long <- df_long %>% filter(!is.na(citations))
df_long

################################################################################
# 4. Identify peak citation year per meta-analysis
peaks <- df_long %>%
  group_by(MA_ID) %>%
  filter(citations == max(citations, na.rm = TRUE)) %>%
  # If multiple peaks, pick earliest one
  slice_min(years_since_pub, with_ties = FALSE) %>%
  ungroup()

peaks

################################################################################
# 5. Compute mean and 95% CI for years_since_pub
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

################################################################################
# visualisation
ggplot(peaks, aes(x = years_since_pub)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  geom_vline(xintercept = summary_stats$mean_peak_years, color = "red", linetype = "dashed") +
  labs(
    x = "Years since publication (at citation peak)",
    y = "Number of meta-analyses",
    title = "Distribution of citation peak years since publication"
  ) +
  theme_minimal()


################################################################################
# modelling
################################################################################
# Load packages
library(dplyr)
library(tidyr)
library(lme4)

################################################################################
# ---- Reshape to long format ----
df_long <- df %>%
  pivot_longer(
    cols = starts_with("GS_citations_"),
    names_to = "citation_year",
    values_to = "citations"
  ) %>%
  mutate(
    citation_year = as.integer(gsub("GS_citations_", "", citation_year)),
    years_since_pub = citation_year - year_of_publication
  ) %>%
  filter(!is.na(citations))  # Drop years before publication (no data)

df_long

# # Optionally drop pre-publication years if you don’t want them:
df_long <- df_long %>% filter(years_since_pub >= 0)
# # subsetting for paper published before 2019
#df_long <- df_long %>% filter(year_of_publication <= 2018)

################################################################################
# ---- Center predictor for numerical stability ----
df_long <- df_long %>%
  mutate(years_c = years_since_pub - mean(years_since_pub, na.rm = TRUE))

################################################################################
# ---- Fit the model ----
# Model: citations ~ years_c + I(years_c^2) + (1 + years_c | MA_ID)
# Poisson link with random intercept and slope per MA

m_glmer <- glmer(
  citations ~ years_c + I(years_c^2) + (1 + years_c | MA_ID),
  data = df_long,
  family = poisson(link = "log"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(m_glmer)

################################################################################
# check for overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq / rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}

overdisp_fun(m_glmer)

# If ratio ≫ 1 or p < 0.05 → overdispersion is likely → use:

# Option 1: Add observation-level random effect (OLRE)
df_long$obs <- 1:nrow(df_long)
m_glmer_nb <- glmer(
  citations ~ years_c + I(years_c^2) + (1 + years_c | MA_ID) + (1 | obs),
  data = df_long,
  family = poisson(link = "log")
)

summary(m_glmer_nb)
overdisp_fun(m_glmer_nb)

################################################################################
# 
# Extract fixed effects and random slopes per meta-analysis (MA_ID),
# Compute each MA’s peak year (the year where predicted citations are highest),
# Derive a mean and 95% CI for the distribution of these peaks across all meta-analyses.

library(broom.mixed)

# 1. Extract coefficients
fixef_vals <- fixef(m_glmer)  # fixed effects: intercept, years_c, years_c^2
ranef_vals <- ranef(m_glmer)$MA_ID  # random effects per MA_ID

# 2. Get fixed slopes
b1 <- fixef_vals["years_c"]
b2 <- fixef_vals["I(years_c^2)"]

# 3. Compute peak per meta-analysis
peaks_df <- ranef_vals %>%
  mutate(
    MA_ID = rownames(ranef_vals),
    slope_adj = b1 + years_c,    # add random slope
    peak_centered = -slope_adj / (2 * b2)
  ) %>%
  as.data.frame() %>%
  select(MA_ID, peak_centered)

# 4. Convert centered peaks to raw scale
mean_years_c <- mean(df_long$years_since_pub, na.rm = TRUE)
peaks_df <- peaks_df %>%
  mutate(peak_raw = peak_centered + mean_years_c)

# 5. Summarize across MAs
peak_summary <- peaks_df %>%
  summarise(
    mean_peak = mean(peak_raw, na.rm = TRUE),
    sd_peak = sd(peak_raw, na.rm = TRUE),
    n = n(),
    se_peak = sd_peak / sqrt(n),
    ci_lower = mean_peak - 1.96 * se_peak,
    ci_upper = mean_peak + 1.96 * se_peak
  )

peak_summary


# After computing peaks, you can verify:
peaks_df <- peaks_df %>%
  left_join(df_long %>%
              group_by(MA_ID) %>%
              summarise(max_observed_year = max(years_since_pub, na.rm = TRUE)),
            by = "MA_ID") %>%
  mutate(out_of_range = peak_raw > max_observed_year)

table(peaks_df$out_of_range)

# This tells you how many predicted peaks are outside observed years. If many 
# are, the model might be too flexible or extrapolating too much.


################################################################################
# Plotting

library(dplyr)
library(ggplot2)
library(ggeffects)

# --- 0. sanity check: objects exist ---
if(!exists("m_glmer_nb") || !exists("df_long")) stop("Make sure 'm_glmer_nb' and 'df_long' exist in your workspace.")

# --- 1. Population-level prediction curve (with 95% CI) ---
# ggpredict gives you population-level effects (marginal over RE)
pred_fe <- ggpredict(m_glmer_nb, terms = "years_c [all]") %>%
  as.data.frame() %>%
  rename(
    years_c = x,
    predicted = predicted,
    conf.low = conf.low,
    conf.high = conf.high
  )

# Convert back to raw scale (years since publication)
mean_years <- mean(df_long$years_since_pub, na.rm = TRUE)
pred_fe <- pred_fe %>%
  mutate(years_since_pub = years_c + mean_years)

# --- 2. Compute per-MA peaks (analytic quadratic vertex), clip to observed range ---
# Extract fixed effects
fe <- fixef(m_glmer_nb)
b1 <- fe["years_c"]
b2 <- fe["I(years_c^2)"]

# Extract random effects for MA_ID
re_ma <- ranef(m_glmer_nb)$MA_ID %>% as.data.frame()
re_ma$MA_ID <- rownames(ranef(m_glmer_nb)$MA_ID)

# Compute per-MA peaks
peaks_df <- re_ma %>%
  rename(u0 = "(Intercept)", u1 = "years_c") %>%
  mutate(
    slope_adj = b1 + u1,                      # Adjusted slope per MA
    peak_centered = -slope_adj / (2 * b2),    # Peak (centered scale)
    peak_raw = peak_centered + mean_years     # Convert to raw years_since_pub
  )

# Clip peaks to observed range
max_obs <- df_long %>%
  group_by(MA_ID) %>%
  summarise(max_obs_year = max(years_since_pub, na.rm = TRUE), .groups = "drop")

peaks_df <- peaks_df %>%
  left_join(max_obs, by = "MA_ID") %>%
  mutate(peak_clipped = pmin(peak_raw, max_obs_year))

# --- 3. Get predicted citation count at per-MA peaks ---
pred_new <- peaks_df %>%
  transmute(
    MA_ID,
    years_since_pub = peak_clipped,
    years_c = peak_clipped - mean_years
  )

# --- Fix: ignore obs-level random effect and keep MA_ID RE ---
pred_new$predicted_count <- predict(
  m_glmer_nb,
  newdata = pred_new,
  type = "response",
  re.form = ~(1 + years_c | MA_ID)  # include only MA_ID RE, ignore obs
)

# Merge predicted counts into peaks_df
peaks_df <- peaks_df %>%
  left_join(pred_new %>% select(MA_ID, predicted_count), by = "MA_ID")

################################################################################
# --- 4. Plot ---
p <- ggplot() +
  # Raw per-MA citation trajectories
  geom_line(
    data = df_long,
    aes(x = years_since_pub, y = citations, group = MA_ID),
    color = "gray70", alpha = 0.5, size = 0.5
  ) +
  # Population-level 95% CI ribbon and mean curve
  geom_ribbon(
    data = pred_fe,
    aes(x = years_since_pub, ymin = conf.low, ymax = conf.high),
    fill = "skyblue", alpha = 0.25
  ) +
  geom_line(
    data = pred_fe,
    aes(x = years_since_pub, y = predicted),
    color = "blue", size = 1.2
  ) +
  # Per-MA predicted peaks
  geom_point(
    data = peaks_df %>% filter(!is.na(predicted_count)),
    aes(x = peak_clipped, y = predicted_count),
    color = "red", fill = "red", size = 2, alpha = 0.8
  ) +
  # Optional: vertical segment to peak
  geom_segment(
    data = peaks_df %>% filter(!is.na(predicted_count)),
    aes(x = peak_clipped, xend = peak_clipped, y = 0, yend = predicted_count),
    color = "red", alpha = 0.3, size = 0.3
  ) +
  labs(
    x = "Years since publication",
    y = "Citations (Google Scholar)",
    title = "Citation trajectories (gray) and population fit (blue)",
    subtitle = "Red points = per-MA estimated peaks (clipped to observed range); ribbon = 95% CI"
  ) +
  theme_minimal(base_size = 14)

print(p)


# ================================================================================
# Plot description:
#
# Title:
# "Citation trajectories (gray) and population fit (blue)"
#
# Axes:
# - X-axis: Years since publication — how many years have passed since a paper was published.
# - Y-axis: Citations (Google Scholar) — the number of citations each paper has received.
#
# Elements in the plot:
# 1. Gray lines:
#    - Each gray line represents a single MA_ID’s citation trajectory over time.
#    - Shows the raw, observed citation counts per paper.
#    - Transparency (alpha = 0.5) keeps overlapping lines visible but not overwhelming.
#
# 2. Blue curve:
#    - Represents the population-level prediction from the quadratic GLMM, marginalizing over random effects.
#    - Shows the average citation trajectory expected across all MAs.
#
# 3. Light blue ribbon around the blue line:
#    - The 95% confidence interval of the population-level prediction.
#    - Gives a sense of uncertainty around the mean trajectory.
#
# 4. Red points:
#    - Indicate the predicted peak citation count for each MA, calculated as the vertex of the quadratic fit, and clipped to the observed years if necessary.
#    - Shows when each MA is expected to reach its highest citation count.
#
# 5. Red vertical lines:
#    - Extend from y = 0 to the predicted peak citation count.
#    - Provide a visual cue linking the x-axis (time) to the peak.
#
# Interpretation:
# - Gray lines show raw variation in trajectories across MAs.
# - Blue curve shows the overall trend, smoothing over individual differences.
# - Red points highlight the timing and magnitude of per-MA peaks, helping identify which MAs reach their maximum influence earlier or later.
# - The plot effectively visualizes both individual-level trajectories and the average pattern, while also showing uncertainty via the confidence ribbon.
# ================================================================================





# ================================================================================
# Population-level peak description:
#
# This calculates the mean peak (vertex) of the population-level citation trajectory
# from the quadratic GLMM, along with an approximate 95% confidence interval.
#
# - pop_peak_raw: Estimated year since publication at which the overall population
#   of MAs reaches maximum citations (based on the fixed-effects curve).
#
# - ci_lower / ci_upper: Approximate 95% confidence interval around the population
#   peak, derived using the delta method from the variance-covariance matrix of
#   the fixed effects.
#
# Interpretation:
# - The population peak represents the "average" timing of maximum impact across
#   all MAs, ignoring MA-specific random effects and obs-level noise.
# - The CI provides a measure of uncertainty in this estimate due to the model's
#   fixed-effects parameter uncertainty.
# - This can be visualized as a single point (or vertical line) on the citation
#   trajectory plot to show where the overall population is expected to peak.
# ================================================================================


# Extract fixed effects
fe <- fixef(m_glmer_nb)
b1 <- fe["years_c"]
b2 <- fe["I(years_c^2)"]

# Population-level peak (centered scale)
pop_peak_centered <- -b1 / (2 * b2)

# Convert to raw scale
mean_years <- mean(df_long$years_since_pub, na.rm = TRUE)
pop_peak_raw <- pop_peak_centered + mean_years
pop_peak_raw

# Variance-covariance of fixed effects
vcov_fe <- vcov(m_glmer_nb)

# Delta method for peak variance
var_peak <- (1 / (4 * b2^2)) * (
  vcov_fe["years_c", "years_c"] +
    (b1^2 / b2^2) * vcov_fe["I(years_c^2)", "I(years_c^2)"] -
    2 * (b1 / b2) * vcov_fe["years_c", "I(years_c^2)"]
)

# Standard error and 95% CI
se_peak <- sqrt(var_peak)
ci_lower <- pop_peak_raw - 1.96 * se_peak
ci_upper <- pop_peak_raw + 1.96 * se_peak

# Print results
pop_peak_raw
ci_lower
ci_upper



# ================================================================================
# Compute average citations per year across papers
# ================================================================================

avg_citations_per_year <- df_long %>%
  group_by(years_since_pub) %>%            # group by years since publication
  summarise(
    mean_citations = mean(citations, na.rm = TRUE),   # average across papers
    sd_citations = sd(citations, na.rm = TRUE),       # standard deviation
    cv_citations = sd_citations/mean_citations,
    n_papers = n()                                    # number of papers per year
  ) %>%
  ungroup()

# View the result
avg_citations_per_year

ggplot(avg_citations_per_year, aes(x = years_since_pub, y = mean_citations)) +
  geom_line(color = "blue", size = 1.2) +
  geom_ribbon(aes(ymin = mean_citations - sd_citations,
                  ymax = mean_citations + sd_citations),
              fill = "skyblue", alpha = 0.25) +
  labs(
    x = "Years since publication",
    y = "Average citations per year per paper",
    title = "Average citation trajectory across papers"
  ) +
  theme_minimal(base_size = 14)


ggplot(avg_citations_per_year, 
       aes(x = years_since_pub, y = mean_citations)) +
  geom_point(size = 3, color = "#2C7BB6") +
  geom_errorbar(aes(ymin = mean_citations - sd_citations,
                    ymax = mean_citations + sd_citations),
                width = 0, size = 0.9, color = "#2C7BB6") +
  scale_x_continuous(breaks = 0:10, limits = c(0, 10)) +
  labs(
    x = "Years since publication",
    y = "Average citations per year per paper",
    title = "Average citation trajectory across papers"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
