################################################################################
# Authors: 
#
# Alfredo Sanchez-Tojar (alfredo.tojar@gmail.com): original code
# Edited and reviewed by SD for producing figures

# Script first created in September 2025


################################################################################
# Description of script and Instructions
################################################################################

# This script is import the final dataset combining the hackathon and the manual
# extraction data to generate the figures for our analyses

################################################################################
# Packages needed
################################################################################

# install.packages("pacman")
# load packages
pacman::p_load(tidyverse,
               viridis,
               binom,
               patchwork)

# cleaning up
rm(list = ls())

################################################################################
# Functions needed
################################################################################

# none

################################################################################
# Data
################################################################################

# importing cleaned and final data from the hackathon + manual assessment
MR_MA <- read.csv("data/02_processed_data/combined/meta-research_meta-analysis_final_2016-2020.csv", header=T)
names(MR_MA)
table(MR_MA$effect_size_category)
table(MR_MA$software_used_category)
table(MR_MA$guidelines)
table(MR_MA$guidelines_name_category)
table(MR_MA$registration)

# importing cleaned and final data from the FAIR assessment
final_FAIR <- read.csv("data/02_processed_data/FAIR_assessment/FAIR_data_assessment_final.csv", header=T)
names(final_FAIR)
table(final_FAIR$article_links2data)
table(final_FAIR$number_of_FAIR_assessments)
table(final_FAIR$data_GUPID_presence_recat)
table(final_FAIR$data_sharing_location_recat_bin)
table(final_FAIR$data_metadata_presence)
table(final_FAIR$data_licence_presence_recat)
table(final_FAIR$data_dictionary_presence)
table(final_FAIR$data_type_FAIR)

# non-repeated variables other than MA_ID, good!
intersect(names(MR_MA),names(final_FAIR))

# importing cleaned and final data from the hackathon + manual assessment
citations_policies <- read.csv("data/02_processed_data/combined/meta-analysis_2016-2020_citations_and_policies_summary.csv", header=T)
names(citations_policies)
table(citations_policies$policy)
table(citations_policies$policy_in_place)

# recoding the existence of policies
citations_policies <- citations_policies %>%
  mutate(
    policy_in_place_type = case_when(
      is.na(policyYYYY) ~ NA_character_,
      policy_in_place == TRUE ~ policy,
      policy_in_place == FALSE ~ "none",
      TRUE ~ NA_character_
    )
  )

table(citations_policies$policy_in_place_type, useNA = "ifany")

# non-repeated variables other than MA_ID, good!
intersect(names(MR_MA),names(citations_policies))

#############################################################################
# Merging datasets
###############################################################################

final_MR_MA <- MR_MA %>%
  left_join(final_FAIR, by = "MA_ID") %>%
  as.data.frame()

names(final_MR_MA)

final_MR_MA <- final_MR_MA %>%
  left_join(citations_policies, by = "MA_ID") %>%
  as.data.frame()

names(final_MR_MA)

# "article_links2data" # Yes, No (Findable)
# "data_GUPID_presence_recat" # Yes, No (Findable)
# "data_sharing_location_recat_bin" # Yes, No (Findable)
# "data_metadata_presence" # Yes, No (Findable)
# "data_downloadable" # Yes, No (Accessible)
# "data_licence_presence_recat" # Yes, No (Interoperable)
# "data_dictionary_presence" # Yes, No (Reusable)
# "data_type_FAIR" # Yes, Partial, No (Reusable)

#############################################################################
# Lollipop Plots with 95% Wilson Confidence Intervals
###############################################################################

# DESCRIPTION:
# This script generates lollipop plots for several key variables in the
# final_MR_MA dataset, showing the percentage of "Yes" responses.
# Confidence intervals (95%) are calculated using the Wilson method via
# binom.confint(), which handles extreme proportions (0% or 100%) gracefully.
# The code covers:
#   1. Code/Data sharing variables
#   2. Effect size sign accounted (revised)
#   3. Alternative Risk of Bias (RoB) assessments

###############################################################################
# 1. Code/Data Sharing: Lollipop Plot (Figure 1)
###############################################################################

# Prepare summary data: select relevant variables and count occurrences
Fig1_sum_data <- final_MR_MA %>%
  select(processed_data_shared_final, 
         raw_data_shared_final, 
         code_shared_final, 
         data_shared_final,
         data_and_code) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(percent = n / sum(n) * 100) %>%  # percentage per variable
  filter(value == "Yes") %>%              # focus only on "Yes" responses
  ungroup()                               # ungroup so fct_reorder works correctly

# Compute 95% Wilson confidence intervals
Fig1_sum_data <- Fig1_sum_data %>%
  rowwise() %>%
  mutate(
    ci = list(binom.confint(x = n, 
                            n = sum(final_MR_MA[[variable]] != ""), 
                            methods = "wilson")),
    lower = ci$lower * 100,
    upper = ci$upper * 100
  )

# Manually define the desired order for the y-axis
desired_order <- c(
  "data_and_code",
  "code_shared_final",
  "processed_data_shared_final", 
  "raw_data_shared_final",
  "data_shared_final"
)

# Convert 'variable' to factor with this exact order
Fig1_sum_data <- Fig1_sum_data %>%
  mutate(variable = factor(variable, levels = desired_order))


# Plot
# Create a gradient palette for the 5 categories (using viridis magma for 
# colourblind friendliness, it is still not the best contrast but it is visible)
magma_palette <-setNames(viridis::magma(n = 5,begin = 0.1,end = 0.8),desired_order)


Figure_1_lollipop_sharing<-ggplot(Fig1_sum_data, aes(x = percent, y = variable)) +
  geom_segment(aes(x = 0, xend = percent, yend = variable),
               color = "grey80", linewidth = 0.7) +
  geom_point(aes(color = variable), size = 5) +  
  geom_errorbar(aes(xmin = lower,xmax = upper, colour = variable),
    orientation = "y", width = 0.25) +
  geom_text(aes(label = sprintf("%.1f%%", percent)),
            vjust = -1.5, size = 4) +
  
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_y_discrete(labels = c(
    "processed_data_shared_final" = "Processed\ndata",
    "raw_data_shared_final" = "Raw\ndata",
    "data_shared_final" = "Any data",
    "data_and_code" = "Data + Code",
    "code_shared_final" = "Code"
  )) +
  scale_color_manual(values = magma_palette, guide = "none") +  
  labs(x = "Percentage of articles", y = NULL) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title.x = element_text(size = 14,colour = "grey35",margin = margin(t = 12)),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "grey85", linewidth = 0.75),  # dashed vertical lines
    axis.line.x = element_line(color = "grey30", linewidth = 0.8),
    axis.line.y = element_line(colour = "grey30", linewidth = 0.8)
  )

ggsave(
  "figures/Figure1-data-code-sharing.png",
  width = 24,
  height = 13,
  units = "cm"
)

# calculating the percentages for no too
# Prepare data: select relevant variables and count occurrences
df_no <- final_MR_MA %>%
  select(processed_data_shared_final, 
         raw_data_shared_final, 
         code_shared_final, 
         data_shared_final,
         data_and_code) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(percent = n / sum(n) * 100) %>%  # percentage per variable
  filter(value == "No") %>%              # focus only on "No" responses
  ungroup()                               # ungroup so fct_reorder works correctly

# Compute 95% Wilson confidence intervals
df_no <- df_no %>%
  rowwise() %>%
  mutate(
    ci = list(binom.confint(x = n, 
                            n = sum(final_MR_MA[[variable]] != ""), 
                            methods = "wilson")),
    lower = ci$lower * 100,
    upper = ci$upper * 100
  )

###############################################################################
# 2. Effect Size Sign Accounted
###############################################################################

# meta-analyses that presumably had to deal with effect size direction comparability
binom.confint(x = 18, n = 81, methods = "wilson")

# Prepare counts excluding "NA" (character)
df_effect_clean <- final_MR_MA %>%
  count(effect_size_sign_accounted_revised) %>%
  filter(effect_size_sign_accounted_revised != "NA") %>%
  mutate(total = sum(n)) %>%
  rowwise() %>%
  mutate(
    # 95% Wilson CI for the percentage of Yes
    ci = list(binom.confint(x = n, n = total, methods = "wilson")),
    percent = n / total * 100,
    lower = ci$lower * 100,
    upper = ci$upper * 100
  ) %>%
  ungroup() %>%
  mutate(effect_size_sign_accounted_revised = 
           fct_reorder(effect_size_sign_accounted_revised, 
                       percent, .desc = FALSE)) %>%
  select(-ci)

# Plot
ggplot(df_effect_clean, aes(x = percent, 
                            y = effect_size_sign_accounted_revised)) +
  geom_segment(aes(x = 0, xend = percent, 
                   y = effect_size_sign_accounted_revised, 
                   yend = effect_size_sign_accounted_revised), 
               color = "grey70", size = 0.8) +
  geom_point(color = "steelblue", size = 5) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), 
                 height = 0.2, color = "steelblue", size = 1) +
  geom_text(aes(label = paste0(sprintf("%.1f", percent), "%")), 
            hjust = +0.4, vjust = -1, size = 4, color = "black") +
  # solid line at 0%
  geom_vline(xintercept = 0, color = "grey30", size = 0.8) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(x = "", 
       y = NULL, 
       title = "Effect Size Sign Accounted (18 studies): Lollipop Plot with 95% CI") +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "grey80"),  # dashed vertical lines
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 16),
    axis.line.x = element_line(color = "grey30", size = 0.8)   # solid x-axis line
  )


###############################################################################
# 3. Alternative Risk of Bias (RoB) Assessment
###############################################################################

# Transform RoB variables into Yes/No
df_RoB <- final_MR_MA %>%
  transmute(
    RoB_assessment_revised_merged_final = 
      ifelse(RoB_assessment_revised_merged_final == "Yes", "Yes", "No"),
    RoB_assessment_alternative_revised_merged_final = 
      ifelse(RoB_assessment_alternative_revised_merged_final == 
               "Experimental_design_Method_comparison", "Yes", "No")
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  count(variable, value) %>%
  group_by(variable) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  complete(variable, value = c("Yes", "No"), fill = list(n = 0)) %>%
  group_by(variable) %>%
  mutate(total = sum(n)) %>%
  rowwise() %>%
  mutate(
    # 95% Wilson CI for "Yes" proportions
    ci = list(binom.confint(x = ifelse(value == "Yes", n, 0), 
                            n = total, methods = "wilson")),
    percent = ifelse(value == "Yes", n / total * 100, NA),
    lower = ifelse(value == "Yes", ci$lower * 100, NA),
    upper = ifelse(value == "Yes", ci$upper * 100, NA)
  ) %>%
  ungroup() %>%
  filter(value == "Yes") %>%
  mutate(variable = fct_reorder(variable, percent, .desc = TRUE)) %>%
  select(-ci)

# Plot
ggplot(df_RoB, aes(x = percent, y = variable)) +
  geom_segment(aes(x = 0, xend = percent, 
                   y = variable, yend = variable), 
               color = "grey70", size = 0.8) +
  geom_point(color = "steelblue", size = 5) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), 
                 height = 0.2, color = "steelblue", size = 1.2) +
  # geom_text(aes(label = paste0(sprintf("%.1f", percent), "%")), 
  #           hjust = -2, vjust = 0.2, size = 4, color = "black") +
  # scale_x_continuous(expand = expansion(mult = c(0, 0.1)), 
  #                    limits = c(0, 100)) +
  # labs(x = "Percentage of 'Yes'", 
  #      y = NULL, 
  #      title = "Risk of Bias Assessment (Revised vs Alternative)") +
  # theme_minimal(base_size = 14) +
  # theme(panel.grid.major.y = element_blank(), 
  #       panel.grid.minor = element_blank())
  geom_text(aes(label = paste0(sprintf("%.1f", percent), "%")), 
            hjust = +0.3, vjust = -1, size = 4, color = "black") +
  # solid line at 0%
  geom_vline(xintercept = 0, color = "grey30", size = 0.8) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_y_discrete(labels = c(
    "RoB_assessment_alternative_revised_merged_final" = "Risk of Bias\n(Alternative)",
    "RoB_assessment_revised_merged_final" = "Risk of Bias")) +
  labs(x = "", 
       y = NULL, 
       title = "Risk of Bias Assessment: Lollipop Plot with 95% CI") +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "grey80"),  # dashed vertical lines
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 16),
    axis.line.x = element_line(color = "grey30", size = 0.8)   # solid x-axis line
  )


###############################################################################
# 4. Registration
###############################################################################

# Prepare counts excluding NA (if any)
df_registration <- final_MR_MA %>%
  count(registration) %>%
  filter(!is.na(registration)) %>%
  mutate(total = sum(n)) %>%
  rowwise() %>%
  mutate(
    # 95% Wilson CI for the percentage of Yes/No
    ci = list(binom.confint(x = n, n = total, methods = "wilson")),
    percent = n / total * 100,
    lower = ci$lower * 100,
    upper = ci$upper * 100
  ) %>%
  ungroup() %>%
  mutate(registration = fct_reorder(registration, percent, .desc = FALSE)) %>%
  select(-ci)

# Plot
ggplot(df_registration, aes(x = percent, y = registration)) +
  geom_segment(aes(x = 0, xend = percent, 
                   y = registration, yend = registration), 
               color = "grey70", size = 0.8) +
  geom_point(color = "steelblue", size = 5) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), 
                 height = 0.2, color = "steelblue", size = 1) +
  geom_text(aes(label = paste0(sprintf("%.1f", percent), "%")), 
            hjust = +0.4, vjust = -1, size = 4, color = "black") +
  # solid line at 0%
  geom_vline(xintercept = 0, color = "grey30", size = 0.8) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(x = "", 
       y = NULL, 
       title = "Registration of Studies: Lollipop Plot with 95% CI") +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "grey80"),  # dashed vertical lines
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 16),
    axis.line.x = element_line(color = "grey30", size = 0.8)   # solid x-axis line
  )


###############################################################################
# 5. Guidelines
###############################################################################

# Total counts
total_studies <- nrow(final_MR_MA)
yes_studies <- sum(final_MR_MA$guidelines == "Yes", na.rm = TRUE)

binom.confint(x = 18, n = 81, methods = "wilson")


# Counts
df_guidelines <- final_MR_MA %>%
  mutate(guidelines_combined = case_when(
    guidelines == "No" ~ "No",
    guidelines == "Yes" & !is.na(guidelines_name_category) ~ paste("Yes:", guidelines_name_category)
  )) %>%
  count(guidelines_combined) %>%
  mutate(
    # percentage
    percent = case_when(
      guidelines_combined == "No" ~ n / total_studies * 100,
      TRUE ~ n / yes_studies * 100
    ),
    # CI
    lower = case_when(
      guidelines_combined == "No" ~ binom.confint(n, total_studies, methods = "wilson")$lower * 100,
      TRUE ~ NA_real_  # optional: do not show CI for subcategories
    ),
    upper = case_when(
      guidelines_combined == "No" ~ binom.confint(n, total_studies, methods = "wilson")$upper * 100,
      TRUE ~ NA_real_
    ),
    # color
    color_group = ifelse(guidelines_combined == "No", "No", "Yes")
  )

# Define desired order for y-axis
# desired_order <- c("No", "Yes: PRISMA", "Yes: CEE", "Yes: Others")
desired_order <- c("Yes: Others", "Yes: CEE", "Yes: PRISMA", "No")

df_guidelines <- df_guidelines %>%
  mutate(guidelines_combined = factor(guidelines_combined, levels = desired_order))

# Plot
ggplot(df_guidelines, aes(x = percent, y = guidelines_combined)) +
  
  # Shaded background for the Yes rows (dynamically determined)
  annotate(
    "rect",
    xmin = 0, xmax = 100,
    ymin = 1 - 0.5, ymax = 3 + 0.5,   # cover all Yes subcategories
    fill = "grey75", alpha = 0.5
  ) +
  
  geom_segment(aes(x = 0, xend = percent, y = guidelines_combined, yend = guidelines_combined),
               color = "grey70", size = 0.8) +
  geom_point(aes(color = color_group), size = 5) +
  geom_errorbarh(aes(xmin = lower, xmax = upper, color = color_group),
                 height = 0.2, size = 1) +
  geom_text(aes(label = paste0(sprintf("%.1f", percent), "%")),
            hjust = +0.4, vjust = -1, size = 4, color = "black") +
  geom_vline(xintercept = 0, color = "grey30", size = 0.8) +
  
  scale_color_manual(values = c("No" = "lightblue", "Yes" = "navy"), guide = "none") +
  scale_x_continuous(
    limits = c(0, 100), breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(x = "", y = NULL, title = "Guidelines Followed: Lollipop Plot") +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "grey80"),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 16),
    axis.line.x = element_line(color = "grey30", size = 0.8)
  )


###############################################################################
# Define the variables by FAIR principle
###############################################################################


findable_vars <- c(
  "article_links2data",
  #"data_GUPID_presence_recat",
  "data_sharing_location_recat_bin",
  "data_metadata_presence"
)

# accessible_vars <- c(
#   "data_downloadable"
# )

accessible_vars <- c(
  "data_downloadable",
  "data_GUPID_presence_recat"
)

interoperable_vars <- c(
  "data_type_FAIR"
)

reusable_vars <- c(
  "data_dictionary_presence",
  "data_licence_presence_recat"
)

all_fair_vars <- c(
  findable_vars,
  accessible_vars,
  interoperable_vars,
  reusable_vars
)

# some summaries
table(final_MR_MA$data_sharing_location)
table(final_MR_MA$data_sharing_location_recat_bin)
final_MR_MA %>%
  summarise(
    x = sum(data_sharing_location_recat_bin == "Yes", na.rm = TRUE),
    n = sum(!is.na(data_sharing_location_recat_bin))
  ) %>%
  mutate(binom.confint(x, n, methods = "wilson"))

#################################
table(final_MR_MA$article_links2data)
final_MR_MA %>%
  summarise(
    x = sum(article_links2data == "Yes", na.rm = TRUE),
    n = sum(!is.na(article_links2data))
  ) %>%
  mutate(binom.confint(x, n, methods = "wilson"))

#################################
table(final_MR_MA$data_metadata_presence)
final_MR_MA %>%
  summarise(
    x = sum(data_metadata_presence == "Yes", na.rm = TRUE),
    n = sum(!is.na(data_metadata_presence))
  ) %>%
  mutate(binom.confint(x, n, methods = "wilson"))

#################################
table(final_MR_MA$data_downloadable)
final_MR_MA %>%
  summarise(
    x = sum(data_downloadable == "Yes", na.rm = TRUE),
    n = sum(!is.na(data_downloadable))
  ) %>%
  mutate(binom.confint(x, n, methods = "wilson"))

#################################
table(final_MR_MA$data_GUPID_presence)
table(final_MR_MA$data_GUPID_presence_recat)
final_MR_MA %>%
  summarise(
    x = sum(data_GUPID_presence_recat == "Yes", na.rm = TRUE),
    n = sum(!is.na(data_GUPID_presence_recat))
  ) %>%
  mutate(binom.confint(x, n, methods = "wilson"))

#################################
table(final_MR_MA$data_type)
table(final_MR_MA$data_type_FAIR)
final_MR_MA %>%
  summarise(
    x = sum(data_type_FAIR == "Yes", na.rm = TRUE),
    n = sum(!is.na(data_type_FAIR))
  ) %>%
  mutate(binom.confint(x, n, methods = "wilson"))

final_MR_MA %>%
  summarise(
    x = sum(data_type_FAIR == "Partial", na.rm = TRUE),
    n = sum(!is.na(data_type_FAIR))
  ) %>%
  mutate(binom.confint(x, n, methods = "wilson"))

#################################
table(final_MR_MA$data_licence_presence)
table(final_MR_MA$data_licence_presence_recat)
final_MR_MA %>%
  summarise(
    x = sum(data_licence_presence_recat == "Yes", na.rm = TRUE),
    n = sum(!is.na(data_licence_presence_recat))
  ) %>%
  mutate(binom.confint(x, n, methods = "wilson"))

#################################
table(final_MR_MA$data_dictionary_presence)
final_MR_MA %>%
  summarise(
    x = sum(data_dictionary_presence == "Yes", na.rm = TRUE),
    n = sum(!is.na(data_dictionary_presence))
  ) %>%
  mutate(binom.confint(x, n, methods = "wilson"))

###############################################################################
# Convert categorical values to numeric scores (NEW *_num variables)

# Maximum FAIR score = 8 
# % reflects proportion of FAIR criteria met per study
final_MR_MA <- final_MR_MA %>%
  mutate(
    across(
      all_of(all_fair_vars),
      ~ case_when(
        .x == "Yes" ~ 1,
        .x == "Partial" ~ 0.5,
        .x == "No" ~ 0,
        TRUE ~ NA_real_
      ),
      .names = "{.col}_num"
    )
  )

###############################################################################
# Create *_num variable lists (IMPORTANT)

findable_vars_num <- paste0(findable_vars, "_num")
accessible_vars_num <- paste0(accessible_vars, "_num")
interoperable_vars_num <- paste0(interoperable_vars, "_num")
reusable_vars_num <- paste0(reusable_vars, "_num")

all_fair_vars_num <- c(
  findable_vars_num,
  accessible_vars_num,
  interoperable_vars_num,
  reusable_vars_num
)

###############################################################################
# Compute overall FAIR score and percentage (USING *_num)

final_MR_MA <- final_MR_MA %>%
  rowwise() %>%
  mutate(
    FAIR_score = sum(c_across(all_of(all_fair_vars_num))),
    FAIR_score_perc = FAIR_score / length(all_fair_vars_num) * 100
  ) %>%
  ungroup()

###############################################################################
# Compute FAIR principle subscores (means, USING *_num)

final_MR_MA <- final_MR_MA %>%
  rowwise() %>%
  mutate(
    Findable_score = mean(c_across(all_of(findable_vars_num)), na.rm = TRUE),
    Accessible_score = mean(c_across(all_of(accessible_vars_num)), na.rm = TRUE),
    Interoperable_score = mean(c_across(all_of(interoperable_vars_num)), na.rm = TRUE),
    Reusable_score = mean(c_across(all_of(reusable_vars_num)), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  as.data.frame()

###############################################################################
# Sanity checks

summary(final_MR_MA$FAIR_score)

final_MR_MA %>%
  dplyr::filter(FAIR_score == 8)

summary(final_MR_MA$FAIR_score_perc)

final_MR_MA %>%
  select(Findable_score, Accessible_score, Interoperable_score, Reusable_score) %>%
  summary()


###############################################################################
# additional summary statistics on software, effect size, etc
###############################################################################
table(final_MR_MA$effect_size_category)

table(final_MR_MA$sampling_variance)
final_MR_MA %>%
  summarise(
    x = sum(sampling_variance == "Yes", na.rm = TRUE),
    n = sum(!is.na(sampling_variance))
  ) %>%
  mutate(binom.confint(x, n, methods = "wilson"))

table(final_MR_MA$software)
final_MR_MA %>%
  summarise(
    x = sum(software == "Yes", na.rm = TRUE),
    n = sum(!is.na(software))
  ) %>%
  mutate(binom.confint(x, n, methods = "wilson"))

table(final_MR_MA$software_used_category)

###############################################################################
# 6. Guidelines matter?
###############################################################################

# ------------------------------------------------------------------------------
# BEGIN exploratory analysis not included in final manuscript
# ------------------------------------------------------------------------------

# The exploratory transparency score included four binary indicators:
# data sharing, code sharing, software reporting, and protocol registration.
# Each "Yes" response contributed one point (range: 0–4), which was subsequently
# expressed as a percentage.

# This analysis and its associated figures were not retained in the final
# manuscript and are therefore commented out below. The code is preserved here
# for transparency and to document analyses considered during manuscript
# development.

# names(final_MR_MA)
# 
# # Define the transparency indicators
# transparency_vars <- c(
#   "data_shared_final", 
#   "code_shared_final", 
#   "software",
#   # "RoB_assessment_revised_merged_final", 
#   "registration"
# )
# 
# # Create summary variable
# final_MR_MA <- final_MR_MA %>%
#   mutate(
#     transparency_score = rowSums(across(all_of(transparency_vars), 
#                                         ~ ifelse(. == "Yes", 1, 0)),
#                                  na.rm = TRUE),
#     transparency_score_perc = 100*transparency_score/length(transparency_vars),
#   )
# 
# # summary per group
# final_MR_MA %>%
#   group_by(guidelines) %>%
#   summarise(
#     n = sum(!is.na(transparency_score_perc)),
#     mean = mean(transparency_score_perc, na.rm = TRUE),
#     sd = sd(transparency_score_perc, na.rm = TRUE),
#     median = median(transparency_score_perc, na.rm = TRUE),
#     min = min(transparency_score_perc, na.rm = TRUE),
#     max = max(transparency_score_perc, na.rm = TRUE),
#     q1 = quantile(transparency_score_perc, 0.25, na.rm = TRUE),
#     q3 = quantile(transparency_score_perc, 0.75, na.rm = TRUE)
#   )
# 
# ggplot(final_MR_MA, aes(x = guidelines, y = transparency_score_perc)) +
#   geom_boxplot()
# 
# # Wilcoxon rank sum test
# wilcox.test(transparency_score_perc ~ guidelines, data = final_MR_MA)
# 
# # Chi-square test
# table_data <- table(final_MR_MA$transparency_score_perc,
#                     final_MR_MA$guidelines)
# 
# chisq.test(table_data)
# 
# ggbetweenstats(
#   data  = final_MR_MA,
#   x     = guidelines,
#   y     = transparency_score_perc,
#   title = NULL,
#   results.subtitle = FALSE,
#   bf.message = FALSE,
#   type = "perm",
#   
#   point.args = list(
#     alpha = 0.7,
#     size  = 3,
#     shape = 16,
#     position = ggplot2::position_jitter(width = 0.12, height = 1.5)
#   ),
#   
#   centrality.plotting = FALSE,
#   messages = FALSE
# ) +
#   
#   scale_y_continuous(
#     limits = c(-5, 105),
#     breaks = seq(0, 100, 25),
#     labels = function(x) paste0(x, "%")
#   ) +
#   
#   scale_color_manual(values = c("No" = "lightblue", "Yes" = "navy")) +
#   scale_fill_manual(values = c("No" = "lightblue", "Yes" = "navy")) +
#   
#   labs(x = "Guidelines", y = "Transparency Score (%)") +
#   
#   theme(
#     axis.title.y = element_text(size = 16),
#     axis.title.x = element_text(size = 16),
#     axis.text    = element_text(size = 16),
#     
#     panel.grid.major.y = element_line(
#       linetype = "dashed",
#       color = "grey80",
#       linewidth = 0.5
#     ),
#     
#     panel.grid.minor.y = element_blank(),
#     
#     axis.line.x = element_line(color = "grey30", linewidth = 0.8),
#     axis.line.y = element_line(color = "grey30", linewidth = 0.8)
#   )
# 
# # Plotting distributions to show how clear the effect seem to be
# 
# levels <- c("0", "25", "50", "75")
# blue_palette <- colorRampPalette(c("lightblue", "navy"))(length(levels))
# names(blue_palette) <- levels
# 
# final_MR_MA %>%
#   mutate(transparency_score_perc = factor(transparency_score_perc,
#                                           levels = c(0, 25, 50, 75))) %>%
#   ggplot(aes(x = guidelines, fill = transparency_score_perc)) +
#   geom_bar(position = "fill") +
#   scale_y_continuous(limits = c(0, 1)) +
#   scale_fill_manual(values = blue_palette) +
#   labs(
#     x = "Guidelines",
#     y = "Proportion",
#     fill = "Transparency score (%)"
#   ) +
#   theme_minimal(base_size = 16) +
#   theme(
#     # axis text styling (match other figure)
#     axis.title.y = element_text(size = 16, face = "bold"),
#     axis.title.x = element_text(size = 16, face = "bold"),
#     axis.text    = element_text(size = 16),
#     
#     # axis lines (match thickness + color)
#     axis.line.x = element_line(color = "grey30", linewidth = 0.8),
#     axis.line.y = element_line(color = "grey30", linewidth = 0.8),
#     
#     # tighten plot margins to match other figure
#     plot.margin = margin(5.5, 5.5, 5.5, 5.5, unit = "pt"),
#     
#     # keep legend styling as-is
#     legend.title = element_text(size = 10),
#     legend.text  = element_text(size = 9),
#     legend.key.size = unit(0.4, "cm"),
#     legend.spacing.y = unit(0.2, "cm"),
#     legend.box.spacing = unit(0.2, "cm")
#   )
# 
# summary(final_MR_MA$transparency_score)
# summary(final_MR_MA$transparency_score_perc)

# ------------------------------------------------------------------------------
# END exploratory analysis not included in final manuscript
# ------------------------------------------------------------------------------
################################################################################
# Creating heatmap to show FAIR scores (Figure 2)
################################################################################


# ---- variables used in the heatmap ----
fair_items <- c(
  "data_sharing_location_recat_bin",  # (i) Data sharing location [F]
  "article_links2data",               # (ii) Article provides direct data mention/link/PID/accession [F]
  "data_metadata_presence",           # (iii) Metadata present [F]
  "data_downloadable",                # (iv) Data downloadable [A]
  "data_GUPID_presence_recat",        # (v) Data-specific PID present [A]
  "data_type_FAIR",                   # (vi) Data file format [I]
  "data_licence_presence_recat",      # (vii) Data licence present [R]
  "data_dictionary_presence"          # (viii) Data dictionary/codebook present [R]
)

fair_labels <- c(
  data_sharing_location_recat_bin = "Searchable\nrepository\n(63.6%)",
  article_links2data = "Article links\nto data\n(98.2%)",
  data_metadata_presence = "Metadata\npresent\n(69.1%)",
  data_downloadable = "Downloadable\ndata\n(100.0%)",
  data_GUPID_presence_recat = "Data-specific\nPID\n(61.8%)",
  data_type_FAIR = "Interoperable\nformat\n(38.2%)",
  data_licence_presence_recat = "Licence\npresent\n(63.6%)",
  data_dictionary_presence = "Data dictionary\npresent\n(45.5%)"
)

fair_components <- c(
  data_sharing_location_recat_bin = "Findable",
  article_links2data = "Findable",
  data_metadata_presence = "Findable",
  data_downloadable = "Accessible",
  data_GUPID_presence_recat = "Accessible",
  data_type_FAIR = "Interoperable",
  data_licence_presence_recat = "Reusable",
  data_dictionary_presence = "Reusable"
)

# data for plotting the heatmap
fair_plot_data <- final_FAIR %>%
  mutate(
    FAIR_score_numeric = rowMeans(
      across(
        all_of(fair_items),
        ~ case_when(
          .x == "Yes" ~ 1,
          .x == "Partial" ~ 0.5,
          .x == "No" ~ 0,
          TRUE ~ NA_real_
        )
      ),
      na.rm = TRUE
    )
  ) %>%
  arrange(desc(FAIR_score_numeric)) %>%
  mutate(MA_ID_ordered = factor(MA_ID, levels = rev(MA_ID))) %>%
  select(MA_ID_ordered, all_of(fair_items)) %>%
  pivot_longer(
    cols = all_of(fair_items),
    names_to = "indicator",
    values_to = "score"
  ) %>%
  mutate(
    indicator_label = fair_labels[indicator],
    component = fair_components[indicator],
    component = factor(component, levels = c("Findable", "Accessible", "Interoperable", "Reusable")),
    indicator_label = factor(
      indicator_label,
      levels = fair_labels[c(
        "data_sharing_location_recat_bin",
        "article_links2data",
        "data_metadata_presence",
        "data_downloadable",
        "data_GUPID_presence_recat",
        "data_type_FAIR",
        "data_licence_presence_recat",
        "data_dictionary_presence"
      )]
    ),
    score = factor(score, levels = c("No", "Partial", "Yes"))
  )

# ---- colourblind-friendly blue-toned palette ----
fair_cols <- c(
  "No"      = "#E8E8E8",
  "Partial" = "#D8A657",
  "Yes"     = "#176B73"
)

# ---- plot ----
p_fair_heatmap <- ggplot(
  fair_plot_data,
  aes(x = indicator_label, y = MA_ID_ordered, fill = score)
) +
  geom_tile(
    # colour = "white",
    linewidth = 0.3,
    width = 0.9,
    height = 0.9
  ) +
  facet_grid(
    . ~ component,
    scales = "free_x",
    space = "free_x",
  ) +
  scale_fill_manual(
    values = fair_cols,
    name = NULL,
    drop = FALSE
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 8) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    
    strip.placement = "outside",
    strip.background = element_rect(fill = "white", colour = NA),
    strip.text = element_text(face = "bold", size = 10),
    
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    
    axis.text.x = element_text(
      size = 9,
      angle = 0,
      hjust = 0.5,
      vjust = 1,
      lineheight = 0.9
    ),
    axis.ticks.x = element_blank(),
    
    panel.spacing.x = unit(0.8, "lines"),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 9),
    
    plot.margin = margin(5, 5, 5, 5)
  ) +
  guides(
    fill = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(colour = NA)
    )
  )

p_fair_heatmap

ggsave(
  "figures/Figure2_FAIR-heatmap.png",
  plot = p_fair_heatmap,
  width = 8,
  height = 8,
  dpi = 600,
  bg = "white"
)

###############################################################################
# Are guidelines associated with a higher FAIR score? (Figure 3)
###############################################################################

summary(final_MR_MA$FAIR_score)
summary(final_MR_MA$FAIR_score_perc)

# summary per group
final_MR_MA %>%
  group_by(guidelines) %>%
  summarise(
    n = sum(!is.na(FAIR_score_perc)),
    mean = mean(FAIR_score_perc, na.rm = TRUE),
    sd = sd(FAIR_score_perc, na.rm = TRUE),
    median = median(FAIR_score_perc, na.rm = TRUE),
    min = min(FAIR_score_perc, na.rm = TRUE),
    max = max(FAIR_score_perc, na.rm = TRUE),
    q1 = quantile(FAIR_score_perc, 0.25, na.rm = TRUE),
    q3 = quantile(FAIR_score_perc, 0.75, na.rm = TRUE)
  )

# Wilcoxon rank sum test
wilcox.test(FAIR_score_perc ~ guidelines, data = final_MR_MA)

# Chi-square test
table_data_2 <- table(final_MR_MA$FAIR_score_perc,
                      final_MR_MA$guidelines)

chisq.test(table_data_2)

# Plot for complete distribution between Guidelines and No-Guidelines Group

levels_fair <- c("0–25", "25–50", "50–75", "75–100")

# fair_palette <- viridis::magma(n = 4,begin = 0.3,end = 0.8, direction = -1)

fair_palette <- c(
  "0–25"   = "#6EC4A1",
  "25–50"  = "#4A94A0",
  "50–75"  = "#3E6493",
  "75–100" = "#3B3873"
)

names(fair_palette) <- levels_fair

plot_data <- final_MR_MA %>%
  filter(!is.na(FAIR_score_perc),
         !is.na(guidelines)) %>% 
  mutate(guidelines = factor(guidelines,
                                   levels = c("Yes", "No"),
                                   labels = c("Yes\n(n = 14)", "No\n(n = 41)")),
         FAIR_score_bin = cut(FAIR_score_perc, 
                              breaks = c(0, 25, 50, 75, 100),
                              include.lowest = TRUE,
                              labels = levels_fair))


Fair_Guidelines<-ggplot(plot_data,aes(x = guidelines,y = FAIR_score_perc)) +

# For the violin distribution
geom_violin(fill = "grey100",
            colour = "grey1", 
            linewidth = 0.3, 
            width = 0.4,
            trim = TRUE) +
# Then the boxplot on top
geom_boxplot(width = 0.12,
             fill = "grey100",
             colour = "grey1", 
             linewidth = 0.3) +
  
# Individual data points
geom_jitter(aes(colour = FAIR_score_bin),
    width = 0.12,
    height = 0,
    size = 1.5,
    alpha = 0.9) +
  
scale_colour_manual(values = fair_palette,
    name = "FAIR score (%)",
    drop = FALSE) +
scale_y_continuous(limits = c(-5, 105),
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%")) +
labs(x = "Guidelines",
    y = "FAIR score (%)") +
theme_minimal(base_size = 16) +
theme(axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 16),
    axis.text = element_text(size = 16),
    panel.grid.major.y = element_line(
      linetype = "dashed",
      colour = "grey80",
      linewidth = 0.5),
panel.grid.minor.y = element_blank(),
panel.grid.major.x = element_blank(),
axis.line.x = element_line(colour = "grey30",
      linewidth = 0.8),
axis.line.y = element_line(colour = "grey30",
      linewidth = 0.8))

# Second plot now (3B) Fair-citation association

Fair_Citation <- plot_data %>%
  filter(!is.na(FAIR_score_perc),!is.na(GS_6_first_years)) %>%
  mutate(FAIR_score_bin = cut(
      FAIR_score_perc,
      breaks = c(0, 25, 50, 75, 100),
      include.lowest = TRUE,
      labels = levels_fair)) %>%
  
  ggplot(aes(x = FAIR_score_perc,
    y = GS_6_first_years,
    colour = FAIR_score_bin)) +
  
  # points with x-jitter
  geom_point(alpha = 1, 
    size = 3,
    position = position_jitter(width = 1, height = 0)) +
  scale_colour_manual(values = fair_palette,
    name = "FAIR score (%)",
    breaks = levels_fair,
    drop = FALSE) +
  scale_x_continuous(limits = c(-5, 105),
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.01, 0.05))) +
  scale_y_continuous(limits = c(-5, 455),
    breaks = seq(0, 400, 100),
    expand = expansion(mult = c(0.01, 0.05))) +
  labs(x = "FAIR Score (%)",y = "First six years citations") +

  # Remove this panel's duplicate legend
  guides(colour = "none") +
  
  theme_minimal(base_size = 16) +
  
  theme(
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text    = element_text(size = 16),
    
    panel.grid.major.y = element_line(
      linetype = "dashed",
      color = "grey80",
      linewidth = 0.5
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "grey30", linewidth = 0.8),
    axis.line.y = element_line(color = "grey30", linewidth = 0.8)
  )


Figure_3 <- (Fair_Guidelines + Fair_Citation) +
  plot_layout(guide = "collect", widths = c(1, 1)) +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position = "right",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.35, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    legend.box.spacing = unit(0.15, "cm")
  )

Figure_3

ggsave(filename = "figures/Figure3-FAIR-Guidelines-Citation.png",
  plot = Figure_3, 
  width = 24, height = 12, units = "cm", dpi = 600)




################################################################################
# Does having any policy increase the likelihood of sharing data?

final_MR_MA <- final_MR_MA %>%
  mutate(
    policy_any_1 = case_when(
      is.na(policy_in_place_type) ~ NA_character_,
      policy_in_place_type == "none" ~ "none",
      TRUE ~ "any"
    )
  )

tab1 <- final_MR_MA %>%
  filter(!is.na(policy_any_1)) %>%
  with(table(data_shared_final, policy_any_1))

tab1

fisher.test(tab1)

################################################################################
# Does policy strength matter for data sharing?

tab2 <- final_MR_MA %>%
  filter(!is.na(policy_in_place_type)) %>%
  with(table(data_shared_final, policy_in_place_type))

tab2

fisher.test(tab2)

# running a glm to see directions and effects
final_MR_MA_glm <- final_MR_MA %>%
  filter(!is.na(policy_in_place_type)) %>%   # explicit exclusion
  mutate(
    data_shared_final = factor(data_shared_final, levels = c("No", "Yes")),
    policy_in_place_type = factor(
      policy_in_place_type,
      levels = c("none", "weak", "strong")
    )
  )

glm.policy <- glm(
  data_shared_final ~ policy_in_place_type,
  data = final_MR_MA_glm,
  family = binomial
)

summary(glm.policy)

exp(cbind(
  OR = coef(glm.policy),
  confint(glm.policy)
))

# ORs are relative to policy = "none"
# OR > 1 higher odds of data sharing
# OR < 1 lower odds

# Analyses excluded six studies for which policy status could not be classified. 
# We tested (i) whether the presence of any policy was associated with increased 
# data sharing using Fisher’s exact test, and (ii) whether policy strength 
# (none, weak, strong) was associated with data sharing using both Fisher’s 
# exact test and logistic regression to estimate odds ratios.


library(broom)

or_df <- tidy(glm.policy, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = recode(
      term,
      policy_in_place_typeweak   = "Weak policy vs none",
      policy_in_place_typestrong = "Strong policy vs none"
    )
  )
ggplot(or_df, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.15,
    color = "grey30",
    linewidth = 1
  ) +
  geom_point(size = 5, color = "grey20") +
  scale_x_log10(
    breaks = c(0.5, 1, 2, 5, 10, 50, 100),
    labels = c("0.5", "1", "2", "5", "10", "50", "100")
  ) +
  labs(
    x = "Odds ratio (log scale)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "grey80"),
    axis.line.x = element_line(color = "grey30", linewidth = 0.8),
    axis.text = element_text(size = 22, color = "grey20"),
    axis.title.x = element_text(size = 26, face = "bold", margin = margin(t = 12))
  )

# Writing final datasets for results

write.csv(final_FAIR,"data/02_processed_data/combined/final_FAIR.csv")
write.csv(final_MR_MA,"data/02_processed_data/combined/final_MR_MA.csv")


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
#   [1] broom_1.0.13      patchwork_1.3.2   ggstatsplot_1.0.0 scales_1.4.0      binom_1.1-2       viridis_0.6.5    
# [7] viridisLite_0.4.3 lme4_2.0-1        Matrix_1.7-5      readxl_1.5.0      writexl_1.5.4     lubridate_1.9.5  
# [13] forcats_1.0.1     stringr_1.6.0     purrr_1.2.2       readr_2.2.0       tidyr_1.3.2       tibble_3.3.1     
# [19] ggplot2_4.0.3     tidyverse_2.0.0   dplyr_1.2.1      
# 
# loaded via a namespace (and not attached):
#   [1] tidyselect_1.2.1       Rmpfr_1.1-2            farver_2.1.2           statsExpressions_2.0.0 S7_0.2.2              
# [6] fastmap_1.2.0          pacman_0.5.1           bayestestR_0.17.0      digest_0.6.39          timechange_0.4.0      
# [11] estimability_2.0.0     lifecycle_1.0.5        multcompView_0.1-11    magrittr_2.0.5         compiler_4.5.2        
# [16] rlang_1.3.0            tools_4.5.2            utf8_1.2.6             yaml_2.3.12            knitr_1.51            
# [21] labeling_0.4.3         here_1.0.2             RColorBrewer_1.1-3     SuppDists_1.1-9.9      withr_3.0.3           
# [26] grid_4.5.2             datawizard_1.3.1       xtable_1.8-8           paletteer_1.7.0        emmeans_2.0.3         
# [31] MASS_7.3-65            insight_1.5.0          cli_3.6.6              mvtnorm_1.4-1          rmarkdown_2.31        
# [36] ragg_1.5.2             reformulas_0.4.4       generics_0.1.4         RcppParallel_5.1.11-2  otel_0.2.0            
# [41] rstudioapi_0.18.0      tzdb_0.5.0             parameters_0.29.0      cachem_1.1.0           minqa_1.2.8           
# [46] splines_4.5.2          effectsize_1.0.2       cellranger_1.1.0       vctrs_0.7.3            boot_1.3-32           
# [51] hms_1.1.4              ggrepel_0.9.8          correlation_0.8.8      systemfonts_1.3.2      BWStest_0.2.3         
# [56] PMCMRplus_1.9.12       glue_1.8.1             rematch2_2.1.2         nloptr_2.2.1           stringi_1.8.7         
# [61] gtable_0.3.6           gmp_0.7-5.1            pillar_1.11.1          htmltools_0.5.9        R6_2.6.1              
# [66] textshaping_1.0.5      Rdpack_2.6.6           rprojroot_2.1.1        evaluate_1.0.5         lattice_0.22-9        
# [71] backports_1.5.1        rbibutils_2.4.1        memoise_2.0.1          rstantools_2.6.0       Rcpp_1.1.2            
# [76] coda_0.19-4.1          gridExtra_2.3          nlme_3.1-169           mgcv_1.9-4             xfun_0.58             
# [81] kSamples_1.2-12        pkgconfig_2.0.3      