################################################################################
# Authors: 
#
# Alfredo Sanchez-Tojar (alfredo.tojar@gmail.com): original code

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
pacman::p_load(dplyr,
               tidyverse,
               ggplot2,
               forcats,
               purrr,
               binom,
               scales,
               ggstatsplot)

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
MR_MA <- read.csv("data/processed_data/combined/meta-research_meta-analysis_final_2016-2020.csv", header=T)
names(MR_MA)
table(MR_MA$effect_size_category)
table(MR_MA$software_used_category)
table(MR_MA$guidelines)
table(MR_MA$guidelines_name_category)
table(MR_MA$registration)

# importing cleaned and final data from the FAIR assessment
final_FAIR <- read.csv("data/processed_data/FAIR_assessment/FAIR_data_assessment_final.csv", header=T)
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
citations_policies <- read.csv("data/processed_data/combined/meta-analysis_2016-2020_citations_and_policies_summary.csv", header=T)
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
# 1. Code/Data Sharing: Lollipop Plot
###############################################################################

# Prepare data: select relevant variables and count occurrences
df_yes <- final_MR_MA %>%
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
df_yes <- df_yes %>%
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
df_yes <- df_yes %>%
  mutate(variable = factor(variable, levels = desired_order))


# Plot
# Create a blue gradient palette for the 6 categories
blue_palette <- gradient_n_pal(c("lightblue", "navy"))(seq(1, 0, length.out = length(desired_order)))

names(blue_palette) <- desired_order  # assign names to match factor levels

ggplot(df_yes, aes(x = percent, y = variable)) +
  geom_segment(aes(x = 0, xend = percent, y = variable, yend = variable),
               color = "grey70", size = 0.8) +
  geom_point(aes(color = variable), size = 5) +  
  geom_errorbarh(aes(xmin = lower, xmax = upper, color = variable), 
                 height = 0.2, size = 1.2) +
  geom_text(aes(label = sprintf("%.1f%%", percent)),
            hjust = +0.4, vjust = -1, size = 4) +
  
  # solid line at 0%
  geom_vline(xintercept = 0, color = "grey30", size = 0.8) +
  
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_y_discrete(labels = c(
    "processed_data_shared_final" = "Processed\ndata",
    "raw_data_shared_final" = "Raw\ndata",
    "data_shared_final" = "Data",
    "data_and_code" = "Data + Code",
    "code_shared_final" = "Code"
  )) +
  scale_color_manual(values = blue_palette, guide = "none") +  
  #labs(x = "", y = NULL, title = "Data/Code Sharing: Lollipop Plot with 95% CI") +
  labs(x = "Percentage of articles", y = NULL, title = NULL) +
  theme_minimal(base_size = 20) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "grey80"),  # dashed vertical lines
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 16),
    axis.line.x = element_line(color = "grey30", size = 0.8)   # solid x-axis line
  )

ggsave(
  "figures/Figure_1_lollipop_sharing.png",
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
# 2. Effect Size Sign Accounted (Revised)
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

# # Order levels manually
# desired_order <- c("No", "Yes: PRISMA", "Yes: CEE", "Yes: Others")
# df_guidelines <- df_guidelines %>%
#   mutate(guidelines_combined = factor(guidelines_combined, levels = desired_order))
# 
# # Plot
# ggplot(df_guidelines, aes(x = percent, y = guidelines_combined)) +
#   geom_segment(aes(x = 0, xend = percent, y = guidelines_combined, yend = guidelines_combined),
#                color = "grey70", size = 0.8) +
#   geom_point(aes(color = color_group), size = 5) +
#   geom_errorbarh(aes(xmin = lower, xmax = upper, color = color_group),
#                  height = 0.2, size = 1) +
#   geom_text(aes(label = paste0(sprintf("%.1f", percent), "%")),
#             hjust = +0.4, vjust = -1, size = 4, color = "black") +
#   geom_vline(xintercept = 0, color = "grey30", size = 0.8) +
#   scale_color_manual(values = c("No" = "lightblue", "Yes" = "navy"), guide = "none") +
#   scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10),
#                      labels = function(x) paste0(x, "%"),
#                      expand = expansion(mult = c(0, 0.05))) +
#   labs(x = "", y = NULL,
#        title = "Guidelines Followed: Lollipop Plot") +
#   theme_minimal(base_size = 20) +
#   theme(panel.grid.major.y = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_line(linetype = "dashed", color = "grey80"),
#         panel.grid.minor.x = element_blank(),
#         axis.text.y = element_text(size = 16),
#         axis.line.x = element_line(color = "grey30", size = 0.8))



###############################################################################
# 5. Guidelines: Lollipop Plot Showing "No" vs "Yes" Subcategories
###############################################################################

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


# ###############################################################################
# # 5. Guidelines
# ###############################################################################
# # Prepare data for stacked bar
# df_guidelines_bar <- final_MR_MA %>%
#   mutate(
#     guidelines_combined = case_when(
#       guidelines == "No" ~ "No",
#       guidelines == "Yes" & !is.na(guidelines_name_category) ~ paste("Yes:", guidelines_name_category)
#     )
#   ) %>%
#   count(guidelines_combined) %>%
#   mutate(percent = n / sum(n) * 100)
# 
# # Optional: define colors
# guideline_colors <- c(
#   "No" = "lightblue",
#   "Yes: CEE" = "navy",
#   "Yes: PRISMA" = "navy",
#   "Yes: Others" = "navy"
# )
# 
# # Stacked bar plot
# ggplot(df_guidelines_bar, aes(x = 1, y = percent, fill = guidelines_combined)) +
#   geom_bar(stat = "identity", width = 0.5, color = "grey30") +
#   geom_text(aes(label = paste0(sprintf("%.1f", percent), "%")),
#             position = position_stack(vjust = 0.5), color = "white", size = 6) +
#   scale_fill_manual(values = guideline_colors, guide = "none") +
#   scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.05))) +
#   labs(
#     x = NULL,
#     y = "Percentage of Studies",
#     title = "Guidelines Followed by Studies"
#   ) +
#   theme_minimal(base_size = 18) +
#   theme(
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank()
#   )



###############################################################################
# Define the variables by FAIR principle

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

names(final_MR_MA)

# creating a variable that summarises all the transparency indicators, using 1
# for Yes, and 0 for No. 
# Transparency indicators: data_shared_final, code_shared_final, software,
# RoB_assessment_revised_merged_final, registration
# Min: 0, Max: 5

# Define the transparency indicators
transparency_vars <- c(
  "data_shared_final", 
  "code_shared_final", 
  "software",
  #"RoB_assessment_revised_merged_final", 
  "registration"
)

# Create summary variable
final_MR_MA <- final_MR_MA %>%
  mutate(
    transparency_score = rowSums(across(all_of(transparency_vars), 
                                        ~ ifelse(. == "Yes", 1, 0)),
                                 na.rm = TRUE),
    transparency_score_perc = 100*transparency_score/length(transparency_vars),
  )

# summary per group
final_MR_MA %>%
  group_by(guidelines) %>%
  summarise(
    n = sum(!is.na(transparency_score_perc)),
    mean = mean(transparency_score_perc, na.rm = TRUE),
    sd = sd(transparency_score_perc, na.rm = TRUE),
    median = median(transparency_score_perc, na.rm = TRUE),
    min = min(transparency_score_perc, na.rm = TRUE),
    max = max(transparency_score_perc, na.rm = TRUE),
    q1 = quantile(transparency_score_perc, 0.25, na.rm = TRUE),
    q3 = quantile(transparency_score_perc, 0.75, na.rm = TRUE)
  )

ggplot(final_MR_MA, aes(x = guidelines, y = transparency_score_perc)) +
  geom_boxplot()

# Wilcoxon rank sum test
wilcox.test(transparency_score_perc ~ guidelines, data = final_MR_MA)

# Chi-square test
table_data <- table(final_MR_MA$transparency_score_perc,
                    final_MR_MA$guidelines)

chisq.test(table_data)

# set.seed(77)
# 
# ggbetweenstats(
#   data  = final_MR_MA,
#   x     = guidelines,
#   y     = transparency_score_perc,
#   title = "Guidelines and Transparency Score",
#   results.subtitle = FALSE,
#   #results.subtitle = TRUE, # for twelch
#   bf.message = FALSE,
#   type = "perm",
#   
#   # raw points
#   point.args = list(
#     alpha = 0.7,
#     size  = 3,
#     shape = 16,
#     position = ggplot2::position_jitter(width = 0.12, height = 0)
#   ),
#   
#   # mean dot
#   centrality.point.args = list(
#     alpha = 1,
#     size  = 6,
#     shape = 16
#   ),
#   
#   # mean label
#   centrality.label.args = list(
#     size = 5,     # increase font size
#     color = "black",
#     position = ggplot2::position_nudge(x = 0.25),  # move the box to the right of the dot
#     #hjust = -1.2,
#     vjust = 1 # adjust vertical position if needed
#   ),
#   
#   messages = FALSE
# ) +
#   scale_y_continuous(
#     limits = c(0, 100),
#     breaks = seq(0, 100, 10),
#     labels = function(x) paste0(x, "%")
#   ) +
#   scale_color_manual(values = c("No" = "lightblue", "Yes" = "navy")) +
#   scale_fill_manual(values = c("No" = "lightblue", "Yes" = "navy")) +
#   labs(x = NULL, y = "Transparency Score (%)") +
#   theme(
#     axis.title.y = element_text(size = 16),
#     axis.text    = element_text(size = 16),
#     plot.title   = element_text(size = 20, hjust = 0.5)
#   )

set.seed(77)

ggbetweenstats(
  data  = final_MR_MA,
  x     = guidelines,
  y     = transparency_score_perc,
  title = NULL,
  results.subtitle = FALSE,
  bf.message = FALSE,
  type = "perm",
  
  point.args = list(
    alpha = 0.7,
    size  = 3,
    shape = 16,
    position = ggplot2::position_jitter(width = 0.12, height = 1.5)
  ),
  
  centrality.plotting = FALSE,
  messages = FALSE
) +
  
  scale_y_continuous(
    limits = c(-5, 105),
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%")
  ) +
  
  scale_color_manual(values = c("No" = "lightblue", "Yes" = "navy")) +
  scale_fill_manual(values = c("No" = "lightblue", "Yes" = "navy")) +
  
  labs(x = "Guidelines", y = "Transparency Score (%)") +
  
  theme(
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text    = element_text(size = 16),
    
    panel.grid.major.y = element_line(
      linetype = "dashed",
      color = "grey80",
      linewidth = 0.5
    ),
    
    panel.grid.minor.y = element_blank(),
    
    axis.line.x = element_line(color = "grey30", linewidth = 0.8),
    axis.line.y = element_line(color = "grey30", linewidth = 0.8)
  )

ggsave(
  "figures/Figure_2A_guidelines_and_transparency.png",
  width = 13,
  height = 13,
  units = "cm"
)

# Plotting distributions to show how clear the effect seem to be

levels <- c("0", "25", "50", "75")
blue_palette <- colorRampPalette(c("lightblue", "navy"))(length(levels))
names(blue_palette) <- levels

final_MR_MA %>%
  mutate(transparency_score_perc = factor(transparency_score_perc,
                                          levels = c(0, 25, 50, 75))) %>%
  ggplot(aes(x = guidelines, fill = transparency_score_perc)) +
  geom_bar(position = "fill") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_manual(values = blue_palette) +
  labs(
    x = "Guidelines",
    y = "Proportion",
    fill = "Transparency score (%)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    # axis text styling (match other figure)
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.text    = element_text(size = 16),
    
    # axis lines (match thickness + color)
    axis.line.x = element_line(color = "grey30", linewidth = 0.8),
    axis.line.y = element_line(color = "grey30", linewidth = 0.8),
    
    # tighten plot margins to match other figure
    plot.margin = margin(5.5, 5.5, 5.5, 5.5, unit = "pt"),
    
    # keep legend styling as-is
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.box.spacing = unit(0.2, "cm")
  )

ggsave(
  "figures/Figure_2B_guidelines_and_transparency.png",
  width = 13,
  height = 13,
  units = "cm"
)


summary(final_MR_MA$transparency_score)
summary(final_MR_MA$transparency_score_perc)




### FOR FAIR

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

ggplot(final_MR_MA, aes(x = guidelines, y = FAIR_score_perc)) +
  geom_boxplot()

# Wilcoxon rank sum test
wilcox.test(FAIR_score_perc ~ guidelines, data = final_MR_MA)

# Chi-square test
table_data_2 <- table(final_MR_MA$FAIR_score_perc,
                    final_MR_MA$guidelines)

chisq.test(table_data_2)


set.seed(77)

# ggbetweenstats(
#   data  = final_MR_MA,
#   x     = guidelines,
#   y     = FAIR_score_perc,
#   title = "Guidelines and FAIR Score",
#   results.subtitle = FALSE,
#   bf.message = FALSE,
#   type = "perm",
#   
#   # raw points
#   point.args = list(
#     alpha = 0.7,
#     size  = 3,
#     shape = 16,
#     position = ggplot2::position_jitter(width = 0.12, height = 0)
#   ),
#   
#   # mean dot
#   centrality.point.args = list(
#     alpha = 1,
#     size  = 6,
#     shape = 16
#   ),
#   
#   # mean label
#   centrality.label.args = list(
#     size = 5,     # increase font size
#     color = "black",
#     position = ggplot2::position_nudge(x = 0.25),  # move the box to the right of the dot
#     #hjust = -1.2,
#     vjust = 1 # adjust vertical position if needed
#   ),
#   
#   messages = FALSE
# ) +
#   scale_y_continuous(
#     limits = c(0, 100),
#     breaks = seq(0, 100, 10),
#     labels = function(x) paste0(x, "%")
#   ) +
#   scale_color_manual(values = c("No" = "lightblue", "Yes" = "navy")) +
#   scale_fill_manual(values = c("No" = "lightblue", "Yes" = "navy")) +
#   labs(x = NULL, y = "FAIR Score (%)") +
#   theme(
#     axis.title.y = element_text(size = 16),
#     axis.text    = element_text(size = 16),
#     plot.title   = element_text(size = 20, hjust = 0.5)
#   )

ggbetweenstats(
  data  = final_MR_MA,
  x     = guidelines,
  y     = FAIR_score_perc,
  title = NULL,
  results.subtitle = FALSE,
  bf.message = FALSE,
  type = "perm",
  
  # raw points (MATCHED)
  point.args = list(
    alpha = 0.7,
    size  = 3,
    shape = 16,
    position = ggplot2::position_jitter(width = 0.12, height = 1.5)
  ),
  
  # REMOVE mean (for consistency with transparency plot)
  centrality.plotting = FALSE,
  
  messages = FALSE
) +
  
  scale_y_continuous(
    limits = c(-5, 105),
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%")
  ) +
  
  scale_color_manual(values = c("No" = "lightblue", "Yes" = "navy")) +
  scale_fill_manual(values = c("No" = "lightblue", "Yes" = "navy")) +
  
  labs(
    x = "Guidelines",
    y = "FAIR Score (%)"
  ) +
  
  theme(
    # MATCH typography exactly
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text    = element_text(size = 16),
    
    # MATCH grid style
    panel.grid.major.y = element_line(
      linetype = "dashed",
      color = "grey80",
      linewidth = 0.5
    ),
    panel.grid.minor.y = element_blank(),
    
    panel.grid.major.x = element_blank(),
    
    # MATCH axis lines
    axis.line.x = element_line(color = "grey30", linewidth = 0.8),
    axis.line.y = element_line(color = "grey30", linewidth = 0.8)
  )

ggsave(
  "figures/Figure_2C_guidelines_and_FAIRness.png",
  width = 13,
  height = 13,
  units = "cm"
)

# Plotting distributions

levels_fair <- c("0–25", "25–50", "50–75", "75–100")

blue_palette_fair <- colorRampPalette(c("lightblue", "navy"))(4)
names(blue_palette_fair) <- levels_fair

final_MR_MA %>%
  filter(!is.na(FAIR_score_perc)) %>%   # ✅ remove NA first
  
  mutate(
    FAIR_score_bin = cut(
      FAIR_score_perc,
      breaks = c(0, 25, 50, 75, 100),
      include.lowest = TRUE,
      labels = levels_fair
    )
  ) %>%
  
  ggplot(aes(x = guidelines, fill = FAIR_score_bin)) +
  geom_bar(position = "fill") +
  
  scale_y_continuous(limits = c(0, 1)) +
  
  scale_fill_manual(values = blue_palette_fair) +
  
  labs(
    x = "Guidelines",
    y = "Proportion",
    fill = "FAIR score (%)"
  ) +
  
  theme_minimal(base_size = 16) +
  
  theme(
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.text    = element_text(size = 16),
    
    axis.line.x = element_line(color = "grey30", linewidth = 0.8),
    axis.line.y = element_line(color = "grey30", linewidth = 0.8),
    
    plot.margin = margin(5.5, 5.5, 5.5, 5.5, unit = "pt"),
    
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.box.spacing = unit(0.2, "cm")
  )

ggsave(
  "figures/Figure_2D_guidelines_and_FAIRness.png",
  width = 13,
  height = 13,
  units = "cm"
)


###############################################################################
# exploring citations
###############################################################################

# final_MR_MA %>%
#   select(FAIR_score_perc, GS_total_citations, GS_6_first_years) %>%
#   tidyr::pivot_longer(
#     cols = c(GS_total_citations, GS_6_first_years),
#     names_to = "citation_metric",
#     values_to = "citations"
#   ) %>%
#   ggplot(aes(FAIR_score_perc, citations)) +
#   geom_point(alpha = 0.7) +
#   scale_y_log10() +
#   geom_smooth(
#     method = "lm",
#     se = TRUE,
#     level = 0.95
#   ) +
#   facet_wrap(~ citation_metric, scales = "free_y") +
#   labs(
#     x = "FAIR score (%)",
#     y = "Citations"
#   ) +
#   theme_minimal()


# final_MR_MA %>%
#   select(FAIR_score_perc, GS_total_citations, GS_6_first_years) %>%
#   tidyr::pivot_longer(
#     cols = c(GS_total_citations, GS_6_first_years),
#     names_to = "citation_metric",
#     values_to = "citations"
#   ) %>%
#   ggplot(aes(x = FAIR_score_perc, y = citations)) +
#   
#   # points
#   geom_point(
#     alpha = 0.7,
#     size = 3,
#     color = "grey30"
#   ) +
#   
#   # linear trend with 95% CI
#   geom_smooth(
#     method = "lm",
#     se = TRUE,
#     color = "grey20",
#     fill = "grey70",
#     linewidth = 1
#   ) +
#   
#   facet_wrap(~ citation_metric, scales = "free_y") +
#   
#   scale_x_continuous(
#     limits = c(0, 100),
#     breaks = seq(0, 100, 20),
#     labels = function(x) paste0(x, "%"),
#     expand = expansion(mult = c(0.01, 0.05))
#   ) +
#   
#   labs(
#     x = "FAIR score",
#     y = "Citations"
#   ) +
#   
#   theme_minimal(base_size = 20) +
#   theme(
#     panel.grid.major.y = element_line(linetype = "dashed", color = "grey80"),
#     panel.grid.major.x = element_line(linetype = "dashed", color = "grey80"),
#     panel.grid.minor = element_blank(),
#     
#     axis.line = element_line(color = "grey30", linewidth = 0.8),
#     axis.text = element_text(color = "grey20"),
#     
#     strip.text = element_text(size = 18),
#     strip.background = element_blank()
#   )


#library(patchwork)

# Prepare the data in long format
final_MR_MA_long <- final_MR_MA %>%
  select(FAIR_score_perc, GS_total_citations, GS_6_first_years) %>%
  pivot_longer(
    cols = c(GS_total_citations, GS_6_first_years),
    names_to = "citation_metric",
    values_to = "citations"
  )

# # Plot 1: GS_total_citations
# p1 <- final_MR_MA_long %>%
#   filter(citation_metric == "GS_total_citations") %>%
#   ggplot(aes(x = FAIR_score_perc, y = citations)) +
#   geom_point(alpha = 0.7, size = 3, color = "grey30") +
#   geom_smooth(method = "lm", se = TRUE, color = "grey20", fill = "grey70", linewidth = 1) +
#   scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), labels = function(x) paste0(x, "%"),
#                      expand = expansion(mult = c(0.01, 0.05))) +
#   labs(x = "FAIR score", y = "Total citations") +
#   theme_minimal(base_size = 20) +
#   theme(
#     panel.grid.major.y = element_line(linetype = "dashed", color = "grey80"),
#     panel.grid.major.x = element_line(linetype = "dashed", color = "grey80"),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(color = "grey30", linewidth = 0.8),
#     axis.text = element_text(color = "grey20")
#   )

# Plot 2: GS_6_first_years
# p2 <- final_MR_MA_long %>%
#   filter(citation_metric == "GS_6_first_years") %>%
#   ggplot(aes(x = FAIR_score_perc, y = citations)) +
#   geom_point(alpha = 0.7, size = 4, color = "grey30") +
#   geom_smooth(method = "lm", se = TRUE, color = "grey20", fill = "grey70", linewidth = 1) +
#   scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), labels = function(x) paste0(x, "%"),
#                      expand = expansion(mult = c(0.01, 0.05))) +
#   labs(x = "FAIR score", y = "Citations in first 6 years") +
#   theme_minimal(base_size = 20) +
#   theme(
#     panel.grid.major.y = element_line(linetype = "dashed", color = "grey80"),
#     panel.grid.major.x = element_line(linetype = "dashed", color = "grey80"),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(color = "grey30", linewidth = 0.8),
#     axis.text = element_text(color = "grey20")
#   )


summary(final_MR_MA$transparency_score_perc)
summary(final_MR_MA$GS_6_first_years)

final_MR_MA[!is.na(final_MR_MA$transparency_score_perc),c("transparency_score_perc","GS_6_first_years")]

hist(final_MR_MA$transparency_score_perc)
hist(final_MR_MA$GS_6_first_years)

kt.trans <- cor.test(
  final_MR_MA$transparency_score_perc,
  final_MR_MA$GS_6_first_years,
  method = "kendall",
  use = "complete.obs"
)
kt.trans


levels <- c("0", "25", "50", "75", "100")
blue_palette <- colorRampPalette(c("lightblue", "navy"))(length(levels))

# set.seed(77)
# 
# p0 <- final_MR_MA %>%
#   filter(!is.na(transparency_score_perc), !is.na(GS_6_first_years)) %>%
#   
#   ggplot(aes(
#     x = transparency_score_perc,
#     y = GS_6_first_years,
#     color = transparency_score_perc
#   )) +
#   
#   # points with x-jitter
#   geom_point(
#     alpha = 0.7,
#     size = 3,
#     position = position_jitter(width = 1, height = 0)
#   ) +
#   
#   scale_color_gradientn(
#     colours = blue_palette
#   ) +
#   
#   scale_x_continuous(
#     limits = c(-5, 105),
#     breaks = seq(0, 100, 25),
#     labels = function(x) paste0(x, "%"),
#     expand = expansion(mult = c(0.01, 0.05))
#   ) +
#   
#   scale_y_continuous(
#     limits = c(-5, 455),
#     breaks = seq(0, 400, 100),
#     expand = expansion(mult = c(0.01, 0.05))
#   ) +
#   
#   labs(
#     x = "Transparency Score (%)",
#     y = "First six years citations"
#   ) +
#   
#   theme_minimal(base_size = 16) +
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
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor = element_blank(),
#     
#     axis.line.x = element_line(color = "grey30", linewidth = 0.8),
#     axis.line.y = element_line(color = "grey30", linewidth = 0.8),
#     
#     legend.position = "none"
#   )
# 
# p0


# FAIR

summary(final_MR_MA$FAIR_score_perc)
summary(final_MR_MA$GS_6_first_years)

final_MR_MA[!is.na(final_MR_MA$FAIR_score_perc),c("FAIR_score_perc","GS_6_first_years")]

hist(final_MR_MA$FAIR_score_perc)
hist(final_MR_MA$GS_6_first_years)

kt.FAIR <- cor.test(
  final_MR_MA$FAIR_score_perc,
  final_MR_MA$GS_6_first_years,
  method = "kendall",
  use = "complete.obs"
)
kt.FAIR


# p2 <- final_MR_MA_long %>%
#   filter(citation_metric == "GS_6_first_years") %>%
#   ggplot(aes(x = FAIR_score_perc, y = citations)) +
#   geom_point(alpha = 0.7, size = 4, color = "grey30") +
#   geom_smooth(method = "lm", se = TRUE, color = "grey20", fill = "grey70", linewidth = 1) +
#   scale_x_continuous(
#     limits = c(-5, 105),
#     breaks = seq(0, 100, 25),
#     labels = function(x) paste0(x, "%"),
#     expand = expansion(mult = c(0.01, 0.05))
#   ) +
#   labs(
#     x = "FAIR score",
#     y = "Six-year citations"
#   ) +
#   theme_minimal() +
#   theme(
#     panel.grid.major.y = element_line(linetype = "dashed", color = "grey80"),
#     panel.grid.major.x = element_line(linetype = "dashed", color = "grey80"),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(color = "grey30", linewidth = 0.8),
#     axis.title.x = element_text(size = 28, face = "bold", margin = margin(t = 12)),
#     axis.title.y = element_text(size = 28, face = "bold", margin = margin(r = 12)),
#     axis.text = element_text(size = 24, color = "grey20"),
#     plot.title = element_text(size = 28, hjust = 0.5)
#   )
# 
# p2

levels <- c("0", "25", "50", "75", "100")
blue_palette <- colorRampPalette(c("lightblue", "navy"))(length(levels))

set.seed(77)

final_MR_MA %>%
  filter(!is.na(FAIR_score_perc), !is.na(GS_6_first_years)) %>%
  
  ggplot(aes(
    x = FAIR_score_perc,
    y = GS_6_first_years,
    color = FAIR_score_perc
  )) +
  
  # points with x-jitter
  geom_point(
    alpha = 0.7,
    size = 3,
    position = position_jitter(width = 1, height = 0)
  ) +
  
  scale_color_gradientn(
    colours = blue_palette
  ) +
  
  scale_x_continuous(
    limits = c(-5, 105),
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  
  scale_y_continuous(
    limits = c(-5, 455),
    breaks = seq(0, 400, 100),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  
  labs(
    x = "FAIR Score (%)",
    y = "First six years citations"
  ) +
  
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
    axis.line.y = element_line(color = "grey30", linewidth = 0.8),
    
    legend.position = "none"
  )

ggsave(
  "figures/Figure_3B_FAIRness_and_6year_citations.png",
  width = 13,
  height = 13,
  units = "cm"
)

# as a factor

# Are distributions different across groups?
kruskal.test(GS_6_first_years ~ transparency_score_perc, data = final_MR_MA)

final_MR_MA %>%
  filter(!is.na(transparency_score_perc),
         !is.na(GS_6_first_years)) %>%
  
  mutate(
    transparency_score_perc = factor(
      transparency_score_perc,
      levels = c(0, 25, 50, 75)
    )
  ) %>%
  
  ggbetweenstats(
    x     = transparency_score_perc,
    y     = GS_6_first_years,
    title = NULL,
    results.subtitle = FALSE,
    bf.message = FALSE,
    type = "nonparametric",
    
    # raw points (MATCHED style)
    point.args = list(
      alpha = 0.7,
      size  = 3,
      shape = 16,
      position = ggplot2::position_jitter(width = 0.12, height = 1.5)
    ),
    
    centrality.plotting = FALSE,
    messages = FALSE
  ) +
  
  scale_y_continuous(
    limits = c(-5, 450),
    breaks = seq(0, 400, 100)
  ) +
  
  scale_color_manual(values = c("0" = "lightblue",
                                "25" = "#9ecae1",
                                "50" = "#3182bd",
                                "75" = "navy")) +
  
  scale_fill_manual(values = c("0" = "lightblue",
                               "25" = "#9ecae1",
                               "50" = "#3182bd",
                               "75" = "navy")) +
  
  labs(
    x = "Transparency Score (%)",
    y = "First six years citations"
  ) +
  
  theme(
    # MATCH typography exactly
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text    = element_text(size = 16),
    
    # MATCH grid style
    panel.grid.major.y = element_line(
      linetype = "dashed",
      color = "grey80",
      linewidth = 0.5
    ),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    
    # MATCH axis lines
    axis.line.x = element_line(color = "grey30", linewidth = 0.8),
    axis.line.y = element_line(color = "grey30", linewidth = 0.8)
  )

ggsave(
  "figures/Figure_3A_transparency_and_6year_citations.png",
  width = 13,
  height = 13,
  units = "cm"
)


# # Combine side by side
# p1 + p2


# set.seed(77)
#
# p3 <- ggbetweenstats(
#   data  = final_MR_MA,
#   x     = data_shared_final,
#   y     = GS_total_citations,
#   title = NULL,  
#   results.subtitle = FALSE,
#   bf.message = FALSE,
#   type = "perm",
#   point.args = list(
#     alpha = 0.7, size = 3, shape = 16, 
#     position = ggplot2::position_jitter(width = 0.12, height = 0)
#   ),
#   centrality.point.args = list(alpha = 1, size = 6, shape = 16),
#   centrality.label.args = list(size = 5, color = "black", 
#                                position = ggplot2::position_nudge(x = 0.25), vjust = 1),
#   messages = FALSE
# ) +
#   labs(
#     x = "Data Shared",
#     y = "Total citations"
#   ) +
#   scale_color_manual(values = c("No" = "lightblue", "Yes" = "navy")) +
#   scale_fill_manual(values = c("No" = "lightblue", "Yes" = "navy")) +
#   theme(
#     axis.title.x = element_text(size = 18),
#     axis.title.y = element_text(size = 18),
#     axis.text = element_text(size = 16),
#     plot.title = element_text(size = 20, hjust = 0.5)
#   )
# set.seed(77)
# 
# # summary per group
# final_MR_MA %>%
#   group_by(data_shared_final) %>%
#   summarise(
#     n = sum(!is.na(GS_6_first_years)),
#     mean = mean(GS_6_first_years, na.rm = TRUE),
#     sd = sd(GS_6_first_years, na.rm = TRUE),
#     median = median(GS_6_first_years, na.rm = TRUE),
#     min = min(GS_6_first_years, na.rm = TRUE),
#     max = max(GS_6_first_years, na.rm = TRUE),
#     q1 = quantile(GS_6_first_years, 0.25, na.rm = TRUE),
#     q3 = quantile(GS_6_first_years, 0.75, na.rm = TRUE)
#   )
# 
# ggplot(final_MR_MA, aes(x = data_shared_final, y = GS_6_first_years)) +
#   geom_boxplot()
# 
# # Wilcoxon rank sum test
# wilcox.test(GS_6_first_years ~ data_shared_final, data = final_MR_MA)
# 
# # Chi-square test
# table_data.4 <- table(final_MR_MA$GS_6_first_years,
#                     final_MR_MA$data_shared_final)
# 
# chisq.test(table_data.4)
# 
# 
# p4 <- ggbetweenstats(
#   data  = final_MR_MA,
#   x     = data_shared_final,
#   y     = GS_6_first_years,
#   title = NULL,
#   results.subtitle = FALSE,
#   bf.message = FALSE,
#   type = "nonparametric",
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
#     limits = c(-5, 450),
#     breaks = seq(0, 400, 100)
#   ) +
#   
#   scale_color_manual(values = c("No" = "lightblue", "Yes" = "navy")) +
#   scale_fill_manual(values = c("No" = "lightblue", "Yes" = "navy")) +
#   
#   labs(
#     x = "Data Shared",
#     y = "First six years citations"
#   ) +
#   
#   theme(
#     # MATCH typography with FAIR/guidelines plots
#     axis.title.y = element_text(size = 16),
#     axis.title.x = element_text(size = 16),
#     axis.text    = element_text(size = 16),
#     
#     # MATCH grid style (ONLY horizontal dashed lines)
#     panel.grid.major.y = element_line(
#       linetype = "dashed",
#       color = "grey80",
#       linewidth = 0.5
#     ),
#     panel.grid.minor.y = element_blank(),
#     panel.grid.major.x = element_blank(),
#     
#     # MATCH axis lines system
#     axis.line.x = element_line(color = "grey30", linewidth = 0.8),
#     axis.line.y = element_line(color = "grey30", linewidth = 0.8)
#   )
# 
# p4


# p4 <- ggbetweenstats(
#   data  = final_MR_MA,
#   x     = data_shared_final,
#   y     = GS_6_first_years,
#   title = NULL,  
#   results.subtitle = FALSE,
#   bf.message = FALSE,
#   type = "perm",
#   
#   centrality.type = "median",
#   
#   point.args = list(
#     alpha = 0.7, size = 3, shape = 16, 
#     position = ggplot2::position_jitter(width = 0.12, height = 0)
#   ),
#   centrality.point.args = list(alpha = 1, size = 6, shape = 16),
#   
#   # turn OFF the misleading default label
#   centrality.label.args = NULL,
#   
#   messages = FALSE
# ) +
#   # add median labels manually
#   stat_summary(
#     fun = median,
#     geom = "text",
#     aes(label = round(after_stat(y), 1)),
#     position = ggplot2::position_nudge(x = 0.25),
#     size = 5,
#     color = "black"
#   ) +
#   labs(
#     x = "Data Shared",
#     y = "Six-year citations"
#   ) +
#   scale_color_manual(values = c("No" = "lightblue", "Yes" = "navy")) +
#   scale_fill_manual(values = c("No" = "lightblue", "Yes" = "navy")) +
#   theme(
#     axis.title.x = element_text(size = 20, margin = margin(t = 12)),
#     axis.title.y = element_text(size = 20, margin = margin(r = 12)),
#     axis.text = element_text(size = 16)
#   )


# Combine side by side
# p3 + p4



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
