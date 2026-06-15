################################################################################
# Authors: 
#
# Alfredo Sanchez-Tojar (alfredo.tojar@gmail.com): original code

# Script first created in August 2025

################################################################################
# Description of script and Instructions
################################################################################

# This script is to clean and prepare the data collected at hackathons aimed to
# assess the transparency, reliability and reproducibility of meta-analyses
# published in ecology and evolution

# Hackathon 1: ESMARConf2025 June 13th 2025
# Hackathon 2: unofficial August 20th 2025

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

setwd("C:/Users/localadmin/Dropbox/EXCELSiOR/projects/meta-research/meta-analysis_quality_and_reproducibility")

# importing data from Google Forms: hackathon 1
hackathon1 <- read.csv("data/raw_data/hackathons/2016-2020/01_raw_master_copies/hackathon_20250613_ESMARConf_Form_v1.csv", header=T)

# importing data from Google Forms: hackathon 2
hackathon2 <- read.csv("data/raw_data/hackathons/2016-2020/01_raw_master_copies/hackathon_20250820_unofficial_Form_v2.csv", header=T)

################################################################################
# Building common dataset
################################################################################

# adding a variable to specify what form version was used
hackathon1$form_version <- "v1"
hackathon2$form_version <- "v2"

# checking differences between datasets before merging, starting with the
# questions

# first, what is the same?
base::intersect(names(hackathon1),
                names(hackathon2))

# we can start renaming these shared variables' names to our preferred choices
hackathon1 <- as.data.frame(
  rename(hackathon1, 
         participant_name = "Your.name..e.g...Shreya.Dimri...Please.make.sure.to.use.the.exact.same.name..copy.paste.recommended..for.each.form.submission.",
         participant_email = "Email..e.g...may.be.used.for.follow.up..if.needed...Please.make.sure.to.use.the.exact.same.email..copy.paste.recommended..for.each.form.submission...",
         coauthorship = "Would.you.like.to.be.invited.as.co.author.and.further.contribution.should.these.data.be.used.in.an.eventual.manuscript.",
         title = "Meta.analysis.Title..please.copy.and.paste.the.title.exactly.as.it.appears.in.the.published.article.",
         effect_size_sign_comments = "Comments",
         inferential_statistics = "Did.the.authors.report.extracting.inferential.statistics..e.g...p.values..t.values..F.values..or.χ..values..from.primary.studies.to.calculate.effect.sizes.",
         inferential_statistics_type = "If.yes..what.types.of.inferential.statistics.were.used.to.calculate.effect.sizes.",
         inferential_statistics_transformations = "Were.the.statistical.transformations.or.formulas.used.to.convert.inferential.statistics.into.effect.sizes.clearly.reported.in.the.main.text.or.supplementary.materials.",
         raw_data_shared = "Did.the.authors.share.the.raw.data.extracted.from.the.primary.studies..e.g...means..variances..sample.sizes..inferential.statistics.values..etc...",
         RoB_assessment = "Did.the.authors.explicitly.report.performing.a.Risk.of.Bias..RoB..assessment.on.the.individual.primary.studies.included.in.the.meta.analysis.",
         RoB_assessment_type = "If.yes..which.Risk.of.Bias..RoB..tool.s..did.they.use..You.ll.find.a.list.of.common.options.below..along.with.a.link.to.a.more.comprehensive.list.here...Please.note.that.PRISMA.and.other.reporting.guidelines.are.not.Risk.of.Bias.tools...",
         inferential_statistics_comments = "Comments.1",
         data_and_code_sharing_comments = "Comments.2",
         RoB_assessment_comments = "Comments.3")
)

hackathon2 <- as.data.frame(
  rename(hackathon2, 
         participant_name = "Your.name..e.g...Shreya.Dimri...Please.make.sure.to.use.the.exact.same.name..copy.paste.recommended..for.each.form.submission.",
         participant_email = "Email..e.g...may.be.used.for.follow.up..if.needed...Please.make.sure.to.use.the.exact.same.email..copy.paste.recommended..for.each.form.submission...",
         coauthorship = "Would.you.like.to.be.invited.as.co.author.and.further.contribution.should.these.data.be.used.in.an.eventual.manuscript.",
         title = "Meta.analysis.Title..please.copy.and.paste.the.title.exactly.as.it.appears.in.the.published.article.",
         effect_size_sign_comments = "Comments",
         inferential_statistics = "Did.the.authors.report.extracting.inferential.statistics..e.g...p.values..t.values..F.values..or.χ..values..from.primary.studies.to.calculate.effect.sizes.",
         inferential_statistics_type = "If.yes..what.types.of.inferential.statistics.were.used.to.calculate.effect.sizes.",
         inferential_statistics_transformations = "Were.the.statistical.transformations.or.formulas.used.to.convert.inferential.statistics.into.effect.sizes.clearly.reported.in.the.main.text.or.supplementary.materials.",
         raw_data_shared = "Did.the.authors.share.the.raw.data.extracted.from.the.primary.studies..e.g...means..variances..sample.sizes..inferential.statistics.values..etc...",
         RoB_assessment = "Did.the.authors.explicitly.report.performing.a.Risk.of.Bias..RoB..assessment.on.the.individual.primary.studies.included.in.the.meta.analysis.",
         RoB_assessment_type = "If.yes..which.Risk.of.Bias..RoB..tool.s..did.they.use..You.ll.find.a.list.of.common.options.below..along.with.a.link.to.a.more.comprehensive.list.here...Please.note.that.PRISMA.and.other.reporting.guidelines.are.not.Risk.of.Bias.tools...",
         inferential_statistics_comments = "Comments.1",
         data_and_code_sharing_comments = "Comments.2",
         RoB_assessment_comments = "Comments.3")
)

# confirming that changes are correct
base::intersect(names(hackathon1),
                names(hackathon2))

# from manual data extraction
# data_shared, data_shared_quote, data_shared_type, data_shared_location
# code_shared, code_shared_quote, code_shared_location, 

# second, what's different?
base::setdiff(names(hackathon1),
              names(hackathon2))
base::setdiff(names(hackathon2),
              names(hackathon1))

# the wording in some questions was updated from version 1 to 2
# check: 
# hackathon 2: 
# [12] "If.you.selected.Others.above..please.specify.the.tool.here..otherwise.type..NA.." 
# "Beyond.using.explicit.Risk.of.Bias.tools..did.the.meta.analysis.address.potential.biases.at.the.individual.study.level.through.other.methods..For.example..by.categorizing.effect.sizes.and.comparing.them.based.on.whether.the.original.studies.used.or.were.affected.by."  

# 
hackathon1 <- as.data.frame(
  rename(hackathon1, 
         MA_ID = "Meta.analysis.ID..which.you.can.find.in.the.list.of.meta.analyses.you.will.receive.by.email.",
         effect_size_sign_accounted = "Did.the.authors.explicitly.state.or.address.whether.effect.size.directions.were.adjusted.to.ensure.consistency.across.the.entire.meta.analytic.dataset.",
         effect_size_sign_other = "If.you.chose.Other.as.the.option.above..please.describe.it.here",
         effect_size_sign_quote = "Please.copy.and.paste.the.relevant.text.from.the.article.that.informed.your.decision.above.",
         inferential_statistics_other = "If.you.chose.Others.above..please.specify.the.inferential.statistics.used.by.the.authors",
         inferential_statistics_quote = "Please.copy.paste.the.text.from.the.article.that.you.used.to.make.the.decision.above.",
         processed_data_shared = "Did.the.authors.share.the.processed.data.used.in.the.meta.analysis..e.g...effect.sizes.for.all.studies..coded.moderators..etc...",
         processed_data_shared_location = "If.you.answered..Yes...where.can.the.processed.data.be.accessed..Please.provide.the.link.s...If.the.data.is.available.in.the.article.s.supplementary.materials..include.the.article.s.link..If.there.is.more.than.one.link..e.g...GitHub.and.Dryad...please.list.all.links.separated.by.a.semicolon......e.g...https...github.com.ASanchez.Tojar.meta.analysis_egg_hormones_and_fitness..http...doi.org.10.5281.zenodo.14930059..",
         raw_data_shared_location = "If.you.answered..Yes...where.can.the.raw.data.be.accessed..Please.provide.the.link.s...If.the.data.is.available.in.the.article.s.supplementary.materials..include.the.article.s.link..If.there.is.more.than.one.link..e.g...GitHub.and.Dryad...please.list.all.links.separated.by.a.semicolon......e.g...https...github.com.ASanchez.Tojar.meta.analysis_egg_hormones_and_fitness..http...doi.org.10.5281.zenodo.14930059..",
         code_shared = "Did.the.authors.share.any.programming.code..e.g...R.scripts...You.don.t.need.to.confirm.if.all.code.was.provided.just.whether.at.least.some.code.was.made.available.",
         code_shared_location = "If.you.answered..Yes...where.can.the.programming.code.be.accessed..Please.provide.the.link.s...If.the.data.is.available.in.the.article.s.supplementary.materials..include.the.article.s.link..If.there.is.more.than.one.link..e.g...GitHub.and.Dryad...please.list.all.links.separated.by.a.semicolon......e.g...https...github.com.ASanchez.Tojar.meta.analysis_egg_hormones_and_fitness..http...doi.org.10.5281.zenodo.14930059..",
         RoB_assessment_alternative = "Beyond.using.explicit.Risk.of.Bias.tools..did.the.meta.analysis.address.potential.biases.at.the.individual.study.level.through.other.methods..For.example..by.categorizing.effect.sizes.and.comparing.them.based.on.whether.the.original.studies.used.or.were.affected.by.",
         RoB_assessment_alternative_other = "If.you.selected.other..please.provide.information.on.the.potential.bias.that.the.authors.accounted.for....Please.note.examples.such.as.comparisons.of.effect.sizes.between.studies.with.different.data.collection.protocols..selective.reporting.checks..or.any.analytical.adjustment.aimed.at.study.level.bias..",
         RoB_assessment_other = "If.you.selected.Others.above..please.specify.the.tool.here"
  )
)

hackathon2 <- as.data.frame(
  rename(hackathon2, 
         MA_ID = "Meta.analysis.ID..which.you.can.find.in.the.list.of.meta.analyses.you.received.by.email..e.g...MA_001.",
         effect_size_sign_accounted = "Did.the.authors.explicitly.address.whether.effect.size.directions.were.adjusted.to.ensure.consistency.across.the.entire.meta.analytic.dataset.",
         effect_size_sign_other = "If.you.chose.Other.as.the.option.above..please.describe.it.here..otherwise.type.NA.",
         effect_size_sign_quote = "Please.copy.and.paste.the.relevant.text.from.the.article.that.informed.your.decision.for.the.question.above..If.you.chose.Not.applicable..NA..please.describe.the.reasoning.for.your.choice.here.",
         inferential_statistics_other = "If.you.chose.Others.above..please.specify.the.inferential.statistics.used.by.the.authors..otherwise.type..NA..",
         inferential_statistics_quote = "Please.copy.paste.the.text.from.the.article.that.you.used.to.make.the.decision.above..otherwise.type..NA..",
         processed_data_shared = "Did.the.authors.share.the.processed.data.used.in.the.meta.analysis..e.g...effect.sizes.and.their.associated.sampling.variances..if.applicable...Note.that.often.times.but.not.always.these.may.correspond.to.columns.yi.and.vi.",
         processed_data_shared_location = "If.you.answered..Yes...where.can.the.processed.data.be.accessed..Please.provide.the.link.s...If.the.data.is.available.in.the.article.s.supplementary.materials..include.the.article.s.link..If.there.is.more.than.one.link..e.g...GitHub.and.Dryad...please.list.all.links.separated.by.a.semicolon......e.g...https...github.com.ASanchez.Tojar.meta.analysis_egg_hormones_and_fitness..http...doi.org.10.5281.zenodo.14930059...otherwise.type.NA.",
         raw_data_shared_location = "If.you.answered..Yes...where.can.the.raw.data.be.accessed..Please.provide.the.link.s...If.the.data.is.available.in.the.article.s.supplementary.materials..include.the.article.s.link..If.there.is.more.than.one.link..e.g...GitHub.and.Dryad...please.list.all.links.separated.by.a.semicolon......e.g...https...github.com.ASanchez.Tojar.meta.analysis_egg_hormones_and_fitness..http...doi.org.10.5281.zenodo.14930059...otherwise.type.NA.",
         code_shared = "Did.the.authors.share.any.programming.code..e.g...R.scripts...You.don.t.need.to.confirm.if.all.code.was.provided.just.whether.at.least.some.code.was.made.available..If.the.authors.used.a.click.based.program.and.did.not.report.a.clear.step.by.step.guide.on.how.to.generate.the.estimates..you.should.answer..No..",
         code_shared_location = "If.you.answered..Yes...where.can.the.programming.code.be.accessed..Please.provide.the.link.s...If.the.data.is.available.in.the.article.s.supplementary.materials..include.the.article.s.link..If.there.is.more.than.one.link..e.g...GitHub.and.Dryad...please.list.all.links.separated.by.a.semicolon......e.g...https...github.com.ASanchez.Tojar.meta.analysis_egg_hormones_and_fitness..http...doi.org.10.5281.zenodo.14930059...otherwise.type..NA..",
         RoB_assessment_other = "If.you.selected.Others.above..please.specify.the.tool.here..otherwise.type..NA..",
         RoB_assessment_alternative = "Beyond.using.explicit.Risk.of.Bias.tools..did.the.meta.analysis.address.assess.potential.biases.at.the.individual.study.level.through.other.methods..For.example..by.categorizing.effect.sizes.studies.and.comparing.them.based.on.whether.the.original.studies.used.or.were.affected.by...Please.keep.in.mind.publication.bias.tests.are.not.RoB.assessment...Some.examples.of.what.would.not.be.included.here.are.....Authors.imputed.SD..SE...Authors.excluded.studies.that.did.not.provide.data...Authors.plotted.a.funnel.plot.to.understand.sampling.bias",
         RoB_assessment_alternative_other = "If.you.selected.other..please.provide.information.on.the.potential.bias.that.the.authors.accounted.for..otherwise.type..NA....Please.note.examples.such.as.comparisons.of.effect.sizes.between.studies.with.different.data.collection.protocols..selective.reporting.checks..but.not.publication.bias.test...or.any.analytical.adjustment.aimed.at.study.level.bias.."
  )
)

# confirming that changes are correct
base::setdiff(names(hackathon1),
              names(hackathon2))
base::setdiff(names(hackathon2),
              names(hackathon1))

# Since all variable names are the same name, we need to make sure the variables
# are in the same order before we combine the two datasets
hackathon2 <- hackathon2[,names(hackathon1)]
names(hackathon1)
names(hackathon2)

# putting both datasets together
hackathon <- rbind(hackathon1,hackathon2)

################################################################################
# Cleaning common dataset
################################################################################

summary(hackathon)
names(hackathon)

# first removing extra spaces in front and at the back of all characters
hackathon <- as.data.frame(
  hackathon %>% mutate_if(is.character, str_trim)
)

# then, reordering columns
hackathon <- as.data.frame(hackathon %>% 
                             relocate(inferential_statistics_comments, 
                                      .after = inferential_statistics_quote) %>%
                             relocate(data_and_code_sharing_comments, 
                                      .after = code_shared_location)
)

# after having gone variable by variable, here is a list of variables for which
# empty entries (i.e., "") and NA need to be changed into "NA" (character type)
# throughout for standardization
columns.NA.cleaning <- c("effect_size_sign_other",
                         "effect_size_sign_quote",
                         "effect_size_sign_comments",
                         "inferential_statistics_type",
                         "inferential_statistics_other",
                         "inferential_statistics_transformations",
                         "inferential_statistics_quote",
                         "inferential_statistics_comments",
                         "processed_data_shared_location",
                         "raw_data_shared_location",
                         "code_shared_location",
                         "data_and_code_sharing_comments",
                         "RoB_assessment_type",
                         "RoB_assessment_alternative",
                         "RoB_assessment_alternative_other",
                         "RoB_assessment_other",
                         "RoB_assessment_comments")


# Apply transformation to each chosen column
hackathon[,columns.NA.cleaning] <- lapply(hackathon[,columns.NA.cleaning], function(x) {
  # Convert NA to "NA"
  x[is.na(x)] <- "NA"
  # Convert "" to "NA"
  x[x == ""] <- "NA"
  return(x)
})

# after having gone variable by variable, here is a list of characters that 
# contain \n values that I want to get rid off to standardise and make things
# more readable. These \n are presumably added when copying and pasting text
# In addition, I am collapsing any spaces that are not single spaces (i.e.,
# double spaces, triple spaces, etc...) to increase readability
columns.n.cleaning <- c("title",
                        "effect_size_sign_other",
                        "effect_size_sign_quote",
                        "effect_size_sign_comments",
                        "inferential_statistics_other",
                        "inferential_statistics_quote",
                        "inferential_statistics_comments",
                        "processed_data_shared_location",
                        "raw_data_shared_location",
                        "code_shared_location",
                        "data_and_code_sharing_comments",
                        "RoB_assessment_alternative_other",
                        "RoB_assessment_other",
                        "RoB_assessment_comments")

hackathon <- as.data.frame(hackathon %>%
                             mutate(across(all_of(columns.n.cleaning), 
                                           ~ str_replace_all(., "\n", " ") %>%
                                             str_replace_all("\\s+", " ")))
)

#table(hackathon$inferential_statistics_quote)

sort(table(hackathon$code_shared_location=="NA"))
sort(table(hackathon$code_shared))


################################################################################
# variable by variable

################################################################################
# participant_name
table(hackathon$participant_name)

# fixing observable typos
hackathon$participant_name <- recode(hackathon$participant_name,
                                     "Christel Hellebrg" = "Christel Hellberg",
                                     "Malgorzata (Losia) Lagisz" = "Malgorzata Lagisz",
                                     "Nerea Piñeiro" = "Nerea Piñeiro-Juncal",
                                     "Pietro" = "Pietro Pollo")

table(hackathon$participant_name)

# number of unique participants
length(unique(hackathon$participant_name))

################################################################################
# participant_email
table(hackathon$participant_email)

# fixing observable typos
hackathon$participant_email <- recode(hackathon$participant_email,
                                      "Pollo" = "pietro_pollo@hotmail.com",
                                      "ryan@ryanfield.me" = "ryan.field@glasgow.ac.uk")

table(hackathon$participant_email)

# number of unique names = number of unique email addresses?
length(unique(hackathon$participant_name))==length(unique(hackathon$participant_email))

################################################################################
# coauthorship
table(hackathon$coauthorship)

# identifying who did not want to be a co-author for later on
unique(hackathon[hackathon$coauthorship=="No",c("participant_name","participant_email")])


################################################################################
# MA_ID
sort(table(hackathon$MA_ID))

# fixing observable typos
hackathon$MA_ID <- recode(hackathon$MA_ID,
                          "4.\tMA_028" = "MA_028")

################################################################################
################################################################
####################################################
# MA_047 is missing! Need to search for it
# from master_list_IDs.csv it seems that it was not assigned
####################################################
################################################################
################################################################################

sort(table(hackathon$MA_ID))

################################################################################
# title
sort(table(hackathon$title))

# everything to small letters
hackathon$title <- tolower(hackathon$title)
sort(table(hackathon$title))

# fixing observable typos
hackathon[hackathon$title == "water regulation by grasslands: a global meta‐analysi","title"] <- "water regulation by grasslands: a global meta‐analysis"
sort(table(hackathon$title))

# remove "" around a title
hackathon$title <- str_replace_all(hackathon$title, "\"", "")
sort(table(hackathon$title))

# substitute all ‐ by -
hackathon$title <- str_replace_all(hackathon$title, "‐", "-")
sort(table(hackathon$title))

# removing additional spaces, not needed, I think, but extra just in case
hackathon$title <- str_trim(hackathon$title, side = c("both"))
sort(table(hackathon$title))


# manual recoding
hackathon$title <- recode(hackathon$title,
                          #"bottom-up vs. top-down effects on terrestrial insect  herbivores: a meta-analysis" = "bottom-up vs. top-down effects on terrestrial insect herbivores: a meta-analysis",
                          #"conventional land‐use intensification reduces species richness and increases production: a global meta‐analysis" = "conventional land-use intensification reduces species richness and increases production: a global meta-analysis",
                          "development of emission factors and ef ciency of two nitri cation inhibitors, dcd and dmpp" = "development of emission factors and efficiency of two nitrification inhibitors, dcd and dmpp",
                          "meta‐analysis reveals enhanced growth of marine harmful algae from temperate regions with warming and elevated co2 levels" = "meta-analysis reveals enhanced growth of marine harmful algae from temperate regions with warming and elevated co2 levels",
                          "potential impact of climate change on parasitism ef ciency of egg parasitoids: a meta-analysis of trichogramma under variable climate conditions" = "potential impact of climate change on parasitism efficiency of egg parasitoids: a meta-analysis of trichogramma under variable climate conditions",
                          "the effects of livestock grazing on biodiversity are multi-trophic: a meta-analysis" = "the effects of livestock grazing on biodiversity are multitrophic: a meta-analysis",
                          "meta analysis of the hypocholesterolemic potentials of turmeric (curcuma longa) in broiler chickens" = "meta-analysis of the hypocholesterolemic potentials of turmeric (curcuma longa) in broiler chickens"
)

sort(table(hackathon$title))
sort(hackathon$title)

# exploring possible typos reported by the participants

# # FROM NEREA The review of article: MA_012 – DOI: 10.1111/eva.12865 may have 
# # been reported with code MA_56. The title is correct. 
# # when I used the DOI for my last article (MA_56) I saw that I had already done 
# # it. It was the first one I did. So, either there are two reports with code 
# # MA_56 (titles are correct). Or I started by the last (for whatever reason...) 
# # and everything is correct.
# 
# hackathon[hackathon$MA_ID=="MA_012",] # only one entry for MA_012, not from Nerea
# hackathon[hackathon$MA_ID=="MA_056",] # indeed, three entries for MA_056
# 
# # is the title of MA_012 in the list of titles of MA_056? Yes
# hackathon[hackathon$MA_ID=="MA_012","title"] %in% hackathon[hackathon$MA_ID=="MA_056","title"] 

# therefore, correct MA_ID for this
hackathon[hackathon$MA_ID=="MA_056" &
            hackathon$title==hackathon[hackathon$MA_ID=="MA_012","title"],
          "MA_ID"] <- "MA_012"

sort(table(hackathon$title))
sort(table(hackathon$MA_ID))

# # # checking if all MA_ID's correspond to their title
# hackathon <- as.data.frame(
#   hackathon %>%
#     group_by(MA_ID) %>% 
#     mutate(same_title = +(n_distinct(title) == 1)) %>% 
#     ungroup
# )
# 
# # print any disagreements between titles from the same ID
# hackathon[hackathon$same_title==0,]

# check number MA_043
# The correct title for MA_043 should be "quantifying the influence of urban 
# land use on mangrove biology and ecology: a meta-analysis" according to the 
# master list
# Nerea Piñeiro-Juncal was assigned with MA_043 during the 20.08.2025, therefore
# what needs to be corrected here, is the title of the MA_043 on the 20.08.2025 
# therefore, correct title for MA_043
hackathon[hackathon$MA_ID=="MA_043",
          "title"] <- "quantifying the influence of urban land use on mangrove biology and ecology: a meta-analysis"

hackathon <- as.data.frame(
  hackathon %>%
    group_by(MA_ID) %>% 
    mutate(same_title = +(n_distinct(title) == 1)) %>% 
    ungroup
)

# checking if it is fixed now 
hackathon[hackathon$same_title==0,]


################################################################################
# effect_size_sign_accounted
sort(table(hackathon$effect_size_sign_accounted))

# manual recoding
hackathon$effect_size_sign_accounted <- recode(hackathon$effect_size_sign_accounted,
                                               "I do not know" = "Unclear",
                                               "Other (please describe this below)" = "Other",
                                               "Other  (please describe this)" = "Other",
                                               "Not applicable/ NA (remember to describe the reasoning behind your choice)" = "NA"
)

sort(table(hackathon$effect_size_sign_accounted))

# in version 1 of the form, "Not applicable/ NA (remember to describe the
# reasoning behind your choice)" was not an option, so some of the NA's might
# have been entered as No or Other

################################################################################
# effect_size_sign_other
sort(table(hackathon$effect_size_sign_other))

################################################################################
# effect_size_sign_quote
sort(table(hackathon$effect_size_sign_quote))

################################################################################
# effect_size_sign_comments
sort(table(hackathon$effect_size_sign_comments))

################################################################################
# inferential_statistics
sort(table(hackathon$inferential_statistics))

# manual recoding
hackathon$inferential_statistics <- recode(hackathon$inferential_statistics,
                                           "I do not know" = "Unclear",
                                           "Not applicable/ NA (this could apply for cases where effect size is an absolute value or an effect size such as correlation coefficient)" = "NA"
)

sort(table(hackathon$inferential_statistics))

# in version 1 of the form, "Not applicable/ NA (this could apply for cases 
# where effect size is an absolute value or an effect size such as correlation
# coefficient)" was not an option, so some of the NA's might have been entered 
# as No

################################################################################
# inferential_statistics_type
sort(table(hackathon$inferential_statistics_type))

# manual recoding
hackathon$inferential_statistics_type <- recode(hackathon$inferential_statistics_type,
                                                "Not applicable/ NA (if you did not choose Yes above, please use this option), Others" = "Other", #when two or more options where chosen, assign Other to check later
                                                "Not applicable/ NA (if you did not choose Yes above, please use this option)" = "NA",
                                                "Others" = "Other",
                                                "Others (please specify this below)" = "Other"
)

sort(table(hackathon$inferential_statistics_type))

################################################################################
# inferential_statistics_other
sort(table(hackathon$inferential_statistics_other))

################################################################################
# inferential_statistics_transformations
sort(table(hackathon$inferential_statistics_transformations))

# manual recoding
hackathon$inferential_statistics_transformations <- recode(hackathon$inferential_statistics_transformations,
                                                           "Yes, but only some formulas were provided" = "Some",
                                                           "Yes, all formulas were provided" = "Yes",
                                                           "No, formulas were not provided and the authors did not reference any sources" = "No",
                                                           "No, formulas were not provided, but the authors referenced the sources they used" = "Only_references",
                                                           "Not applicable/ NA (Use this for cases when you chose NA for inferential statistics type)" = "NA"
)

sort(table(hackathon$inferential_statistics_transformations))


################################################################################
# inferential_statistics_quote
sort(table(hackathon$inferential_statistics_quote))

###############################################################################
# inferential_statistics_comments
sort(table(hackathon$inferential_statistics_comments))

################################################################################
# processed_data_shared
sort(table(hackathon$processed_data_shared))

sort(table(hackathon$processed_data_shared_location=="NA"))
sort(table(hackathon$processed_data_shared))

hackathon[hackathon$processed_data_shared == "Yes" & hackathon$processed_data_shared_location=="NA",]
hackathon[hackathon$processed_data_shared == "No" & hackathon$processed_data_shared_location!="NA",]

################################################################################
# processed_data_shared_location
sort(table(hackathon$processed_data_shared_location))

# manual recoding to faciliate link extraction
hackathon$processed_data_shared_location <- recode(hackathon$processed_data_shared_location,
                                                   'no effec sizes here: "The complete database is deposited in Dryad under https ://doi.org/10.5061/d ryad.kj67m68."' = 'no effec sizes here: "The complete database is deposited in Dryad under https ://doi.org/10.5061/dryad.kj67m68."'
)

sort(table(hackathon$processed_data_shared_location))
# sort(table(is.na(hackathon$processed_data_shared_location)))
# sort(table(hackathon$processed_data_shared_location==""))

################################################################################
# raw_data_shared
sort(table(hackathon$raw_data_shared))

sort(table(hackathon$raw_data_shared_location=="NA"))
sort(table(hackathon$raw_data_shared))

hackathon[hackathon$raw_data_shared == "Yes" & hackathon$raw_data_shared_location=="NA",]
hackathon[hackathon$raw_data_shared == "No" & hackathon$raw_data_shared_location!="NA",]


################################################################################
# raw_data_shared_location
sort(table(hackathon$raw_data_shared_location))

# manual recoding to faciliate link extraction
hackathon$processed_data_shared_location <- recode(hackathon$processed_data_shared_location,
                                                   'https ://doi.org/10.15131/ shef. data.11590902.v1' = 'https://doi.org/10.15131/shef.data.11590902.v1'
)

sort(table(hackathon$raw_data_shared_location))
# sort(table(is.na(hackathon$raw_data_shared_location)))
# sort(table(hackathon$raw_data_shared_location==""))

###############################################################################
# code_shared
sort(table(hackathon$code_shared))

sort(table(hackathon$code_shared_location=="NA"))
sort(table(hackathon$code_shared))

hackathon[hackathon$code_shared == "Yes" & hackathon$code_shared_location=="NA",]
hackathon[hackathon$code_shared == "No" & hackathon$code_shared_location!="NA",]


################################################################################
# code_shared_location
sort(table(hackathon$code_shared_location))

# manual recoding to faciliate link extraction
hackathon$code_shared_location <- recode(hackathon$code_shared_location,
                                         "GitHub (github.com/DochtermannLab/BehavioralHeritability) and archived online at Dryad (doi:10.5061/dryad.b38k42m)." = "GitHub (github.com/DochtermannLab/BehavioralHeritability) and archived online at Dryad (https://doi.org/10.5061/dryad.b38k42m)."
)
sort(table(hackathon$code_shared_location))
# sort(table(is.na(hackathon$code_shared_location)))
# sort(table(hackathon$code_shared_location==""))


################################################################################
# data_and_code_sharing_comments
sort(table(hackathon$data_and_code_sharing_comments))

################################################################################
# RoB_assessment
sort(table(hackathon$RoB_assessment))

# there seem to be an NA in this question, despite being mandatory, check what
# happened
hackathon[hackathon$RoB_assessment=="",]
# all RoB questions here are empty, not sure what happened
# actually, it seems that this questions were not mandatory in v1! Lucky it was
# only one case. Ask Pietro to do it?

# manual recoding
hackathon$RoB_assessment <- recode(hackathon$RoB_assessment,
                                   "I do not know" = "Unclear"
)

sort(table(hackathon$RoB_assessment))

# fixing an entry error submitting by one participant
hackathon[hackathon$MA_ID=="MA_007",]
hackathon[hackathon$MA_ID=="MA_007" & 
            hackathon$participant_email=="losialagisz@gmail.com",]
hackathon[hackathon$MA_ID=="MA_007" & 
            hackathon$participant_email=="losialagisz@gmail.com",
          "RoB_assessment_alternative"] <- "Other"

# comment to be added
info.RoB.MA_007 <- "RoB was assessed indirectly (not-explicitly) by categorising 
study designs. From Supplement, Study method: the studies were classified into 
three methods: (1) capture-recapture, (2) naïve count-based or (3) count-based 
with distance sampling. This variable focuses on possible biases 
associated with methodology"

# making sure it is clean before adding
info.RoB.MA_007 <- str_replace_all(info.RoB.MA_007, "\n", " ")
info.RoB.MA_007 <- str_replace_all(info.RoB.MA_007, "\\s+", " ")

hackathon[hackathon$MA_ID=="MA_007" & 
            hackathon$participant_email=="losialagisz@gmail.com",
          "RoB_assessment_alternative_other"] <- info.RoB.MA_007



################################################################################
# RoB_assessment_type
sort(table(hackathon$RoB_assessment_type))

# manual recoding
hackathon$RoB_assessment_type <- recode(hackathon$RoB_assessment_type,
                                        "CEESAT (Collaboration for Environmental Evidence Synthesis Assessment Tool)" = "CEESAT",
                                        "Others (Please specify below)" = "Other",
                                        "Others" = "Other",
                                        "Not applicable/ NA" = "NA"
)

sort(table(hackathon$RoB_assessment_type))


################################################################################
# RoB_assessment_alternative
sort(table(hackathon$RoB_assessment_alternative))

# in contrast to v2 of the form, v1 of the form did not have an entry 
#specifically saying "None", therefore, "NA" and "None" should be categorised
# as "None"
hackathon$RoB_assessment_alternative <- ifelse(hackathon$RoB_assessment_alternative=="NA",
                                               "None",
                                               hackathon$RoB_assessment_alternative)

sort(table(hackathon$RoB_assessment_alternative))

################################################################################
# RoB_assessment_alternative_other
sort(table(hackathon$RoB_assessment_alternative_other))

################################################################################
# RoB_assessment_other
sort(table(hackathon$RoB_assessment_other))

################################################################################
# RoB_assessment_comments
sort(table(hackathon$RoB_assessment_comments))


################################################################################
# Summary statistics
################################################################################

# adding a variable with the count of how many times each MA_ID was assessed
hackathon <- as.data.frame(
  hackathon %>%
    add_count(MA_ID, name = "number_of_assessments")
)


################################################################################
# Extracting links for data and code
################################################################################

################################################################################
# processed data

hackathon <- as.data.frame(
  hackathon %>%
    mutate(
      # Normalize text: getting rid of some extra spaces that were introduced by
      # mistake
      clean_location = str_replace_all(processed_data_shared_location, "\\s*:\\/\\/", "://"),
      clean_location = str_replace_all(clean_location, "doi\\.org/\\s*", "doi.org/")
    ) %>%
    rowwise() %>% # process row by row
    mutate(
      # Extract URLs per row
      processed_data_links = str_extract_all(
        clean_location,
        "(https?://\\S+|doi\\.org/\\S+|datadryad\\.org/\\S+|figshare\\.com/\\S+)"
      ) %>%
        unlist() %>%                               # flatten result
        # Remove trailing punctuation (.,; or ,) from all extracted links
        # [\\.,;"]+ matches one or more of ., ,, ;, or "
        # $ ensures it only matches at the end of the string
        # This will remove sequences like ., .., ,;.", .;, etc., but leave 
        # everything before untouched.
        str_replace_all('[\\.,;"]+$', '') %>%      # clean trailing chars
        paste(collapse = "; "),                    # collapse into single string
      
      # for entries that provide more than one link
      processed_data_number_of_links = ifelse(processed_data_links == "", 0,
                                              str_count(processed_data_links, ";") + 1),
      
      processed_data_chosen_link = case_when(
        # if the entry is "NA" add "NA" to the new variables
        processed_data_shared_location == "NA" ~ "NA",
        # if the entry is not "NA" but there is some text but not apparent links
        # add "check_missing_link" to figure out what's happening there
        processed_data_number_of_links == 0 & processed_data_shared_location != "NA" ~ "check_missing_link",
        # Priority rules: links from doi, figshare, and if not, use the first
        # link presented
        TRUE ~ {
          lks <- unlist(str_split(processed_data_links, ";\\s*"))
          doi_link <- lks[str_detect(lks, "doi\\.org")]
          figshare_link <- lks[str_detect(lks, "figshare")]
          if (length(doi_link) > 0) doi_link[1]
          else if (length(figshare_link) > 0) figshare_link[1]
          else lks[1]
        }
      )
    ) %>%
    ungroup() %>%
    select(-clean_location)
)

#names(hackathon)
# hackathon[,c("MA_ID",
#              "processed_data_shared","processed_data_shared_location",
#              "processed_data_links","processed_data_number_of_links",
#              "processed_data_chosen_link")]

################################################################################
# raw data
hackathon <- as.data.frame(
  hackathon %>%
    mutate(
      # Normalize text: getting rid of some extra spaces that were introduced by
      # mistake
      clean_location = str_replace_all(raw_data_shared_location, "\\s*:\\/\\/", "://"),
      clean_location = str_replace_all(clean_location, "doi\\.org/\\s*", "doi.org/")
    ) %>%
    rowwise() %>% # process row by row
    mutate(
      # Extract URLs per row
      raw_data_links = str_extract_all(
        clean_location,
        "(https?://\\S+|doi\\.org/\\S+|datadryad\\.org/\\S+|figshare\\.com/\\S+)"
      ) %>%
        unlist() %>%                               # flatten result
        # Remove trailing punctuation (.,; or ,) from all extracted links
        # [\\.,;"]+ matches one or more of ., ,, ;, or "
        # $ ensures it only matches at the end of the string
        # This will remove sequences like ., .., ,;.", .;, etc., but leave 
        # everything before untouched.
        str_replace_all('[\\.,;"]+$', '') %>%      # clean trailing chars
        paste(collapse = "; "),                    # collapse into single string
      
      # for entries that provide more than one link
      raw_data_number_of_links = ifelse(raw_data_links == "", 0,
                                        str_count(raw_data_links, ";") + 1),
      
      raw_data_chosen_link = case_when(
        # if the entry is "NA" add "NA" to the new variables
        raw_data_shared_location == "NA" ~ "NA",
        # if the entry is not "NA" but there is some text but not apparent links
        # add "check_missing_link" to figure out what's happening there
        raw_data_number_of_links == 0 & raw_data_shared_location != "NA" ~ "check_missing_link",
        # Priority rules: links from doi, figshare, and if not, use the first
        # link presented
        TRUE ~ {
          lks <- unlist(str_split(raw_data_links, ";\\s*"))
          doi_link <- lks[str_detect(lks, "doi\\.org")]
          figshare_link <- lks[str_detect(lks, "figshare")]
          if (length(doi_link) > 0) doi_link[1]
          else if (length(figshare_link) > 0) figshare_link[1]
          else lks[1]
        }
      )
    ) %>%
    ungroup() %>%
    select(-clean_location)
)


# names(hackathon)
# hackathon[,c("MA_ID",
#              "raw_data_shared","raw_data_shared_location",
#              "raw_data_links","raw_data_number_of_links",
#              "raw_data_chosen_link")]

################################################################################
# code

hackathon <- as.data.frame(
  hackathon %>%
    mutate(
      # Normalize text: getting rid of some extra spaces that were introduced by
      # mistake
      clean_location = str_replace_all(code_shared_location, "\\s*:\\/\\/", "://"),
      clean_location = str_replace_all(clean_location, "doi\\.org/\\s*", "doi.org/")
    ) %>%
    rowwise() %>% # process row by row
    mutate(
      # Extract URLs per row
      code_links = str_extract_all(
        clean_location,
        "(https?://\\S+|doi\\.org/\\S+|datadryad\\.org/\\S+|figshare\\.com/\\S+)"
      ) %>%
        unlist() %>%                               # flatten result
        # Remove trailing punctuation (.,; or ,) from all extracted links
        # [\\.,;"]+ matches one or more of ., ,, ;, or "
        # $ ensures it only matches at the end of the string
        # This will remove sequences like ., .., ,;.", .;, etc., but leave 
        # everything before untouched.
        str_replace_all('[\\.,;"]+$', '') %>%      # clean trailing chars
        paste(collapse = "; "),                    # collapse into single string
      
      # for entries that provide more than one link
      code_number_of_links = ifelse(code_links == "", 0,
                                    str_count(code_links, ";") + 1),
      
      code_chosen_link = case_when(
        # if the entry is "NA" add "NA" to the new variables
        code_shared_location == "NA" ~ "NA",
        # if the entry is not "NA" but there is some text but not apparent links
        # add "check_missing_link" to figure out what's happening there
        code_number_of_links == 0 & code_shared_location != "NA" ~ "check_missing_link",
        # Priority rules: links from doi, figshare, and if not, use the first
        # link presented
        TRUE ~ {
          lks <- unlist(str_split(code_links, ";\\s*"))
          doi_link <- lks[str_detect(lks, "doi\\.org")]
          figshare_link <- lks[str_detect(lks, "figshare")]
          if (length(doi_link) > 0) doi_link[1]
          else if (length(figshare_link) > 0) figshare_link[1]
          else lks[1]
        }
      )
    ) %>%
    ungroup() %>%
    select(-clean_location)
)

# names(hackathon)
# hackathon[,c("MA_ID",
#              "code_shared","code_shared_location",
#              "code_links","code_number_of_links",
#              "code_chosen_link")]



# sort(table(hackathon$processed_data_chosen_link))
# sort(table(hackathon$processed_data_chosen_link==""))
# sort(table(hackathon$raw_data_chosen_link==""))
# sort(table(hackathon$code_chosen_link==""))

# add a final step in which, if the raw_data_chosen_link is not "NA" or 
# "check_missing_link", then check if it starts with https:// and if not, added it
hackathon <- as.data.frame(
  hackathon %>%
    mutate(
      across(
        c(processed_data_chosen_link, raw_data_chosen_link, code_chosen_link),
        ~ case_when(
          .x %in% c("NA", "check_missing_link") ~ .x,
          !str_starts(.x, "https://") ~ paste0("https://", .x),
          TRUE ~ .x
        )
      )
    )
)


################################################################################
# Data extraction error checks (in addition to the few above)
################################################################################

# If _shared == "Yes", _shared_location != "NA"
hackathon <- hackathon %>%
  mutate(
    processed_data_shared_location = case_when(
      processed_data_shared == "Yes" & processed_data_shared_location == "NA" ~ "check_missing_link",
      TRUE ~ processed_data_shared_location
    ),
    raw_data_shared_location = case_when(
      raw_data_shared == "Yes" & raw_data_shared_location == "NA" ~ "check_missing_link",
      TRUE ~ raw_data_shared_location
    ),
    code_shared_location = case_when(
      code_shared == "Yes" & code_shared_location == "NA" ~ "check_missing_link",
      TRUE ~ code_shared_location
    )
  )


# If _shared == "No", 
# _shared_location, _links, _number_of_links AND _chosen_link should be NA
hackathon <- hackathon %>%
  mutate(
    processed_data_shared_location = case_when(
      processed_data_shared == "No" & processed_data_shared_location != "NA" ~ "why_is_there_text_here",
      TRUE ~ processed_data_shared_location
    ),
    raw_data_shared_location = case_when(
      raw_data_shared == "No" & raw_data_shared_location != "NA" ~ "why_is_there_text_here",
      TRUE ~ raw_data_shared_location
    ),
    code_shared_location = case_when(
      code_shared == "No" & code_shared_location != "NA" ~ "why_is_there_text_here",
      TRUE ~ code_shared_location
    )
  )

sort(table(hackathon$processed_data_shared_location))
sort(table(hackathon$raw_data_shared_location))
sort(table(hackathon$code_shared_location))

#hackathon[1:10,c("code_shared","code_shared_location")]


################################################################################
# Assessing potential errors or unstandardization
################################################################################

# implementing several checks to know which rows need an extra double check to 
# assess if the extracted data may be erroneous or it is simply some missing
# information or different/unexpected formatting

hackathon <- as.data.frame(
  hackathon %>%
    mutate(
      # if effect_size_sign_accounted == "Other" then 
      # effect_size_sign_other should be != "NA", if not, write, why_is_there_no_text_here
      # effect_size_sign_quote should be != "NA", if not, write, why_is_there_no_text_here
      effect_size_sign_other = case_when(
        effect_size_sign_accounted == "Other" & effect_size_sign_other == "NA" ~ "why_is_there_no_text_here",
        TRUE ~ effect_size_sign_other
      ),
      effect_size_sign_quote = case_when(
        effect_size_sign_accounted == "Other" & effect_size_sign_quote == "NA" ~ "why_is_there_no_text_here",
        TRUE ~ effect_size_sign_quote
      ),
      # if effect_size_sign_accounted == "Yes" then
      # effect_size_sign_quote should be != "NA", if not, write, why_is_there_no_text_here
      effect_size_sign_quote = case_when(
        effect_size_sign_accounted == "Yes" & effect_size_sign_quote == "NA" ~ "why_is_there_no_text_here",
        TRUE ~ effect_size_sign_quote
      ),
      # if effect_size_sign_accounted == "Unclear" then 
      # effect_size_sign_comments should be != "NA", if not, write, why_is_there_no_text_here
      effect_size_sign_comments = case_when(
        effect_size_sign_accounted == "Unclear" & effect_size_sign_comments == "NA" ~ "why_is_there_no_text_here",
        TRUE ~ effect_size_sign_comments
      ),
      # if inferential_statistics_type == "Other" then 
      # inferential_statistics_other should be != "NA", if not, write, why_is_there_no_text_here
      # inferential_statistics_quote should be != "NA", if not, write, why_is_there_no_text_here
      inferential_statistics_other = case_when(
        inferential_statistics_type == "Other" & inferential_statistics_other == "NA" ~ "why_is_there_no_text_here",
        TRUE ~ inferential_statistics_other
      ),
      inferential_statistics_quote = case_when(
        inferential_statistics_type == "Other" & inferential_statistics_quote == "NA" ~ "why_is_there_no_text_here",
        TRUE ~ inferential_statistics_quote
      ),
      # if inferential_statistics == "Yes" then
      # inferential_statistics_quote should be != "NA", if not, write, why_is_there_no_text_here
      inferential_statistics_quote = case_when(
        inferential_statistics == "Yes" & inferential_statistics_quote == "NA" ~ "why_is_there_no_text_here",
        TRUE ~ inferential_statistics_quote
      ),
      # if inferential_statistics_type != "NA" then
      # inferential_statistics_quote should be != "NA", if not, write, why_is_there_no_text_here
      inferential_statistics_quote = case_when(
        inferential_statistics_type != "NA" & inferential_statistics_quote == "NA" ~ "why_is_there_no_text_here",
        TRUE ~ inferential_statistics_quote
      ),
      # if inferential_statistics == "Unclear" then 
      # inferential_statistics_comments should be != "NA", if not, write, why_is_there_no_text_here
      inferential_statistics_comments = case_when(
        inferential_statistics == "Unclear" & inferential_statistics_comments == "NA" ~ "why_is_there_no_text_here",
        TRUE ~ inferential_statistics_comments
      ),
      # if RoB_assessment == "Yes" then
      # RoB_assessment_type should be != "NA", if not, write, why_is_there_no_text_here
      RoB_assessment_type = case_when(
        RoB_assessment == "Yes" & RoB_assessment_type == "NA" ~ "why_is_there_no_text_here",
        TRUE ~ RoB_assessment_type
      ),
      # if RoB_assessment_type == "Other" then 
      # RoB_assessment_alternative should be != "None", if not, write, why_is_there_no_text_here
      RoB_assessment_alternative = case_when(
        RoB_assessment_type == "Other" & RoB_assessment_alternative == "None" ~ "why_is_there_no_text_here",
        TRUE ~ RoB_assessment_alternative
      ),
      # if RoB_assessment_alternative == "Other" then 
      # RoB_assessment_alternative_other should be != "None", if not, write, why_is_there_no_text_here
      RoB_assessment_alternative_other = case_when(
        RoB_assessment_alternative == "Other" & RoB_assessment_alternative_other == "None" ~ "why_is_there_no_text_here",
        TRUE ~ RoB_assessment_alternative_other
      ),
      # if RoB_assessment == "Unclear" then 
      # RoB_assessment_comments should be != "NA", if not, write, why_is_there_no_text_here
      RoB_assessment_comments = case_when(
        RoB_assessment == "Unclear" & RoB_assessment_comments == "NA" ~ "why_is_there_no_text_here",
        TRUE ~ RoB_assessment_comments
      )
    )
)

# sort(table(hackathon$effect_size_sign_other=="why_is_there_no_text_here"))
# sort(table(hackathon$effect_size_sign_quote=="why_is_there_no_text_here"))
# sort(table(hackathon$effect_size_sign_comments=="why_is_there_no_text_here"))
# sort(table(hackathon$inferential_statistics_other=="why_is_there_no_text_here"))
# sort(table(hackathon$inferential_statistics_quote=="why_is_there_no_text_here"))
# sort(table(hackathon$inferential_statistics_comments=="why_is_there_no_text_here"))
# sort(table(hackathon$RoB_assessment_type=="why_is_there_no_text_here"))
# sort(table(hackathon$RoB_assessment_alternative=="why_is_there_no_text_here"))
# sort(table(hackathon$RoB_assessment_alternative_other=="why_is_there_no_text_here"))
# sort(table(hackathon$RoB_assessment_comments=="why_is_there_no_text_here"))

################################################################################
# Create a new variable "entry_revision_type" that flags rows where 
# certain markers (check_missing_link, why_is_there_text_here, 
# why_is_there_no_text_here) appear anywhere in the character columns. 
# 
# For each row:
# - Look across all character variables only
# - Collect all occurrences of the markers
# - If multiple markers are present, join them with "; "
# - If no markers are present, return "NA"
#
# This ensures we can easily identify which entries require revision 
# based on these special codes, even if they occur multiple times.

# Define the target marker values to search for
markers <- c("check_missing_link", "why_is_there_text_here", "why_is_there_no_text_here")

hackathon <- as.data.frame(
  hackathon %>%
    rowwise() %>%
    mutate(
      entry_revision_type = {
        # Only look across character columns
        # Extract all character values for the current row
        row_vals <- unlist(c_across(where(is.character)))
        # Keep only those values that match the markers
        matches <- row_vals[row_vals %in% markers]
        # Collapse matches into a single string (with "; " as separator)
        # If no matches, return "NA"
        if (length(matches) > 0) paste(matches, collapse = "; ") else "NA"
      }
    ) %>%
    ungroup()# Return to normal (non-rowwise) data frame
)

sort(table(hackathon$entry_revision_type))



################################################################################
# Assessing agreement between observers
################################################################################

# Before that, let's collapse the data sharing information into a single 
# variable called "data_shared" since some of the disagreements between observers
# might have simply been due to disagreements in what processed vs raw data 
# means. At this point, knowing whether there is some data already helps

hackathon <- as.data.frame(
  hackathon %>%
    mutate(
      data_shared = case_when(
        processed_data_shared == "Yes" | raw_data_shared == "Yes" ~ "Yes",
        TRUE ~ "No"
      )
    )
)

table(hackathon$data_shared)
table(hackathon$processed_data_shared)
table(hackathon$raw_data_shared)


# Create a variable "observer_disagreement" that flags where 
# two (or more) observers disagreed on extracted variables.
#
# Logic:
# - If an article (MA_ID) was extracted only once → mark as "NA"
# - If extracted more than once:
#     * Check agreement across the chosen variables
#     * If values differ between observers, record the variable name
#     * Concatenate multiple disagreements with "; "
#
# This allows us to quickly identify which variables need adjudication
# or closer inspection.

names(hackathon)

# Variables to check for agreement/disagreement between observers
vars_to_check <- c("effect_size_sign_accounted", "inferential_statistics",
                   "processed_data_shared", "raw_data_shared",
                   "data_shared",
                   "code_shared", "RoB_assessment")


hackathon <- as.data.frame(
  hackathon %>%
    # For each article ID, summarise whether each variable shows disagreement
    group_by(MA_ID) %>%
    summarise(
      # For each variable, check if more than one distinct value exists
      # (TRUE means disagreement, FALSE means all observers gave same value)
      across(all_of(vars_to_check), ~ n_distinct(.x) > 1),
      .groups = "drop"
    ) %>%
    
    # Work row by row to collapse disagreements into a string
    rowwise() %>%
    mutate(
      observer_disagreement = {
        # Get TRUE/FALSE flags for the six variables
        flags <- c_across(all_of(vars_to_check))
        # Select only the names of variables where disagreement = TRUE
        labs  <- vars_to_check[flags]
        # Collapse into a single string separated by "; ", or empty string if none
        if (length(labs) == 0) "" else paste(labs, collapse = "; ")
      }
    ) %>%
    ungroup() %>%
    
    # Keep only the article ID and the new disagreement column
    select(MA_ID, observer_disagreement) %>%
    
    # Join disagreement info back to the original dataset
    right_join(hackathon, by = "MA_ID") %>%
    
    # If an article has only one extraction, set disagreement to "NA"
    mutate(observer_disagreement = if_else(number_of_assessments == 1,
                                           "NA",
                                           observer_disagreement))
)

names(hackathon)
sort(table(hackathon$observer_disagreement))

# counting how many different types of disagreements affect each row
hackathon <- as.data.frame(
  hackathon %>%
    mutate(
      observer_disagreement_count = if_else(
        observer_disagreement == "NA",
        0L,  # no disagreements
        str_count(observer_disagreement, ";") + 1L
      )
    )
)

summary(hackathon$observer_disagreement_count)


################################################################################
# Revising potential misunderstandings in link extraction
################################################################################

# Create a new column 'same_data_link' in 'hackathon'
hackathon$same_data_link <- ifelse(
  
  # Condition: both processed and raw links contain "https://"
  grepl("https://", hackathon$processed_data_chosen_link) & 
    grepl("https://", hackathon$raw_data_chosen_link),
  
  # If the condition is TRUE, compare the links
  ifelse(
    hackathon$processed_data_chosen_link == hackathon$raw_data_chosen_link,
    "same_link",      # Assign "same_link" if they match
    "different_link"  # Assign "different_link" if they don't match
  ),
  
  # If either link is missing (does not contain "https://"), assign NA
  "NA"       # Use "NA" to ensure the column remains character type
)

# Explanation:
# 1. grepl("https://", ...) checks whether a valid link exists in each column.
# 2. The outer ifelse ensures we only compare links if both exist.
# 3. The inner ifelse performs the actual comparison and assigns a label.
# 4. Using "NA" avoids mixing logical NA and character, which keeps the column type consistent.

table(hackathon$same_data_link)


################################################################################
# Preparing dataset for revision outside R
################################################################################

names(hackathon)

# generating a unique id for each row
hackathon$MA_ID_participant <- paste0(hackathon$MA_ID,
                                      sep="_",
                                      hackathon$participant_name)


# reordering columns
hackathon <- as.data.frame(hackathon %>% 
                             relocate(observer_disagreement, 
                                      .after = entry_revision_type) %>%
                             relocate(data_shared, 
                                      .after = inferential_statistics_comments) %>%
                             relocate(same_data_link, 
                                      .after = raw_data_chosen_link) %>%
                             relocate(title, 
                                      .after = MA_ID) %>%
                             relocate(MA_ID_participant, 
                                      .before = MA_ID)
)

names(hackathon)


################################################################################
# exporting dataset for exploring potential errors
################################################################################

# The plan is: 
# 1. Export only the rows/variables that need reassessment to CSV, not the whole 
# dataset.
# 2. Edit only those extracts in Excel (with a changelog added below).
# 3. Re-import into R, then join/replace back into the full dataset

# This way: 
# We reduce error risk (only touching problem rows)
# We still preserve reproducibility

# starting with the easy simple ones that are likely simple typos or missing
# info
hackathon.errors <- as.data.frame(
  hackathon %>% 
    filter(entry_revision_type != "NA")
)


# exporting the dataset. I am using .xlsx because .csv and Excel made a mess 
# with all special characters, despite using write_csv which per default has
# UTF-8 enconding
# readr::write_csv(hackathon.errors,
#                  "data/raw_data/hackathons/2016-2020/02_error_fixing/01_entry_revision_type_to_correct.csv")
# writexl::write_xlsx(hackathon.errors,
#                     "data/raw_data/hackathons/2016-2020/02_error_fixing/01_entry_revision_type_to_correct.xlsx")

# reimporting and replacing
hackathon.corrections <- readxl::read_excel("data/raw_data/hackathons/2016-2020/02_error_fixing/01_entry_revision_type_to_correct_AST.xlsx")

# Changelog

# MA_008_Ryan Field: effect_size_sign_other from why_is_there_no_text_here to 
# AST: probably not applicable

# MA_026_Pietro Pollo: effect_size_sign_comments changed from why_is_there_no_text_here
# to the corresponding value in effect_size_sign_quote preceded by "AST: ", 
# which is unclear whether it is a quote or a comment from the participant

# MA_050_Heikel Balti: effect_size_sign_comments changed from why_is_there_no_text_here
# to AST: all indicates effect size sign was accounted for

# MA_037_Nerea Piñeiro-Juncal: inferential_statistics_comments changed from 
# why_is_there_no_text_here to AST: all indicates inferential statistics should
# be No

# MA_051_Gabe Winter: inferential_statistics_comments changed from 
# why_is_there_no_text_here to AST: all indicates inferential statistics should
# be NA

# MA_031_Malgorzata Lagisz: processed_data_shared_location changed from 
# why_is_there_text_here to NA

# MA_064_Malgorzata Lagisz: processed_data_shared_location changed from 
# why_is_there_text_here to NA

# MA_012_Nerea Piñeiro-Juncal: RoB_assessment_alternative changed from 
# why_is_there_no_text_here to AST: Non-comparable_article_exclusion using the 
# information provided in RoB_assessment_other

# MA_050_Nerea Piñeiro-Juncal: RoB_assessment_alternative changed from 
# why_is_there_no_text_here to AST: Experimental_design_comparison using the 
# information provided in RoB_assessment_other

# MA_055_Gabe Winter: RoB_assessment_alternative changed from 
# why_is_there_no_text_here to AST: None using the information provided in 
# RoB_assessment_other

# MA_058_Nerea Piñeiro-Juncal: RoB_assessment_alternative changed from 
# why_is_there_no_text_here to AST: Experimental_design_comparison using the 
# information provided in RoB_assessment_other

# MA_001_Meagan Harper: processed_data_chosen_link changed from check_missing_link
# to https://doi.org/10.5061/dryad.qv000, which corresponds to the dryad 
# repository DOI. The data is also provided in Table S1, which is what the 
# participant observed, but since we prioritise dryad, I have added the DOI of 
# the Dryad repository. Note that code is also provided in the repository, but
# the participant categorised the study as not providing code

# MA_016_Nicholas Moran: processed_data_chosen_link changed from check_missing_link
# to https://doi.org/10.1111/gcb.13424, which is the article's DOI link since
# data was seemingly provided in Table 1 (and 2?). Problem with this is whether#
# this paper is even a realy meta-analysis, which I do not think it is. AST.

# MA_021_Nerea Piñeiro-Juncal: processed_data_chosen_link and raw_data_chosen_link
# changed from check_missing_link to http://doi.org/10.1111/gcb.14394, which is the 
# article's DOI link since data is seemingly provided as supplementary material


# MA_041_Nerea Piñeiro-Juncal: processed_data_chosen_link and raw_data_chosen_link
# changed from check_missing_link to http://doi.org/10.1002/ecs2.2354, which is 
# the article's DOI link since data is seemingly provided in Table 2

# MA_042_Meagan Harper: raw_data_chosen_link changed from check_missing_link to 
# http://doi.org/10.1002/jwmg.21456, which is the article's DOI link since data
# is seemingly provided in Tables 2 and 3


# updating new values
hackathon_clean_1 <- as.data.frame(
  hackathon %>%
    rows_update(hackathon.corrections, by = "MA_ID_participant")
)

# as.data.frame(
#   hackathon_clean_1 %>% 
#     filter(entry_revision_type != "NA")
# )

# hackathon_clean_1[c(16,38,80,86,97,102,107,108,111,112,116,117,124),]
# unique(hackathon_clean_1[c(16,38,80,86,97,102,107,108,111,112,116,117,124),"MA_ID"])

################################################################################
# Reassessing RoB_assessment & RoB_assessment_alternative
################################################################################

# starting with RoB related ones, which are expected to be easier to deal with
# the idea is to create four new variables that are the revised versions of
# RoB_assessment, RoB_assessment_type, RoB_assessment_alternative, and 
# RoB_assessment_comments, and export them as well as all the RoB related 
# questions. 
# In Excel, AST will add a value to the three revised versions of those three 
# variables using all information available in the dataset, nothing else. For 
# articles appearing twice, the information for both extractions would be used 
# to make the decision. AST will add comments to the variable 
# RoB_assessment_comments_revised
# "Inconclusive" will be used whenever AST cannot make a decision a variable 
# based on the provided information.
# If not information is provided in the comments or any other text, the 
# _revised variables will be left as "NA" instead of "Inconclusive" since there
# is no information to make a decision.
# That is, at this point, we are not looking back at the articles

# "Experimental_design_Method_comparison" will be used for studies that compare
# different types of experimental designs (e.g., exp vs obs, or different
# experimental designs, etc.) or methods.

# therefore, Other contains c("Experimental_design_Method_comparison")

hackathon.RoB.revision <- as.data.frame(
  hackathon_clean_1 %>% 
    mutate(RoB_assessment_revised = "",
           RoB_assessment_type_revised = "",
           RoB_assessment_alternative_revised = "",
           RoB_assessment_comments_revised = "") %>% 
    select(MA_ID_participant,
           RoB_assessment_revised,
           RoB_assessment, 
           RoB_assessment_type_revised,
           RoB_assessment_type, 
           RoB_assessment_alternative,
           RoB_assessment_alternative_revised,
           RoB_assessment_alternative_other,
           RoB_assessment_other,
           RoB_assessment_comments,
           RoB_assessment_comments_revised) %>%
    arrange(MA_ID_participant)
)


# exporting the dataset. I am using .xlsx because .csv and Excel made a mess 
# with all special characters, despite using write_csv which per default has
# UTF-8 enconding
# writexl::write_xlsx(hackathon.RoB.revision,
#                     "data/raw_data/hackathons/2016-2020/02_error_fixing/02_RoB_revision.xlsx")


# # exporting dataset for Shreya to use it while preparing the manually extracted
# # dataset
# readr::write_csv(hackathon_clean_1 %>% 
#                    select(MA_ID,
#                           data_shared,
#                           processed_data_shared, 
#                           processed_data_shared_location,
#                           processed_data_links,
#                           processed_data_number_of_links,
#                           processed_data_chosen_link,
#                           raw_data_shared, 
#                           raw_data_shared_location,
#                           raw_data_links,
#                           raw_data_number_of_links,
#                           raw_data_chosen_link,
#                           code_shared,
#                           code_shared_location,
#                           code_links,
#                           code_number_of_links,
#                           code_chosen_link,
#                           data_and_code_sharing_comments,
#                           observer_disagreement),
#                  "data/raw_data/hackathons/2016-2020/999_data_and_code_variables_SD.csv")
# 



# reimporting and adding the new _revised entries, including the
# comments_revised, which explains the decisions made for each entry
hackathon.RoB.revision.done <- readxl::read_excel("data/raw_data/hackathons/2016-2020/02_error_fixing/02_RoB_revision_AST.xlsx")

# selecting only the variables of interest to add them to our dataset
hackathon.RoB.revision.done.subset <- hackathon.RoB.revision.done %>%
  select(MA_ID_participant,ends_with("_revised"))

# merging to add the new _revised variables as well as to then order the columns
# so that the _revised ones appear right afer their original counterparts
hackathon_clean_2 <- as.data.frame(
  hackathon_clean_1 %>%
    left_join(hackathon.RoB.revision.done.subset, 
              by = "MA_ID_participant") %>% 
    relocate(RoB_assessment_revised, 
             .after = RoB_assessment) %>%
    relocate(RoB_assessment_type_revised, 
             .after = RoB_assessment_type) %>%
    relocate(RoB_assessment_alternative_revised, 
             .after = RoB_assessment_alternative) %>%
    relocate(RoB_assessment_comments_revised, 
             .after = RoB_assessment_comments)
  
)

names(hackathon)

sort(table(hackathon_clean_2$RoB_assessment))
sort(table(hackathon_clean_2$RoB_assessment_revised))

sort(table(hackathon_clean_2$RoB_assessment_type))
sort(table(hackathon_clean_2$RoB_assessment_type_revised))

sort(table(hackathon_clean_2$RoB_assessment_alternative))
sort(table(hackathon_clean_2$RoB_assessment_alternative_revised))
# 
# sort(table(hackathon_clean_2$RoB_assessment_comments))
# sort(table(hackathon_clean_2$RoB_assessment_comments_revised))


# Merge original and revised Risk of Bias (RoB) assessment columns
#
# Step 1: RoB_assessment_revised_merged
#   - Combines the original and revised RoB assessment.
#   - If the revised value (RoB_assessment_revised) is the string "NA", it means
#     that based on the provided text by the participants, AST could not revise
#     it, and therefore, we are going to keep the original value (RoB_assessment) 
#   - Otherwise, the revised value is used (since it should be the correct one
#     based on the information provided by the participants)
#
# Step 2: RoB_assessment_type_revised_merged
#   - Creates a new column based on RoB_assessment_revised_merged:
#       * "No"              --> sets value to "NA", because that's what the value
#                               should be if RoB_assessment is truly a "No"
#       * "" (empty string) --> sets value to "needs_checking", which in this 
#                               case is because there is a missing extraction
#       * "Yes"             --> uses the revised assessment type 
#                               (RoB_assessment_type_revised), thought since
#                               there are no Yes's, this does not matter (yet)
#       * Any other value   --> flags as "unexpected value"
#
# Step 3: RoB_assessment_alternative_revised_merged
#   - Combines the original and revised alternative assessment:
#       * If RoB_assessment_alternative_revised != "NA", use the revised value
#       * Else if RoB_assessment_alternative == "None", keep the original
#       * Otherwise, set value to "needs_confirmation".
#       What I am doing here is to prioritise RoB_assessment_alternative_revised
#       since it should be the correct one. If this value is "NA" it means 
#       AST could not revise based on the provided text by the participants
#       so we prioritise what is available in RoB_assessment_alternative, but
#       given that anything other than "None" is pretty rare in our dataset
#       I am adding needs_confirmation for those that are different to "None"
#       because it means that there was not text for me to revised them, which 
#       is certainly suspicious
#
# This pipeline ensures that valid information from both original and revised 
# assessments is preserved and that assessment types and alternative assessments
# are updated consistently and reproducibly.


hackathon_clean_2 <- as.data.frame(
  hackathon_clean_2 %>%
    mutate(
      # Merge original and revised assessment
      RoB_assessment_revised_merged = if_else(
        RoB_assessment_revised == "NA",    # treat "NA" string as missing
        RoB_assessment,                    # use original value
        RoB_assessment_revised             # otherwise use revised
      ),
      
      # Create merged assessment type based on the merged assessment
      RoB_assessment_type_revised_merged = case_when(
        RoB_assessment_revised_merged == "No"          ~ "NA",
        RoB_assessment_revised_merged == ""            ~ "needs_checking",
        RoB_assessment_revised_merged == "Yes"         ~ RoB_assessment_type_revised,
        TRUE                                           ~ "unexpected_value"  # fallback for unexpected values
      ),
      
      # Merge original and revised alternative assessment
      RoB_assessment_alternative_revised_merged = case_when(
        RoB_assessment_alternative_revised != "NA" ~ RoB_assessment_alternative_revised,
        RoB_assessment_alternative == "None"       ~ RoB_assessment_alternative,
        TRUE                                       ~ "needs_confirmation"
      )
    ) %>% 
    relocate(RoB_assessment_revised_merged, 
             .after = RoB_assessment_revised) %>%
    relocate(RoB_assessment_type_revised_merged, 
             .after = RoB_assessment_type_revised) %>%
    relocate(RoB_assessment_alternative_revised_merged, 
             .after = RoB_assessment_alternative_revised)
)

sort(table(hackathon_clean_2$RoB_assessment))
sort(table(hackathon_clean_2$RoB_assessment_revised))
sort(table(hackathon_clean_2$RoB_assessment_revised_merged))

# sort(table(hackathon_clean_2$RoB_assessment_type))
# sort(table(hackathon_clean_2$RoB_assessment_type_revised))
# sort(table(hackathon_clean_2$RoB_assessment_type_revised_merged))

# sort(table(hackathon_clean_2$RoB_assessment_alternative))
# sort(table(hackathon_clean_2$RoB_assessment_alternative_revised))
# hackathon_clean_2[,c("RoB_assessment_alternative","RoB_assessment_alternative_revised")]
# sort(table(hackathon_clean_2$RoB_assessment_alternative_revised_merged))


# It is time to finish up cleaning the RoB related variables. I am going to
# export the subset that needs checking or confirmation, revise the entries by
# going through their respective pdfs, import the updated dataset, and 
# substitute the old values (in _merged) by the updated ones based on this 
# exercise, so that at the end, it is the _merged variables that we consider
# the most correct data

hackathon.RoB.revision.PDF <- as.data.frame(
  hackathon_clean_2 %>% 
    # filter(
    #   if_any(starts_with("RoB_"), ~ . == "Inconclusive") |
    #     (RoB_assessment_type_revised_merged %in% c("needs_checking", "unexpected_value")) |
    #     (RoB_assessment_alternative_revised_merged %in% c("needs_confirmation"))
    # ) %>%
    filter(
      if_any(
        starts_with("RoB_"),
        ~ . %in% c("needs_checking", "unexpected_value", "needs_confirmation", "Inconclusive")
      )
    ) %>%
    select(MA_ID,
           title,
           MA_ID_participant,
           RoB_assessment,
           RoB_assessment_revised,
           RoB_assessment_revised_merged, 
           RoB_assessment_type, 
           RoB_assessment_type_revised,
           RoB_assessment_type_revised_merged,
           RoB_assessment_alternative,
           RoB_assessment_alternative_revised,
           RoB_assessment_alternative_revised_merged,
           RoB_assessment_alternative_other,
           RoB_assessment_other,
           RoB_assessment_comments,
           RoB_assessment_comments_revised)
)

# exporting the dataset. I am using .xlsx because .csv and Excel made a mess 
# with all special characters, despite using write_csv which per default has
# UTF-8 enconding
# writexl::write_xlsx(hackathon.RoB.revision.PDF,
#                     "data/raw_data/hackathons/2016-2020/02_error_fixing/03_RoB_revision_PDF.xlsx")

# reimporting and replacing
hackathon.RoB.revision.PDF.done <- readxl::read_excel("data/raw_data/hackathons/2016-2020/02_error_fixing/03_RoB_revision_PDF_AST.xlsx")

# Changelog

# MA_022_Shreya Dimri: after reading the PDF, 
# RoB_assessment_alternative_revised_merged updated to None.
# I could also confirm that the study does not use inferential statistics as it
# focuses on lnRR, and that the effect_size_sign_accounted should be "NA"

# MA_027_Nerea Piñeiro-Juncal: after reading the PDF, 
# RoB_assessment_alternative_revised_merged updated to None. They do some tests
# that could go into a possible alternative RoB direction (e.g., "(3) measures 
# of yield expressed in very different terms") but it is not clear whether that 
# applies here and the authors wording do not suggest that those are done as
# alternative RoB tests

# I could also confirm that the study does not use inferential statistics since
# "Studies that did not report means or sample sizes were excluded from the 
# analysis", and the calculated lnRR. The also focused on species richness or 
# yield, and there is no evidence that sign should be a concern here, thus,
# effect_size_sign_accounted should be "NA"


# MA_035_Ryan Field and MA_035_Malgorzata Lagisz: after reading the PDF, 
# RoB_assessment_alternative_revised_merged updated to 
# Experimental_design_Method_comparison because the meta-analysis tested 
# differences between Field and Laboratory, even arguing different possibilities
# for why to expect this biologically and methodologically.

# I could also confirm that the study does not use inferential statistics since
# it focuses on heritabilities, which are treated as Pearson's r, and only 
# transformed to Zr. In addition, effect_size_sign_accounted should be "NA"
# because heritabilities cannot be negative (at least not real h2)


# MA_043_Chengsheng Zheng and MA_043_Nerea Piñeiro-Juncal: after reading the 
# PDF, RoB_assessment_alternative_revised_merged updated to None. 

# The author reported: "Differences in individual study methodologies, time 
# periods and geographical locations present challenges in integrating 
# observations and in interpreting results. For this reason, I have added 
# additional constraints to provide better standardization of all observations. 
# Species observations, for example, include only presence, not abundance. 
# Nutrient measurements had the additional constraint of being a measurement of 
# green leaves only, and metal measurements pertain only to those from surface 
# sediments.". HOWEVER, removing studies is not a RoB assessment per se. In 
# addition, the author did some test on how different size-selections (1-20km)
# matter, but this does not reflect differences between studies. Thus, 
# RoB_assessment_alternative should be None 

# The list of variables explored, which includes Mangrove coverage, mangrove 
# leaf δ15N content, leaf carbon to nitrogen ratios, sediment concentrations of 
# heavy metals, diversity of subsistence fisheries, worm diversity, mangrove 
# specialist bird diversity, suggest that effect_size_sign_accounted should be 
# "NA"

# From all I can see, it seems that inferential_statistics should be NA, as it
# does not really seem to apply (author is extracting reported values from 
# the papers and analysing them directly, rather than focusing on comparisons
# or correlations from the papers).

# Data provided, and I would say it is the raw data, no code to be seen. Data
# provided in the supplements, and part of it, the super raw raster dataset 
# provided at http://hdl.handle.net/11603/1584

# The RoB_assessment_comments comment by MA_043_Nerea is confusing since the 
# supplements only contain .csv datasets

# Last, this does not seem to be a meta-analysis per se, at least, certainly not 
# weighted, as only lm and ANOVA's are used


# MA_056_Nerea Piñeiro-Juncal: 

# Based on "Articles [that met these criteria (452) w]...] were then further 
# evaluated to identify those that [...] included mean value, listed standard 
# error or standard deviation, and the sample size stated", 
# inferential_statistics should be "NA" as it does not apply

# Based on what the authors study, which includes anthropogenic noise effects 
# on fish behavior and physiology, and the fact that they look into variables
# such as "foraging ability, predation risk, and reproductive success", 
# and "To account for the large amount of dissimilarity present within the 
# response variables, the directionality of each study was determined to ensure 
# that negative and positive effect sizes represented negative and positive 
# responses, respectively. For example, for a response variable such as growth 
# rate, an increase would result in a positive effect size, while an increase in
# a response variable such as cortisol (a common stress hormone) would also lead
# to a positive effect size, despite being an undesirable response. Accounting 
# for the directionality of each response variable is thus a critical step for 
# a meta-analysis of this nature", effect_size_sign_accounted should be "Yes"

# There is no code provided, but there is some data, at least as part of the
# foresplots (means and 95% CI). Thus, raw_data_shared is clear a "No" becuase
# no means, SDs, sample sizes are provided. But one could consider that
# processed_data_shared is a "Yes" if we count figures, which we are probably
# not doing...

# MA_061_Pietro Pollo: 

# Based on what the authors study, effect_size_sign_accounted should be "NA"
# "We "investigated the impacts of selective logging on the mass–abundance 
# scaling of avian communities"
# "Mass–abundance scaling describes the negative relationship between a species'
# body mass and population abundance"
# "Data were obtained from 30 studies [...] that contained information on 
# abundance or capture rate for avian species in both selectively logged forests 
# and old-growth primary forest controls across the tropics"

# Based on what effect size the authors used, which is (1) a custom relative 
# standardized abundance calculated with abundance data extracted directly from
# the studies of interest, and (2) performing a quantile regression with the
# species' mass, so that later on Hedges' g is calculated to estimate the mean
# difference in that mass-abundance scaling between logged and primary groups, 
# inferential_statistics should be "NA" as it does not apply

# There is no RoB type of test, therefore, RoB_assessment and
# RoB_assessment_revised_merged as "No" (the participant had not extracted 
# information for these variables), and RoB_assessment_type_revised_merged
# as "NA

# raw_data_shared should indeed be "Yes", processed_data_shared is "No" because
# the mass-scaling values are not provided as far as I can see, and finally,
# code_shared is also "No"

# MA_067_Ryan Field:

# "examined variation in species behaviour, distribution and population dynamics 
# in birds and mammals in response to shrub cover"
# papers [...] that reported a direct association between a quantitative 
# characteristic of shrub relevant to shrub encroachment (e.g. shrub cover, 
# height or patchiness) and a characteristic of wildlife relevant to population 
# density or distribution (e.g. behaviour, reproductive success, population size 
# and occurrence). In addition, 
# The study looks at variables such as: Nest success and Nest predation,
# Probability of colonization and Probability of extinction... therefore, 
# effect_size_sign_accounted should NOT be "NA", and it should indeed be "No"
# seems nothing is mentioned about how they handled the signs.

# The authors look at associations and also "for differences between two means 
# we calculated Cohen's d". For the associations, they extracted effect sizes
# "extracted β coefficients for each single shrub-related variable and their 
# associated confidence intervals. In models including interaction terms, [...]
# extracted the effects for each condition. Where authors reported effect sizes 
# and group means from transformed data, our calculated effect sizes therefore 
# also reflect those transformations. Where authors performed non-parametric 
# tests, our results may differ from those reported from the author as 
# calculation of Cohen's d assumes normality of data.

# In addition, the authors "assessed the relationship between vegetation biomass 
# across a species’ range and the likelihood of positive responses to shrub cover 
# by performing logistic regression between the proportion of positive responses 
# to shrub cover and mean NDVI across a species’ range.

# The authors "performed weighted multiple logistic regressions on the proportion 
# of positive responses for each response type recorded from each study for each 
# species", so for all I can see, inferential_statistics should be "NA"
# (and actually inferential_statistics_transformations should be "NA")

# There is nothing about RoB or anything that could be considered RoB, so 
# RoB_assessment_alternative should be "None"

# Tables S1 and S2 provide something close to the processed data, but incomplete
# as no all effect sizes per sepecies are seemingly provided


# updating new values
hackathon_clean_3 <- as.data.frame(
  hackathon_clean_2 %>%
    rows_update(hackathon.RoB.revision.PDF.done, by = "MA_ID_participant")
)

table(hackathon_clean_3$RoB_assessment_alternative_revised_merged)
table(hackathon_clean_2$RoB_assessment_alternative_revised_merged)


################################################################################
# Reassessing effect_size_sign_accounted & inferential_statistics
################################################################################

# let's start by revising entries based on what information the participants 
# provided, i.e., without looking at the PDFs. We are going to do this for
# both effect_size_sign_accounted & inferential_statistics at the same time
# because here the information overlaps.

hackathon.ES.IS.revision <- as.data.frame(
  hackathon_clean_3 %>% 
    mutate(effect_size_sign_accounted_revised = "",
           effect_size_sign_inferential_statistics_comments_revised = "",
           inferential_statistics_revised = "") %>% 
    select(MA_ID_participant,
           effect_size_sign_accounted,
           effect_size_sign_accounted_revised, 
           effect_size_sign_other,
           effect_size_sign_quote,
           effect_size_sign_comments,
           inferential_statistics,
           inferential_statistics_revised,
           inferential_statistics_type,
           inferential_statistics_other,
           inferential_statistics_transformations,
           inferential_statistics_quote,
           inferential_statistics_comments,
           effect_size_sign_inferential_statistics_comments_revised) %>%
    arrange(MA_ID_participant)
)


# exporting the dataset. I am using .xlsx because .csv and Excel made a mess 
# with all special characters, despite using write_csv which per default has
# UTF-8 enconding
# writexl::write_xlsx(hackathon.ES.IS.revision,
#                     "data/raw_data/hackathons/2016-2020/02_error_fixing/04_ES_IS_revision.xlsx")


# reimporting and adding the new _revised entries, including the
# comments_revised, which explains the decisions made for each entry
hackathon.ES.IS.revision.done <- readxl::read_excel("data/raw_data/hackathons/2016-2020/02_error_fixing/04_ES_IS_revision_AST.xlsx")

# While revising them, extra findings:

# MA_007: Processed data (means, 95% CI) available in Figure 2

# MA_060: RoB could be Yes, since they record "experimental type, including 
# field investigations and pot laboratory studies"

# selecting only the variables of interest to add them to our dataset
hackathon.ES.IS.revision.done.subset <- hackathon.ES.IS.revision.done %>%
  select(MA_ID_participant,ends_with("_revised"))

# merging to add the new _revised variables as well as to then order the columns
# so that the _revised ones appear right afer their original counterparts
hackathon_clean_4 <- as.data.frame(
  hackathon_clean_3 %>%
    left_join(hackathon.ES.IS.revision.done.subset,
              by = "MA_ID_participant") %>%
    relocate(effect_size_sign_accounted_revised,
             .after = effect_size_sign_accounted) %>%
    relocate(inferential_statistics_revised,
             .after = inferential_statistics) %>%
    relocate(effect_size_sign_and_inferential_statistics_comments_revised,
             .after = inferential_statistics_comments)
)

names(hackathon_clean_4)


# It is time to finish up cleaning the RoB related variables. I am going to
# export the subset that needs checking or confirmation, revise the entries by
# going through their respective pdfs, import the updated dataset, and 
# substitute the old values (in _merged) by the updated ones based on this 
# exercise, so that at the end, it is the _merged variables that we consider
# the most correct data

hackathon.ES.IS.revision.PDF <- as.data.frame(
  hackathon_clean_4 %>% 
    filter(effect_size_sign_accounted_revised == "Inconclusive" |
             inferential_statistics_revised == "Inconclusive") %>%
    select(MA_ID,
           title,
           MA_ID_participant,
           effect_size_sign_accounted,
           effect_size_sign_accounted_revised,
           effect_size_sign_other, 
           effect_size_sign_quote, 
           effect_size_sign_comments,
           inferential_statistics,
           inferential_statistics_revised,
           inferential_statistics_type,
           inferential_statistics_other,
           inferential_statistics_transformations,
           inferential_statistics_quote,
           inferential_statistics_comments,
           effect_size_sign_and_inferential_statistics_comments_revised)
)

# exporting the dataset. I am using .xlsx because .csv and Excel made a mess 
# with all special characters, despite using write_csv which per default has
# UTF-8 enconding
# writexl::write_xlsx(hackathon.ES.IS.revision.PDF,
#                     "data/raw_data/hackathons/2016-2020/02_error_fixing/05_ES_IS_revision_PDF.xlsx")

# reimporting and replacing
hackathon.ES.IS.revision.PDF.done <- readxl::read_excel("data/raw_data/hackathons/2016-2020/02_error_fixing/05_ES_IS_revision_PDF_AST.xlsx")

# Changelog

# MA_002: after reading the PDF, 
# investigate how mowing intensity impacts the ecology of urban lawns
# invertebrate and plant diversity
# The Abiotic Processes group represents variables that can negatively affect 
# ecosystem balance, such as soil moisture deficit, increased soil temperature, 
# and carbon loss.

# effect_size_sign_accounted_revised remained as/changed to NA (though not easy to decide)
# inferential_statistics_revised remained as/changed to NA

# processed_data_shared should be No (though provided in Figure 2)
# raw_data_shared should be Yes
# code_shared should be No
# RoB_assessment_revised_merged should be No
# RoB_assessment_alternative_revised_merged should be None 
# The authors compared studies based on journal of publication but that is more 
# of a PB test than a RoB


# MA_004: after reading the PDF, 
# Hedges’d value for study marked * was inverted to represent change in stock loss
# Sample sizes, means, and standard deviations were extracted from the text, 
# tables, or figures from each article or calculated from the data provided.

# effect_size_sign_accounted_revised remained as/changed to Yes
# inferential_statistics_revised remained as/changed to NA

# processed_data_shared should be Yes (but notice that it is provided in PDF Table S2) 
# raw_data_shared should be No
# code_shared should be No
# RoB_assessment_revised_merged should be No
# RoB_assessment_alternative_revised_merged should be None
# The authors compare different mitigation methods but not as a RoB rather as
# their main question of interest


# MA_005: after reading the PDF, 
# relationships between elevation and soil microbial biomass (SMB)
# We recorded the SMB data, corresponding elevation, and the sample size from 
# each study by extracting data directly from the text, tables or digitized 
# figures.

# effect_size_sign_accounted_revised remained as/changed to NA
# inferential_statistics_revised remained as/changed to  NA

# processed_data_shared should be No
# raw_data_shared should be Yes
# code_shared should be No
# RoB_assessment_revised_merged should be No
# RoB_assessment_alternative_revised_merged should be None


# MA_010: after reading the PDF, 
# Plant traits included total dry weight, % germination, disease severity,
# mass, cell membrane injury...
# For Microbe traits similar, e.g., colonization and elimination
# BUT variable Response is all positive variables related to weight, length, etc.

# effect_size_sign_accounted_revised remained as/changed to NA (though slightly unclear)
# inferential_statistics_revised remained as/changed to NA

# processed_data_shared should be No
# raw_data_shared should be Yes
# code_shared should be No (the authors mentioned that code is in supplementary 3, but there is not such file as far as I can find it)
# RoB_assessment_revised_merged should be No
# RoB_assessment_alternative_revised_merged should be None


# MA_012: after reading the PDF, 
# parasite was significantly more prevalent in its introduced range (i.e., 
# short-term interaction) than in its native range (long-term interaction), a 
# result that was also supported by a meta-analysis of prevalence data covering 
# the 50 years since its introduction

# effect_size_sign_accounted_revised remained as/changed to NA
# inferential_statistics_revised remained as/changed to 

# processed_data_shared should be Yes
# raw_data_shared should be Yes
# code_shared should be No
# RoB_assessment_revised_merged should be No
# RoB_assessment_alternative_revised_merged should be None


# MA_013: after reading the PDF, 
# Table S2 shows that "Character" inludes longevity, size, ... dispersal...
# parasitism rate... 
# Extreme weather-related events ... crop growing regions, potentially affecting
# control of lepidopteran pests by egg parasitoids
# firstly review information on the effectiveness of Trichogramma egg parasitoids 
# under stressful climate conditions
# secondly, we conduct a meta-analysis of cases of parasitism
# Trichogramma parasitism decreased...
# meta-analysis to investigate whether extreme events involving temperature and 
# precipitation effect on egg parasitism levels using published literature on 
# Trichogramma
# Mean parasitism, standard deviation and sample size for two different treatments 
# were collected from text sections

# The methods are really tricky to read and understand. I based my decision 
# on the fact that what's seems to be meta-analysed is simply parasitism.
# Despite that Table S2 shows different traits with opposite expectations were 
# studied. I acknowledge that No could also be used

# effect_size_sign_accounted_revised remained as/changed to NA
# inferential_statistics_revised remained as/changed to NA

# processed_data_shared should be No (deceiving, looks like data would be given in Table S1-S3 but it is not) 
# raw_data_shared should be No (deceiving, looks like data would be given in Table S1-S3 but it is not)
# code_shared should be No
# RoB_assessment_revised_merged should be No
# RoB_assessment_alternative_revised_merged should be None


# MA_018: after reading the PDF, 
# effects of vegetation type (i.e. forest, grassland and scrubland) on runoff 
# and sediment yields

# effect_size_sign_accounted_revised remained as/changed to NA 
# inferential_statistics_revised remained as/changed to NA

# processed_data_shared should be No 
# raw_data_shared should be Yes (but seemingly incomplete)
# code_shared should be No
# RoB_assessment_revised_merged should be No
# RoB_assessment_alternative_revised_merged should be None


# MA_024: after reading the PDF, 
# two key traits linked to pace-of-life in birds: survival and clutch size
# For studies where data on population means, standard errors or standard 
# deviations were only available on figures, we used WebPlotDigitizer to
# extract the variable of interest from the graphs
# absolute risk reduction as our effect size metric 

# effect_size_sign_accounted_revised remained as/changed to NA
# inferential_statistics_revised remained as/changed to NA (a bit unclear)

# processed_data_shared should be Yes (Data for survival and clutch size analysis provided in the Supplementary Word Doc)
# raw_data_shared should be No
# code_shared should be Yes (Codes for survival and clutch size analysis provided in the Supplementary Word Doc)
# RoB_assessment_revised_merged should be No
# RoB_assessment_alternative_revised_merged should be None (though one could
# consider Experimental_design_Method_comparison since tracking method is 
# included in one model, but no other informaiton about why or about the 
# results of this is provided)


# MA_026: after reading the PDF, 
# determine whether outdoor access is a significant risk factor for parasitic 
# infection in domestic pet cats across 19 different pathogens
# OR effect size

# effect_size_sign_accounted_revised remained as/changed to NA
# inferential_statistics_revised remained as/changed to NA

# processed_data_shared should be No
# raw_data_shared should be Yes
# code_shared should be Yes (random-effects metanalysis ran :'( )
# RoB_assessment_revised_merged should be No
# RoB_assessment_alternative_revised_merged should be None


# MA_029: after reading the PDF, 
# summarize published estimates of band reporting probabilities for North 
# American waterfowl
# reporting probability estimates (always positive, including those that they
# reanalysed themselves; as far as I can see)
# summarized data on direct recovery probabilities from standard- and 
# reward-banded mallards
# Studies of band reporting probabilities have typically relied exclusively on 
# direct recoveries [...], but a few studies included indirect recoveries from 
# subsequent hunting seasons [...]. Formal reanalysis of such extended data 
# would involve estimating survival probabilities during intervening years 
# [...], but in cases where we reanalyzed such data, we assumed that survival 
# and harvest probabilities were equal between reward and control bands and 
# simply focused on numbers of indirect recoveries versus number originally 
# banded for estimating relative reporting probabilities

# effect_size_sign_accounted_revised remained as/changed to NA
# inferential_statistics_revised remained as/changed to NA

# processed_data_shared should be Yes
# raw_data_shared should be Yes
# code_shared should be Yes
# RoB_assessment_revised_merged should be No
# RoB_assessment_alternative_revised_merged should be Experimental_design_Method_comparison
# The reason for me here is that they report the following: "For remaining 
# studies that adjusted for solicitation and did not report how many solicited 
# bands had been excluded (Garrettson et al. 2014), we assigned a dummy variable
# to code for solicitation and accounted for this effect statistically.". They 
# do this particularly in the context of discussing how methodological
# differences between studies can affect the results. They also account for:
# "most rigorous estimates of band reporting probabilities are from the most 
# recent period"


# MA_031: after reading the PDF, 
# toxin content and toxicity, and growthrate

# Supplements: CO2 effects on growth rates and elemental composition
# Type of culture experiment (batch, chemostat, etc)

# effect_size_sign_accounted_revised remained as/changed to NA
# inferential_statistics_revised remained as/changed to NA

# processed_data_shared should be No (at least some of it could be extracted from Figure S2-S4)
# raw_data_shared should be Yes (slightly unclear)
# code_shared should be No
# RoB_assessment_revised_merged should be No
# RoB_assessment_alternative_revised_merged should be None (type of culture
# extracted but not seemingly used or discussed)


# MA_036: after reading the PDF, 
# From provided data, some variables are only positive and some only negative
# which tentatively suggest their signs may need to be flipped to make them 
# comparable. Not clear though. From the description in the article and from 
# Table S1 and S2 seemingly all variables positive, so I go for NA

# effect_size_sign_accounted_revised remained as/changed to NA
# inferential_statistics_revised remained as/changed to NA

# processed_data_shared should be No
# raw_data_shared should be Yes
# code_shared should be No
# RoB_assessment_revised_merged should be No
# RoB_assessment_alternative_revised_merged should be None


# MA_037: after reading the PDF, 
# The used the correlations and the PCA's factor loadings directly, no
# transformations as far as I can see.

# effect_size_sign_accounted_revised remained as/changed to Yes
# inferential_statistics_revised remained as/changed to NA

# processed_data_shared should be Yes 
# raw_data_shared should be Yes
# code_shared should be Yes
# RoB_assessment_revised_merged should be No
# RoB_assessment_alternative_revised_merged should be None


# MA_053: after reading the PDF, 
# test whether species abundance is positively correlated with environmental suitability
# We used correlation coefficients to measure the relationship between abundance 
# and environmental suitability derived from ecological niche models (ENM)
# correlation coefficient used varied across studies (e.g. Pearson's r, 
# Spearman's rs, Kendall's tau)
# When only the coefficient of determination was provided by the authors, we 
# calculated the correlation coefficient by taking the square root of the
# coefficient of determination (VanDerWal et al. 2009, Tôrres et al. 2012), 
# taking into account if the relationship was positive or negative. When 
# different correlation values were provided using training and test data, 
# we chose only the correlation provided for the training data (Nielsen et al. 
# 2005), because training data are used to develop the model and test data are 
# used to test the model predictions. If correlation values using both 
# presence/absence and presence-only data were provided (Pearce and Ferrier 
# 2001, Nielsen et al. 2005), we selected correlation values considering 
# presence and absence (abundance equal to zero).


# effect_size_sign_accounted_revised remained as/changed to NA (slightly unclear)
# inferential_statistics_revised remained as/changed to Yes (because R2)

# processed_data_shared should be No (assuming that variable best corresponds to r instead of Zr, unclear though)
# raw_data_shared should be Yes (assuming that variable best corresponds to r, unclear though)
# code_shared should be No
# RoB_assessment_revised_merged should be No
# RoB_assessment_alternative_revised_merged should be Experimental_design_Method_comparison
# based on Other methodological issues may affect the strength of AS 
# correlations. For example, there are three different approaches used to 
# model ecological niches: 1) statistical models, 2) similarity and 3) 
# machine learning methods; which they test: section Comparing ENM approaches


# MA_057: after reading the PDF, 
# Table S2 indicates all variables positive
# response ratios

# effect_size_sign_accounted_revised remained as/changed to NA
# inferential_statistics_revised remained as/changed to NA

# processed_data_shared should be No (The data that support the findings of this study are available from the corresponding author upon reasonable request.)
# raw_data_shared should be No (The data that support the findings of this study are available from the corresponding author upon reasonable request.)
# code_shared should be No
# RoB_assessment_revised_merged should be No
# RoB_assessment_alternative_revised_merged should be None


# MA_064: after reading the PDF, 
# effectiveness of nitrification inhibitors (NIs) to reduce N2O emissions
# Dicyadiamide (DCD) and 3,4-dimethylpyrazole phosphate (DMPP) were studied to 
# determine the variability in their efficiency


# effect_size_sign_accounted_revised remained as/changed to NA
# inferential_statistics_revised remained as/changed to NA

# processed_data_shared should be Yes (Appendix Table 1)
# raw_data_shared should be Yes (Appendix Table 1)
# code_shared should be No
# RoB_assessment_revised_merged should be No
# RoB_assessment_alternative_revised_merged should be None


# MA_065: after reading the PDF, 
# biodiversity–ecosystem functioning (BEF) experiments in wetland systems
# Experiments comparing the effect of plant monocultures and mixtures on water
# quality to improve pollutant removal efficiency in treatment wetlands
# we evaluated plant diversity effects on water purification through a 
# meta-analysis of freshwater experimental wetlands comparing monocultures to 
# mixtures
# removal efficiency (in percent) and standard deviation of at least one of the 
# following common measures of water quality were reported or could be calculated
# from the data presented in the paper: total phosphorus (TP), total nitrogen 
# (TN), total suspended solids (TSS) or chemical oxygen demand (COD) as a 
# measure of organic matter. The all mean that the higher they are, the lower
# the quality
# we calculated a ratio between the outcome of the mixture and the monoculture
# treatments, and used the natural logarithm of that ratio (hereafter 
# log(response ratio))

# effect_size_sign_accounted_revised remained as/changed to NA
# inferential_statistics_revised remained as/changed to NA

# processed_data_shared should be No (as far as I can see, but unclear)
# raw_data_shared should be Yes (a lot of data, difficult to navigate, no metadata)
# code_shared should be No
# RoB_assessment_revised_merged should be No
# RoB_assessment_alternative_revised_merged should be Experimental_design_Method_comparison
# based on "An analysis of moderator variables shows that the experimental context 
# (size of the experimental units, nutrient load, duration of the experiment)"


# MA_066: after reading the PDF, 
# effects on animal abundance, diversity, fitness, and ecosystem function
# We extracted means, statistical variations and sample sizes for invaded and 
# uninvaded plots for each response variable. These data were extracted directly 
# from the text and tables or measured from graphs using the image processing 
# software ImageJ

# effect_size_sign_accounted_revised remained as/changed to No
# inferential_statistics_revised remained as/changed to NA

# processed_data_shared should be No
# raw_data_shared should be No
# code_shared should be No
# RoB_assessment_revised_merged should be No
# RoB_assessment_alternative_revised_merged should be None


# MA_067: after reading the PDF, 
# published evidence for effects of canopy-forming shrubs on birds and mammals
# weighted multiple logistic regressions on the proportion 
# of positive responses for each response type recorded from each study for each 
# species"
# The study looks at variables such as: Nest success and Nest predation,
# Probability of colonization and Probability of extinction... 

# effect_size_sign_accounted_revised remained as/changed to No
# inferential_statistics_revised remained as/changed to NA

# processed_data_shared should be No (only part of it as Table S1 & S2) 
# raw_data_shared should be No
# code_shared should be No
# RoB_assessment_revised_merged should be No
# RoB_assessment_alternative_revised_merged should be None


# MA_068: after reading the PDF, 
# proportion of attracted dung beetles (Ni/Nmax)
# species richness
# rarefied diversity 
# analysis on the degree of trophic specialisation of dung beetles. We 
# summarised 45 studies, covering the resource preferences of a total of 
# 994503 individuals, to calculate the dung specificity in each study region. 
# Our results highlighted a significant (4.3-fold) increase in the diversity 
# of beetles attracted to vertebrate dung towards the equator.

# effect_size_sign_accounted_revised remained as/changed to NA
# inferential_statistics_revised remained as/changed to NA

# processed_data_shared should be Yes
# raw_data_shared should be Yes
# code_shared should be No
# RoB_assessment_revised_merged should be No
# RoB_assessment_alternative_revised_merged should be None


# MA_069: after reading the PDF, 
# effects of tillage practices on N losses in the sloping upland of the Three 
# Gorges Reservoir (TGR) area was studied by comparing N losses of three 
# conservation tillage practices (no-till, mulch-till and minimum till) in a 
# short-term field experiment with the conventional one. Moreover, a 
# meta-analysis of related data [...] showed that conservation tillage [...]
# significantly reduced N losses more than conventional tillage.
# response ratio (R) with the natural log

# effect_size_sign_accounted_revised remained as/changed to NA
# inferential_statistics_revised remained as/changed to NA

# processed_data_shared should be No
# raw_data_shared should be No
# code_shared should be No
# RoB_assessment_revised_merged should be No
# RoB_assessment_alternative_revised_merged should be None


# MA_070: after reading the PDF, 
# quantitatively meta-analysed the results of 110 site-specific studies 
# analysing infiltration (83) and evapotranspiration (28) responses to 
# grasslands alterations by grazing, crops, and afforestation
# infiltration and evapotranspiration seemingly both agree that higher is better
# log response ratio

# effect_size_sign_accounted_revised remained as/changed to NA
# inferential_statistics_revised remained as/changed to NA

# processed_data_shared should be Yes (Table S1 in Supplementary Word Doc)
# raw_data_shared should be No
# code_shared should be No
# RoB_assessment_revised_merged should be No
# RoB_assessment_alternative_revised_merged should be None

# updating new values
hackathon_clean_5 <- as.data.frame(
  hackathon_clean_4 %>%
    rows_update(hackathon.ES.IS.revision.PDF.done, by = "MA_ID_participant")
)

table(hackathon_clean_4$effect_size_sign_accounted_revised)
table(hackathon_clean_5$effect_size_sign_accounted_revised)

table(hackathon_clean_4$inferential_statistics_revised)
table(hackathon_clean_5$inferential_statistics_revised)

# table(hackathon_clean_4$RoB_assessment_revised_merged)
# table(hackathon_clean_5$RoB_assessment_revised_merged)
# 
# table(hackathon_clean_4$RoB_assessment_alternative_revised_merged)
# table(hackathon_clean_5$RoB_assessment_alternative_revised_merged)


# exploring NA's because I have been trying to use "NA" for the time being and
# want to keep that consistency just for now
table(is.na(hackathon_clean_5))

# Count how many real NA's per column
colSums(is.na(hackathon_clean_5))

# it affects: processed_data_links (12), raw_data_links (7), code_links (14)
sort(table(hackathon_clean_5$processed_data_links))
sort(table(hackathon_clean_5$raw_data_links))
sort(table(hackathon_clean_5$code_links))

# Get all rows that have at least one NA (real missing)
rows_with_na <- hackathon_clean_5 %>% 
  filter(if_any(everything(), is.na))


# Print them
print(rows_with_na)

# # Count "NA" strings per column
# sapply(hackathon_clean_5, function(x) sum(x == "NA", na.rm = TRUE))
# 
# # Get rows that have "NA" as a string
# rows_with_string_NA <- hackathon_clean_5 %>%
#   filter(if_any(everything(), ~ .x == "NA"))
# 
# print(rows_with_string_NA)

###############################################################################
# Adding additional data extraction performed while reviewing PDF
###############################################################################

# Purpose:
# While reviewing the PDFs, I manually collected extra information for 27
# studies. These details are not in the original coding dataset but are 
# important for verifying and complementing existing variables. That information
# is available in the corresponding Changelog's above. Importantly, these
# additional information only refers to variables on RoB and data & code sharing,
# since the ones pertaining effect size signs and inferential statistics have 
# now been fully checked and standardised.
#
# To avoid managing this in Excel build a small "lookup table" in R using
# tribble() with one row per study. This allows us to:
#   - Store the additional PDF-derived variables in a clean, transparent way
#   - Join them safely onto the existing dataset using MA_ID as the key
#   - Facilitate later comparisons/merging with the original variables
#
# The new variables added:
#   processed_data_shared_PDF
#   raw_data_shared_PDF
#   code_shared_PDF
#   RoB_assessment_revised_merged_PDF
#   RoB_assessment_alternative_revised_merged_PDF
#   data_and_code_sharing_comments_PDF
#   RoB_assessment_comments_revised_PDF
#
# Approach:
# - Define the additional information in pdf_info via tribble()
# - Merge with the main dataset (hackathon_clean_5) by MA_ID
#
# Notes:
# - Ensure MA_ID values here exactly match those in hackathon_clean_5
# - This approach makes the provenance of manually added values explicit
# -------------------------------------------------------------------------


# Create a table of manually collected information for specific MA_IDs
PDF_info <- tribble(
  ~MA_ID,   
  ~processed_data_shared_PDF, 
  ~raw_data_shared_PDF, 
  ~code_shared_PDF,
  ~data_and_code_sharing_comments_PDF,
  ~RoB_assessment_revised_merged_PDF, 
  ~RoB_assessment_alternative_revised_merged_PDF,
  ~RoB_assessment_comments_revised_PDF,
  
  # Fill in the values manually for each study you reviewed
  "MA_002", "No", "Yes", "No", "Processed data available in Figure 2", "No", "None", "The authors compared studies based on journal of publication but that is more of a PB test than a RoB",
  "MA_004", "Yes", "No", "No", "Processed data available in Table S2 2 as PDF, which we are considering as data shared", "No", "None", "The authors compare different mitigation methods, but not as a RoB assessement; rather, as their main question of interest",
  "MA_005", "No", "Yes", "No", "", "No", "None", "",
  "MA_007", "", "", "", "Processed data available in Figure 2", "", "", "",
  "MA_010", "No", "Yes", "No", "The authors write that all R code is provided in Supplementary Information 3, but there is not such file", "No", "None", "",
  "MA_012", "Yes", "Yes", "No", "", "No", "None", "",
  "MA_013", "No", "No", "No", "This case is deceiving. It looks like data would be given in Tables S1-S3 but it is not", "No", "None", "",
  "MA_018", "No", "Yes", "No", "But the raw data seems to be incomplete", "No", "None", "",
  "MA_024", "Yes", "No", "Yes", "Data and Codes for survival and clutch size analysis provided in the Supplementary Word Doc", "No", "None", "It could be considered as Experimental_design_Method_comparison since tracking method is included in one model, but no other information about why nor the results of this is provided",
  "MA_026", "No", "Yes", "Yes", "Random-effects meta-analysis was run, which is likely not appropriate", "No", "None", "",
  "MA_029", "Yes", "Yes", "Yes", "", "No", "Experimental_design_Method_comparison", "The reason for me to consider it as a RoB alternative is that they report the following: For remaining studies that adjusted for solicitation and did not report how many solicited bands had been excluded (Garrettson et al. 2014), we assigned a dummy variable to code for solicitation and accounted for this effect statistically. The authors do this particularly in the context of discussing how methodological differences between studies can affect the results. They also account for: most rigorous estimates of band reporting probabilities are from the most recent period",
  "MA_031", "No", "Yes", "No", "Processed data (at least some) available in Figures S2-S4. Whether all raw data is provides is slightly unclear", "No", "None", "The authors extracted type of culture but they did not seemingly used it for any test or even discussed it",
  "MA_036", "No", "Yes", "No", "", "No", "None", "",
  "MA_037", "Yes", "Yes", "Yes", "", "No", "None", "",
  "MA_043", "No", "Yes", "No", "Data provided in the supplements and the raster dataset provided in a repository", "No", "None", "Removing studies is not a RoB assessment per se. In addition, the author did some test on how different size-selections (1-20km) matter, but this does not reflect differences between studies",
  "MA_053", "No", "Yes", "No", "The dataset provides one effect size variable, which I am assuming it is r rather than Zr", "No", "Experimental_design_Method_comparison", "Decision based on the authors reporting that other methodological issues may affect the strength of AS correlations. For example, there are three different approaches used to model ecological niches: 1) statistical models, 2) similarity and 3) machine learning methods; which they test: section Comparing ENM approaches",
  "MA_056", "No", "No", "No", "Processed data (at least some) available in foresplots", "", "", "",
  "MA_057", "No", "No", "No", "data that support the findings of this study are available from the corresponding author upon reasonable request", "No", "None", "",
  "MA_060", "", "", "", "", "No", "Experimental_design_Method_comparison", "RoB alternative based on the authors testing for differences between field and pot (i.e., experimental type)",
  "MA_061", "No", "Yes", "No", "Processed data is No becuase as far as I can see, the mass-scaling values are not provided", "No", "", "",
  "MA_064", "Yes", "Yes", "No", "Data in Appendix Table 1", "No", "None", "",
  "MA_065", "No", "Yes", "No", "There is a lot of data and no clear information on how to navigate it (no metadata), but it looks like processed data is not there", "No", "Experimental_design_Method_comparison", "RoB alternative based on the authors reporting: An analysis of moderator variables shows that the experimental context (size of the experimental units, nutrient load, duration of the experiment)...",
  "MA_066", "No", "No", "No", "", "No", "None", "",
  "MA_067", "No", "No", "No", "Some processed data available in Tables S1 & S2, but not all", "No", "None", "",
  "MA_068", "Yes", "Yes", "No", "", "No", "None", "",
  "MA_069", "No", "No", "No", "", "No", "None", "",
  "MA_070", "Yes", "No", "No", "Processed data available in Table S1 in the Supplementary Word Doc", "No", "None", "",
  # 11.09.2025: adding an additional study here that Shreya asked me to do becuase she had no access to the supplementary material
  "MA_017", "Yes", "Yes", "No", "Processed and raw data available in supplementary tables S1-S10 as PDF, which we are considering as data shared", "No", "None", "The authors compare different mitigation methods, but not as a RoB assessement; rather, as their main question of interest. In addition, they had coded experimental type but do not seemingly used it despite that this could be considered a type of RoB",
  "MA_073", "", "", "", "Processed data available in Figures 1-3", "", "", "",
)

PDF_info
table(is.na(PDF_info))
table(PDF_info$processed_data_shared_PDF)
table(PDF_info$raw_data_shared_PDF)
table(PDF_info$code_shared_PDF)
table(PDF_info$RoB_assessment_revised_merged_PDF)
table(PDF_info$RoB_assessment_alternative_revised_merged_PDF)

# number of studies for which ROB was re-checked in this list
table(PDF_info$RoB_assessment_alternative_revised_merged_PDF!="")[2]

# keep in mind that for MA_007 there is no data, just an extra comment that 
# data could be in Figure 2
as.data.frame(PDF_info[,c("MA_ID","processed_data_shared_PDF",
                          "raw_data_shared_PDF","code_shared_PDF",
                          "RoB_assessment_revised_merged_PDF",
                          "RoB_assessment_alternative_revised_merged_PDF")])


# Merge the PDF-derived info into the cleaned dataset
# left_join() ensures we only add columns where MA_ID matches
# unmatched IDs will get NA values in the new variables
hackathon_clean_6 <- as.data.frame(
  hackathon_clean_5 %>%
    # Join extra PDF information by study ID
    left_join(PDF_info, by = "MA_ID") %>%
    # converting all "NA" string to real NA's from now on
    mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
    
    # Place new "PDF" columns next to their existing counterparts
    relocate(processed_data_shared_PDF,
             .after = processed_data_shared) %>%
    relocate(raw_data_shared_PDF,
             .after = raw_data_shared) %>%
    relocate(code_shared_PDF,
             .after = code_shared) %>%
    relocate(data_and_code_sharing_comments_PDF,
             .after = data_and_code_sharing_comments) %>%
    relocate(RoB_assessment_revised_merged_PDF,
             .after = RoB_assessment_revised_merged) %>%
    relocate(RoB_assessment_alternative_revised_merged_PDF,
             .after = RoB_assessment_alternative_revised_merged) %>%
    relocate(RoB_assessment_comments_revised_PDF,
             .after = RoB_assessment_comments_revised)
)

# table(hackathon_clean_6$processed_data_shared)
# table(hackathon_clean_6$processed_data_shared_PDF)
# table(hackathon_clean_6$raw_data_shared)
# table(hackathon_clean_6$raw_data_shared_PDF)
# table(hackathon_clean_6$RoB_assessment_revised_merged)
# table(hackathon_clean_6$RoB_assessment_revised_merged_PDF)
# table(hackathon_clean_6$RoB_assessment_alternative_revised_merged)
# table(hackathon_clean_6$RoB_assessment_alternative_revised_merged_PDF)

names(hackathon_clean_6)
table(is.na(hackathon_clean_6))
table(hackathon_clean_6=="NA")


###############################################################################
# adding the data for the meta-analysis that did not get done during the 
# hackathon
###############################################################################

# Create a "template" empty row with the same columns
new_row <- hackathon_clean_6[0, ] %>% 
  slice_head(n = 1) %>% 
  mutate(across(everything(), ~NA))  # all columns NA

# Fill only the ones you care about
new_row <-  tibble(
  MA_ID_participant = "MA_047_Alfredo Sanchez-Tojar",
  MA_ID = "MA_047",
  title = "modeling nitrous oxide emission from rivers: a global assessment",
  participant_name = "Alfredo Sanchez-Tojar",
  participant_email = "alfredo.tojar@gmail.com",
  coauthorship = "Yes",
  effect_size_sign_accounted_revised = NA,
  effect_size_sign_quote = "The compiled database included 169 observations for N2O fluxes, concentrations of nitrate (NO3, 0.001–21.2 mg N L−1), ammonium (NH4, 0.001–12.5 mg N L−1), dissolved inorganic N (DIN, 0.002–21.2 mg N L−1), N2O (0.01–15.5 μg N L−1), dissolved oxygen (DO, 1.5–12.2 mg L−1), and dissolved organic carbon (DOC, 0.26–31.5 mg L−1), N2O saturation (42–2500%), river discharge (0.001–31710 m3 s−1) and water temperature (9.7–28.3 °C).",
  inferential_statistics_revised = NA,
  inferential_statistics_quote = "Due to the limited number of riverine N2O emission factor values directly provided in the literature (<10), three methods were adopted to estimate emission factors in this study (Table 1)",
  effect_size_sign_and_inferential_statistics_comments_revised = "All variables are positive, and the authors used values and transformations of them directly from the source articles",
  processed_data_shared_PDF = "No",
  raw_data_shared_PDF = "No",
  code_shared_PDF = "No",
  data_and_code_sharing_comments_PDF = "Authors provide a summary of the sources used, but not data (i.e., no EF, no N2O flux), and a small piece of code in the Supplementary Word Doc",
  RoB_assessment_revised_merged_PDF = "No",
  RoB_assessment_alternative_revised_merged_PDF = "None",
  RoB_assessment_comments_revised_PDF = "The authors compiled information about differences in methods: The methods for measuring N2O fluxes were floating chambers (14%), water–air gas exchange models (80%) and combined chambers and gas exchange models (9%).",
  form_version = NA,
  processed_data_chosen_link = NA,
  raw_data_chosen_link = NA,
  code_chosen_link = NA
) %>%
  mutate(across(everything(), as.character))  # force all columns to character


# No PRISMA or other guidelines
# No weighted meta-analysis, not a meta-analysis actually

# Add missing columns as NA so it matches hackathon_clean_6
new_row <- add_column(new_row, !!!setNames(rep(list(NA), 
                                               length(setdiff(names(hackathon_clean_6),
                                                              names(new_row)))),
                                           setdiff(names(hackathon_clean_6), 
                                                   names(new_row))
))

# Bind
hackathon_clean_6 <- bind_rows(hackathon_clean_6, new_row)
table(is.na(hackathon_clean_6))
table(hackathon_clean_6=="NA")



################################################################################
# Then merged the RoB variables, which we are considering the final ones
################################################################################

# For processed_data_shared, raw_data_shared, and code_shared, we will wait 
# until we obtain the manual data extraction, to revise the data accordingly

hackathon_clean_7 <- hackathon_clean_6 %>%
  mutate(
    RoB_assessment_revised_merged_final = 
      ifelse(!is.na(RoB_assessment_revised_merged_PDF) & 
               RoB_assessment_revised_merged_PDF != "",
             RoB_assessment_revised_merged_PDF, 
             RoB_assessment_revised_merged),
    RoB_assessment_alternative_revised_merged_final = 
      ifelse(!is.na(RoB_assessment_alternative_revised_merged_PDF) & 
               RoB_assessment_alternative_revised_merged_PDF != "",
             RoB_assessment_alternative_revised_merged_PDF, 
             RoB_assessment_alternative_revised_merged)
  ) %>%
  relocate(RoB_assessment_revised_merged_final,
           .after = RoB_assessment_revised_merged_PDF) %>%
  relocate(RoB_assessment_alternative_revised_merged_final,
           .after = RoB_assessment_alternative_revised_merged_PDF)


# table(hackathon_clean_7$RoB_assessment_revised_merged)
# table(hackathon_clean_7$RoB_assessment_revised_merged_final)
# table(hackathon_clean_7$RoB_assessment_alternative_revised_merged)
# table(hackathon_clean_7$RoB_assessment_alternative_revised_merged_final)
# 
# 
# hackathon_clean_7[,c("MA_ID","code_shared")]
# hackathon_clean_7[,c("MA_ID","code_shared","code_shared_PDF")]
# 



################################################################################
# exporting dataset for script 002
################################################################################

# only the variables of interest will be exported from here to simplify the
# dataset

# starting with the easy simple ones that are likely simple typos or missing
# info
hackathon_clean_7_subset <- hackathon_clean_7 %>% 
  select(MA_ID,
         title,
         participant_name,
         participant_email,
         Timestamp,
         form_version,
         number_of_assessments,
         effect_size_sign_accounted_revised,
         inferential_statistics_revised,
         processed_data_shared,
         processed_data_shared_PDF,
         raw_data_shared,
         raw_data_shared_PDF,
         code_shared,
         code_shared_PDF,
         data_and_code_sharing_comments,
         data_and_code_sharing_comments_PDF,
         RoB_assessment_revised_merged_final,
         RoB_assessment_alternative_revised_merged_final)

summary(hackathon_clean_7_subset)
names(hackathon_clean_7_subset)
str(hackathon_clean_7_subset)

# exporting the dataset
write.csv(as.data.frame(hackathon_clean_7_subset),
          "data/processed_data/hackathons/2016-2020/01_hackathon_data_cleaned.csv",
          row.names=FALSE)
