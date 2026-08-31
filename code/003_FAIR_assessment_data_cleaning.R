################################################################################
# Authors: 
#
# Alfredo Sanchez-Tojar (alfredo.tojar@gmail.com): original code

# Script first created in December 2025

################################################################################
# Description of script and Instructions
################################################################################

# This script is to clean and prepare the data collected from several authors
# using a Google Form aimed at assessing the FAIRness of the 55 datasets 
# detected during our study on transparency, reliability and reproducibility of 
# meta-analyses published in ecology and evolution

################################################################################
# Packages needed
################################################################################

# install.packages("pacman")
# load packages
pacman::p_load(tidyverse,
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

# importing data from Google Forms:
FAIR.response.raw <- read.csv("data/01_raw_data/FAIR_assessment/FAIR_data_assessment_Form_Responses.csv", header=T)

# we can start renaming these shared variables' names to our preferred choices
FAIR.response.raw <- as.data.frame(
  rename(FAIR.response.raw, 
         participant_name = "Your.name..e.g...Shreya.Dimri...Please.make.sure.to.use.the.exact.same.name..copy.paste.recommended..for.each.form.submission.",
         participant_email = "Email..e.g...may.be.used.for.follow.up..if.needed...Please.make.sure.to.use.the.exact.same.email..copy.paste.recommended..for.each.form.submission...",
         MA_ID = "Meta.analysis.ID..e.g...MA_082...which.you.can.find.in.the.list.of.meta.analyses.you.will.receive.by.email.",
         title = "Meta.analysis.title..please.copy.and.paste.the.title.exactly.as.it.appears.in.the.email.you.received..",
         data_GUPID_presence = "Please.choose.the.globally.unique.and.persistent.identifier.associated.with.the.data..NOT.the.published.article.itself..",
         data_GUPID = "Please.copy.paste.the.persistent.identifier.here..whenever.possible.prioritise.the.identifier.rather.than.the.link..i.e...10.1002.eap.2074.instead.of.https...doi.org.10.1002.eap.2074..",
         data_sharing_location = "Where.is.the.data.shared..Data.is.registered.indexed.shared.in.a.searchable.resource..Repositories.like.Dryad..Zenodo..Figshare..OSF..etc..can.be.searched.using.the.repositories..search.engine.search.query..However..Supplementary.Materials.of.an.article..Private.Links..e.g...personal.websites...etc..are.not.indexed.on.a.searchable.database.or.repository.",
         data_metadata_presence = "Does.metadata.accompany.the.dataset.s...usually.added.to.a.recognised.repository..Assessing.the.completeness.of.the.metadata.is.beyond.our.purposes..and.thus..here..we.only.want.to.know.if.there.is.metadata.available..In.repositories.like.Dryad.and.Zenodo..basic.metadata.is.required.upon.submission..usually.including.title..abstract..and.authors..names..how.and.when.the.data.was.collected.and.a.brief.description.of.the.dataset...thus..for.data.hosted.in.those.repositories..metadata.accompanies.data.by.default.",
         data_metadata_type = "How.is.the.metadata.shared.",
         data_metadata_comments = "Please..leave.any.comments.you.think.would.be.helpful.for.us.to.understand.your.decisions.about.meta.data..Optional..",
         data_downloadable = "Can.the.data.be.downloaded...For.example..if.a.DOI.is.provided..e.g...10.5281.zenodo.14930059...you.should.be.able.to.access.and.download.the.data.by.visiting.the.corresponding.link..https...doi.org.10.5281.zenodo.14930059...Please..confirm.that.the.data.you.are.assessing.can.be.accessed.by.downloading.the.data.to.your.computer..",
         data_downloadable_comments = "Please..leave.any.comments.you.think.would.be.helpful.for.us.to.understand.your.decision.about.whether.data.can.be.downloaded..Optional..",
         data_type = "Please.choose.the.format.in.which.the.data..raw.processed..is.shared..please.select.multiple.options.if.they.apply.",
         data_type_comments = "Please..leave.any.comments.you.think.would.be.helpful.for.us.to.understand.your.decision.about.the.format.of.the.data..Optional..",
         data_licence_presence = "Please.choose.which.license.is.used.for.the.shared.dataset.s...and.or.the.licence.associated.to.the.repository.that.contains.the.data..e.g...Dryad.repositories..always..have.a.CC0.licence.per.default...Absence.of.a.license.does.not.allow.users.to.legally.use.the.data...Available.upon.request....Contact.the.authors..or..Rights.reserved.by.publishers..would.also.imply.that.the.data.is.not.accessible.and.lacks.an.open.use.license.",
         data_dictionary_presence = "Is.a.data.dictionary.or.code.book.provided..A.data.dictionary.or.a.codebook.explains.what.each.variable.means.in.more.or.less.detail..including.a.clear.description.of.the.variables..their.units..and.their.coding..e.g...sex..male..female...The.description.of.variables.can.also.be.provided.as.part.of.the.README.file..However..note.that.if.the.README.contains.only.the.title.author.details..details.about.how.the.data.was.collected..this.is.not.considered.as.a..Yes..",
         data_dictionary_comments = "If.a.data.dictionary.or.code.book.is.provided..please.briefly.describe.the.contents.of.it..If.not..use.NA")
)


################################################################################
# Cleaning dataset
################################################################################

summary(FAIR.response.raw)
names(FAIR.response.raw)

# first removing extra spaces in front and at the back of all characters
FAIR.response.raw <- as.data.frame(
  FAIR.response.raw %>% mutate_if(is.character, str_trim)
)


################################################################################
# variable by variable

################################################################################
# participant_name
table(FAIR.response.raw$participant_name)

# number of unique participants
length(unique(FAIR.response.raw$participant_name))


################################################################################
# participant_email
table(FAIR.response.raw$participant_email)

# fixing observable typos
FAIR.response.raw$participant_email <- recode(FAIR.response.raw$participant_email,
                                              "a.culina@nioo.knaw.nl" = "antica.culina@nioo.knaw.nl",
                                              "afoxx@u.northwestern.edu" = "afoxx@chicagobotanic.org",
                                              "ryan@ryanfieldme" = "ryan@ryanfield.me")

table(FAIR.response.raw$participant_email)

# number of unique names = number of unique email addresses?
length(unique(FAIR.response.raw$participant_name))==length(unique(FAIR.response.raw$participant_email))


################################################################################
# MA_ID
sort(table(FAIR.response.raw$MA_ID))

# fixing observable typos
FAIR.response.raw$MA_ID <- recode(FAIR.response.raw$MA_ID,
                                  "MA_04" = "MA_041") # error communicated by Rayan

sort(table(FAIR.response.raw$MA_ID))
length(unique(FAIR.response.raw$MA_ID))


################################################################################
# title
sort(table(FAIR.response.raw$title))

# everything to small letters
FAIR.response.raw$title <- tolower(FAIR.response.raw$title)
sort(table(FAIR.response.raw$title))


################################################################################
# data_GUPID_presence
sort(table(FAIR.response.raw$data_GUPID_presence))

# There is one entry that contains an accession number but there remaining of 
# the data was not provided, therefore, we are categorizing this entry as 
# "No unique and persistent identifier associated with the data"

FAIR.response.raw <- FAIR.response.raw %>%
  mutate(
    data_GUPID_presence = case_when(
      data_GUPID_presence == "Digital Object Identifier (DOI)" ~
        "Digital Object Identifier (DOI)",
      
      data_GUPID_presence == "I really cannot decide/I don't know" ~
        "I really cannot decide/I don't know",
      
      TRUE ~ "No unique and persistent identifier associated with the data"
    )
  )

sort(table(FAIR.response.raw$data_GUPID_presence))

#nrow(FAIR.response.raw)-sum(table(FAIR.response.raw$data_GUPID_presence))
table(is.na(FAIR.response.raw$data_GUPID_presence))

################################################################################
# data_GUPID

sort(table(FAIR.response.raw$data_GUPID))

# There is one in which the article cites a DOI https://doi.org/10.6084/m9.figshare.c.4462778.v1
# but the dataset is not there. The dataset is somewhere in figshare:
# https://figshare.com/s/3eebaf42e161c0e7e1ef?file=13199204, but it says that
# this item is shared privately. Therefore, we are treating this case as the
# data not being assigned with a DOI

table(is.na(FAIR.response.raw$data_GUPID))


FAIR.response.raw <- FAIR.response.raw %>%
  mutate(
    data_GUPID = case_when(
      str_detect(data_GUPID, "Only as supp material")                 ~ NA,
      str_detect(data_GUPID, "Thermoprofundales")                     ~ NA,
      str_detect(data_GUPID, "The data is in figshare:")              ~ NA,
      str_detect(data_GUPID, "Only available as supp material:")      ~ NA,
      str_detect(data_GUPID, "Collection DOI: ")                      ~ "10.6084/m9.figshare.c.5230301.v1;10.6084/m9.figshare.13336604;10.6084/m9.figshare.13336613",
      data_GUPID %in% c("N/A","None","none")                         ~ NA,
      data_GUPID %in% c("no data shared")                             ~ "Data is not shared",
      TRUE ~ data_GUPID
    ),
    data_GUPID = data_GUPID %>%
      str_replace_all("^https://doi.org/", "") %>%
      #str_replace_all("^doi.org/", "") %>%
      str_replace_all("\n", ";")
  )

sort(table(FAIR.response.raw$data_GUPID))


################################################################################
# corrections brought up by the observer after submitting the form
FAIR.response.raw <- FAIR.response.raw %>%
  mutate(
    data_GUPID = case_when(
      participant_email == "m.l.jones@exeter.ac.uk" & MA_ID == "MA_018" ~ "10.5061/dryad.547d7wm4r",
      participant_email == "m.l.jones@exeter.ac.uk" & MA_ID == "MA_061" ~ "10.15131/shef.data.11590902",
      participant_email == "m.l.jones@exeter.ac.uk" & MA_ID == "MA_071" ~ "10.5061/dryad.8kt4675",
      participant_email == "m.l.jones@exeter.ac.uk" & MA_ID == "MA_077" ~ "10.5061/dryad.jt48n1c",
      participant_email == "m.l.jones@exeter.ac.uk" & MA_ID %in% c("MA_029", "MA_022") ~ NA,
      TRUE ~ data_GUPID
    ),
    data_GUPID_presence = case_when(
      participant_email == "m.l.jones@exeter.ac.uk" & MA_ID %in% c("MA_029", "MA_022") ~
        "No unique and persistent identifier associated with the data",
      TRUE ~ data_GUPID_presence
    )
  )

sort(table(FAIR.response.raw$data_GUPID))
table(is.na(FAIR.response.raw$data_GUPID))


################################################################################
# data_sharing_location
sort(table(FAIR.response.raw$data_sharing_location))

FAIR.response.raw <- FAIR.response.raw %>%
  mutate(
    data_sharing_location = case_when(
      str_detect(data_sharing_location, "They also provide accession numbers for") ~ "NCBI GenBank, Supplementary materials",
      data_sharing_location %in% c("no data shared")    ~ "Data is not shared",
      TRUE ~ data_sharing_location
    ),
    data_sharing_location = data_sharing_location %>%
      str_replace_all(", ", ";")
  )

sort(table(FAIR.response.raw$data_sharing_location))
table(is.na(FAIR.response.raw$data_sharing_location))

################################################################################
# data_metadata_presence
sort(table(FAIR.response.raw$data_metadata_presence))

FAIR.response.raw <- FAIR.response.raw %>%
  mutate(
    data_metadata_presence = case_when(
      data_metadata_presence %in% c("no data shared")    ~ "Data is not shared",
      TRUE ~ data_metadata_presence
    )
  )

sort(table(FAIR.response.raw$data_metadata_presence))
table(is.na(FAIR.response.raw$data_metadata_presence))

################################################################################
# data_metadata_type

sort(table(FAIR.response.raw$data_metadata_type))

# fixing observable typos
FAIR.response.raw <- FAIR.response.raw %>%
  mutate(
    data_metadata_type = case_when(
      data_metadata_type %in% c("As 2 .xlsx file's tab") ~ ".xls(x) tab",
      str_detect(data_metadata_type, "in 'notes' sheet") ~ "As OSF Wiki;.xls(x) tab",
      str_detect(data_metadata_type, "as a tab of a .xlsx file") ~ "As part of the data repository (Zenodo/Dryad);.xls(x) tab",
      str_detect(data_metadata_type, "Meta-data is not shared, Codebook") ~ "No",
      data_metadata_type %in% c("Metadata provided within the .xlxs") ~ ".xls(x) tab",
      data_metadata_type %in% c("no data shared") ~ "Data is not shared",
      TRUE ~ data_metadata_type
    )
  )
sort(table(FAIR.response.raw$data_metadata_type))
table(is.na(FAIR.response.raw$data_metadata_type))

################################################################################
# data_downloadable
sort(table(FAIR.response.raw$data_downloadable))
table(is.na(FAIR.response.raw$data_downloadable))

################################################################################
# data_type
sort(table(FAIR.response.raw$data_type))

# fixing observable typos
FAIR.response.raw <- FAIR.response.raw %>%
  mutate(
    data_type = case_when(
      data_type %in% c("no data shared","not shared") ~ "Data is not shared",
      data_type %in% c("Table within article","as a table in the webpage") ~ ".html",
      TRUE ~ data_type
    ),
    data_type = data_type %>%
      str_replace_all(", ", ";")
  )
sort(table(FAIR.response.raw$data_type))
table(is.na(FAIR.response.raw$data_type))

################################################################################
# data_licence_presence
sort(table(FAIR.response.raw$data_licence_presence))

# fixing observable typos
FAIR.response.raw <- FAIR.response.raw %>%
  mutate(
    data_licence_presence = case_when(
      data_licence_presence %in% c("no data shared","data not shared",
                                   "Data not deposited") ~ "Data is not shared",
      data_licence_presence %in% c("No license associated with the supp matt") ~ "Publisher copyright/Rights reserved",
      str_detect(data_licence_presence, "does the journal publisher have the copyright?") ~ "Publisher copyright/Rights reserved",
      str_detect(data_licence_presence, "DochtermannLab/BehavioralHeritability") ~ "CC0 (Public Domain Dedication);GNU",
      TRUE ~ data_licence_presence
    )
  )

sort(table(FAIR.response.raw$data_licence_presence))
table(is.na(FAIR.response.raw$data_licence_presence))


################################################################################
# data_dictionary_presence
sort(table(FAIR.response.raw$data_dictionary_presence))

# fixing observable typos
FAIR.response.raw <- FAIR.response.raw %>%
  mutate(
    data_dictionary_presence = case_when(
      data_dictionary_presence %in% c("no data shared") ~ "Data is not shared",
      data_dictionary_presence %in% c("desciption of variables in the excel file") ~ "Yes",
      TRUE ~ data_dictionary_presence
    )
  )

sort(table(FAIR.response.raw$data_dictionary_presence))
table(is.na(FAIR.response.raw$data_dictionary_presence))


################################################################################
# corrections brought up by the observers after submitting their forms

FAIR.response.raw <- FAIR.response.raw %>%
  mutate(
    data_sharing_location = if_else(
      MA_ID == "MA_024",
      "Supplementary materials",
      data_sharing_location
    ),
    data_licence_presence = if_else(
      MA_ID == "MA_024",
      "Publisher copyright/Rights reserved",
      data_licence_presence
    )
  )

# table(is.na(FAIR.response.raw$data_sharing_location))
# table(is.na(FAIR.response.raw$data_licence_presence))

# View(FAIR.response.raw)

# # exporting the dataset
# write.csv(as.data.frame(FAIR.response.raw),
#           "data/02_processed_data/FAIR_assessment/FAIR_data_assessment_Form_Responses_Cleaning_Round.csv",
#           row.names=FALSE)

###
# Revising agreements and disagreements between observers
###
# MA_018: Antica Culina and Matt Lloyd Jones: full agreement
# MA_020: Heikel Balti and Losia Lagisz: full agreement, other than Heikel mentioned
#         .xls(x) tab variable description of Metadata, which I am deleting
# MA_027: Pietro Pollo and Gabe Winter: disagreement in the existence of data dic
#         where Gabe found that there is indeed a data dictionary in the supplem.
#         .docx
# MA_029: Nerea Piñero-Juncal and Matt Lloyd Jones: full disagreement since Nerea
#         interpreted a table as being all the data, whereas if at all, it is a
#         very small proportion of the data used in the analyses. However, both
#         missed that the data and code are somewhere else: "All data and computer 
#         code used to fit these models have been archived with the University 
#         of Minnesota's Digital Conservancy (Arnold 2019)." 
#         For the following variables, need to make those changes after discovering
#         there was indeed data:
#         data_GUPID_presence	"Handle (link has hdl or handle.net)"
#         data_GUPID	"https://hdl.handle.net/11299/208786"
#         data_sharing_location	"Institutional repository"
#         data_metadata_presence "Yes"
#         data_metadata_type "As part of the data repository (Zenodo/Dryad)"
#         data_metadata_comments "Metadata present in the institutuional repository"	
#         data_downloadable	"Yes"
#         data_type	".csv/tsv"
#         data_licence_presence	"CC BY-NC (Attribution, NonCommercial)"
#         data_dictionary_presence "No"
# MA_032: Antica Culina and Heikel Balti: disagreement because Antica interpreted
#         there was no data shared, which is fair given that the article does not
#         cite the link where the data was shared. However, the authors said that
#         "Data will be available in Dryad once the paper is published." and 
#         indeed, data is shared on dryad.
#         Changes needed to Antica's entry
#         data_GUPID_presence	"Digital Object Identifier (DOI)"
#         data_GUPID	"10.5061/dryad.pk8hp"
#         data_sharing_location	"Dryad"
#         data_metadata_presence "Yes"
#         data_metadata_type "As part of the data repository (Zenodo/Dryad)"
#         data_downloadable	"Yes"
#         data_type	".txt"
#         data_licence_presence	"CC0 (Public Domain Dedication)"
#         data_dictionary_presence "No"
# MA_038: Ryan Field and Losia Lagisz: full agreement
# MA_039: Nerea Piñero-Juncal and Nicholas Moran: They agree on content mostly
#         but differ on interpretation. First, Nerea added a data_GUPID, which
#         should be NA since the data is part of the supplementary material. 
#         Most importantly, there is both metadata and data dictionary. Nerea
#         seemingly also found the data dictionary based on the comments, but 
#         categorised it as No. Data licencing not understood by Nicholas. 
# MA_044: Alicia Foxx and Edward Ivimey-Cook: generally good agreement, but needs
#         some cleaning. For example, need to use No for data_metadata, rather
#         than "not in a depository" as used by Alicia. Data licencing not 
#         understood by Ed. Importantly, both missed out that a data dictionary,
#         strictly speaking, is provided at the bottom of each .docx table
# MA_058: Ryan Field and Gabe Winter: full agreement
# MA_064: Pietro Pollo and Edward Ivimey-Cook: They generally agree. However, 
#         data licencing not understood by Ed. In addition, Pietro mentioned that 
#         data dictionary might be provided with the basic information provided 
#         in the .docx table but was unsure. I agree we can consider this as
#         enough data dictionary.
# MA_078: Alicia Foxx and Nicholas Moran: They generally agree but data licencing 
#         not understood by Nicholas. 

FAIR.response.raw <- FAIR.response.raw %>%
  mutate(
    data_metadata_type = case_when(
      participant_email == "balti.heikel@gmail.com" & MA_ID == "MA_020" ~ "As part of the data repository (Zenodo/Dryad)",
      TRUE ~ data_metadata_type
    ),
    data_dictionary_presence = case_when(
      participant_email == "pietro_pollo@hotmail.com" & MA_ID == "MA_027" ~ "Yes",
      TRUE ~ data_dictionary_presence
    ),
    data_GUPID_presence = case_when(
      MA_ID == "MA_029" ~ "Handle (link has hdl or handle.net)",
      TRUE ~ data_GUPID_presence
    ),
    data_GUPID = case_when(
      MA_ID == "MA_029" ~ "hdl.handle.net/11299/208786",
      TRUE ~ data_GUPID
    ),
    data_sharing_location = case_when(
      MA_ID == "MA_029" ~ "Institutional repository",
      TRUE ~ data_sharing_location
    ),
    data_metadata_presence = case_when(
      MA_ID == "MA_029" ~ "Yes",
      TRUE ~ data_metadata_presence
    ),
    data_metadata_type = case_when(
      MA_ID == "MA_029" ~ "As part of the data repository (Zenodo/Dryad)",
      TRUE ~ data_metadata_type
    ),
    data_metadata_comments = case_when(
      MA_ID == "MA_029" ~ "Metadata present in the institutional repository",
      TRUE ~ data_metadata_comments
    ),
    data_downloadable = case_when(
      MA_ID == "MA_029" ~ "Yes",
      TRUE ~ data_downloadable
    ),
    data_type = case_when(
      MA_ID == "MA_029" ~ ".csv/tsv",
      TRUE ~ data_type
    ),
    data_licence_presence = case_when(
      MA_ID == "MA_029" ~ "CC BY-NC (Attribution, NonCommercial)",
      TRUE ~ data_licence_presence
    ),
    data_dictionary_presence = case_when(
      MA_ID == "MA_029" ~ "No",
      TRUE ~ data_dictionary_presence
    ),
    data_GUPID_presence = case_when(
      participant_email == "antica.culina@nioo.knaw.nl" & MA_ID == "MA_032" ~ "Digital Object Identifier (DOI)",
      TRUE ~ data_GUPID_presence
    ),
    data_GUPID = case_when(
      participant_email == "antica.culina@nioo.knaw.nl" & MA_ID == "MA_032" ~ "10.5061/dryad.pk8hp",
      TRUE ~ data_GUPID
    ),
    data_sharing_location = case_when(
      participant_email == "antica.culina@nioo.knaw.nl" & MA_ID == "MA_032" ~ "Dryad",
      TRUE ~ data_sharing_location
    ),
    data_metadata_presence = case_when(
      participant_email == "antica.culina@nioo.knaw.nl" & MA_ID == "MA_032" ~ "Yes",
      TRUE ~ data_metadata_presence
    ),
    data_metadata_type = case_when(
      participant_email == "antica.culina@nioo.knaw.nl" & MA_ID == "MA_032" ~ "As part of the data repository (Zenodo/Dryad)",
      TRUE ~ data_metadata_type
    ),
    data_downloadable = case_when(
      participant_email == "antica.culina@nioo.knaw.nl" & MA_ID == "MA_032" ~ "Yes",
      TRUE ~ data_downloadable
    ),
    data_type = case_when(
      participant_email == "antica.culina@nioo.knaw.nl" & MA_ID == "MA_032" ~ ".txt",
      TRUE ~ data_type
    ),
    data_licence_presence = case_when(
      participant_email == "antica.culina@nioo.knaw.nl" & MA_ID == "MA_032" ~ "CC0 (Public Domain Dedication)",
      TRUE ~ data_licence_presence
    ),
    data_dictionary_presence = case_when(
      participant_email == "antica.culina@nioo.knaw.nl" & MA_ID == "MA_032" ~ "No",
      TRUE ~ data_dictionary_presence
    ),
    data_GUPID = case_when(
      participant_email == "np.juncal@gmail.com" & MA_ID == "MA_039" ~ NA,
      TRUE ~ data_GUPID
    ),
    data_metadata_type = case_when(
      participant_email == "np.juncal@gmail.com" & MA_ID == "MA_039" ~ "As .xlsx tab",
      TRUE ~ data_metadata_type
    ),
    data_metadata_type = case_when(
      participant_email == "nicholaspatrickmoran@gmail.com" & MA_ID == "MA_039" ~ "As .xlsx tab",
      TRUE ~ data_metadata_type
    ),
    data_type = case_when(
      participant_email == "np.juncal@gmail.com" & MA_ID == "MA_039" ~ ".xlsx",
      TRUE ~ data_type
    ),
    data_licence_presence = case_when(
      participant_email == "nicholaspatrickmoran@gmail.com" & MA_ID == "MA_039" ~ "Publisher copyright/Rights reserved",
      TRUE ~ data_licence_presence
    ),
    data_dictionary_presence = case_when(
      participant_email == "np.juncal@gmail.com" & MA_ID == "MA_039" ~ "Yes",
      TRUE ~ data_dictionary_presence
    ),
    data_metadata_presence = case_when(
      participant_email == "afoxx@chicagobotanic.org" & MA_ID == "MA_044" ~ "No",
      TRUE ~ data_metadata_presence
    ),
    data_licence_presence = case_when(
      participant_email == "e.ivimeycook@gmail.com" & MA_ID == "MA_044" ~ "Publisher copyright/Rights reserved",
      TRUE ~ data_licence_presence
    ),
    data_dictionary_presence = case_when(
      MA_ID == "MA_044" ~ "Yes",
      TRUE ~ data_dictionary_presence
    ),
    data_dictionary_comments = case_when(
      MA_ID == "MA_044" ~ "Variable description and units added at the bottom of each of the provided .docx tables",
      TRUE ~ data_dictionary_comments
    ),
    data_licence_presence = case_when(
      participant_email == "e.ivimeycook@gmail.com" & MA_ID == "MA_064" ~ "Publisher copyright/Rights reserved",
      TRUE ~ data_licence_presence
    ),
    data_dictionary_presence = case_when(
      MA_ID == "MA_064" ~ "Yes",
      TRUE ~ data_dictionary_presence
    ),
    data_licence_presence = case_when(
      participant_email == "nicholaspatrickmoran@gmail.com" & MA_ID == "MA_078" ~ "Publisher copyright/Rights reserved",
      TRUE ~ data_licence_presence
    )
  )

################################################################################
# Exploring the dataset again variable by variable

################################################################################
# participant_name
table(FAIR.response.raw$participant_name)
length(unique(FAIR.response.raw$participant_name))

################################################################################
# participant_email
table(FAIR.response.raw$participant_email)
length(unique(FAIR.response.raw$participant_name))==length(unique(FAIR.response.raw$participant_email))

################################################################################
# MA_ID
sort(table(FAIR.response.raw$MA_ID))
length(unique(FAIR.response.raw$MA_ID))

################################################################################
# title
sort(table(FAIR.response.raw$title))

################################################################################
# data_GUPID_presence
sort(table(FAIR.response.raw$data_GUPID_presence))
table(is.na(FAIR.response.raw$data_GUPID_presence))

# Revising the "I really cannot decide/I don't know", which is indeed a very 
# tricky case because the article cites a DOI https://doi.org/10.6084/m9.figshare.c.4462778.v1
# but the dataset is not there. The dataset is somewhere in figshare:
# https://figshare.com/s/3eebaf42e161c0e7e1ef?file=13199204, but it says that
# this item is shared privately. Therefore, we are treating this case as the
# data not being assigned with a DOI, but the data is shared, and adjusting the
# following variables accordingly
FAIR.response.raw[FAIR.response.raw$data_GUPID_presence=="I really cannot decide/I don't know",]

FAIR.response.raw <- FAIR.response.raw %>%
  mutate(
    data_GUPID_presence = case_when(
      participant_email == "pietro_pollo@hotmail.com" & MA_ID == "MA_026" ~ "No unique and persistent identifier associated with the data",
      TRUE ~ data_GUPID_presence
    ),
    data_sharing_location = case_when(
      participant_email == "pietro_pollo@hotmail.com" & MA_ID == "MA_026" ~ "Figshare",
      TRUE ~ data_sharing_location
    )
  )

sort(table(FAIR.response.raw$data_GUPID_presence))
table(is.na(FAIR.response.raw$data_GUPID_presence))

################################################################################
# data_GUPID
sort(table(FAIR.response.raw$data_GUPID))
table(is.na(FAIR.response.raw$data_GUPID))

# Revising the Data is not shared case, which is indeed a bit of a tricky case
# because the data availability statement is available at the very bottom of the
# pdf and not obvious from the online version
FAIR.response.raw[FAIR.response.raw$data_GUPID=="Data is not shared" & !(is.na(FAIR.response.raw$data_GUPID)),]

FAIR.response.raw <- FAIR.response.raw %>%
  mutate(
    data_GUPID_presence = case_when(
      participant_email == "gabewinter@gmail.com" & MA_ID == "MA_065" ~ "Digital Object Identifier (DOI)",
      TRUE ~ data_GUPID_presence
    ),
    data_GUPID = case_when(
      participant_email == "gabewinter@gmail.com" & MA_ID == "MA_065" ~ "10.6084/m9.figshare.4958687",
      TRUE ~ data_GUPID
    ),
    data_sharing_location = case_when(
      participant_email == "gabewinter@gmail.com" & MA_ID == "MA_065" ~ "Figshare",
      TRUE ~ data_sharing_location
    ),
    data_metadata_presence = case_when(
      participant_email == "gabewinter@gmail.com" & MA_ID == "MA_065" ~ "No",
      TRUE ~ data_metadata_presence
    ),
    data_metadata_type = case_when(
      participant_email == "gabewinter@gmail.com" & MA_ID == "MA_065" ~ "Meta-data is not shared",
      TRUE ~ data_metadata_type
    ),
    data_downloadable = case_when(
      participant_email == "gabewinter@gmail.com" & MA_ID == "MA_065" ~ "Yes",
      TRUE ~ data_downloadable
    ),
    data_downloadable_comments = case_when(
      participant_email == "gabewinter@gmail.com" & MA_ID == "MA_065" ~ "",
      TRUE ~ data_downloadable_comments
    ),
    data_type = case_when(
      participant_email == "gabewinter@gmail.com" & MA_ID == "MA_065" ~ ".xlsx",
      TRUE ~ data_type
    ),
    data_licence_presence = case_when(
      participant_email == "gabewinter@gmail.com" & MA_ID == "MA_065" ~ "CC BY (Attribution)",
      TRUE ~ data_licence_presence
    ),
    data_dictionary_presence = case_when(
      participant_email == "gabewinter@gmail.com" & MA_ID == "MA_065" ~ "No",
      TRUE ~ data_dictionary_presence
    )
  )

sort(table(FAIR.response.raw$data_GUPID))
table(is.na(FAIR.response.raw$data_GUPID))

################################################################################
# data_sharing_location
sort(table(FAIR.response.raw$data_sharing_location))
table(is.na(FAIR.response.raw$data_sharing_location))

# Revising the Data is not shared case, which is an error as the data and code
# are provided as part of .xlsx files added to the supplementary material
FAIR.response.raw[FAIR.response.raw$data_sharing_location=="Data is not shared" & !(is.na(FAIR.response.raw$data_sharing_location)),]

FAIR.response.raw <- FAIR.response.raw %>%
  mutate(
    data_sharing_location = case_when(
      participant_email == "m.l.jones@exeter.ac.uk" & MA_ID == "MA_022" ~ "Supplementary materials",
      TRUE ~ data_sharing_location
    ),
    data_metadata_presence = case_when(
      participant_email == "m.l.jones@exeter.ac.uk" & MA_ID == "MA_022" ~ "Yes",
      TRUE ~ data_metadata_presence
    ),
    data_metadata_type = case_when(
      participant_email == "m.l.jones@exeter.ac.uk" & MA_ID == "MA_022" ~ "As .xlsx tab",
      TRUE ~ data_metadata_type
    ),
    data_downloadable = case_when(
      participant_email == "m.l.jones@exeter.ac.uk" & MA_ID == "MA_022" ~ "Yes",
      TRUE ~ data_downloadable
    ),
    data_type = case_when(
      participant_email == "m.l.jones@exeter.ac.uk" & MA_ID == "MA_022" ~ ".xlsx",
      TRUE ~ data_type
    ),
    data_licence_presence = case_when(
      participant_email == "m.l.jones@exeter.ac.uk" & MA_ID == "MA_022" ~ "Publisher copyright/Rights reserved",
      TRUE ~ data_licence_presence
    ),
    data_dictionary_presence = case_when(
      participant_email == "m.l.jones@exeter.ac.uk" & MA_ID == "MA_022" ~ "Yes",
      TRUE ~ data_dictionary_presence
    )
  )

sort(table(FAIR.response.raw$data_sharing_location))

# Revising the Table within article case, which is indeed a Table within the
# article, which we do not consider a real problem for not filling in the 
# variables.
FAIR.response.raw[FAIR.response.raw$data_sharing_location=="Table within article" & !(is.na(FAIR.response.raw$data_sharing_location)),]

FAIR.response.raw <- FAIR.response.raw %>%
  mutate(
    data_metadata_presence = case_when(
      participant_email == "ryan@ryanfield.me" & MA_ID == "MA_041" ~ "Yes",
      TRUE ~ data_metadata_presence
    ),
    data_metadata_type = case_when(
      participant_email == "ryan@ryanfield.me" & MA_ID == "MA_041" ~ "As part of the article",
      TRUE ~ data_metadata_type
    ),
    data_downloadable = case_when(
      participant_email == "ryan@ryanfield.me" & MA_ID == "MA_041" ~ "Yes",
      TRUE ~ data_downloadable
    ),
    data_downloadable_comments = case_when(
      participant_email == "ryan@ryanfield.me" & MA_ID == "MA_041" ~ "Since the article can be downloaded and the table is within",
      TRUE ~ data_downloadable_comments
    ),
    data_type = case_when(
      participant_email == "ryan@ryanfield.me" & MA_ID == "MA_041" ~ ".pdf",
      TRUE ~ data_type
    ),
    data_type_comments = case_when(
      participant_email == "ryan@ryanfield.me" & MA_ID == "MA_041" ~ "alternatively, .html could be used but downloading is most common/easy as .pdf",
      TRUE ~ data_type_comments
    ),
    data_licence_presence = case_when(
      participant_email == "ryan@ryanfield.me" & MA_ID == "MA_041" ~ "Publisher copyright/Rights reserved",
      TRUE ~ data_licence_presence
    ),
    data_dictionary_presence = case_when(
      participant_email == "ryan@ryanfield.me" & MA_ID == "MA_041" ~ "Yes",
      TRUE ~ data_dictionary_presence
    ),
    data_dictionary_comments = case_when(
      participant_email == "ryan@ryanfield.me" & MA_ID == "MA_041" ~ "Variable description and units added at the bottom of the table within article",
      TRUE ~ data_dictionary_comments
    )
  )

sort(table(FAIR.response.raw$data_sharing_location))

# checking for disagreements between variables
FAIR.response.raw %>%
  filter(
    data_sharing_location %in% c("Supplementary materials",
                                 "Table within article",
                                 "NCBI GenBank;Supplementary materials") ,
    data_GUPID_presence != "No unique and persistent identifier associated with the data"
  )

# correcting two errors
FAIR.response.raw <- FAIR.response.raw %>%
  mutate(
    data_GUPID_presence = case_when(
      participant_email == "np.juncal@gmail.com" & MA_ID == "MA_019" ~ "No unique and persistent identifier associated with the data",
      TRUE ~ data_GUPID_presence
    ),
    data_GUPID = case_when(
      participant_email == "np.juncal@gmail.com" & MA_ID == "MA_019" ~ NA,
      TRUE ~ data_GUPID
    ),
    data_GUPID_presence = case_when(
      participant_email == "np.juncal@gmail.com" & MA_ID == "MA_070" ~ "No unique and persistent identifier associated with the data",
      TRUE ~ data_GUPID_presence
    ),
    data_GUPID = case_when(
      participant_email == "np.juncal@gmail.com" & MA_ID == "MA_070" ~ NA,
      TRUE ~ data_GUPID
    )
  )

sort(table(FAIR.response.raw$data_sharing_location))
sort(table(FAIR.response.raw$data_GUPID_presence))

# If Supplementary materials or Table within article only, then data_licence_presence
# should be "Publisher copyright/Rights reserved"

# with(FAIR.response.raw,
#      all(data_licence_presence[data_sharing_location == "Supplementary materials"] ==
#            "Publisher copyright/Rights reserved"))
# 
# violations <- FAIR.response.raw[
#   FAIR.response.raw$data_sharing_location == "Supplementary materials" &
#     FAIR.response.raw$data_licence_presence != "Publisher copyright/Rights reserved",
# ]
# 
# nrow(violations)

# violations <- FAIR.response.raw %>%
#   filter(
#     data_sharing_location == "Supplementary materials",
#     data_licence_presence != "Publisher copyright/Rights reserved"
#   )
# 
# violations

# Changing them to "Publisher copyright/Rights reserved" after revision
FAIR.response.raw <- FAIR.response.raw %>%
  mutate(
    data_licence_presence = if_else(
      data_sharing_location == "Supplementary materials" &
        data_licence_presence != "Publisher copyright/Rights reserved",
      "Publisher copyright/Rights reserved",
      data_licence_presence
    )
  )

table(FAIR.response.raw$data_licence_presence)


################################################################################
# data_metadata_presence
sort(table(FAIR.response.raw$data_metadata_presence))

# here we are going to change the "No but the description of the variables in 
# the dataset(s) is present" diretly to No, since the information about the 
# data dictionary is part of the data_dictionary_presence variable

FAIR.response.raw$data_metadata_presence <- ifelse(FAIR.response.raw$data_metadata_presence=="No but the description of the variables in the dataset(s) is present",
                                                   "No", FAIR.response.raw$data_metadata_presence)

sort(table(FAIR.response.raw$data_metadata_presence))
table(is.na(FAIR.response.raw$data_metadata_presence))


################################################################################
# data_metadata_type
sort(table(FAIR.response.raw$data_metadata_type))
table(is.na(FAIR.response.raw$data_metadata_type))

# exploring repositories vs metadata presence
FAIR.response.raw %>%
  filter(
    data_sharing_location == "Dryad",
    data_metadata_type != "As part of the data repository (Zenodo/Dryad)"
  )

FAIR.response.raw %>%
  filter(
    data_sharing_location == "Zenodo",
    data_metadata_type != "As part of the data repository (Zenodo/Dryad)"
  )

FAIR.response.raw %>%
  filter(
    data_sharing_location == "Dryad;GitHub or GitLab",
    data_metadata_type != "As part of the data repository (Zenodo/Dryad)"
  )

FAIR.response.raw %>%
  filter(
    data_sharing_location == "Dryad;Supplementary materials",
    data_metadata_type != "As part of the data repository (Zenodo/Dryad)"
  )

FAIR.response.raw %>%
  filter(
    data_sharing_location == "Figshare",
    data_metadata_type != "As part of the data repository (Zenodo/Dryad)"
  )


FAIR.response.raw <- FAIR.response.raw %>%
  mutate(
    # changing the following entry for standardising purposes, thought it is true
    # that for this particular case metadata associated with the repository is
    # minimal and almost nonexistent
    data_metadata_presence = case_when(
      participant_email == "pietro_pollo@hotmail.com" & MA_ID == "MA_014" ~ "Yes",
      TRUE ~ data_metadata_presence
    ),
    data_metadata_type = case_when(
      participant_email == "pietro_pollo@hotmail.com" & MA_ID == "MA_014" ~ "As part of the data repository (Zenodo/Dryad)",
      TRUE ~ data_metadata_type
    ),
    # standardising the following entries
    data_metadata_type = case_when(
      participant_email == "afoxx@chicagobotanic.org" & MA_ID == "MA_015" ~ "As part of the data repository (Zenodo/Dryad)",
      TRUE ~ data_metadata_type
    ),
    data_metadata_type = case_when(
      participant_email == "gabewinter@gmail.com" & MA_ID == "MA_030" ~ "As README file/ .txt file; As part of the data repository (Zenodo/Dryad)",
      TRUE ~ data_metadata_type
    ),
    # correcting the following entry
    data_metadata_presence = case_when(
      participant_email == "m.l.jones@exeter.ac.uk" & MA_ID == "MA_071" ~ "Yes",
      TRUE ~ data_metadata_presence
    ),
    data_metadata_type = case_when(
      participant_email == "m.l.jones@exeter.ac.uk" & MA_ID == "MA_071" ~ "As part of the data repository (Zenodo/Dryad)",
      TRUE ~ data_metadata_type
    ),
    # correcting the following entry
    data_metadata_presence = case_when(
      participant_email == "pietro_pollo@hotmail.com" & MA_ID == "MA_035" ~ "Yes",
      TRUE ~ data_metadata_presence
    ),
    data_metadata_type = case_when(
      participant_email == "pietro_pollo@hotmail.com" & MA_ID == "MA_035" ~ "As part of the data repository (Zenodo/Dryad)",
      TRUE ~ data_metadata_type
    ),
    data_dictionary_presence = case_when(
      participant_email == "pietro_pollo@hotmail.com" & MA_ID == "MA_035" ~ "Yes",
      TRUE ~ data_dictionary_presence
    ),
    data_dictionary_comments = case_when(
      participant_email == "pietro_pollo@hotmail.com" & MA_ID == "MA_035" ~ "There is a description of the variables as part of the repository Usage notes",
      TRUE ~ data_dictionary_comments
    ),
    # correcting the following entry
    data_metadata_type = case_when(
      participant_email == "balti.heikel@gmail.com" & MA_ID == "MA_037" ~ "As part of the data repository (Zenodo/Dryad)",
      TRUE ~ data_metadata_type
    ),
    # correcting the following entry
    data_metadata_presence = case_when(
      participant_email == "e.ivimeycook@gmail.com" & MA_ID == "MA_033" ~ "Yes",
      TRUE ~ data_metadata_presence
    ),
    data_metadata_type = case_when(
      participant_email == "e.ivimeycook@gmail.com" & MA_ID == "MA_033" ~ "As part of the data repository (Zenodo/Dryad)",
      TRUE ~ data_metadata_type
    ),
    # correcting the following entry
    data_metadata_presence = case_when(
      participant_email == "gabewinter@gmail.com" & MA_ID == "MA_065" ~ "Yes",
      TRUE ~ data_metadata_presence
    ),
    data_metadata_type = case_when(
      participant_email == "gabewinter@gmail.com" & MA_ID == "MA_065" ~ "As part of the data repository (Zenodo/Dryad)",
      TRUE ~ data_metadata_type
    )
  )

sort(table(FAIR.response.raw$data_metadata_type))

# revising the special cases to confirm them
FAIR.response.raw[FAIR.response.raw$data_metadata_type == "As OSF Wiki;.xls(x) tab" & 
                    !(is.na(FAIR.response.raw$data_metadata_type)),]

FAIR.response.raw[FAIR.response.raw$data_metadata_type == "As README file/ .txt file, As part of the data repository (Zenodo/Dryad)" &
                    !(is.na(FAIR.response.raw$data_metadata_type)),]

FAIR.response.raw[FAIR.response.raw$data_metadata_type == "In the .pdf file (sup. infos)" &
                    !(is.na(FAIR.response.raw$data_metadata_type)),]

FAIR.response.raw[FAIR.response.raw$data_metadata_type == "As .xlsx tab" &
                    !(is.na(FAIR.response.raw$data_metadata_type)),]

FAIR.response.raw <- FAIR.response.raw %>%
  mutate(
    # correcting the following entry
    data_metadata_type = case_when(
      participant_email == "ryan@ryanfield.me" & MA_ID == "MA_028" ~ "Meta-data is not shared",
      TRUE ~ data_metadata_type
    ),
    # correcting the following entry to note that the data is provided in Table 3
    data_sharing_location = case_when(
      participant_email == "balti.heikel@gmail.com" & MA_ID == "MA_042" ~ "Table within article",
      TRUE ~ data_sharing_location
    ),
    data_metadata_type = case_when(
      participant_email == "balti.heikel@gmail.com" & MA_ID == "MA_042" ~ "As part of the article",
      TRUE ~ data_metadata_type
    ),
    data_downloadable_comments = case_when(
      participant_email == "balti.heikel@gmail.com" & MA_ID == "MA_042" ~ "Since the article can be downloaded and the table is within",
      TRUE ~ data_downloadable_comments
    ),
    data_type_comments = case_when(
      participant_email == "balti.heikel@gmail.com" & MA_ID == "MA_042" ~ "alternatively, .html could be used but downloading is most common/easy as .pdf",
      TRUE ~ data_type_comments
    )
  )


sort(table(FAIR.response.raw$data_metadata_type))

# revising and standardising entries even more
FAIR.response.raw <- FAIR.response.raw %>%
  mutate(
    data_metadata_type = case_when(
      data_metadata_type == "No" ~ "Meta-data is not shared",
      data_metadata_type == "As README file/ .txt file, As part of the data repository (Zenodo/Dryad)" ~ "As README file/ .txt file; As part of the data repository",
      data_metadata_type == "As README file/ .txt file; As part of the data repository (Zenodo/Dryad)" ~ "As README file/ .txt file; As part of the data repository",
      data_metadata_type == "As part of the data repository (Zenodo/Dryad)" ~ "As part of the data repository",
      TRUE ~ data_metadata_type
    )
  )

sort(table(FAIR.response.raw$data_metadata_type))

# checking for disagreements between variables
FAIR.response.raw %>%
  filter(
    data_metadata_type != "Meta-data is not shared",
    data_metadata_presence == "No"
  )


FAIR.response.raw <- FAIR.response.raw %>%
  mutate(
    # correcting the following entry
    data_metadata_presence = case_when(
      participant_email == "m.l.jones@exeter.ac.uk" & MA_ID == "MA_077" ~ "Yes",
      TRUE ~ data_metadata_presence
    )
  )

sort(table(FAIR.response.raw$data_metadata_type))
sort(table(FAIR.response.raw$data_metadata_presence))


################################################################################
# data_downloadable
sort(table(FAIR.response.raw$data_downloadable))
table(is.na(FAIR.response.raw$data_downloadable))

################################################################################
# data_type
sort(table(FAIR.response.raw$data_type))
table(is.na(FAIR.response.raw$data_type))

################################################################################
# data_licence_presence
sort(table(FAIR.response.raw$data_licence_presence))
table(is.na(FAIR.response.raw$data_licence_presence))

# exploring a special case
FAIR.response.raw[FAIR.response.raw$data_licence_presence=="No license provided or associated with the repository, I guess its associated with the article not the data (CC 4.0)",]

# revising and standardising entries
FAIR.response.raw <- FAIR.response.raw %>%
  mutate(
    data_licence_presence = case_when(
      data_licence_presence == "CC BY 4.0" ~ "CC BY (Attribution)",
      data_licence_presence == "No license provided or associated with the repository, I guess its associated with the article not the data (CC 4.0)" ~ "Publisher copyright/Rights reserved",
      TRUE ~ data_licence_presence
    )
  )

sort(table(FAIR.response.raw$data_licence_presence))

# checking for disagreements between variables
FAIR.response.raw %>%
  filter(
    data_sharing_location != "Supplementary materials",
    data_licence_presence == "Publisher copyright/Rights reserved"
  )

# all is good here, those three cases comply with the rules


################################################################################
# data_dictionary_presence
sort(table(FAIR.response.raw$data_dictionary_presence))
table(is.na(FAIR.response.raw$data_dictionary_presence))

# checking for disagreements between variables
table(FAIR.response.raw$data_dictionary_presence,FAIR.response.raw$data_metadata_presence)

FAIR.response.raw %>%
  filter(
    data_dictionary_presence == "Yes",
    data_metadata_presence == "No"
  )

# all good there


################################################################################
################################################################################
# Exploring the dataset again variable by variable, and generating processed,
# recategorised versions of the variables, whenever necessary
################################################################################
################################################################################

FAIR.response.processed <- FAIR.response.raw

################################################################################
# data_GUPID_presence
sort(table(FAIR.response.processed$data_GUPID_presence))


# binary variable
FAIR.response.processed$data_GUPID_presence_recat <- ifelse(FAIR.response.processed$data_GUPID_presence=="No unique and persistent identifier associated with the data",
                                                            "No",
                                                            "Yes")

table(FAIR.response.processed$data_GUPID_presence_recat)

################################################################################
# data_sharing_location
sort(table(FAIR.response.processed$data_sharing_location))

# recategorisation
FAIR.response.processed <- FAIR.response.processed %>%
  mutate(
    data_sharing_location_recat = case_when(
      
      # Repositories
      data_sharing_location %in% c(
        "Dryad", "Zenodo", "Figshare",
        "DataVerse", "Institutional repository",
        "Dryad;GitHub or GitLab",
        "Dryad;Supplementary materials"
      ) ~ "Repository",
      
      # Supplements (combined or explicit)
      data_sharing_location %in% c(
        "NCBI GenBank;Supplementary materials","Supplementary materials"
      ) ~ "Supplements",
      
      # Within article
      data_sharing_location == "Table within article" ~
        "Within_article",
      
      # # Personal website
      # data_sharing_location == "Personal website" ~
      #   "Personal_web",
      
      # Catch-all
      TRUE ~ NA_character_
    )
  )


FAIR.response.processed %>%
  count(data_sharing_location, data_sharing_location_recat)

sort(table(FAIR.response.processed$data_sharing_location_recat))

# binary variable
FAIR.response.processed$data_sharing_location_recat_bin <- ifelse(FAIR.response.processed$data_sharing_location_recat=="Repository",
                                                                  "Yes",
                                                                  "No")

table(FAIR.response.processed$data_sharing_location_recat_bin)


################################################################################
# data_metadata_presence
sort(table(FAIR.response.processed$data_metadata_presence))

# # Think about metadata Yes or No for data provided within the article or supplementary materials
# # checking for disagreements between variables
# FAIR.response.processed %>%
#   filter(
#     data_sharing_location %in% c("Supplementary materials",
#                                  "Table within article",
#                                  "NCBI GenBank;Supplementary materials") ,
#     data_metadata_presence == "No"
#   )
# FAIR.response.processed$data_metadata_presence_generous <- 
#   ifelse(FAIR.response.processed$data_sharing_location %in% c("Supplementary materials",
#                                                               "Table within article",
#                                                               "NCBI GenBank;Supplementary materials"),
#          "Yes",
#          FAIR.response.processed$data_metadata_presence)
# 
# sort(table(FAIR.response.processed$data_metadata_presence_generous))


################################################################################
# data_metadata_type
sort(table(FAIR.response.processed$data_metadata_type))

################################################################################
# data_downloadable
sort(table(FAIR.response.processed$data_downloadable))

################################################################################
# data_type
sort(table(FAIR.response.processed$data_type))

# Data formats were classified according to their alignment with the FAIR 
# principles, focusing on interoperability and reusability. Non-proprietary, 
# machine-readable formats (CSV/TSV), structured text files (TXT), and 
# domain-standard phylogenetic formats (TRE) were classified as FAIR-aligned 
# (‘Yes’). Spreadsheet formats (XLS/XLSX) were classified as partially FAIR 
# (‘Partial’): although XLSX is based on an open specification and is 
# machine-readable in principle, spreadsheet formats are frequently used in ways
# that introduce implicit structure (e.g. multiple sheets, formatting, embedded 
# formulas), while legacy XLS files further raise concerns regarding 
# sustainability and interoperability. Document-based formats (PDF, DOC, DOCX) 
# were classified as non-FAIR for data reuse (‘No’), as they primarily support 
# human readability rather than machine interoperability. For records containing 
# multiple file formats, the classification reflected the least FAIR 
# data-relevant format present.

FAIR.response.processed$data_type_FAIR <- with(FAIR.response.processed,
                                               dplyr::case_when(
                                                 # NO: only document formats
                                                 grepl("\\.(pdf|docx|doc)", data_type, ignore.case = TRUE) &
                                                   !grepl("\\.(csv|tsv|txt|tre|xls|xlsx)", data_type, ignore.case = TRUE) ~ "No",
                                                 
                                                 # YES: only fully FAIR data formats
                                                 grepl("\\.(csv|tsv|txt|tre)", data_type, ignore.case = TRUE) &
                                                   !grepl("\\.(xls|xlsx|pdf|docx|doc)", data_type, ignore.case = TRUE) ~ "Yes",
                                                 
                                                 #PARTIAL: anything else with at least some data
                                                 grepl("\\.(csv|tsv|txt|tre|xls|xlsx)", data_type, ignore.case = TRUE) ~ "Partial",
                                                 
                                                 # fallback
                                                 TRUE ~ NA_character_
                                               )
)

sort(table(FAIR.response.processed$data_type_FAIR))

FAIR.response.processed %>%
  count(data_type, data_type_FAIR)

################################################################################
# data_licence_presence
sort(table(FAIR.response.processed$data_licence_presence))
table(is.na(FAIR.response.processed$data_licence_presence))

FAIR.response.processed %>%
  count(data_sharing_location, data_licence_presence)

# binary variable
FAIR.response.processed$data_licence_presence_recat <- ifelse(FAIR.response.processed$data_licence_presence=="Publisher copyright/Rights reserved",
                                                              "No",
                                                              "Yes")

sort(table(FAIR.response.processed$data_licence_presence_recat))


################################################################################
# data_dictionary_presence
sort(table(FAIR.response.processed$data_dictionary_presence))



################################################################################
# Generating a dataset with one row per MA_ID after having resolved all 
# conflicts between entries of the same MA_ID for the variables that apply
################################################################################

FAIR.response.processed %>%
  dplyr::filter(MA_ID %in% MA_ID[duplicated(MA_ID)]) %>%
  dplyr::arrange(MA_ID)


# Collapse dataset to one row per MA_ID
FAIR.response.processed.final <- FAIR.response.processed %>%
  
  # remove some unnecessary variables:
  select(-Timestamp, -participant_email, -title) %>%
  
  # Group by article identifier
  group_by(MA_ID) %>%
  
  mutate(number_of_FAIR_assessments = n()) %>%
  #ungroup()
  
  # Summarise each column: take first() for standardized variables
  summarise(
    
    # Article metadata
    number_of_FAIR_assessments = first(number_of_FAIR_assessments), # number of entries for this article
    
    # Concatenate participant names with ; separator
    FAIR_assessment_names_conc = paste((participant_name), collapse = "; "),
    
    # Choosing the first entry for the following variables, which have the same
    # value in both entries.
    
    # data_GUPID_presence / data_GUPID / data_sharing_location
    data_GUPID_presence = first(data_GUPID_presence),
    data_GUPID_presence_recat = first(data_GUPID_presence_recat),
    data_GUPID = first(data_GUPID),
    data_sharing_location = first(data_sharing_location),
    data_sharing_location_recat = first(data_sharing_location_recat),
    data_sharing_location_recat_bin = first(data_sharing_location_recat_bin),
    data_metadata_presence = first(data_metadata_presence),
    data_metadata_type = first(data_metadata_type),
    data_downloadable = first(data_downloadable),
    data_licence_presence = first(data_licence_presence),
    data_licence_presence_recat = first(data_licence_presence_recat),
    data_dictionary_presence = first(data_dictionary_presence),
    data_type = first(data_type),
    data_type_FAIR = first(data_type_FAIR),
    
    # Concatenate the following variables with ; separator to keep the information
    # available
    data_metadata_comments_conc = paste((data_metadata_comments), collapse = "; "),
    data_downloadable_comments_conc = paste((data_downloadable_comments), collapse = "; "),
    data_type_comments_conc = paste((data_type_comments), collapse = "; "),
    data_dictionary_comments_conc = paste((data_dictionary_comments), collapse = "; "),
    
    
    # Ungroup after summarise to return a plain data frame
    .groups = "drop"
    
  ) %>%
  
  # converting all "NA" string to real NA's from now on
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) %>%
  # converting all "" string to real NA's from now on
  mutate(across(where(is.character), ~ na_if(.x, ""))) %>%
  
  # convert bare ";" in _conc variables to NA
  mutate(
    across(
      ends_with("_conc"),
      ~ ifelse(grepl("^\\s*;\\s*$", .x), NA_character_, .x)
    )
  ) %>%
  
  # Convert to base R data.frame for compatibility with other code
  as.data.frame()


# Before I finish the export of this dataset, there is one additional FAIR point
# I want to add to our original list. This FAIR point relates to findability. 
# FAIRness is a property of the data and its scholarly context. Thus, when 
# publications did not provide a direct link, DOI, or accession number to the 
# underlying data, but data were located independently, these cases were 
# considered to exhibit reduced FAIRness with respect to findability, as the 
# discoverability of the data from the publication itself was not ensured.
# We had not considered this option originally, but we discovered one case in 
# which this was the case, so we are going to add a variable to the dataset to
# reflect thes:

FAIR.response.processed.final$article_links2data <- ifelse(FAIR.response.processed.final$MA_ID == "MA_032",
                                                           "No",
                                                           "Yes")


################################################################################
# Saving data
################################################################################

write.csv(FAIR.response.processed.final,
          "data/02_processed_data/FAIR_assessment/FAIR_data_assessment_final.csv",
          row.names=FALSE)


################################################################################
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
#   [1] readxl_1.5.0    writexl_1.5.4   lubridate_1.9.5 forcats_1.0.1   stringr_1.6.0   purrr_1.2.2     readr_2.2.0    
# [8] tidyr_1.3.2     tibble_3.3.1    ggplot2_4.0.3   tidyverse_2.0.0 dplyr_1.2.1    
# 
# loaded via a namespace (and not attached):
#   [1] gtable_0.3.6       compiler_4.5.2     tidyselect_1.2.1   gridExtra_2.3      scales_1.4.0       yaml_2.3.12       
# [7] fastmap_1.2.0      here_1.0.2         R6_2.6.1           generics_0.1.4     patchwork_1.3.2    knitr_1.51        
# [13] viridis_0.6.5      rprojroot_2.1.1    tzdb_0.5.0         pillar_1.11.1      RColorBrewer_1.1-3 rlang_1.3.0       
# [19] utf8_1.2.6         stringi_1.8.7      xfun_0.58          S7_0.2.2           otel_0.2.0         timechange_0.4.0  
# [25] viridisLite_0.4.3  cli_3.6.6          withr_3.0.3        magrittr_2.0.5     digest_0.6.39      grid_4.5.2        
# [31] rstudioapi_0.18.0  hms_1.1.4          lifecycle_1.0.5    vctrs_0.7.3        evaluate_1.0.5     glue_1.8.1        
# [37] farver_2.1.2       cellranger_1.1.0   pacman_0.5.1       rmarkdown_2.31     tools_4.5.2        pkgconfig_2.0.3   
# [43] htmltools_0.5.9 