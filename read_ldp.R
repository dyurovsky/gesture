library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)

# Read in LDP data
ldp <- src_sqlite("~/Documents/LDP/ldp.db")

tbl(ldp,"subjects")

# Get all participants
subjs <- tbl(ldp, "subjects") %>%
  collect()

# Get visit data
visits <- tbl(ldp, "visits") %>%
  collect() %>%
  select(subject, session, date, child_age, child_age_years, child_age_months, 
         income, mother_education)

#Subset to just typically-developing paricipants
td <- subjs %>%
  filter(lesion == "") %>%
  select(id, dob, sex, race, ethn) %>%
  left_join(visits, by=c("id"="subject"))

#Estimate ages
session_ages <- td %>%
  filter(child_age_months != 0) %>%
  group_by(session) %>%
  summarise_each(funs(mean,min,max), child_age_months)

#Previously coded ps
prev_subjs <- list.files("raw_data", "*.tsv") %>%
  str_split(., "\\.") %>%
  map(first) %>%
  map(as.integer) %>%
  unlist() %>%
  unique()

# Get all utterances and gestures for TD children
utterances <- tbl(ldp, "utterances") %>%
  select(subject, session, time, line, p_chat, p_form, p_obj, p_gloss, 
         p_g_type, p_gs_rel, c_chat, c_form, c_obj, c_gloss, c_g_type, c_gs_rel,
         context) %>%
  filter(subject %in% td$id) %>%
  collect(n = Inf)

# Get just the data for the sessions I want now
session_data <- utterances %>%
  filter(subject %in% prev_subjs, session < 5) %>%
  split(paste(.$subject, .$session, sep = "_")) 

# Write data for the relevant session
walk(session_data, ~write_csv(., paste0("ldp_data/", .$subject[1], "_",
                                       .$session[1], ".csv")))
