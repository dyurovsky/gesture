library(dplyr)
library(tidyr)
library(data.table)
library(magrittr)
library(readr)
library(langcog)
library(purrr)
library(broom)
library(bit64)
library(lubridate)
library(stringr)

# Data from two children at 18-month and 24 mo
files <- list.files("raw_data/", "*.tsv")

# Some regular expressions for cleaning up speech to get out the intended referents
clean_string <- function(string) {gsub("[^[:alnum:] /']", "", string)}
split_string <- function(string) {strsplit(string, " +")}

# Main subject loading function
load_subj <- function(file) {

  splits <- strsplit(file,"\\.")[[1]]
  subj_name <- splits[1]
  subj_session <- splits[2]

  # Read the raw .tsv file
  subj <- suppressWarnings(fread(paste0("raw_data/",file), sep = "\t",
                                 data.table = F))

  # Convert ids to integers and time to time in minutes
  subj_clean <- subj %>%
    mutate(id = 1:n(),
           time = (period_to_seconds(hms(time)))/60)

  # Fix a timing error where the hour-unit did not track correctly
  if(is.na(subj_clean[1,"time"]))
    subj_clean[1,"time"] <- 0

  # Do a bunch of munging to get the data in a tidier format
  subj_clean %>%
    mutate(time = na.approx(time, na.rm = F),
           time_fix = time < cummax(time),
           time = ifelse(time_fix, time + 60, time)) %>% # Interpolate time
    gather(type, value, p_utts:c_gs_rel) %>%
    separate(type, c("person", "type"), extra = "merge") %>%
    mutate(person = ifelse(person == "c", "child", "parent")) %>%
    filter(type %in% c("utts", "obj", "gloss", "form", "gs_rel")) %>%
    spread(type, value) %>%
    select(-time_fix) %>%
    mutate_each(funs(clean_string), utts, form, gloss, obj) %>%
    filter((nchar(utts) + nchar(obj) + nchar(gloss) + nchar(form)) > 0) %>%
    mutate(subj = subj_name, session = subj_session)
}

# Load data
loaded_subjs <- lapply(files, load_subj) %>%
  bind_rows()

#Recode gestures to be English readable
code_gesture <- function(gesture) {
  if(gesture == "") "None"
  else if(gesture == "ADD") "Add"
  else if(gesture == "X") "Only Gesture"
  else if(gesture == "RF") "Reinforce"
  else if(gesture == "FA") "Add"
  else if(gesture == "DA") "Disambiguate"
  else if(gesture == "ELAB") "Elaborate"
  else if(gesture == "UC") "Unclear"
  else if(gesture == "E") "Emphasis"
  else if(gesture == "MS") "Meaningless Sound"
}

coded_subjs <- loaded_subjs %>%
  mutate(gs_rel = sub("[.;( ].*", "", gs_rel)) %>%
  rowwise() %>%
  mutate(gs_rel = code_gesture(gs_rel)) %>%
  select(subj, session, time, person, utts, form, gloss, obj, gs_rel)

# Show the 20 rows of the new data frame
kable(coded_subjs[51:70,])

split_subjs <- coded_subjs %>%
  mutate(spoken_obj = "", gestured_obj = "", referent = "", comments = "") %>%
  split(paste(.$subj, .$session)) %>%
  map(function(df) write_csv(df, paste0("new_data/", df[1,"subj"], "_", 
                                        df[1,"session"], ".csv")))
