library(dplyr)
library(tidyr)
library(readr)
library(langcog)
library(purrr)
library(broom)
library(bit64)
library(lubridate)
library(stringr)
library(zoo)

# Data from two children at 18-month and 24 mo
#files <- list.files("raw_data/", "*.tsv")
files <- list.files("ldp_data/", "*.csv")

# Some regular expressions for cleaning up speech to get out the intended referents
clean_string <- function(string) {gsub("[^[:alnum:] /']", "", string)}

# Main subject loading function
load_subj <- function(file) {
  
  splits <- str_split(file, "[_\\.]") %>% unlist()
  
  subj_name <- splits[1]
  subj_session <- splits[2]
  
  subj <- suppressMessages(read_csv(paste0("ldp_data/", file),
                                    col_types = cols(
                                      time = col_character()))) %>%
    select(-line) %>%
    mutate(line = 1:n()) %>%
    as_data_frame()

  # Fix a timing error where the hour-unit did not track correctly
  # if(is.na(subj_clean[1,"time"]))
  #   subj_clean[1,"time"] <- 0

  # Do a bunch of munging to get the data in a tidier format
  
  tmp <- subj %>%
    gather(type, value, p_chat:c_gs_rel) %>%
    separate(type, c("person", "type"), extra = "merge") %>%
    mutate(person = ifelse(person == "c", "child", "parent")) %>%
    filter(type %in% c("chat", "obj", "gloss", "form", "gs_rel")) %>%
    spread(type, value) %>%
    filter(!is.na(chat) | !is.na(obj) | !is.na(gloss) | !is.na(form) |
             !is.na(context)) %>%
    mutate(subj = subj_name, session = subj_session) %>%
    arrange(line)
}

# Load data
loaded_subjs <- lapply(files, load_subj) %>%
  bind_rows() %>%
  as_data_frame()

coded_subjs <- loaded_subjs %>%
  mutate(gs_rel = sub("[.;( ].*", "", gs_rel)) %>%
  mutate(gs_rel = if_else(gs_rel == "", "empty", gs_rel)) %>%
  mutate(gs_rel = recode(gs_rel, 
                         "empty" = "None",
                         "ADD" = "Add",
                         "X" = "Only Gesture",
                         "RF" = "Reinforce",
                         "FA" = "Add",
                         "DA" = "Disambiguate",
                         "ELAB" = "Elaborate",
                         "UC" = "Unclear",
                         "E" = "Emphasis",
                         "MS" = "Meaningless Sound")) %>%
  select(subj, session, time, person, chat, form, gloss, obj, gs_rel, context) %>%
  mutate_each(funs(if_else(is.na(.), "", .)),
              time, chat, form, gloss, obj, gs_rel, context)

# Show the 20 rows of the new data frame
coded_subjs[51:70,]

split_subjs <- coded_subjs %>%
  mutate(spoken_obj = "", gestured_obj = "", referent = "", comments = "") %>%
  split(paste(.$subj, .$session)) %>%
  walk(~write_csv(.,paste0("to_code_data/", .$subj[1], "_", .$session[1], ".csv")))