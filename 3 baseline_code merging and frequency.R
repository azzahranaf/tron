##################################### Meta ######################################

# Project Tron - Baseline study
# Code for code merging and counting code frequency
# October 15, 2025
# Contributors : Azzah


############################ Environment setup #################################


# Clear console and environment 
graphics.off(); rm(list=ls());cat("\14");

# Load packages
#install.packages("pacman") # install the package if you haven't 
pacman::p_load(rmarkdown, knitr, dplyr, readxl, writexl, data.table,
               haven, devtools, stringr, stringdist, tidyverse)

# Retrieve the current system username
current_user <- Sys.info()[["user"]]

# Check if the username matches and set the working directory accordingly
if (current_user == "User") {
  base_dir <- "H:/"
} else if (current_user %in% c("azzah", "USER")) {
  base_dir <- "G:/"
}

# Set directory
master <- file.path(base_dir, 
                    "Shared drives", 
                    "Projects", 
                    "2025", 
                    "Tron", 
                    "Breadcrumbs",
                    "1. Research",
                    "Baseline Study",
                    "6 Data & analysis")
setwd(master)


# Preparing location
path_code = file.path(getwd(),"4 QCoder","codes")
path_doc = file.path(getwd(),"4 QCoder","documents")
path_memo = file.path(getwd(),"4 QCoder","memos")
path_tf = file.path(getwd(), "1 Transcript files")
path_sheet = file.path(getwd(), "3 NLM initial coding", "2 Code snippet (sheet)")
path_ct = file.path(getwd(),"5 Coded transcript","1 Initial autotagging R")
path_sys = file.path(getwd(), "6 Code system and frequency")

################ COUNTING INITIAL CODE FREQUENCY FROM SNIPPET SHEET ###############

# Opening the sheet of snippet and code dictionary
all_snippet <- read.csv(file.path(path_code, "snippet for autotagging.csv"))
code_dict <- read.csv(file.path(path_code, "codes.csv"))

# Initial code frequency
initial_freq <- all_snippet %>%
  group_by(code_id) %>%
  summarize(freq = n()) %>%
  full_join(code_dict, by = "code_id")
  

################ PREP FOR MERGING CODES INTO NEW CODES ###############

# Import code system
code_system <- read_excel(file.path(path_sys, "Code system.xlsx"), 
                          sheet = "merging")

unique_code_sys <- code_system %>%
  group_by(code_id) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
  
edit_code_system <- code_system %>%
  filter(!(code_id==735 & category=="KEUNGGULAN DAN CITRA KENDARAAN LISTRIK (EV/BEV)"))

# Merging code system with code freq
merging_code_freq <- edit_code_system %>%
  full_join(initial_freq, by = "code_id")

# Export rows with missing new code
miss_new_code <- merging_code_freq %>%
  filter(is.na(new_code))

write.csv(
  miss_new_code,
  file.path(path_sys, paste0("Missing new code.xlsx")),
  #row.names = FALSE
)

# Listing initial categories
category_freq <- merging_code_freq %>%
  group_by(category) %>%
  summarise(count = n())

##########################################################

# Prep merging codes for missing rows
code_system_pt2 <- read_excel(file.path(path_sys, "Code system.xlsx"), 
                          sheet = "merging miss")

# Combine all codes
all_code_system <-  bind_rows(edit_code_system, code_system_pt2)

unique_all_code_sys <- all_code_system %>%
  group_by(code_id) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

edit_all_code_system <- all_code_system %>%
  filter(!(code_id == 999 & is.na(new_code))) %>%
  mutate(code_description = case_when(
    code_id == 999 ~ "Transisi penuh EV untuk industri ini diperkirakan membutuhkan waktu",
    TRUE ~ code_description
  ))

# Merging code system with code freq
merging_all_code_freq <- edit_all_code_system %>%
  full_join(initial_freq, by = "code_id")

all_code_dict <- merging_all_code_freq %>%
  select(code_id, code.y, code.description, new_code, new_code_description, category) %>%
  rename(code = code.y,
         code.description = code.description) %>%
  arrange(category, new_code, code_id) 

new_code_dict <- all_code_dict %>%
  distinct(new_code, new_code_description, category)


################ COUNTING FREQ FOR NEW CODES ###############

# Update code frequency
new_freq <- merging_all_code_freq %>%
  group_by(new_code) %>%
  summarize(code_freq = n()) %>%
  full_join(new_code_dict, by = "new_code") %>%
  filter(!is.na(new_code_description)) %>%
  select(category, new_code, new_code_description, code_freq) %>%
  ungroup() %>%
  group_by(category) %>%
  mutate(category_freq = sum(code_freq)) %>%
  ungroup() %>%
  arrange(desc(category_freq), category, desc(code_freq))

write.csv(
  new_freq,
  file.path(path_sys, paste0("Code frequency final.xlsx")),
  #row.names = FALSE
)

# Count by categories
new_category_freq <- merging_all_code_freq %>%
  group_by(category) %>%
  summarise(category_freq = n()) %>%
  arrange(desc(category_freq))

write.csv(
  new_category_freq,
  file.path(path_sys, paste0("Category frequency final.xlsx")),
  #row.names = FALSE
)
