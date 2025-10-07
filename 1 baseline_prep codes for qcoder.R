##################################### Meta ######################################

# Project Tron - Baseline study
# Code for preparing code dictionary for QCoder
# October 7, 2025
# Contributors : Azzah

# Reference for QCoder: https://ropenscilabs.github.io/qcoder/


############################ Environment setup #################################


# Clear console and environment 
graphics.off(); rm(list=ls());cat("\14");

# Load packages
#install.packages("pacman") # install the package if you haven't 
pacman::p_load(rmarkdown, knitr, dplyr, readxl, writexl, data.table,
               haven, devtools, stringr, tidyverse)

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
path_pdf = file.path(getwd(), "1 Transcript (PDF)")
path_sheet = file.path(getwd(), "3 NLM initial coding", "2 Code snippet (sheet)")


###################### READING THE INITIAL CODING #####################

# Specify the path to your Excel file
file_path <- file.path(path_sheet, "Gemini code snippet.xlsx")

# Get a vector of all sheet names in the file
sheet_names <- excel_sheets(path = file_path)

# Use map_df() to read each sheet and combine them into one data frame
# The .id argument automatically creates a column to identify the source sheet
coding_data <- map_df(sheet_names, ~read_excel(path = file_path, sheet = .x), .id = "sheet_name")

# View the first few rows of the combined data to confirm it worked
head(combined_data)


################ CREATING A NEW CODE_ID FOR ALL THE CODES ###############

# creating new id
combined_data <- coding_data %>%
  mutate(
    # Combine the 'group' and 'code' columns into a single string
    group_code = paste(group, code, sep = "_"),
    # Convert the new string column to a factor and then to a numeric ID
    id = as.numeric(factor(group_code, levels = unique(group_code)))
  ) 

# exporting codes
codes_df <- combined_data %>%
  distinct(id, code, code_description) %>%
  rename(code_id = id)

write.csv(codes_df, file.path(path_code, "codes.csv"), row.names = FALSE)

# exporting all codes data for tagging
tagging_df <- combined_data %>%
  select(group, id, code, code_description, snippet, respondent_id) %>%
  rename(code_id = id)
  
write.csv(tagging_df, file.path(path_code, "snippet for autotagging.csv"), row.names = FALSE)



#############################################################################3