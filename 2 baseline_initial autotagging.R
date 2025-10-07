##################################### Meta ######################################

# Project Tron - Baseline study
# Code for tagging initial thematic codes from NotebookLM to FGD and IDI transcript
# October 7, 2025
# Contributors : Azzah

# Reference for QCoder: https://ropenscilabs.github.io/qcoder/

############################ Environment setup #################################


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
path_pdf = file.path(getwd(), "1 Transcript (PDF)")
path_sheet = file.path(getwd(), "3 NLM initial coding", "2 Code snippet (sheet)")

#### Install QCoder for QDA ####

install.packages("devtools")
devtools::install_github("ropenscilabs/qcoder")
library(qcoder)

# Import project
import_project_data(project = "4 QCoder")


############################ Preparing 3-Levels Autotagging #################################

# 1. Normalization function
normalize_text <- function(x) {
  x |>
    iconv(from = "UTF-8", to = "UTF-8") |>        # force proper encoding
    str_replace_all("\u00A0", " ") |>             # non-breaking spaces → normal
    str_replace_all("\u200B", "") |>              # zero-width spaces → remove
    str_replace_all("[\u2018\u2019]", "'") |>     # curly apostrophes → straight
    str_replace_all("[\u201C\u201D]", '"') |>     # curly quotes → straight
    str_replace_all("[\u2013\u2014]", "-") |>     # en/em dash → hyphen
    str_replace_all("…", "...") |>                # ellipsis → 3 dots
    str_replace_all("[[:space:]]+", " ") |>       # collapse spaces
    str_trim()
}

# 2. Regex escaping helper
escape_regex <- function(x) {
  str_replace_all(x, "([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1")
}

# 3. Flexible regex generator
make_flexible_pattern <- function(snippet) {
  words <- unlist(str_split(snippet, "\\s+"))
  # Allow up to 2 "extra" words between snippet words
  regex_str <- paste0(words, collapse = "\\s+(?:\\w+\\s+){0,2}?")
  paste0(regex_str)
}

# 4. Fuzzy matching helper
find_fuzzy_match <- function(snippet, text, max_dist = 5) {
  snippet <- tolower(snippet)
  text <- tolower(text)
  # Sliding windows by words
  words <- str_split(text, " ")[[1]]
  n <- length(str_split(snippet, " ")[[1]])
  for (i in 1:(length(words)-n+1)) {
    window <- paste(words[i:(i+n-1)], collapse = " ")
    if (stringdist(window, snippet, method = "lv") <= max_dist) {
      return(window)
    }
  }
  return(NA)
}

# 5. Tagging function
add_qcoder_tag <- function(text, code_id, snippet) {
  snippet_clean <- normalize_text(snippet)
  pattern <- escape_regex(snippet_clean)
  
  tagged <- FALSE
  updated_text <- text
  
  # --- Try exact match
  if (str_detect(updated_text, fixed(snippet_clean, ignore_case = TRUE))) {
    updated_text <- str_replace_all(
      updated_text,
      fixed(snippet_clean, ignore_case = TRUE),
      paste0("(QCODE)", snippet_clean, "(/QCODE){#", code_id, "}")
    )
    tagged <- TRUE
    method <- "exact"
  }
  
  # --- Try flexible regex if still not tagged
  if (!tagged) {
    flex_pat <- make_flexible_pattern(snippet_clean)
    if (str_detect(updated_text, regex(flex_pat, ignore_case = TRUE))) {
      updated_text <- str_replace_all(
        updated_text,
        regex(flex_pat, ignore_case = TRUE),
        paste0("(QCODE)", snippet_clean, "(/QCODE){#", code_id, "}")
      )
      tagged <- TRUE
      method <- "flexible"
    }
  }
  
  # --- Try fuzzy if still not tagged
  if (!tagged) {
    fuzzy_match <- find_fuzzy_match(snippet_clean, updated_text)
    if (!is.na(fuzzy_match)) {
      updated_text <- str_replace(
        updated_text,
        fixed(fuzzy_match, ignore_case = TRUE),
        paste0("(QCODE)", snippet_clean, "(/QCODE){#", code_id, "}")
      )
      tagged <- TRUE
      method <- "fuzzy"
    }
  }
  
  if (!tagged) {
    method <- "missed"
  }
  
  list(text = updated_text, method = method)
}


########################## Processing 3-Levels Autotagging for FGD ##########################

# Define all transcript filenames in order
fgd_files <- c(
  "Transkrip_FGD 1_JKT_4W_M",
  "Transkrip_FGD 2_JKT_4W_F",
  "Transkrip_FGD 3_JKT_2W_F",
  "Transkrip_FGD 4_JKT_2W_M",
  "Transkrip_FGD 5_JKT_PU_M",
  "Transkrip_FGD 6_JKT_PU_F",
  "Transkrip_FGD 7_MDN_PU_F",
  "Transkrip_FGD 8_MDN_PU_M",
  "Transkrip_FGD 9_MDN_2W_M",
  "Transkrip_FGD 10_MDN_2W_F",
  "Transkrip_FGD 11_MDN_4W_F",
  "Transkrip_FGD 12_MDN_4W_M"
)

# Loop over all FGDs
for (fgd_num in seq_along(fgd_files)) {
  message("Processing ", fgd_files[fgd_num], "...")
  
  # Load codes
  codes <- read.csv(file.path(path_code, "snippet for autotagging.csv")) %>%
    filter(group == paste0("FGD", fgd_num))
  
  # Load transcript
  transcript_path <- file.path(path_doc, paste0(fgd_files[fgd_num], ".txt"))
  
  if (!file.exists(transcript_path)) {
    warning("File not found: ", transcript_path)
    next
  }
  
  transcript_text <- readLines(transcript_path, encoding = "UTF-8", warn = FALSE)
  transcript_text <- paste(transcript_text, collapse = "  ")
  transcript_text <- normalize_text(transcript_text)
  
  # Sort codes by snippet length
  codes <- codes %>%
    mutate(
      snippet = normalize_text(snippet),
      snippet = escape_regex(snippet),
      snippet_length = nchar(snippet)
    ) %>%
    arrange(desc(snippet_length))
  
  
  # Initialize
  working_transcript <- transcript_text
  log_results <- data.frame(
    code_id = character(),
    snippet = character(),
    method = character(),
    stringsAsFactors = FALSE
  )
  
  # Loop over codes
  for (i in seq_len(nrow(codes))) {
    res <- add_qcoder_tag(working_transcript, codes$code_id[i], codes$snippet[i])
    #safe_snippet <- escape_regex(codes$snippet[i])
    #res <- add_qcoder_tag(working_transcript, codes$code_id[i], safe_snippet)
    
    
    working_transcript <- res$text
    log_results <- rbind(
      log_results,
      data.frame(
        code_id = codes$code_id[i],
        snippet = codes$snippet[i],
        method = res$method,
        stringsAsFactors = FALSE
      )
    )
  }
  
  # Write tagged transcript
  path_tagged <- file.path(path_doc, paste0("Tagged ", fgd_files[fgd_num], ".txt"))
  writeLines(working_transcript, path_tagged, useBytes = TRUE)
  
  # Save log
  write.csv(
    log_results,
    file.path(path_memo, paste0("Tagging log_FGD ", fgd_num, ".csv")),
    row.names = FALSE
  )
}


########################## Processing 3-Levels Autotagging for IDI ##########################

# Define all transcript filenames in order
idi_files <- c(
  "Transkrip_IDI 1_JKT_IT_Sewa Mobil",
  "Transkrip_IDI 2_JKT_IT_Bus AKAP",
  "Transkrip_IDI 3_JKT_IL_Mobil Pindahan",
  "Transkrip_IDI 4_MDN_IL_Truk Barang",
  "Transkrip_IDI 5_MDN_IT_Bus Pariwisata"
)

# Loop over all IDIs
for (idi_num in seq_along(idi_files)) {
  message("Processing ", idi_files[idi_num], "...")
  
  # Load codes
  codes <- read.csv(file.path(path_code, "snippet for autotagging.csv")) %>%
    filter(group == paste0("IDI", idi_num))
  
  # Load transcript
  transcript_path <- file.path(path_doc, paste0(idi_files[idi_num], ".txt"))
  
  if (!file.exists(transcript_path)) {
    warning("File not found: ", transcript_path)
    next
  }
  
  transcript_text <- readLines(transcript_path, encoding = "UTF-8", warn = FALSE)
  transcript_text <- paste(transcript_text, collapse = "  ")
  transcript_text <- normalize_text(transcript_text)
  
  # Sort codes by snippet length
  codes <- codes %>%
    mutate(
      snippet = normalize_text(snippet),
      snippet = escape_regex(snippet),
      snippet_length = nchar(snippet)
    ) %>%
    arrange(desc(snippet_length))
  
  
  # Initialize
  working_transcript <- transcript_text
  log_results <- data.frame(
    code_id = character(),
    snippet = character(),
    method = character(),
    stringsAsFactors = FALSE
  )
  
  # Loop over codes
  for (i in seq_len(nrow(codes))) {
    res <- add_qcoder_tag(working_transcript, codes$code_id[i], codes$snippet[i])
    #safe_snippet <- escape_regex(codes$snippet[i])
    #res <- add_qcoder_tag(working_transcript, codes$code_id[i], safe_snippet)
    
    
    working_transcript <- res$text
    log_results <- rbind(
      log_results,
      data.frame(
        code_id = codes$code_id[i],
        snippet = codes$snippet[i],
        method = res$method,
        stringsAsFactors = FALSE
      )
    )
  }
  
  # Write tagged transcript
  path_tagged <- file.path(path_doc, paste0("Tagged ", idi_files[idi_num], ".txt"))
  writeLines(working_transcript, path_tagged, useBytes = TRUE)
  
  # Save log
  write_xlsx(
    log_results,
    file.path(path_memo, paste0("Tagging log_IDI ", idi_num, ".xlsx")),
    #row.names = FALSE
  )
}


###############################################################################

########################## WORK IN QCODER ###################################

#install.packages("devtools")
#devtools::install_github("ropenscilabs/qcoder")
library(qcoder)

import_project_data(project = "4 QCoder")

# Open QCoder
qcode()

# Next steps
## 1. Open qcode and check initial code frequency
## 2. Manual tagging all snippets that is "missed" by tagging log (for all transcripts)
## 3. Check the initial code, is there any codes that can be merged
## 4. Adjust the coding
