##################################### Meta ######################################

# Project Tron - Baseline study
# Code for tagging initial thematic codes from NotebookLM to FGD and IDI transcript quantitative raw data to be published in LabNarasi (data platform)
# September 25, 2025
# Contributors : Azzah

# Reference for QCoder: https://ropenscilabs.github.io/qcoder/


############################ Environment setup #################################


# Clear console and environment 
graphics.off(); rm(list=ls());cat("\14");

# Load packages
install.packages("pacman") # install the package if you haven't 
pacman::p_load(rmarkdown, knitr, dplyr, readxl, writexl, data.table, haven)

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



#### Install QCoder for QDA ####

install.packages("devtools")
devtools::install_github("ropenscilabs/qcoder")
library(qcoder)

# Creating blank project
create_qcoder_project("4 QCoder", sample = FALSE)
import_project_data(project = "4 QCoder")

# Preparing location
path_code = file.path(getwd(),"4 QCoder","codes")
path_doc = file.path(getwd(),"4 QCoder","documents")
path_pdf = file.path(getwd(), "1 Transcript (PDF)")

code_path

############################ Autotagging IDI 1 #################################
## Notes: ini belum berhasil ngetag semua, kayanya ada masalah sama case sensitive.

# Read your code data from the CSV file
codes <- read.csv(file.path(path_code, "Gemini_Code snippet - IDI 1.csv"))

# Read your raw transcript text file
transcript_path <- file.path(path_doc, "[Tron] Transcript_IDI 1_Jakarta_Industri Transportasi Penumpang_Fokus Transportasi.txt")
transcript_text <- readLines(transcript_path, encoding = "UTF-8", warn = FALSE)

# Collapse the transcript into a single string.
# Use two spaces to replace original line breaks, which helps with quotes spanning multiple lines.
transcript_text <- paste(transcript_text, collapse = "  ")

# Function to escape special regex characters and handle quotation marks
clean_snippet <- function(snippet) {
  # Remove leading/trailing quotes and trim whitespace
  cleaned <- str_remove_all(snippet, '^"|"$')
  cleaned <- str_trim(cleaned)
  # Escape any special regex characters to ensure a literal match
  return(str_escape(cleaned))
}

# Add QCoder tags around matched snippets in the transcript
add_qcoder_tags <- function(text, code_id, snippet) {
  # Clean the snippet to make it a literal search pattern
  pattern <- clean_snippet(snippet)
  add_qcoder_tags <- function(text, code_id, snippet) {
    # Clean the snippet to make it a literal search pattern
    pattern <- clean_snippet(snippet)
    
    # Check if the snippet contains a forward slash to denote a break in the quote.
    # The original quote may have been split from a single line in the transcript.
    if (str_detect(pattern, "/")) {
      # Replace the slash with ".*?" to allow for any characters (including line breaks)
      # between the two parts of the quote in the transcript.
      pattern <- str_replace_all(pattern, "/", ".*?")
      # Use perl=TRUE for non-greedy matching with ".*?"
      tagged_text <- str_replace_all(text, pattern, paste0("(QCODE)", snippet, "(/QCODE){#", code_id, "}"), perl = TRUE)
    } else {
      # If no slash, use a simple, case-insensitive, fixed pattern match
      tagged_text <- str_replace_all(text, fixed(pattern, ignore_case = TRUE), paste0("(QCODE)", snippet, "(/QCODE){#", code_id, "}"))
    }
    return(tagged_text)
  }
}  

add_qcoder_tags <- function(text, code_id, snippet) {
  # Clean the snippet to make it a literal search pattern
  pattern <- clean_snippet(snippet)
  
  # Check if the snippet contains a forward slash to denote a break in the quote.
  # The original quote may have been split from a single line in the transcript.
  if (str_detect(pattern, "/")) {
    # Replace the slash with ".*?" to allow for any characters (including line breaks)
    # between the two parts of the quote in the transcript.
    pattern <- str_replace_all(pattern, "/", ".*?")
    # Use perl=TRUE for non-greedy matching with ".*?"
    tagged_text <- str_replace_all(text, pattern, paste0("(QCODE)", snippet, "(/QCODE){#", code_id, "}"))
  } else {
    # If no slash, use a simple, case-insensitive, fixed pattern match
    tagged_text <- str_replace_all(text, fixed(pattern, ignore_case = TRUE), paste0("(QCODE)", snippet, "(/QCODE){#", code_id, "}"))
  }
  return(tagged_text)
}

# ---

# Loop over codes and tag transcript text. It's better to sort the snippets by length
# in descending order to avoid tagging a smaller snippet that is part of a larger one first.
codes$snippet_length <- nchar(codes$snippet)
codes <- codes[order(-codes$snippet_length),]

# Initialize a new variable to ensure a clean start for the tagging process.
working_transcript <- transcript_text
for (i in seq_len(nrow(codes))) {
  working_transcript <- add_qcoder_tags(working_transcript, codes$code_id[i], codes$snippet[i])
}

# Write out the tagged transcript for QCoder import
path_tagged <- file.path(path_doc, "[Tron] Transcript tagged_IDI 1.txt")
writeLines(working_transcript, path_tagged, useBytes = TRUE)


###############################################################################

library(stringr)
library(dplyr)

# Function to escape regex safely
escape_regex <- function(x) {
  str_replace_all(x, "([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1")
}

clean_snippet <- function(snippet) {
  cleaned <- str_remove_all(snippet, '^"|"$') |> str_trim()
  escape_regex(cleaned)
}

add_qcoder_tags <- function(text, code_id, snippet) {
  pattern <- clean_snippet(snippet)
  
  if (str_detect(snippet, "/")) {
    # Handle break marker by replacing with regex
    pattern <- str_replace_all(pattern, "/", ".*?")
    tagged_text <- str_replace_all(
      text,
      regex(pattern, ignore_case = TRUE),
      paste0("(QCODE)", snippet, "(/QCODE){#", code_id, "}")
    )
  } else {
    tagged_text <- str_replace_all(
      text,
      fixed(snippet, ignore_case = TRUE),
      paste0("(QCODE)", snippet, "(/QCODE){#", code_id, "}")
    )
  }
  
  tagged_text
}

# --- Main tagging loop ---
codes <- codes %>%
  mutate(snippet_length = nchar(snippet)) %>%
  arrange(desc(snippet_length))

working_transcript <- transcript_text
for (i in seq_len(nrow(codes))) {
  working_transcript <- add_qcoder_tags(
    working_transcript,
    codes$code_id[i],
    codes$snippet[i]
  )
}

# Write tagged transcript
path_tagged <- file.path(path_doc, "[Tron] Transcript tagged_IDI 1.txt")
writeLines(working_transcript, path_tagged, useBytes = TRUE)


## Debugging
for (i in seq_len(nrow(codes))) {
  if (!str_detect(working_transcript, fixed(codes$snippet[i], ignore_case = TRUE))) {
    cat("❌ Not found:", codes$snippet[i], "\n")
  }
}


############################ [TRY AGAIN 3-LEVELS] Autotagging IDI 1 #################################

library(stringr)
library(dplyr)
library(stringdist)

# --------------------------
# 1. Normalization function
# --------------------------
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

# --------------------------
# 2. Regex escaping helper
# --------------------------
escape_regex <- function(x) {
  str_replace_all(x, "([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1")
}

# --------------------------
# 3. Flexible regex generator
# --------------------------
make_flexible_pattern <- function(snippet) {
  words <- unlist(str_split(snippet, "\\s+"))
  # Allow up to 2 "extra" words between snippet words
  regex_str <- paste0(words, collapse = "\\s+(?:\\w+\\s+){0,2}?")
  paste0(regex_str)
}

# --------------------------
# 4. Fuzzy matching helper
# --------------------------
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

# --------------------------
# 5. Tagging function
# --------------------------
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

# --------------------------
# 6. Main process
# --------------------------

# Load codes
codes <- read.csv(file.path(path_code, "Gemini_Code snippet - IDI 1.csv"))

# Load transcript (txt)
transcript_path <- file.path(
  path_doc,
  "[Tron] Transcript_IDI 1_Jakarta_Industri Transportasi Penumpang_Fokus Transportasi.txt"
)
transcript_text <- readLines(transcript_path, encoding = "UTF-8", warn = FALSE)
transcript_text <- paste(transcript_text, collapse = "  ")
transcript_text <- normalize_text(transcript_text)


# Alternative load transcript from docx
install.packages("readtext")
library(readtext)

transcript_path <- file.path(
  path_doc,
  "[Tron] Transcript_IDI 1_Jakarta_Industri Transportasi Penumpang_Fokus Transportasi.docx"
  )
doc <- readtext(transcript_path)
transcript_text <- doc$text

install.packages("officer")
library(officer)

transcript_path <- file.path(
  path_doc,
  "[Tron] Transcript_IDI 1_Jakarta_Industri Transportasi Penumpang_Fokus Transportasi.docx"
)

doc <- read_docx(transcript_path)

# Extract paragraphs
transcript_text <- docx_summary(doc)$text
transcript_text <- paste(transcript_text, collapse = "  ")


# Sort codes by snippet length
codes <- codes %>%
  mutate(snippet = normalize_text(snippet),
         snippet_length = nchar(snippet)) %>%
  arrange(desc(snippet_length))

# Initialize
working_transcript <- transcript_text
log_results <- data.frame(
  code_id = character(),
  snippet = character(),
  method = character(),
  stringsAsFactors = FALSE
)

# Loop
install.packages("stringdist")
library(stringdist)

for (i in seq_len(nrow(codes))) {
  res <- add_qcoder_tag(working_transcript, codes$code_id[i], codes$snippet[i])
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

# Write output
path_tagged <- file.path(path_doc, "[Tron] Transcript tagged_IDI 1.txt")
writeLines(working_transcript, path_tagged, useBytes = TRUE)

# Save log so you know what happened
write.csv(log_results, file.path(path_doc, "tagging_log_IDI 1.csv"), row.names = FALSE)


###############################################################################

## Open QCoder
qcode()
