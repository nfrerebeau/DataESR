## code to prepare `DATASET` dataset goes here
esr_status <- read.table(
  "./data-raw/esr_status.csv",
  header = TRUE, row.names = NULL,
  sep = ",", quote = "\"",
  encoding = "utf-8", stringsAsFactors = FALSE
)
esr_status$keep <- as.logical(esr_status$keep)

esr_level <- read.table(
  "./data-raw/esr_level.csv",
  header = TRUE, row.names = NULL,
  sep = ",", quote = "\"",
  encoding = "utf-8", stringsAsFactors = FALSE
)

usethis::use_data(esr_status, esr_level, overwrite = TRUE, internal = FALSE)