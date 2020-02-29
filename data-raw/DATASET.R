## code to prepare `DATASET` dataset goes here
esr_status <- read.table(
  "./data-raw/esr_status.csv",
  header = TRUE, row.names = NULL,
  sep = ",", quote = "\"",
  fileEncoding = "UTF-8", encoding = "UTF-8",
  stringsAsFactors = FALSE
)
esr_status$deprecated <- as.logical(esr_status$deprecated)

esr_level <- read.table(
  "./data-raw/esr_level.csv",
  header = TRUE, row.names = NULL,
  sep = ",", quote = "\"",
  fileEncoding = "UTF-8", encoding = "UTF-8",
  stringsAsFactors = FALSE
)

usethis::use_data(esr_status, esr_level, overwrite = FALSE, internal = FALSE)
