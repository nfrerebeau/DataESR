## code to prepare `DATASET` dataset goes here
esr_status <- read.table(
  "./data-raw/esr_status.csv",
  header = TRUE, row.names = NULL,
  sep = ",", quote = "\"",
  fileEncoding = "UTF-8", encoding = "UTF-8",
  stringsAsFactors = FALSE
)
esr_status$id <- factor(esr_status$id)
esr_status$label <- factor(esr_status$label)
esr_status$deprecated <- as.logical(esr_status$deprecated)
esr_level$level <- factor(esr_level$level)

esr_level <- read.table(
  "./data-raw/esr_level.csv",
  header = TRUE, row.names = NULL,
  sep = ",", quote = "\"",
  fileEncoding = "UTF-8", encoding = "UTF-8",
  stringsAsFactors = FALSE
)
esr_level$level <- factor(esr_level$level)
esr_level$label <- factor(esr_level$label, levels = esr_level$label)

usethis::use_data(esr_status, esr_level, overwrite = FALSE, internal = FALSE)
