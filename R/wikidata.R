# WIKIDATA HELPERS

#' @rdname wikidata
get_item_label.wikidata <- function(item) {
  label_fr <- item[[1]]$labels$fr$value[[1]]
  label_en <- item[[1]]$labels$en$value[[1]]
  label <- ifelse(is.null(label_fr), label_en, label_fr)
  return(label)
}

#' @rdname wikidata
get_item_alias.wikidata <- function(item) {
  a <- c(item[[1]]$aliases$fr$value, get_item_label(item))
  return(a[which.min(nchar(a))][1])
}

#' @rdname wikidata
get_statement_year.wikidata <- function(item, property) {
  tryCatch(
    date <- item[[1]]$claims[[property]]$mainsnak$datavalue$value$time[1],
    error = function(e) date <- NULL
  )
  date <- if (is.null(date)) NA else substr(as.character.Date(date), 2, 5)
  return(date)
}

#' @rdname wikidata
get_statement_list.wikidata <- function(item, property) {
  I(list(item[[1]]$claims[[property]]$mainsnak$datavalue$value$id))
}

#' @rdname wikidata
get_statement_qualifier.wikidata <- function(item, property, qualifier) {
  dates <- item[[1]]$claims[[property]]$qualifiers[[qualifier]]
  if (is.null(dates))
    return(NA)

  l <- unlist(
    lapply(
      X = item[[1]]$claims[[property]]$qualifiers[[qualifier]],
      FUN = function(x) substr(x$datavalue$value$time, 2, 5)
    )
  )
  return(I(list(l)))
}

#' @rdname wikiesr
get_item_status.wikidata <- function(item) {
  item_id <- item[[1]]$id
  instance_of_id <- get_statement_list(item, "P31")[[1]][[1]]

  if (is.null(instance_of_id)) {
    instance_of_id <- "NOID"
    warning(
      "The instance of wikidata item ", item_id, " is not set.\n",
      "* Default level (size of the node) is set to 7.\n",
      "* Please check the property P31 at https://www.wikidata.org/wiki/", item_id,
      call. = FALSE
    )
  }

  if (!(instance_of_id %in% esr_status$id)) {
    it <- WikidataR::get_item(id = instance_of_id)
    label <- get_item_alias(it)

    warning(
      "The instance of wikidata item ", item_id,
      " is unknown by wikidataESR: ", label, ".\n",
      "* Default level (size of the node) is set to 4.\n",
      "* Please check the property P31 at https://www.wikidata.org/wiki/", item_id,
      call. = FALSE
    )

    esr_status <- data.frame(
      id         = instance_of_id,
      label      = label,
      deprecated = TRUE,
      level      = 5,
      wikipedia  = "",
      note       = "statut inexistant dans la base wikidataESR",
      stringsAsFactors = FALSE
    )
  }

  status <- esr_status[esr_status$id == instance_of_id, ]
  if (status$deprecated & getOption("verbose")) {
    note <- ifelse(status$note != "", status$note, "status not specific enough")
    warning(
      "The instance of wikidata item ", item_id,
      " is deprecated: ", status$label,".\n",
      "* Reason is: ", note,".\n",
      "* Please check https://www.wikidata.org/wiki/", item_id,
      call. = FALSE
    )
  }

  return(status)
}
