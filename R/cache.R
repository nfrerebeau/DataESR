# Create an empty cache environment
esr_cache <- new.env()

#' Package Cache
#'
#' @param file A \code{\link{connection}} or a \code{\link{character}} string
#'  giving the name of the file where the data will be saved or the name of the
#'  file to load (see \code{\link{save}} and \code{\link{load}}).
#' @param overwrite_status A \code{\link{logical}} scalar: should the status
#'  informations be overwriten by the embeded data (default to \code{FALSE}).
#' @details
#'  To improve the performances, wikidata items are cached.
#'  Must be used whenever the data of wikidata are modified during a R session.
#' @return The cache \code{\link{environment}}, invisibly.
#' @example inst/examples/ex-cache.R
#' @author Julien Gossa, Nicolas Frerebeau
#' @name cache
#' @rdname cache
NULL

#' @rdname cache
#' @export
wdesr_clear_cache <- function() {
  rm(list = ls(), envir = esr_cache)
  assign("status", value = WikidataESR::esr_status, envir = esr_cache)
  invisible(NULL)
}

#' @rdname cache
#' @export
wdesr_save_cache <- function(file) {
  save(list = ls(all.names = TRUE), envir = esr_cache, file = file)
  invisible(NULL)
}

#' @rdname cache
#' @export
wdesr_load_cache <- function(file, overwrite_status = FALSE) {
  load(file = file, envir = esr_cache)
  if (overwrite_status)
    assign("status", value = WikidataESR::esr_status, envir = esr_cache)
  invisible(NULL)
}
