#' Getters for Wikidata
#'
#' \code{get_item_label} gets the label of a \code{wikidata} item.
#'
#' \code{get_item_alias} gets the alias of a \code{wikidata} item.
#'
#' \code{get_statement_year} gets the year from a statement of a
#' \code{wikidata} item.
#'
#' \code{get_statement_list} gets the list of statements for a given
#' property.
#'
#' \code{get_statement_qualifier} gets the qualifiers of statements from a
#' \code{wikidata} item.
#' @param item An object of class \code{\link[WikidataR]{WikidataR}}.
#' @param property A \code{\link{character}} string giving the property to get
#'  the data from.
#' @param qualifier A \code{\link{character}} string giving the qualifier to
#'  be retrieved.
#' @return
#'  \code{get_item_label} returns a \code{\link{character}} string
#'  (look for french label first, and return english otherwise).
#'
#'  \code{get_item_alias} returns a \code{\link{character}} string
#'  (look for shortest french label/alias).
#'
#'  \code{get_statement_year} returns a \code{\link{character}} string giving
#'  the year stated in the item.
#'
#'  \code{get_statement_list} returns a \code{\link{list}} of statements.
#'
#'  \code{get_statement_list} returns a \code{\link{list}} of item statement
#'  qualifiers.
#' @example inst/examples/ex-wikidata.R
#' @seealso \code{\link[WikidataR]{WikidataR}}
#' @author J. Gossa, N. Frerebeau
#' @name wikidata
#' @rdname wikidata
#' @keywords internal
NULL

#' @rdname wikidata
get_item_label <- function(item, ...)
  UseMethod("get_item_label")

#' @rdname wikidata
get_item_alias <- function(item, ...)
  UseMethod("get_item_alias")

#' @rdname wikidata
get_statement_year <- function(item, ...)
  UseMethod("get_statement_year")

#' @rdname wikidata
get_statement_list <- function(item, ...)
  UseMethod("get_statement_list")

#' @rdname wikidata
get_statement_qualifier <- function(item, ...)
  UseMethod("get_statement_qualifier")

# ==============================================================================
#' Get the ESR Status of a wikidata Item
#'
#' \code{get_item_status} gets the *legal status* of an ESR institution.
#' @param item An object of class \code{\link[WikidataR]{WikidataR}}.
#' @note It is based on the instance_of property (P31) and converted thanks to a
#' local dataset.
#' @return The status the item.
#' @example inst/examples/ex-wikidata-esr.R
#' @author J. Gossa, N. Frerebeau
#' @keywords internal
#' @name wikiesr
#' @rdname wikiesr
get_item_status <- function(item, ...) UseMethod("get_item_status")
