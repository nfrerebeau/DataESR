#' Status of French Higher Education Institutions
#'
#' A dataset containing the main status of French higher education
#' institutions, together with some additionnal informations.
#' @format A data frame with 6 variables:
#'  \describe{
#'    \item{id}{Wikipedia ID of the item.}
#'    \item{label}{the label of the item.}
#'    \item{deprecated}{whether this item is of recommanded use.}
#'    \item{level}{level of the item (see \code{\link{esr_level}}).}
#'    \item{wikipedia}{URL to the wikipedia notice.}
#'    \item{note}{note to help the user.}
#'  }
#' @source \url{https://www.wikidata.org}
#' @family datasets
#' @keywords datasets
"esr_status"

#' Levels of French ESR Institution Status
#'
#' A dataset containing arbitrary levels to institutions statuts.
#' Used to set sizes when plotting things.
#' @format A data frame with 4 variables:
#'  \describe{
#'   \item{level}{an integer to id the level.}
#'   \item{label}{the label of the level.}
#'   \item{description}{a description of the level.}
#'   \item{example}{some example of status of this level.}
#'  }
#' @family datasets
#' @keywords datasets
"esr_level"

#' List of Dissolved French Universities
#'
#' A dataset containing French universities that disappeared in the 1970s.
#' @format A data frame with 2 variables:
#'  \describe{
#'   \item{id}{A \code{\link{character}} string giving the wikidata id.}
#'   \item{alias}{A \code{\link{character}} string giving the name of the
#'   university.}
#'  }
#' @family datasets
#' @keywords datasets
"esr_dissolved"

#' List of Merged French Universities
#'
#' A dataset containing French universities that were merged in the 2000s-2010s.
#' @format A data frame with 2 variables:
#'  \describe{
#'   \item{id}{A \code{\link{character}} string giving the wikidata id.}
#'   \item{alias}{A \code{\link{character}} string giving the name of the
#'   university.}
#'  }
#' @family datasets
#' @keywords datasets
"esr_merged"
