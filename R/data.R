#' Status of French ESR Institutions
#'
#' A dataset containing the main status of French ESR
#' institutions, together with some additionnal informations.
#'
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
