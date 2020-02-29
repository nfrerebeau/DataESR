#' Load Data of a University
#'
#' @param id A \code{\link{character}} string specifying the the wikidata id of
#'  the university.
#' @param verbose A \code{\link{logical}} scalar: should a diagnostic message be
#'  generated?
#' @return A \code{\link{data.frame}}.
#' @seealso \link{wdesr_get_data}
#' @example inst/examples/ex-get.R
#' @references
#'  \href{https://www.wikidata.org}{Wikidata}.
#' @author J. Gossa, N. Frerebeau
#' @keywords internal
wdesr_get_item <- function(id, verbose = getOption("verbose")) {

  if (verbose) message("Loading ", id, "...")
  item <- WikidataR::get_item(id = id)
  status <- get_item_status(item)

  data <- data.frame(
    id                  = id,
    label               = get_item_label(item),
    alias               = get_item_alias(item),
    status              = status$label,
    level               = status$level,

    inception           = get_statement_year(item, "P571"),
    dissolved           = get_statement_year(item, "P576"),

    has_part            = get_statement_list(item, "P527"),
    part_of             = get_statement_list(item, "P361"),

    subsidiary          = get_statement_list(item, "P355"),
    parent_organization = get_statement_list(item, "P749"),

    replaces            = get_statement_list(item, "P1365"),
    replaces_pit        = get_statement_qualifier(item, "P1365", "P585"),
    replaced_by         = get_statement_list(item, "P1366"),
    replaced_by_pit     = get_statement_qualifier(item, "P1366", "P585"),

    separated_from      = get_statement_list(item, "P807"),
    separated_from_pit  = get_statement_qualifier(item, "P807", "P585"),

    member_of           = get_statement_list(item, "P463"),
    stringsAsFactors    = FALSE
  )
  return(data)
}
mdesr_get_item <- memoise::memoise(wdesr_get_item)
mdesr_clear_cache <- function() memoise::forget(mdesr_get_item)

#' Load Data of a Set of Universities
#'
#' @param id A \code{\link{character}} vector specifying the wikidata IDs
#'  to be accessed.
#' @param simplify A \code{\link{logical}} scalar: should the result be
#'  simplified to a data frame?
#' @param verbose A \code{\link{logical}} scalar: should diagnostic messages be
#'  generated?
#' @return If \code{simplify} is \code{TRUE}, returns a
#'  \code{\link{data.frame}}, else returns a \code{\link{list}}.
#' @example inst/examples/ex-get.R
#' @references
#'  \href{https://www.wikidata.org}{Wikidata}.
#' @author J. Gossa, N. Frerebeau
#' @export
wdesr_get_data <- function(id, simplify = TRUE,
                           verbose = getOption("verbose")) {
  items <- lapply(X = id, FUN = mdesr_get_item, verbose = verbose)
  if (simplify) items <- do.call(rbind.data.frame, items)
  return(items)
}

#' Get a Graph of Universities
#'
#' From a root wikidata id, the function follows a given set of properties,
#' building vertice and edges along the way.
#' @param id A \code{\link{character}} string specifying the wikidata id of the
#'  root.
#' @param property A \code{\link{character}} vector specifying the set of
#'  properties to follow (see details).
#' @param depth An \code{\link{integer}} giving the depth of the graph
#'  (default to 3).
#' @param active_only A \code{\link{logical}} scalar: should dissolved
#'  universities be filtered? If \code{TRUE}, dissolved universities are
#'  ignored.
#' @param stop_at A \code{\link{character}} vector specifying types of nodes
#'  that must not be visited furthermore (default to "\code{EPST}").
#' @param verbose A \code{\link{logical}} scalar: should diagnostic messages be
#'  generated?
#' @details
#' Allowed \code{property} values:
#' \describe{
#'  \item{inception}{Wikidata property
#'   "\href{http://www.wikidata.org/wiki/Property:P571}{P571}".}
#'  \item{dissolved}{Wikidata property
#'   "\href{http://www.wikidata.org/wiki/Property:P576}{P576}".}
#'  \item{has_part}{Wikidata property
#'   "\href{http://www.wikidata.org/wiki/Property:P527}{P527}".}
#'  \item{part_of}{Wikidata property
#'   "\href{http://www.wikidata.org/wiki/Property:P361}{P361}".}
#'  \item{subsidiary}{Wikidata property
#'   "\href{http://www.wikidata.org/wiki/Property:P355}{P355}".}
#'  \item{parent_organization}{Wikidata property
#'   "\href{http://www.wikidata.org/wiki/Property:P749}{P749}."}
#'  \item{replaces}{Wikidata property
#'   "\href{http://www.wikidata.org/wiki/Property:P1365}{P1365}".}
#'  \item{replaces_pit}{Wikidata properties
#'   "\href{http://www.wikidata.org/wiki/Property:P1365}{P1365}" and
#'   "\href{http://www.wikidata.org/wiki/Property:P585}{P585}"
#'   (point in time).}
#'  \item{replaced_by}{Wikidata property
#'   "\href{http://www.wikidata.org/wiki/Property:P1366}{P1366}".}
#'  \item{replaced_by_pit}{Wikidata properties
#'   "\href{http://www.wikidata.org/wiki/Property:P1366}{P1366}" and
#'   "\href{http://www.wikidata.org/wiki/Property:P585}{P585}"
#'   (point in time).}
#'  \item{separated_from}{Wikidata property
#'   "\href{http://www.wikidata.org/wiki/Property:P807}{P807}".}
#'  \item{separated_from_pit}{Wikidata properties
#'   "\href{http://www.wikidata.org/wiki/Property:P807}{P807}" and
#'   "\href{http://www.wikidata.org/wiki/Property:P585}{P585}"
#'   (point in time).}
#'  \item{member_of}{Wikidata property
#'   "\href{http://www.wikidata.org/wiki/Property:P463}{P463}".}
#' }
#' @return A \code{\link{list}} of edges and vertices.
#' @references
#'  \href{https://www.wikidata.org}{Wikidata}.
#' @example inst/examples/ex-graph.R
#' @author J. Gossa, N. Frerebeau
#' @export
wdesr_get_graph <- function(id, property, depth = 3,
                            active_only = FALSE, stop_at = c("EPST"),
                            verbose = getOption("verbose")) {

  wgge <- new.env()
  wgge$edges <- data.frame(
    from = character(),
    to = character(),
    stringsAsFactors = FALSE
  )
  wgge$vertices <- data.frame()

  wdesr_get_subgraph(wgge, id = id, property = property,
                     depth = depth, active_only = active_only,
                     stop_at = stop_at, verbose = verbose)

  wgge$vertices <- wgge$vertices[order(wgge$vertices$id), ]
  clean <- lapply(X = wgge$vertices,
                  FUN = function(x) if (is.list(x)) as.character(x) else x)
  wgge$vertices <- as.data.frame(clean, stringsAsFactors = FALSE)

  res <- structure(
    list(edges = wgge$edges, vertices = wgge$vertices),
    class = "esr_graph"
  )
  return(res)
}

#' Get a Sub-Graph of Universities
#'
#' @param wgge An \code{\link{environment}}.
#' @inheritParams wdesr_get_graph
#' @return A \code{\link{list}} of edges and vertices.
#' @seealso \code{\link{wdesr_get_graph}}
#' @author Julien Gossa
#' @keywords internal
#' @noRd
wdesr_get_subgraph <- function(wgge, id, property, depth = 3,
                               active_only = FALSE, stop_at = c("EPST"),
                               verbose = getOption("verbose")) {

  from <- wdesr_get_data(id, simplify = TRUE, verbose = verbose)
  wgge$vertices <- rbind(wgge$vertices, from)

  props <- property[property %in% colnames(from)]
  if (length(props) == 0)
    # TODO: better error message
    stop("Invalid properties: ", paste(property, collapse = ", "),
         call. = FALSE)

  for(p in props) {
    ppit <- paste(p, "pit", sep = "_")
    to <- wdesr_get_data(unlist(from[, p]), verbose = verbose)
    # Remove dissolved
    if (active_only) to <- to[is.na(to$dissolution), ]
    # Remove existing to -> from edges
    tmp <- wgge$edges[wgge$edges$to == id, ]$from
    to <- to[!(to$id %in% tmp), ]
    # Skip if empty
    if (nrow(to) == 0) next()

    edges <- data.frame(
      from  = from$id,
      to    = to$id,
      type  = p,
      date  = ifelse(ppit %in% colnames(from), unlist(from[, ppit]), NA),
      depth = depth
    )
    wgge$edges <- rbind(wgge$edges, edges)

    if(depth == 1) {
      wgge$vertices <- rbind(
        wgge$vertices,
        to[!(to$id %in% wgge$vertices$id), ]
      )
    } else {
      wgge$vertices <- rbind(
        wgge$vertices,
        to[!(to$id %in% wgge$vertices$id) && to$status %in% stop_at, ]
      )
      for(id in to[!(to$status %in% stop_at), ]$id) {
        if (!(id %in% wgge$vertices$id))
          wdesr_get_subgraph(wgge, id = id, property = property,
                             depth = depth-1, active_only = active_only,
                             stop_at = stop_at, verbose = verbose)
      }
    }
  }
}
