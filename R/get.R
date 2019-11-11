#' Load the data of one university.
#'
#' @param id A \code{\link{character}} string specifying the the wikidata id of
#' the university.
#' @return A \code{\link{data.frame}}.
#' @examples wdesr_get_item("Q61716176")
#' @references
#'  \href{https://www.wikidata.org}{wikidata}
#' @author J. Gossa, N. Frerebeau
#' @noRd
wdesr_get_item <- function(id) {

  item <- WikidataR::get_item(id = id)
  status <- get_item_status(item)

  data <- data.frame(
    id               = id,
    label            = get_item_label(item),
    alias            = get_item_alias(item),
    statut           = status$label,
    level            = status$level,

    fondation        = get_statement_year(item, "P571"),
    dissolution      = get_statement_year(item, "P576"),

    associé          = get_statement_list(item, "P527"),
    associé_de       = get_statement_list(item, "P361"),

    composante       = get_statement_list(item, "P355"),
    composante_de    = get_statement_list(item, "P749"),

    prédécesseur     = get_statement_list(item, "P1365"),
    prédécesseur_pit = get_statement_qualifier(item, "P1365", "P585"),
    successeur       = get_statement_list(item, "P1366"),
    successeur_pit   = get_statement_qualifier(item, "P1366", "P585"),

    séparé_de        = get_statement_list(item, "P807"),
    séparé_de_pit    = get_statement_qualifier(item, "P807", "P585"),

    membre_de        = get_statement_list(item, "P463"),
    stringsAsFactors = FALSE
  )
  return(data)
}
mdesr_get_item <- memoise::memoise(wdesr_get_item)
wdesr_clear_cache <- function() memoise::forget(mdesr_get_item)

#' Load the data of a set of universities.
#'
#' @param id A \code{\link{character}} vector specifying the wikidata IDs
#'  to be accessed.
#' @param simplify A \code{\link{logical}} scalar: should the result be
#' simplified to a vector, matrix or higher dimensional array if possible?
#' @return A \code{\link{dataframe}}.
#' @examples r <- wdesr_get_data(c("Q3551576", "Q2013017"))
#' @seealso \link{wdesr_get_item}
#' @references
#'  \href{https://www.wikidata.org}{wikidata}
#' @author J. Gossa, N. Frerebeau
#' @noRd
wdesr_get_data <- function(id, simplify = TRUE) {
  items <- lapply(X = id, FUN = mdesr_get_item)
  if (simplify) items <- do.call(rbind.data.frame, items)
  # paste0("Loading: ", wdid)
  return(items)
}

#' Get a graph of universities.
#'
#' From a root wikipedia id, the function follows a given set of properties,
#' building vertice and edges along the way.
#'
#' Data are cached: use \code{\link{wdesr_clear_cache}} to refresh data from wikidata.
#'
#' @param id The wikidata id of the root.
#' @param property The set of properties to follow.
#' @param depth The depth of the graph (more or less) (default to 3).
#' @param active_only TRUE to filter dissolved universities (default to FALSE).
#' @param stop_at A list of type of nodes that must not be visited furthermore
#' (default to "EPST").
#' @return A \code{\link{list}} of edges and vertices.
#' @examples
#' g <- wdesr_get_graph("Q61716176", c('composante','associé'), 1)
#' g$edges
#' g$vertice
#' @author J. Gossa, N. Frerebeau
#' @export
wdesr_get_graph <- function(id, property, depth = 3, active_only = FALSE,
                            stop_at = c("EPST") ) {

  wgge <- new.env()
  wgge$edges <- data.frame(
    from = character(),
    to = character(),
    stringsAsFactors = FALSE
  )
  wgge$vertices <- data.frame()

  wdesr_get_subgraph(wgge, id, property, depth, active_only, stop_at)

  wgge$vertices <- wgge$vertices[order(wgge$vertices$id), ]
  clean <- lapply(X = wgge$vertices,
                  FUN = function(x) if (is.list(x)) as.character(x) else x)
  wgge$vertices <- as.data.frame(clean, stringsAsFactors = FALSE)
  # wgge$vertices <- wgge$vertices %>% dplyr::mutate_if(is.list, as.character) %>%
    # dplyr::arrange(id)
  #wgge$vertices$niveau <- factor(wgge$vertices$niveau, levels = wdesr.niveaux$niveau)

  res <- structure(
    list(edges = wgge$edges, vertices = wgge$vertices),
    class = "esr_graph"
  )
  return(res)
}

#' Get a sub graph of universities.
#' @return A list of edges and vertices.
#' @noRd
#'
#' @seealso \code{\link{wdesr_get_graph}}
#' @author Julien Gossa, \email{gossa@unistra.fr}
wdesr_get_subgraph <- function(wgge, id, property, depth = 3,
                               active_only = FALSE, stop_at = c("EPST") ) {

  from <- wdesr_get_data(id)
  #df.from$depth <- depth
  wgge$vertices <- rbind(wgge$vertices, from)

  #print(wgge$vertices$id)
  #print(wgge$vertices[, 1:2])

  props <- property[property %in% colnames(from)]
  if (length(props) == 0)
    # TODO: better error message
    stop("Invalid properties.")

  for(p in props) {
    ppit <- paste(p, "pit", sep = "_")
    to <- wdesr_get_data(unlist(from[, p]))
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

    #df.to$depth <- depth - 1
    if(depth == 1) {
      wgge$vertices <- rbind(
        wgge$vertices,
        subset(to, !id %in% wgge$vertices$id)
      )
    } else {
      wgge$vertices <- rbind(
        wgge$vertices,
        subset(to, !id %in% wgge$vertices$id && statut %in% stop_at)
      )
      for(id in subset(to, !statut %in% stop_at)$id) {
        if (!id %in% wgge$vertices$id)
          wdesr_get_subgraph(wgge, id, property, depth-1, active_only, stop_at)
      }
    }
  }
}
