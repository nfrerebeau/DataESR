#' Load the data of one university.
#'
#' @param id The wikidata id of the university.
#' @return A \code{\link{data.frame}}.
#' @examples wdesr_load_item("Q3551576")
#' @references
#'  \href{https://www.wikidata.org}{wikidata}
#' @author J. Gossa, N. Frerebeau
#' @noRd
wdesr_load_item <- function(id) {

  item <- WikidataR::get_item(id = id)
  status <- get_item_status(item)

  data <- data.frame(
    id               = wdid,
    label            = get_item_label(item),
    alias            = get_item_alias(item),
    statut           = status$libellé,
    niveau           = status$niveau,

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

#' Load the data of a set of universities.
#'
#' @param wdids A set of wikidata ids.
#' @return A dataframe with the data of the universities.
#' @examples wdesr_load_items(c("Q3551576","Q2013017"))
#' @references
#'  \href{https://www.wikidata.org}{wikidata}
#' @author J. Gossa, N. Frerebeau
#' @noRd
wdesr_load_items <- function(wdids) {
  for(subids in wdids) {
    for(wdid in subids) {
      print(paste("Loading: ",wdid))
      r <- wdesr_load_item(wdid)
      wdesr.cache$items <- rbind(wdesr.cache$items,r)
    }
  }
}

#' Loader of universities data.
#'
#' Get the data of a set of universities.
#'
#' Data are cached: use \code{\link{wdesr_clear_cache}} to refresh data from wikidata.
#'
#' @param wdids A set of wikidata ids.
#'
#' @return A dataframe with the data of the universities.
#' @export
#'
#' @examples items <- wdesr_get_data(c("Q3551576","Q2013017"))
#'
#' @references
#' - \url{https://github.com/juliengossa/DataESR/tree/master/etablissements.esr/wikidataESR}
#' - \url{https://www.wikidata.org}
#' @seealso \code{\link{wdesr_clear_cache}}
#' @author Julien Gossa, \email{gossa@unistra.fr}
wdesr_get_data <- function(wdids) {
  if (is.null(wdesr.cache$status)) {
    wdesr_clear_cache()
  }

  wdesr_load_items(wdids[! wdids %in% wdesr.cache$items$id])

  return(subset(wdesr.cache$items, id %in% wdids))
}


#' Get a graph of universities.
#'
#' From a root wikipedia id, the function follows a given set of properties,
#' building vertice and edges along the way.
#'
#' Data are cached: use \code{\link{wdesr_clear_cache}} to refresh data from wikidata.
#'
#' @param wdid The wikidata id of the root.
#' @param props The set of properties to follow.
#' @param depth The depth of the graph (more or less) (default to 3).
#' @param active_only TRUE to filter dissolved universities (default to FALSE).
#' @param stop_at A list of type of nodes that must not be visited furthermore (default to "EPST").
#'
#' @return A list of edges and vertices.
#' @export
#'
#' @examples
#' g <- wdesr_get_graph("Q61716176",c('composante','associé'), 1)
#' g$edges
#' g$vertice
#'
#' @references
#' - \url{https://github.com/juliengossa/DataESR/tree/master/etablissements.esr/wikidataESR}
#' - \url{https://www.wikidata.org}
#' @seealso \code{\link{wdesr_clear_cache}}
#' @author Julien Gossa, \email{gossa@unistra.fr}
wdesr_get_graph <- function(wdid, props, depth = 3, active_only = FALSE, stop_at = c("EPST") ) {

  wgge <- new.env()
  wgge$edges <- data.frame(from=character(),to=character(),stringsAsFactors = FALSE)
  wgge$vertices <- data.frame()

  wdesr_get_subgraph(wgge, wdid, props, depth, active_only, stop_at)

  wgge$vertices <- wgge$vertices %>% mutate_all(as.character) %>% arrange(id)
  #wgge$vertices$niveau <- factor(wgge$vertices$niveau, levels = wdesr.niveaux$niveau)

  res <- list('edges'=wgge$edges, 'vertices'=wgge$vertices)

  return(res)
}

#' Get a sub graph of universities.
#' @return A list of edges and vertices.
#' @noRd
#'
#' @seealso \code{\link{wdesr_get_graph}}
#' @author Julien Gossa, \email{gossa@unistra.fr}
wdesr_get_subgraph <- function(wgge, wdid, props, depth = 3, active_only = FALSE, stop_at = c("EPST") ) {

  df.from <- wdesr_get_data(c(wdid))

  #df.from$depth <- depth
  wgge$vertices <- rbind(wgge$vertices, df.from)

  #print(wdid)
  #print(wgge$vertices$id)
  #print(wgge$vertices[,1:2])

  for(p in props) {
    if(is.null(unlist(df.from[,p])))
      next()

    ppit <- paste(p,'_pit',sep='')

    df.to <- wdesr_get_data(unlist(df.from[,p]))

    # Remove dissolved
    if (active_only) df.to <- subset(df.to, is.na(dissolution))

    # Remove existing to -> from edges
    tmp <- subset(wgge$edges, to == wdid)$from
    df.to <- subset(df.to, !id %in% tmp)

    if (nrow(df.to) == 0) next()

    edges <- data.frame(
      from  = df.from$id,
      to    = df.to$id,
      type  = p,
      date  = ifelse(ppit %in% colnames(df.from),unlist(df.from[,ppit]),NA),
      depth = depth
      )
    wgge$edges <- rbind(wgge$edges,edges)

    #df.to$depth <- depth - 1

    if(depth==1) {
      wgge$vertices <- rbind(wgge$vertices, subset(df.to, !id %in% wgge$vertices$id))
    } else {
      wgge$vertices <- rbind(wgge$vertices, subset(df.to, !id %in% wgge$vertices$id & statut %in% stop_at))

      for(id in subset(df.to, !statut %in% stop_at)$id) {
        if (!id %in% wgge$vertices$id)
          wdesr_get_subgraph(wgge, id,props,depth-1,active_only,stop_at)
      }
    }
  }
}

#' Label builder.
#'
#' Build the label of the nodes.
#'
#' @param node_label Either "alias", "alias_date", "long", or "long_date" (default to "alias").
#' @param alias The alias of the item.
#' @param label The label of the item.
#' @param fondation The foundation date of the item.
#' @param dissolution The dissolution date of the item.
#'
#' @return a label for a node, as a string
#'
#' @examples node_label_aes("alias", alias, label, fondation, dissolution)
#' @references
#' - \url{https://github.com/juliengossa/DataESR/tree/master/etablissements.esr/wikidataESR}
#' - \url{https://www.wikidata.org}
#' @seealso \code{\link{wdesr_clear_cache}}
#' @author Julien Gossa, \email{gossa@unistra.fr}
#' @noRd
wdesr_node_label_aes <- function(node_label = "alias", alias, label, fondation, dissolution) {
  switch(node_label,
         alias = {
           alias},
         alias_date = {
           paste(alias,paste('(',fondation,'-',dissolution,')',sep=''),sep='\n')},
         long = {
           label},
         long_date = {
           paste(label,paste('(',fondation,'-',dissolution,')',sep=''),sep='\n')},
         ""
  )
}

#' Get geom_nodeX function.
#'
#' Get the suitable geom_nodeX function according the node_type
#'
#' @param node_type Either "text", "text_repel", "label", or "label_repel" (default to "text").
#'
#' @return The suitable geom_nodeX function according the node_type.
#'
#' @examples wdesr_node_geom("text_repel")
#'
#' @references
#' - \url{https://github.com/juliengossa/DataESR/tree/master/etablissements.esr/wikidataESR}
#' - \url{https://www.wikidata.org}
#' @seealso \code{\link{wdesr_clear_cache}}
#' @author Julien Gossa, \email{gossa@unistra.fr}
#' @noRd
wdesr_node_geom <- function(node_type = "text") {
  switch(node_type,
         text = {geom_nodetext},
         text_repel = {geom_nodetext_repel},
         label = {geom_nodelabel},
         label_repel = {geom_nodelabel_repel},
         geom_blank
  )
}
