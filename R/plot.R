#' Plot an ESR graph.
#'
#' A wrapper for ggplot2 to plot graph as returned by \code{\link{wdesr_get_graph}}.
#'
#' @param df.g A dataframe representing a graph, as returned by wdesr_get_graph.
#' @param layout The layout to use to plot the graph as in \code{\link[sna]{gplot.layout}}.
#' @param active_only TRUE to filter the dissolved nodes (default to FALSE).
#' @param node_sizes The size of the nodes, either a single value or a range c(min,max).
#' @param label_sizes The size of the nodes, either a single value or a range c(min,max).
#' @param node_label Define the label for the nodess. Either "alias", "alias_date", "long", or "long_date" (default to "alias").
#' @param node_type Define the type of drawing for the nodes. Either "text", "text_repel", "label", or "label_repel" (default to "text").
#' @param edge_label TRUE to plot dates on edges (default to "TRUE").
#' @param arrow_gap A parameter that will shorten the network edges in order to avoid overplotting edge arrows and nodes see \code{\link[ggnetwork]{fortify.network}}.
#' @param size_guide TRUE to plot the guide for sizes (defalut to "FALSE").
#'
#' @return A ggplot2.
#' @export
#'
#' @examples
#' df.aslace <- wdesr_get_graph("Q61716176",c('composante','associé'), 1)
#'
#' wdesr_ggplot_graph(df.alsace,
#'   node_size = c(10,30), label_sizes = c(3,5), arrow_gap = 0.0,
#'   node_label = "alias", node_type = "text",
#'   edge_label = FALSE)
#' @references
#' - \url{https://github.com/juliengossa/DataESR/tree/master/etablissements.esr/wikidataESR}
#' - \url{https://www.wikidata.org}
#' @seealso \code{\link{wdesr_clear_cache}}
#' @author Julien Gossa, \email{gossa@unistra.fr}
wdesr_ggplot_graph <- function( df.g,
                                layout = "kamadakawai",
                                active_only = FALSE,
                                node_sizes = c(10,30),
                                label_sizes = c(4,6),
                                node_label = "alias",
                                node_type = "text",
                                edge_label = TRUE,
                                arrow_gap = 0.05,
                                size_guide = FALSE) {

  if( nrow(df.g$vertices) == 0 | nrow(df.g$edges) == 0 )
    stop("Empty ESR graph: something went wrong with the graph production parameters")

  #df.g$edges$weight <- scales::rescale(as.numeric(df.g$edges$depth),c(1,2))
  geom_node_fun <- wdesr_node_geom(node_type)

  net <<- network::network(df.g$edges,
                           vertex.attr=df.g$vertices,
                           matrix.type="edgelist", ignore.eval=FALSE,
                           directed = TRUE)

  ggnet <<- ggnetwork(net,
                      layout = layout,
                      weights = "weight",
                      radii  = scales::rescale(-as.numeric(df.g$vertices$niveau)),
                      arrow.gap = arrow_gap)

  g <- ggplot(ggnet, aes(x = x, y = y, xend = xend, yend = yend))
  g <- g + geom_edges(aes(linetype = type),#, size = weight),
                      arrow = arrow(length = unit(8, "pt"), type = "closed"),
                      alpha=1,
                      color="darkgrey")

  if(edge_label) g <- g + geom_edgetext(aes(label=date), size = min(label_sizes))

  g <- g + geom_nodes(aes(
    color=statut,
    alpha = (dissolution != "NA"),
    size = factor(niveau, levels=wdesr.niveaux$niveau)  #scales::rescale(-as.numeric(df.g$vertices$niveau),node_sizes)
  ))
  g <- g + geom_node_fun(aes(
    label = wdesr_node_label_aes(node_label,alias,label,fondation,dissolution),
    fill = statut),
    size = scales::rescale(-as.numeric(df.g$vertices$niveau),label_sizes)
  )
  g <- g + scale_alpha_manual(labels=c("dissous","actif"), values = (c(0.6,1)), name='statut')
  g <- g + scale_size_manual(breaks=as.character(wdesr.niveaux$niveau),
                             values=scales::rescale(-as.numeric(wdesr.niveaux$niveau),node_sizes),
                             labels=wdesr.niveaux$libellé,
                             name="niveau",
                             drop=FALSE,
                             guide=ifelse(size_guide,"legend",FALSE))
  g <- g + xlim(-0.2,1.2) + ylim(-0.03,1.03)
  g <- g + theme_blank()

  return(g)
}

# ggplotly_wdesr_graph <- function(df.g) {
#   net <<- network(df.g$edges,
#                   vertex.attr=df.g$vertices %>% mutate_all(as.character) %>% arrange(id),
#                   matrix.type="edgelist", ignore.eval=FALSE)
#
#   ggnet <<- ggnetwork(net, layout = "kamadakawai",directed=TRUE)
#
#   ggplot(ggnet, aes(x = x, y = y, xend = xend, yend = yend)) +
#     geom_edges(aes(color = type, text=paste('date :',date)),
#                arrow = arrow(length = unit(10, "pt"), type = "closed")) +
#     #geom_edgetext(aes(color = type, label=date)) +
#     geom_nodetext(aes(
#       label = alias,
#       text = paste(label,status,paste('(',fondation,'-',dissolution,')',sep=''),paste("wikidata id:",id),sep='\n'),
#       color=status)) +
#     theme_blank()
#
#   ggplotly(tooltip="text")
# }


#' Wrapper to load and plot ESR graphs.
#'
#' Conveniently call \code{\link{wdesr_get_graph}} and then \code{\link{wdesr_ggplot_graph}}.
#'
#' @param wdid The wikidata id of the root.
#' @param props The properties to follows.
#' @param depth The depth of the following
#' @param plot_type Either "ggplot" or "plotly" (default to ggplot).
#' @param ... Additionnal parameters for the plot; see \code{\link{wdesr_ggplot_graph}} for details.
#' @param active_only TRUE to filter dissolved universities (default to FALSE).
#' @return A ggplot or a plotly.
#' @export
#'
#' @examples
#' wdesr_load_and_plot("Q61716176",c('composante','associé'), 1,
#'   node_size = c(10,30), label_sizes = c(3,5), arrow_gap = 0.0,
#'   node_label = "alias", node_type = "text",
#'   edge_label = FALSE)
#' @references
#' - \url{https://github.com/juliengossa/DataESR/tree/master/etablissements.esr/wikidataESR}
#' - \url{https://www.wikidata.org}
#' @seealso \code{\link{wdesr_clear_cache}}
#' @author Julien Gossa, \email{gossa@unistra.fr}
wdesr_load_and_plot <- function( wdid,
                                 props          = c('composante','associé'),
                                 depth          = 3,
                                 active_only    = FALSE,
                                 plot_type      = 'ggplot',
                                 ...) {

  df.g <<- wdesr_get_graph(wdid,props,depth,active_only)

  if(plot_type == 'plotly') {
    wdesr_ggplotly_graph(df.g)
  } else {
    wdesr_ggplot_graph(df.g,...)
  }
}

