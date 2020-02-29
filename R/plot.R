# PLOT

#' Plot
#'
#' Renders a graph.
#' @param x An object of class \code{\link[=wdesr_get_graph]{esr_graph}}.
#' @param layout A \code{\link{character}} string specifying the layout to be
#'  used to plot the graph as in \code{\link[sna]{gplot.layout}}.
#' @param edge_label A \code{\link{logical}} scalar: should the dates be plotted
#'  on edges (default to \code{TRUE})?
#' @param node_label A \code{\link{character}} string specifying the label of
#'  the nodes. It must be one of "\code{alias}" (the default) or "\code{label}".
#' @param node_dates A \code{\link{logical}} scalar: should the date be added
#'  to the label of the nodes?
#' @param node_type A \code{\link{character}} string specifying the geometry of
#'  the label of the nodes. It must be one of "\code{text}" (the default),
#'  "\code{text_repel}", "\code{label}" or "\code{label_repel}".
#' @param size_guide A \code{\link{logical}} scalar: should the sizes guide be
#'  plotted (defalut to \code{FALSE})?
#' @param ... Currently not used.
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @example inst/examples/ex-graph.R
#' @seealso \link{wdesr_get_graph}
#' @author J. Gossa, N. Frerebeau
#' @export
plot.esr_graph <- function(x, layout = "kamadakawai", edge_label = TRUE,
                           node_label = "alias", node_dates = FALSE,
                           node_type = "text", size_guide = FALSE, ...) {
  # Validation
  if(nrow(x$vertices) == 0 || nrow(x$edges) == 0 )
    stop("Empty graph: something went wrong with the graph parameters.",
         call. = FALSE)

  # Helpers
  node_size <- 30 / x$vertices$level
  names(node_size) <- x$vertices$status

  net <- network::network(x$edges, vertex.attr = x$vertices,
                          matrix.type = "edgelist", ignore.eval = FALSE,
                          directed = TRUE)
  ggnet <- ggnetwork::ggnetwork(net, layout = layout, weights = "weight")
  ggnet <- ggnet[order(ggnet$level), ]
  ggnet$status <- factor(ggnet$status, levels = unique(ggnet$status))

  g <- ggplot(ggnet, aes(x = .data$x, y = .data$y,
                         xend = .data$xend, yend = .data$yend))
  g <- g + geom_edges(
    aes(linetype = .data$type),
    arrow = arrow(length = unit(8, "pt"), type = "closed"),
    alpha = 1,
    color = "darkgrey"
  )
  g <- g + geom_nodes(aes(
    color = .data$status,
    alpha = ifelse(is.na(.data$dissolved), "actif", "dissous"),
    size = .data$status
  ))
  g <- g + make_node_geom(node_type)(
    aes(label = make_node_label(.data, type = node_label, dates = node_dates))
  )
  g <- g + scale_alpha_manual(values = c(dissous = 0.6, actif = 1))
  g <- g + scale_size_manual(values = node_size, guide = FALSE)
  g <- g + labs(linetype = "Relation", size = "Statut",
                colour = "Statut", alpha = "Etat")
  g <- g + theme_blank()
  if (edge_label && all(!is.na(x$edges$date)))
    g <- g + geom_edgetext(aes(label = .data$date))
  # suppressWarnings(print(g))
  g
}

make_node_label <- function(data, type = c("alias", "label"), dates = FALSE) {
  type <- match.arg(type, several.ok = FALSE)
  lab <- data[[type]]
  if (dates) {
    from_to <- paste0("(", data$fondation, " - " , data$dissolution, ")")
    lab <- paste(lab, from_to, sep = "\n")
  }
  return(lab)
}
make_node_geom <- function(type = "text") {
  switch(
    type,
    text = geom_nodetext,
    text_repel = geom_nodetext_repel,
    label = geom_nodelabel,
    label_repel = geom_nodelabel_repel,
    geom_blank
  )
}
