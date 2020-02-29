# PLOT

#' Plot
#'
#' Renders a graph.
#' @param x TODO.
#' @param layout The layout to use to plot the graph as in
#' \code{\link[sna]{gplot.layout}}.
#' @param edge_label TRUE to plot dates on edges (default to "TRUE").
#' @param node_label Define the label for the nodess. Either "alias",
#' "alias_date", "long", or "long_date" (default to "alias").
#' @param node_dates TODO.
#' @param node_type Define the type of drawing for the nodes. Either "text",
#' "text_repel", "label", or "label_repel" (default to "text").
#' @param size_guide TRUE to plot the guide for sizes (defalut to "FALSE").
#' @param ... Currently not used.
#' @return A \code{\link[ggplot2]{ggplot}} object.
# @example
#' @author J. Gossa, N. Frerebeau
#' @export
plot.esr_graph <- function(x, layout = "kamadakawai", edge_label = TRUE,
                           node_label = "alias", node_dates = FALSE,
                           node_type = "text", size_guide = FALSE, ...) {
  # Validation
  if(nrow(x$vertices) == 0 || nrow(x$edges) == 0 )
    stop("Empty ESR graph: something went wrong with the graph parameters.",
         call. = FALSE)
  # Helpers
  node_size <- 30 / x$vertices$level
  names(node_size) <- x$vertices$statut

  net <- network::network(x$edges, vertex.attr = x$vertices,
                          matrix.type = "edgelist", ignore.eval = FALSE,
                          directed = TRUE)
  ggnet <- ggnetwork::ggnetwork(net, layout = layout, weights = "weight")
  ggnet <- ggnet[order(ggnet$level), ]
  ggnet$statut <- factor(ggnet$statut, levels = unique(ggnet$statut))

  g <- ggplot(ggnet, aes(x = .data$x, y = .data$y,
                         xend = .data$xend, yend = .data$yend))
  g <- g + geom_edges(
    aes(linetype = .data$type),
    arrow = arrow(length = unit(8, "pt"), type = "closed"),
    alpha = 1,
    color = "darkgrey"
  )
  g <- g + geom_nodes(aes(
    color = .data$statut,
    alpha = ifelse(is.na(.data$dissolution), "actif", "dissous"),
    size = .data$statut
  ))
  g <- g + make_node_geom(node_type)(
    aes(label = make_node_label(.data, node_label, node_dates))
  )
  g <- g + scale_alpha_manual(values = c(dissous = 0.6, actif = 1))
  g <- g + scale_size_manual(values = node_size, guide = FALSE)
  g <- g + labs(linetype = "Relation", size = "Statut",
                colour = "Statut", alpha = "Etat")
  g <- g + theme_blank()
  if (edge_label)
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
