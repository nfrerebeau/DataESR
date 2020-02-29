\donttest{
## Warning: an active internet connection is needed
## Get a graph of Strasbourg University
g <- wdesr_get_graph("Q61716176", property = c("subsidiary", "has_part"),
                     depth = 2, verbose = TRUE)
g$edges
g$vertice

## Plot the graph
plot(g)
}
