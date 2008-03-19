`node.removal.apl.n` <-
function (graph, n)

{

	library(igraph)
	graph_x <- graph
	v.data <- 0
	v.name <- ""
	v.data[1] <- average.path.length(graph_x)
	v.name[1] <- "v.i"

	for( i in 1:n) {

		v.name[i+1] <- get.vertex.attribute(graph,"id",i-1)		
		graph_x <- delete.vertices(graph_x, 0)
		v.data[i+1] <- average.path.length(graph_x)

	}
	
	names(v.data) <- v.name
	a <- list(v.data, graph_x)
	names(a) <- c("apl","graph")
	return(a)

}

