`node.removal.cls.s` <-
function (graph)

{

	library(igraph)
	n <- vcount(graph)
	v.data <- 0
	v.name <- ""
	v.data[1] <- no.clusters(graph)
	v.name[1] <- "v.i"

	for( i in 1:n) {

		v.name[i+1] <- get.vertex.attribute(graph,"id",i-1)		
		v.data[i+1] <- no.clusters(delete.vertices(graph, i-1))

	}
	
	names(v.data) <- v.name
	return(v.data)

}

