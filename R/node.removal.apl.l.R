`node.removal.apl.l` <-
function (graph, v)

{
	library(igraph)
	graph_x <- graph
	v.data <- 0
	v.name <- ""
	v <- sort(v)
	
	v.data[1] <- average.path.length(graph_x)
	v.name[1] <- "v.i"
	
	    
	for( i in 1:length(v)) {
		
		v.name[i+1] <- get.vertex.attribute(graph,"id",v[i]-1)

	} 

	
	for( i in 1:length(v)) {

		graph_x <- delete.vertices(graph_x, v[i]-1)
		v.data[i+1] <- average.path.length(graph_x)
		v <- v -1
		
	}

	names(v.data) <- v.name
	a <- list(v.data, graph_x)
	names(a) <- c("apl","graph")
	return(a)

}

