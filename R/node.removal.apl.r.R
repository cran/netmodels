`node.removal.apl.r` <-
function (graph, n)

{
	library(igraph)
	graph_x <- graph
	v.data <- 0
	v.name <- ""
	v.data[1] <- average.path.length(graph_x)
	v.name[1] <- "v.i"
	
	size <- vcount (graph_x) -1
	v <- c(1:n)
	    
	for( i in 1:n) {
		
		v[i] <- runif(1,0,1)*size
		v.name[i+1] <- get.vertex.attribute(graph,"id",v[i])
		size <- size -1  
	} 
	
	for( i in 1:length(v)) {

		graph_x <- delete.vertices(graph_x, v[i])
		v.data[i+1] <- average.path.length(graph_x)
		
	}

	names(v.data) <- v.name
	a <- list(v.data, graph_x)
	names(a) <- c("apl","graph")
	return(a)

}

