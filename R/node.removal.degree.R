`node.removal.degree` <-
function (graph, n, recursive= "FALSE")

{

	library(igraph)
	graph_x <- graph
	l <- vcount(graph)-1

	for( i in l:0) {

		if(recursive == "TRUE" || recursive == "true") {
		
			if(degree(graph_x,i) <= n) graph_x <- delete.vertices(graph_x, i)
		
		} else {
		
			if(degree(graph,i) <= n) graph_x <- delete.vertices(graph_x, i)
		}
	}
	
	return(graph_x)

}

