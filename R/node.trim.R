`node.trim` <-
function(graph)

{
	library(igraph)
	graph_x <- graph
	n <- vcount(graph)
	
	for(i in n:1){
	
		if(degree(graph_x,i-1) == 0) {
			
			graph_x <- delete.vertices(graph_x, i-1)
		}	
	}

	return(graph_x)
}

