`node.removal.cls.0` <-
function (graph)

{
	library(igraph)
	v <- 0
	j <- 1
	n <- vcount(graph)
	r <- node.removal.cls.s(graph)

	for( i in 2:n+1) {
		
		if(r[i] == r[1]){
			
			v[j] = i -1 
			j <- j+1
			
		}
	}
	
	names(v) <- get.vertex.attribute(graph,"id",v-1)
	
	m <- node.removal.cls.l(graph,v)
	
	v.data <- list(m$cls,m$graph,v)
	names(v.data) <- c("cls","graph","nodes")
	return(v.data)

}

