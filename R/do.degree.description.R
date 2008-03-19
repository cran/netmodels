`do.degree.description` <-
function(graph, filename=NULL)

{

	library(igraph)
	
	base.degree <- degree(graph)
	names(base.degree) <- get.vertex.attribute(graph, "id", 0:vcount(graph)-1)
	simplified.degree <- degree(simplify(graph))

	v = data.frame(base.degree,simplified.degree)


	if(length(filename) > 0) {
		
		write.csv(v,file=paste(filename,".degree.description",".csv",sep=""))
			
	}
	
	return(v)

	
}

