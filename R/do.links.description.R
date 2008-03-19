`do.links.description` <-
function(graph, filename=NULL)

{
	
	library(igraph)
	m <- get.edgelist(graph)
	n <-0	
	l <- vcount(graph)
	
	for(i in 1:l) {
		
		n[i] <- get.vertex.attribute(graph,"id",i-1)

	}
	
	vm <-0
	
	for(i in 1:length(m)/2){
	
		x <- m[i,1]
		y <- m[i,2]

		ifelse(x <= y, vm[i] <- paste (n[x+1], n[y+1],sep="-"), vm[i] <- paste (n[y+1],n[x+1],sep="-"))
	}


	vf <- factor(vm)
	vt <- table(vf)
	vr <- sort(vt,TRUE)
	
	if(length(filename) > 0) {
		
		write.csv(vr,file=paste(filename,".links.description",".csv",sep=""))
			
	} 
	
	return(vr)


}

