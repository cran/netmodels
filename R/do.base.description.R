`do.base.description` <-
function(graph, filename=NULL)

{

	library(igraph)
	
	n <- vcount(graph)
	e <- ecount(graph)
	g <- mean(degree(graph))
	re <- ecount(simplify(graph))
	di <- diameter(graph)
	apl <- average.path.length(simplify(graph))
	de <- graph.density(simplify(graph))
	no <- no.clusters(graph)

	var = c("Nodes","Links","Average Degree","ULinks","Link Ratio","Diameter","Average Path Length","Density","No.Clusters")
	result = c(n,e,g,re,e/re,di,apl,de,no)
	v = data.frame(var,result)
		
	if(length(filename) > 0) {
		
		write.csv(v,file=paste(filename,".base.description",".csv",sep=""))
			
	} else {
	
		cat("\n\n")
		cat("--------------------------------\n")
		cat("Network Description\n")
		cat("Nodes............ ",n,"\n")
		cat("Links............ ",e,"\n")
		cat("Average Degree... ",g,"\n")
		cat("Unique Links..... ",re,"\n")
		cat("Link Ratio....... ",e/re,"\n")
		cat("Diameter......... ",di,"\n")
		cat("Average Path L... ",apl,"\n")
		cat("Density.......... ",de,"\n")				
		cat("No.Clusters...... ",no,"\n")
		cat("--------------------------------\n")
		}
	
	return(v)

	
}

