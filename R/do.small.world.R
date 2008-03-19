`do.small.world` <-
function(graph, filename=NULL)

{

	library(igraph)
	graphx <- simplify(graph)
	v <- vcount(graphx)
	e <- ecount(graphx)
	
	er.graph <- erdos.renyi.game(v,e,type="gnm")
	
	l <- average.path.length(graphx, FALSE)
	l.er <- average.path.length(er.graph, FALSE)
	
	cc <- transitivity(graphx,type="undirected")
	cc.er <- transitivity(er.graph,type="undirected")

	var = c("L","Ler","Ratio L","CC","CCer","Ratio CC")
	result = c(l,l.er,l/l.er,cc,cc.er,cc/cc.er)
	v = data.frame(var,result)
		
	if(length(filename) > 0) {
		
		write.csv(v,file=paste(filename,".small.world",".csv",sep=""))
			
	} else {
	
		cat("\n\n")
		cat("--------------------------------\n")
		cat("Average Path Length\n")
		cat("L................ ",l,"\n")
		cat("Ler.............. ",l.er,"\n")
		cat("Ratio............ ",l/l.er,"\n")
		cat("--------------------------------\n")
		cat("Clustering Coeficient (Transitivity)","\n")
		cat("CC............... ", cc,"\n")
		cat("CCer............. ", cc.er,"\n")
		cat("Ratio............ ",cc/cc.er,"\n")
		cat("--------------------------------\n\n\n")
	}
	
	return(v)
}

