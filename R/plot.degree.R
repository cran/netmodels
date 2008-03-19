`plot.degree` <-
function(x, main = "Degree Distribution", type = "p" , xlab = "Node" , ylab = "Degree" , col = "blue", log="xy", filename=NULL,...)

{
	library(igraph)
	
	if(is.igraph(x)) {
	
		x <- sort(degree(x),TRUE)
	}
	else {
	
		x <-sort(x,TRUE)
	}
	
	if (missing(main)) main = "Degree Distribution"
	if (missing(type)) type = "p"
	if (missing(xlab)) xlab = "Node"
	if (missing(ylab)) ylab = "Degree"
	if (missing(col)) col = "blue"
	if (missing(log)) log = "xy"

	
	if(length(filename) > 0) {

		pdf(file=paste(filename,".degree.pdf",sep=""), height=6, width=6)
		plot.default(x, main=main, type=type, xlab=xlab, ylab=ylab, log= log ,col=col,...)
		dev.off()
	}
	
	else {

		plot.default(x, main=main, type=type, xlab=xlab, ylab=ylab, log= log ,col=col, ...)
		
	}

}

