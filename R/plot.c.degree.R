`plot.c.degree` <-
function(x, main = "Cumulative Distribution", type = "p" , xlab = "Rank" , ylab = "Frequency" , col = "blue", log="xy", filename=NULL, ...)

{
	library(igraph)
	
	if(is.igraph(x)) {
	
		x <- sort(degree(x),TRUE)
	}
	else {
	
		x <-sort(x,TRUE)
	}
	
	if (missing(main)) main = "Cumulative Distribution"
	if (missing(type)) type = "p"
	if (missing(xlab)) xlab = "Rank"
	if (missing(ylab)) ylab = "Frequency"
	if (missing(col)) col = "blue"
	if (missing(log)) log = "xy"

	t <- tabulate(x)
		
	if(length(filename) > 0) {

		pdf(file=paste(filename,".c.degree.pdf",sep=""), height=6, width=6)
		plot.default(sort(x), sort(t[x],TRUE), type= type , main= main, xlab= xlab , ylab= ylab , log= "xy" ,col= col)
		dev.off()
	}
	else {
		
		plot.default(sort(x), sort(t[x],TRUE), type= type , main= main, xlab= xlab , ylab= ylab , log= "xy" ,col= col)
	}
}

