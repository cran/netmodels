`cplot` <-
function(obj1, obj2, main ="Comparative cumulative fraction plot of the degree distribution", type ="s", xlab ="X", ylab = "", color1 = "blue", color2 = "red", sub="", log="x", filename=NULL, ...)

{

	library(igraph)
	
	if(is.igraph(obj1)) {
	
		dist1 <- sort(degree(obj1),TRUE)
	}
	else {
	
		dist1 <-sort(obj1,TRUE)
	}

	if(is.igraph(obj2)) {
	
		dist2 <- sort(degree(obj2),TRUE)
	}
	else {
	
		dist2 <-sort(obj2,TRUE)
	}

	if (missing(main)) main = "Comparative cumulative fraction plot of the degree distribution"
	if (missing(type)) type = "s"
	if (missing(xlab)) xlab = "X"
	if (missing(ylab)) ylab = ""
	if (missing(color1)) color1 = "blue"
	if (missing(color2)) color2 = "red"
	if (missing(sub)) sub = ""
	if (missing(log)) log = "x"

	t1 <- c(1:length(dist1))/length(dist1)
	plot.default(sort(dist1), t1, type=type, main=main, sub=sub, xlab=xlab,ylab=ylab, log="x", col= color1)
	t2 <- c(1:length(dist2))/length(dist2)
	lines(sort(dist2), t2, type=type, col =color2)



	if(length(filename) > 0) {

		pdf(file=filename, height=6, width=6)
		plot.default(sort(dist1), t1, type=type, main=main, sub=sub, xlab=xlab,ylab=ylab, log="x", col= color1)
		lines(sort(dist2), t2, type=type, col =color2)
		dev.off()
	}

}

