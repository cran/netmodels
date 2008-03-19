`do.power.law` <-
function (obj, filename=NULL, main="Comparative cumulative fraction plot of the degree distribution", report=FALSE)

{

	library(igraph)
	if(is.igraph(obj)) {
	
		dist <- sort(degree(obj),TRUE)
	}
	else {
	
		dist <-sort(obj,TRUE)
	}
	
	main = main
	d1 <- dist /sum(dist)
	alpha <- calc.alpha(dist)
	c <- calc.c(alpha)
	d2 <- dist.power.law (alpha, length(d1))
	
	r <- ks.test(d1,d2)
	v.data <- r$statistic
	r2 <- r$p.value

	names(alpha) <- c("")
	names(c) <- c("")
	names(v.data) <- c("")
	names(r2) <- c("")

	var = c("Alpha","C","D","P")
	result = c(alpha,c,v.data,r2)
	v = data.frame(var,result)

	if(report) sub=sprintf("Power Law Fit (A: %f ; C: %f) KS-Results (D: %f ; P: %f)",alpha,c,v.data,r2) else sub=""
	

	cplot(d1, d2, main=main, type ="s", xlab ="X", ylab = "", color1 = "blue", color2 = "red", sub=sub)
	


	if(length(filename) > 0) {
		
		write.csv(v, file=paste(filename,".power.law.csv",sep=""))
		pdf(file=paste(filename,".ks.pdf",sep=""), height=6, width=6)
		cplot(d1, d2, main=main, type ="s", xlab ="X", ylab = "", color1 = "blue", color2 = "red", sub=sub,filename=filename)
		dev.off()
	}		
	 else {
	
		cat("\n\n")
		cat("--------------------------------\n")
		cat("Power Law Fit\n")
		cat("Alpha............ ",alpha,"\n")
		cat("C................ ",c,"\n")
		cat("--------------------------------\n")
		cat("Kolmogorov-Smirnov Test Results","\n")
		cat("D................ ", v.data,"\n")
		cat("P................ ", r2,"\n")
		cat("--------------------------------\n\n\n")
	}
	
	return(v)

}

