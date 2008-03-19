`calc.alpha` <-
function(dist)

{
	library(igraph)
	v <- attr(power.law.fit(dist),"coef")
	return(v)
}

