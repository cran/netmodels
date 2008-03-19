`calc.c` <-
function(alpha)

{
	library(VGAM)
	v <- 1/zeta(alpha, 0)
	names(v) <- c("c")
	return(v)
}

