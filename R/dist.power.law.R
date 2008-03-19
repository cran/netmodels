`dist.power.law` <-
function(alpha, n)

 {
 	c <- calc.c(alpha)
	v <- c(1:n)
	names(v) <- c(1:n)
	v <- c*v^-alpha
	return(v)
 }

