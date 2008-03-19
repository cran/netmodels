\name{calc.c}
\alias{calc.c}
\title{ Calculates de c value of the power law distribution}
\description{
This function calculates the closing parameter (c) for the alpha value of the power law distribution.
}
\usage{
calc.c(alpha)
}

\arguments{
  \item{alpha}{ Alpha value of the power law distribution }
}
\details{
This function uses the Zeta function provided by the VGAM library
}
\value{
The c value for the power law distribution of a given alpha
}
\references{ }
\author{ Domingo Vargas }
\note{
}
\seealso{
}
\examples{

library(VGAM)
library(igraph)
data(test.net,package="netmodels")
dist <- degree(test.net)
alpha <- calc.alpha(dist)

c <- calc.c(alpha)

}
\keyword{ graphs }