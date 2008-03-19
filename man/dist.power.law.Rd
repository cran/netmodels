\name{dist.power.law}
\alias{dist.power.law}
\title{ Power law distribution }
\description{
Given an alpha value, calculates the first n values for the power law distribution.
}
\usage{
dist.power.law(alpha, n)
}
\arguments{
  \item{alpha}{ Alpha value of the power law distribution }
  \item{n}{ Number of values to be calculate }
}
\details{
}
\value{
Vector with the first n values of the power law distribution
}
\references{
}
\author{ Domingo Vargas }
\note{

}
\seealso{
calc.alpha
calc.c
}
\examples{

library(igraph)
data(test.net,package="netmodels")
dist <- degree(test.net)
alpha <- calc.alpha(dist)
dist.power.law(alpha,5)

}
\keyword{ graphs }
\keyword{ nonparametric }