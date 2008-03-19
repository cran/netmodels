\name{calc.alpha}
\alias{calc.alpha}
\title{ Calculates the alpha value }
\description{
This function calculates the alpha value for the power law distribution that best matches the degree distribution of the network.

Please note this functions is designed to work with discrete power law distributions.

}
\usage{
calc.alpha(dist)
}
\arguments{
  \item{dist}{ Degree distribution of the network }
}
\details{
  This functions uses the function power.law.fit of the igraph library. This function calculates the alpha value an returns a MLE class object.
}
\value{
  The alpha value is returned.
}
\references{  }
\author{ Domingo Vargas}
\note{
}
\seealso{
}
\examples{

library(igraph)
data(test.net,package="netmodels")
dist <- degree(test.net)
alpha <- calc.alpha(dist)

}
\keyword{ graphs }