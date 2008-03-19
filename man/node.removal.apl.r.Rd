\name{node.removal.apl.r}
\alias{node.removal.apl.r}
\title{ Test the effect of random node removal on the average path length }
\description{
This function tests the effect of the random node removal on the average path length. The nodes are selected randomly using the lineal random number generator provided by R.
}
\usage{
node.removal.apl.r(graph, n)
}
\arguments{
  \item{graph}{ Igraph network object }
  \item{n}{ Number of nodes to be randomly removed}
}
\details{
}
\value{
List object containing two objects:
  \item{$apl }{ a vector with the average path length after n-node removal}
  \item{$graph }{Igraph network object describing the resulting network}
}
\references{
}
\author{ Domingo Vargas }
\note{ 
}
\seealso{
}
\examples{

data(test.net,package="netmodels")

vl <- node.removal.apl.r(test.net,42)
}
\keyword{ graphs }