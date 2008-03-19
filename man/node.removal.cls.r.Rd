\name{node.removal.cls.r}
\alias{node.removal.cls.r}
\title{ Test the effect of random node removal on the clustering coeffiencient }
\description{
This function tests the effect of the random node removal on the clustering coefficient. The nodes are selected randomly using the lineal random number generator provided by R.
}
\usage{
node.removal.cls.r(graph, n)
}
\arguments{
  \item{graph}{ Igraph network object }
  \item{n}{ Number of nodes to be randomly removed}
}
\details{
}
\value{
List object containing two objects:
  \item{$cls }{ a vector with the clustering coefficient after n-node removal}
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

vl <- node.removal.cls.r(test.net,42)
}
\keyword{ graphs }