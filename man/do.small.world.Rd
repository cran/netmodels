\name{do.small.world}
\alias{do.small.world}
\title{ Performs the small world test of the given network. }
\description{
This function provides the ratio of the average path length and the clustering coefficient to verify the small world behavior of the network
}
\usage{
do.small.world(graph, filename = NULL)
}
\arguments{
  \item{graph}{ Igraph network object }
  \item{filename}{ If it is specified, a file in csv format is created with the results}
}
\details{
}
\value{
  Dataframe containing the names(var) and the results is returned.
}
\references{
Watts, D. (2004): Small worlds, the dynamics of networks between order and randomness. Princenton University Press.
}
\author{ Domingo Vargas }
\note{ 
}
\seealso{
}
\examples{

data(test.net,package="netmodels")
v <- do.small.world(test.net)
}
\keyword{ graphs }