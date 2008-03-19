\name{node.removal.cls.0}
\alias{node.removal.cls.0}
\title{ Creates a network with the articulation points }
\description{
This function first searches the network for the nodes whose single removal does not affect the clustering coefficient. Then those nodes are removed providing a tool to build a network with the articulation points of the graph.
}
\usage{
node.removal.cls.0(graph)
}
\arguments{
  \item{graph}{ Igraph network object }
}
\details{
}
\value{
List object containing three objects:
  \item{$cls }{ a vector with the clustering coefficient after n-node removal}
  \item{$graph }{Igraph network object describing the resulting network}
  \item{$nodes }{List of the removed nodes}
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

vl <- node.removal.cls.0(test.net)
}
\keyword{ graphs }