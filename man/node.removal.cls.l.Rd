\name{node.removal.cls.l}
\alias{node.removal.cls.l}
\title{ Test the effect of node removal on the clustering coeffiencient }
\description{
This function tests the effect of the node removal on the clustering coefficient. The provided nodes (list) are removed. Please note the nodes are removed according with the order they appear on the provided list.
}
\usage{
node.removal.cls.l(graph, v)
}
\arguments{
  \item{graph}{ Igraph network object }
  \item{v}{ List of nodes to be removed from the network}
}
\details{
Please note the nodes are defined according with the order they appear on the network and the first node is represented by the number 1 no 0. This differs from the Igraph object definition where 0 represents the first node of the network.
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
v <- c(1:10)

vl <- node.removal.cls.l(test.net,v)
}
\keyword{ graphs }