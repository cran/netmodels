\name{node.removal.cls.n}
\alias{node.removal.cls.n}
\title{ Test the effect of sequential node removal on the clustering coeffiencient }
\description{
This function tests the effect of the sequential node removal on the clustering coefficient. The n-first nodes of the network are removed. Please note the nodes are removed according with the order they were defined on the Igraph object.
}
\usage{
node.removal.cls.n(graph, n)
}
\arguments{
  \item{graph}{ Igraph network object }
  \item{n}{ The n-fisrt nodes to be removed from the network}
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

vl <- node.removal.cls.n(test.net,42)
}
\keyword{ graphs }