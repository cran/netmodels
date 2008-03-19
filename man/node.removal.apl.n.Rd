\name{node.removal.apl.n}
\alias{node.removal.apl.n}
\title{ Test the effect of sequential node removal on the average path length }
\description{
This function tests the effect of the sequential node removal on the average path length. The n-first nodes of the network are removed. Please note the nodes are removed according with the order they were defined on the Igraph object.
}
\usage{
node.removal.apl.n(graph, n)
}
\arguments{
  \item{graph}{ Igraph network object }
  \item{n}{ The n-fisrt nodes to be removed from the network}
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

vl <- node.removal.apl.n(test.net,42)
}
\keyword{ graphs }