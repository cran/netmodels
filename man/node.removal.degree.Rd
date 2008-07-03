\name{node.removal.degree}
\alias{node.removal.degree}
\title{ Removes nodes with a degree less or equal than the value specified by the user }
\description{
This function removes nodes with a degree less or equal than the value specified by the user. The node removal can be performed recursively. The function returns a network object with the remaining nodes.
}
\usage{
node.removal.degree(graph, n, recursive = "FALSE")
}
\arguments{
  \item{graph}{ Igraph network object }
  \item{n}{ The degree value to be applied to the network }
  \item{recursive}{ Logical flag to defined if the degree should be calculated after each node removal }
}
\details{
Please note the nodes are removed starting with the last node of the network, therefore in the case of recursive node removal the ongoing degree values depends on how does the removed nodes affect the network. It is advisable to sort the network's nodes based on the degree distribution (descending) to produce standarized results.
This function is intended to undirected networks.
}
\value{
  \item{grpah }{The resulting network}
}
\references{
}
\author{ Domingo Vargas }
\note{}
\seealso{
}
\examples{
data(test.net,package="netmodels")
vl <- node.removal.degree(test.net,2)
}