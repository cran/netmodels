\name{node.removal.apl.l}
\alias{node.removal.apl.l}
\title{ Removes the specified nodes }
\description{
This function calculates the average path length of the network after the removal of the selected nodes. The nodes to be removed are defined on the vector provided by the user.
}
\usage{
node.removal.apl.l(graph, v)
}
\arguments{
 \item{graph}{ Igraph network object }
 \item{v}{ List object with the nodes to be removed }
}
\details{
}
\value{
List object containing two objects: $apl a vector with the average path length after node removal and $graph an object with the Igraph network object describing the resulting network.
}
\references{
}
\author{ Domingo Vargas }
\note{
}
\examples{

v <- c(1:10)
data(test.net,package="netmodels")
vl <- node.removal.apl.l(test.net,v)

}
\keyword{ graphs }