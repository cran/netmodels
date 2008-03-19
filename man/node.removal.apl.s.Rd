\name{node.removal.apl.s}
\alias{node.removal.apl.s}

\title{ Test the single node removal on the average path length}
\description{
This function tests the effect of the single node removal on the average path length. All nodes of the network are tested.
}
\usage{
node.removal.apl.s(graph)
}
\arguments{
   \item{graph}{ Igraph network object }
}
\details{
}
\value{
Vector (list) containing the network's average path length after single node removal.
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

v <- node.removal.apl.s(test.net)
}
\keyword{ graphs }