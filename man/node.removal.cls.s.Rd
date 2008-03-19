\name{node.removal.cls.s}
\alias{node.removal.cls.s}

\title{ Test the single node removal on the clustering coeffiencient}
\description{
This function tests the effect of the single node removal on the clustering coefficient. All nodes of the network are tested.
}
\usage{
node.removal.cls.s(graph)
}
\arguments{
   \item{graph}{ Igraph network object }
}
\details{
}
\value{
Vector (list) containing the network's clustering coefficient after single node removal.
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

v <- node.removal.cls.s(test.net)
}
\keyword{ graphs }