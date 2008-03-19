\name{node.trim}
\alias{node.trim}
\title{ Deletes the isolates nodes of the network }
\description{
  This function deletes all the single nodes with 0 edges off the network.
}
\usage{
node.trim(graph)
}
\arguments{
   \item{graph}{ Igraph network object }
}
\details{
}
\value{
Returns a new graph object with the selected nodes removed.
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
new.net <- node.trim(test.net)
}
\keyword{ graphs }