\name{do.degree.description}
\alias{do.degree.description}
\title{ Degree description of the network }
\description{
Calculates the degree distribution for both the original network and the simplified one (the network whose multiple edges have been removed)
}
\usage{
do.degree.description(graph, filename = NULL)
}
\arguments{
  \item{graph}{ Igraph network object }
  \item{filename}{ If it is specified, a file in csv format is created with the results }
}
\details{
}
\value{
  Dataframe with the original degree and the simplified one for all the nodes of the network.
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
v <- do.degree.description(test.net)

}
\keyword{ graphs }