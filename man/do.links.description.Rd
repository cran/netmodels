\name{do.links.description}
\alias{do.links.description}
\title{ Prints the frequency of links between the nodes of the network }
\description{
Calculates the frequency of links between the nodes of the network.
}
\usage{
do.links.description(graph, filename = NULL)
}
\arguments{
  \item{graph}{ Igraph network object }
  \item{filename}{ If it is specified, a file in csv format is created with the results }
}
\details{
}
\value{
  Dataframe containing the frequency between the nodes is returned.
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
v <- do.links.description(test.net)

}
\keyword{ graphs }