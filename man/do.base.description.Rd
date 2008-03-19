\name{do.base.description}
\alias{do.base.description}
\title{ Calculates some key variables of the network }
\description{
These function calculates the number of nodes, links, average degree, "ulinks" ("unique links": number of links in the network whose multiple links have been removed), links ratio (overall recurrence of multiple links between nodes), diameter, average path length, density and number of clusters).
}
\usage{
do.base.description(graph, filename = NULL)
}
\arguments{
  \item{graph}{ Igraph network object }
  \item{filename}{ If it is specified, a file in csv format is created with the results }
}
\details{
}
\value{
  Dataframe containing the names(var) and the results is returned.
}
\references{
}
\author{ Domingo Vargas}
\note{
}
\seealso{
}
\examples{

data(test.net,package="netmodels")
v <- do.base.description(test.net)

}
\keyword{ graphs }
\keyword{ dplot }