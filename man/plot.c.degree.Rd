\name{plot.c.degree}
\alias{plot.c.degree}
\title{ Plots the cumulative degree distribution of the network }
\description{
Plots the cumulative degree distribution of the graph, the output may be redirected to a PDF file.
}
\usage{
plot.c.degree(x, main = "Cumulative Distribution", type = "p", xlab = "Rank", ylab = "Frequency", col = "blue", log = "xy", filename = NULL, ...)
}
\arguments{
  \item{x}{ Could be a igraph network object or the degree distribution of the network }
  \item{main}{ Title of the output graphic, default = "Cumulative Distribution" }
  \item{type}{ R graphic parameter for the plot style, default = "p" (points) }
  \item{xlab}{ X-Axis name, default = "Rank" }
  \item{ylab}{ Y-Axis name, default = "Frequency" }
  \item{col}{ Plot color, default = "blue" }
  \item{log}{ Axis log scaling, default="xy" }
  \item{filename}{ Filename of the pdf output file (optional), default = NULL }
  \item{...}{Additional arguments used by the standard function plot}
}
\details{
}
\value{
}
\references{
}
\author{ Domingo Vargas }
\note{
}
\seealso{

Generic R plot function: plot
}

\examples{

data(test.net,package="netmodels")
plot.c.degree(test.net)

}
\keyword{ graphs }
\keyword{ dplot }