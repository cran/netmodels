\name{plot.degree}
\alias{plot.degree}
\title{ Plots the degree distribution of the network }
\description{
Plots the degree distribution of the graph, the output may be redirected to a PDF file.
}
\usage{
plot.degree(x, main = "Degree Distribution", type = "p", xlab = "Node", ylab = "Degree", col = "blue", log= "xy", filename = NULL, ...)
}
\arguments{
  \item{x}{ Could be a igraph network object or the degree distribution of the network }
  \item{main}{ Title of the output graphic, default = "Degree Distribution" }
  \item{type}{ R graphic parameter for the plot style, default = "p" (points) }
  \item{xlab}{ X-Axis name, default = "Node" }
  \item{ylab}{ Y-Axis name, default = "Degree" }
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
plot.degree(test.net)

}
\keyword{ graphs }
\keyword{ dplot }