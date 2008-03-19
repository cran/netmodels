\name{cplot}
\alias{cplot}
\title{ Plots the comparative cumulative fraction distribution of two degree distributions }
\description{
Plots the comparative cumulative fraction plot of two distributions. This function is designed to compare the observed degree distribution with the predicted one, providing a visual tool to support the results of the KS Test.
}
\usage{
cplot(obj1, obj2, main = "Comparative cumulative fraction plot of the degree distribution", type = "s", xlab = "X", ylab = "", color1 = "blue", color2 = "red", sub="", log="x", filename = NULL, ...)
}
\arguments{
  \item{obj1}{ Could be a igraph network object or the degree distribution of the network, represents the 1st Degree distribution to plot (ie. observed) }
  \item{obj2}{ Could be a igraph network object or the degree distribution of the network, represents the 2st Degree distribution to plot (ie. predicted) }
  \item{main}{ Title of the output graphic, default = "Degree Distribution" }
  \item{type}{ R graphic parameter for the plot style, default = "p" (points) }
  \item{xlab}{ X-Axis name, default = "Node" }
  \item{ylab}{ Y-Axis name, default = "Degree" }
  \item{color1}{ Plot color of the 1st distribution, default = "blue" }
  \item{color2}{ Plot color of the 2nd distribution, default = "red" }
  \item{sub} { Sub-title of the output graphic, default="" }
  \item{log}{ Axis log scaling, default="x" }
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

library(igraph)
data(test.net, package="netmodels")
dist1 <- degree(test.net)
alpha <- calc.alpha(dist1)
dist2 <- dist.power.law(alpha,length(dist1))
cplot(dist1/sum(dist1),dist2)

}
\keyword{ graphs }
\keyword{ nonparametric }