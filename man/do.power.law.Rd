\name{do.power.law}
\alias{do.power.law}
\title{ Compares the degree distribution with the power law distribution }
\description{
Performs the goodness of fit (KS) test for network's degree distribution versus the theoretical power law distribution. The test results are printed along with the graphical representation of both distributions.
}
\usage{
do.power.law(obj, filename = NULL, main = "Comparative cumulative fraction plot of the degree distribution", report = FALSE)
}
\arguments{
  \item{obj}{ Could be a igraph network object or the degree distribution of the network }
  \item{filename}{ If it is specified, two files are created: 1) csv file with the test results and 2) pdf file with the graphic chart}
  \item{main}{ Title to be printed on the graphic chart, default="Comparative cumulative fraction plot of the degree distribution" }
  \item{report}{ If it is TRUE, the KS results are displayed on the graphic chart }
}
\details{
}
\value{
Dataframe containing the names(var) and the results is returned.
}
\references{
}
\author{
}
\note{
This tests is designed to check if the degree distribution of the network compares with equivalent theorical power law distribution. If you find the degree distribution follows the power law distribution, futher testing is required to determine if the networks is scale free. Please refer to the following paper for the complete testing procedure:

Newman, M.E.J.(2003):The structure and function of complex networks.Proceedings of the National Academy of Science the United States of America.

The next version of this rutine will perform the complete testing procedure, including the generation of synthetic power law data.
}
\seealso{
}
\examples{

data(test.net,package="netmodels")
v <- do.power.law(test.net)

}
\keyword{ graphs }
\keyword{ nonparametric }
