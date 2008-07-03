\name{netmodels-package}
\alias{netmodels-package}
\alias{netmodels}
\docType{package}
\title{
Set of functions for the study of scale free and small world networks
}
\description{
Provides a set of functions designed to help in the study of scale free and small world networks. This functions are high level abstractions of the functions provided by the Igraph package.
}
\details{
\tabular{ll}{
Package: \tab netmodels\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2008-07-03\cr
License: \tab  Artistic License 2.0\cr
}
This package was designed to help in the testing process of scale free and small world networks.

Its have been developed around the functionality of the Igraph package and its focused on the testing process (whether or not the network meet the scalee free and/or small world criteria) and the simulation of node removal effect on the average path length and number of clusters.

Please note the functions are designed to work with undirected networks only. If the network is directed the program may produce unexpected and unreliable results. Because this package uses the Igraph functions to perform most of the calculations, the results depends on how those functions handle the directed networks under undirected scenario.

There are basically two types of functions:

- Building blocks functions: Designed to provide an encapsulated manipulation of the network and/or to perform some of the calculations (ie. clossing parameter of the power law distribution). Currently the building block functions are the following: calc.alpha, calc.c, dist.power.law.

- High level functions: Theses functions do most of the testing and ploting required to validate the scale free or the small world properties of the network. The high level functions can be divided also in two types: The functions designed to perform network manipulation for the node removal process (m.edges.removal, node.removal.apl.s, node.removal.apl.n, node.removal.apl.r, node.removal.apl.l, node.removal.cls.s, node.removal.cls.n, node.removal.cls.r, node.removal.cls.l, node.removal.cls.0, node.trim) and the functions designed to produce the test results (do.power.law, do.small.world, do.base.description, do.degree.description, do.links.description, plot.degree, plot.c.degree, plot.cf.degree.comp). 

The required external packages are the following:

- Igraph package version 0.5 or above
- VGAM package version 0.7-5 or above

}
\author{
Domingo Vargas

Maintainer: Domingo Vargas <dvar1965@gmail.com>

}
\references{

Barabasi, A.L., Dezso, Z., Ravasz, E., Yook, S.H., Oltvai, Z. (2002): Scale-Free and hierarchical structures in complex networks. Department of Physics, University of Notre Dame press.
Newman, M., Barabasi, A.L, Watts, D.L. (2006): The structure and dynamics of networks: (Princenton studies in complexity). Princeton University Press.
Newman, M.E.J. (2007): Power law distribution in empirical data. Proceedings of the National Academy of Science the United States of America.
Watts, D. (2004): Small worlds, the dynamics of networks between order and randomness. Princenton University Press.

}

\keyword{
}
\seealso{

}
\examples{

#
# The folowing example shows the utilization of the high level functions
#

library(netmodels)

# Loading the test data object (please note this is a Igraph object)

data(test.net,package="netmodels")

# Test the power law behavior on the degree distribution of the network:

r <- do.power.law(test.net)

# Test the small world behavior of the network:

r <- do.small.world(test.net)

# Get a basic description of the network:

r <- do.base.description(test.net)

# If the network have a multiple links between nodes, maybe its useful to have a comparison of both scenarios:

r <- do.degree.description(test.net)

# Link frequency between nodes:

r <- do.links.description(test.net) 

# Plot the cumulative degree distribution:

plot.c.degree(test.net)

}