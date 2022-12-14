% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_grid.R
\name{make_grid}
\alias{make_grid}
\title{Makes grid for vectors of purities and ploidies}
\usage{
make_grid(purity, ploidy)
}
\arguments{
\item{purity}{vector of purities}

\item{ploidy}{vector of ploidies}
}
\value{
matrix comprising the cartesian product of the vector of purities and the vector of ploidies
}
\description{
\code{make_grid} creates the cartesian product of a vector of purities and a vector of ploidies.
}
\examples{
# to make grid as in van Dijk et al.
grid = make_grid(purity = seq(0.2, 1, 0.01), ploidy = seq(1.5, 5, 0.01))
}
\references{
{\insertRef{van2021chromosomal}{CNHplus}}
}
