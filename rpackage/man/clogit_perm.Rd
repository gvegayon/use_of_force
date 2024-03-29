% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clogit_perm.R
\name{clogit_perm}
\alias{clogit_perm}
\title{Conditional logit with permutation}
\usage{
clogit_perm(
  nperm = 1000,
  formula,
  data,
  ...,
  ncpus = parallel::detectCores(),
  parallel_args = list()
)
}
\arguments{
\item{nperm}{Integer. Number of permutations}

\item{formula, data, ...}{Parameters passed to \code{\link[survival:clogit]{survival::clogit()}}}

\item{ncpus}{Integer. Number or cores.}

\item{parallel_args}{Parameters passed to \code{\link[parallel:makeCluster]{parallel::makeCluster()}}.}
}
\description{
Conditional logit with permutation
}
\details{
In the case that p-values go to zero, these are replaced with the
pseudo-count, this is, \code{1/nperm} (Knijnenburg et al., 2009.)
}
\references{
Knijnenburg, T. A., Wessels, L. F. A., Reinders, M. J. T., & Shmulevich, I. (2009).
Fewer permutations, more accurate P-values. Bioinformatics, 25(12), 161–168.
\doi{10.1093/bioinformatics/btp211}
}
