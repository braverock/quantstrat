\name{apply.paramset}
\alias{apply.paramset}
\title{Apply a paramset to the strategy}
\usage{
  apply.paramset(strategy, paramset.label, portfolio.st,
    nsamples = 0, verbose = FALSE)
}
\arguments{
  \item{strategy}{the name of the strategy object}

  \item{paramset.label}{a label uniquely identifying the
  paramset within the strategy}

  \item{portfolio.st}{a string variable}

  \item{nsamples}{if > 0 then take a sample of only size
  nsamples from the paramset}

  \item{verbose}{return full information, in particular the
  .blotter environment, default FALSE}
}
\description{
  This function will run applyStrategy() on portfolio.st,
  once for each parameter combination as specified by the
  parameter distributions and constraints in the paramset.
  Results are gathered and returned as a list containing a
  slot for each parameter combination.
}
\details{
  apply.paramset uses the foreach package to start the runs
  for each parameter combination, and as such allows for
  parallel processing. It is up to the caller to load and
  register an appropriate backend, eg. doMC, doParallel or
  doRedis.
}
\author{
  Jan Humme
}
\seealso{
  \code{\link{add.constraint}},
  \code{\link{add.constraint}},
  \code{\link{delete.paramset}}
}

