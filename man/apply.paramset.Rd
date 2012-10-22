\name{apply.paramset}
\alias{apply.paramset}
\title{Apply a paramset to the strategy}
\usage{
  apply.paramset(strategy, paramset.label, portfolio.st,
    nsamples = 0, verbose = FALSE)
}
\arguments{
  \item{strategy:}{the name of the strategy object}

  \item{paramset.label:}{a label uniquely identifying the
  paramset within the strategy}

  \item{portfolio.st:}{a string variable}

  \item{nsamples:}{if > 0 then take a sample of only size
  nsamples from the paramset}

  \item{verbose}{}
}
\description{
  Apply a paramset to the strategy
}
\author{
  Jan Humme
}

