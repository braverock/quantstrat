\name{strategy}
\alias{strategy}
\title{constructor for objects of type 'strategy'}
\usage{
  strategy(name, ..., assets = NULL, constraints = NULL,
    store = FALSE)
}
\arguments{
  \item{name}{character string naming the strategy}

  \item{...}{any other passthru parameters}

  \item{assets}{optional list of assets to apply the
  strategy to, should normally be defined in the portfolio,
  not here}

  \item{constraints}{optional portfolio constraints object
  matching assets}

  \item{store}{TRUE/FALSE whether to store the strategy in
  the .strategy environment, or return it.  default FALSE}
}
\description{
  variables passed in dots will be added to the strategy
  object, and may be used by initialization and wrapup
  functions, as well as indicators, signals, and rules.
}
\seealso{
  \code{\link{applyStrategy}}
}

