\name{add.constraint}
\alias{add.constraint}
\title{Adds a constraint on 2 distributions within a paramset}
\usage{
  add.constraint(strategy, paramset.label,
    distribution.label.1, distribution.label.2, operator,
    label, store = TRUE)
}
\arguments{
  \item{strategy}{the name of the strategy object to add
  the constraint to}

  \item{paramset.label}{a label uniquely identifying the
  paramset within the strategy}

  \item{distribution.label.1}{a label identifying the first
  distribution}

  \item{distribution.label.2}{a label identifying the
  second distribution}

  \item{operator}{an operator specifying the relational
  constraint between the 2 distributions}

  \item{label}{a label uniquely identifying the constraint
  within the paramset}

  \item{store}{indicates whether to store the strategy in
  the .strategy environment}
}
\description{
  Creates a constraint on 2 distributions in a paramset,
  i.e. a restriction limiting the allowed combinations from
  the ranges for distribution 1 and distribution 2.
}
\author{
  Jan Humme
}
\seealso{
  \code{\link{add.distribution}},
  \code{\link{delete.paramset}},
  \code{\link{apply.paramset}}
}

