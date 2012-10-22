\name{add.constraint}
\alias{add.constraint}
\title{Adds a constraint to 2 distributions within a paramset}
\usage{
  add.constraint(strategy, paramset.label,
    distribution.label.1, distribution.label.2, operator,
    label)
}
\arguments{
  \item{strategy:}{the name of the strategy object}

  \item{paramset.label:}{a label uniquely identifying the
  paramset within the strategy}

  \item{distribution.label.1:}{a label identifying the
  first distribution}

  \item{distribution.label.2:}{a label identifying the
  second distribution}

  \item{operator:}{an operator specifying the relational
  constraint between the 2 distributions}

  \item{label:}{a label uniquely identifying the constraint
  within the paramset}
}
\description{
  Adds a constraint to 2 distributions within a paramset
}
\author{
  Jan Humme
}

