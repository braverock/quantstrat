\name{sigCrossover}
\alias{sigCrossover}
\title{generate a crossover signal}
\usage{
  sigCrossover(label, data = mktdata, columns,
    relationship = c("gt", "lt", "eq", "gte", "lte"))
}
\arguments{
  \item{label}{text label to apply to the output}

  \item{data}{data to apply crossover to}

  \item{columns}{named columns to apply crossover of the
  first against the second}

  \item{relationship}{one of c("gt","lt","eq","gte","lte")
  or reasonable alternatives}
}
\description{
  This will generate a crossover signal, which is a
  dimension-reduced version of a comparison signal
  \code{\link{sigComparison}}.
}
\details{
  It will return TRUE on the period in which there is a
  crossover in the direction specified by
  \code{relationship}, and NA otherwise.

  If you want all the information, use a comparison
  instead.
}

