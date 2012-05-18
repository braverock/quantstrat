\name{paramConstraint}
\alias{paramConstraint}
\title{Internal function used in applyParameter function for process constraints on relationship between two parameter values. Basicly is the same as sigComparison function in signal.R written by Brian, with miner change.}
\usage{
  paramConstraint(label, data = mktdata, columns,
    relationship = c("gt", "lt", "eq", "gte", "lte"))
}
\arguments{
  \item{label}{text label to apply to the constraint}

  \item{data}{data to apply comparison to}

  \item{columns}{named columns to apply comparison to}

  \item{relationship}{one of
  c("gt","lt","eq","gte","lte","op") or reasonable
  alternatives}
}
\description{
  Currently, this function compares two columns. Patches to
  compare an arbitrary number of columns would be gladly
  accepted.
}
\details{
  Comparison will be applied from the first to the second
  column in the \code{columns} vector.

  Relationship 'op' means 'opposite' side.  Reasonable
  attempt will be made to match.
}

