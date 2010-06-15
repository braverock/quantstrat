\name{sigComparison}
\alias{sigComparison}
\title{generate comparison signal...}
\usage{sigComparison(label, data, columns, relationship=c("gt", "lt", "eq",
    "gte", "lte"))}
\description{generate comparison signal}
\details{Currently, this function compares two columns.  
Patches to compare an arbitrary number of columns would be gladly accepted.

Comparison will be applied from the first to the second column in the \code{columns} vector.

Relationship 'op' means 'opposite' side.  Reasonable attempt will be made to match.}
\arguments{\item{label}{text label to apply to the output}
\item{data}{data to apply comparison to}
\item{columns}{named columns to apply comparison to}
\item{relationship}{one of c("gt","lt","eq","gte","lte","op") or reasonable alternatives}}
