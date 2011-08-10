\name{getParameterTable}
\alias{getParameterTable}
\title{Extract the parameter structure from a strategy object.}
\usage{
  getParameterTable(strategy)
}
\arguments{
  \item{strategy}{The name of the strategy}
}
\description{
  This function is not required for user to specify the
  distribution of parameters, Users can use this function
  to extract the parameters used in a strategy, and as a
  reminder/ cheatsheet when they create the parameter
  distribution.
}
\details{
  In the returned object: $paramNameList is the list of
  parameters used in the strategy, easy for print and view
  as a table. $strategyName is the name of the strategy
  itself. $structure is the detailed paramter structure,
  can be ignored.
}
\author{
  Yu Chen
}

