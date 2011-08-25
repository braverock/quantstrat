\name{getParameterTable}
\alias{getParameterTable}
\title{Extract the parameter structure from a strategy object.}
\usage{
  getParameterTable(strategy)
}
\arguments{
  \item{strategy}{The strategy object.}
}
\value{
  A list of objects that contains the parameter structure
  information" \describe{ \item{paramNameList}{the list of
  parameters used in the strategy, for printing or viewing
  as a table.} \item{strategyName}{ string name of the
  strategy} \item{structure}{the detailed paramter
  structure in the input strategy, can be used when user
  wants to look into more details of the parameter
  structure.} }
}
\description{
  Users can use this function to extract the parameters
  used in a strategy, and as a reminder/ cheatsheet when
  they create the parameter distribution. But it's not
  required to be used to specify the distribution of
  parameters.
}
\examples{
# When strategy object stratMACD has already been created by macd.R demo:
# bfollowing line will return the parameter information to object paramStructure:
\dontrun{
x<-getParameterTable(stratMACD)
}
}
\author{
  Yu Chen
}

