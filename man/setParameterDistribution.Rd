\name{setParameterDistribution}
\alias{setParameterDistribution}
\title{Function used to set distribution of a parameter in a strategy.}
\usage{
  setParameterDistribution(paramDist = NULL, type = NULL,
  indexnum = 0, distribution = NULL, weight, label, psindex
  = NULL)
}
\arguments{
  \item{paramDist}{the object name that store the parameter
  list}

  \item{type}{indicator/signal/enter/exit/order}

  \item{indexnum}{tells the sequence within the type, (if
  the type is signal, indexnum =2 means the 2nd signal in
  the strategy)}

  \item{distribution}{distribution of the parameter, can be
  any function that return a vector. (example: 1:10 or
  sample(1:20,6)}

  \item{weight}{the weight of each value in the
  distribution, default value will be all equally
  weighted.}

  \item{psindex}{specify the index within the parameter
  distribution object, it is used to make change/ repalce a
  parameter distribution in the object.}
}
\description{
  Function used to set distribution of a parameter in a
  strategy.
}
\author{
  Yu Chen
}

