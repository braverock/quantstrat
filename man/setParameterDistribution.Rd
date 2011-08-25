\name{setParameterDistribution}
\alias{setParameterDistribution}
\title{Function used to set distribution of a parameter to be generated from, for testing a strategy.}
\usage{
  setParameterDistribution(paramDist = NULL, type = NULL,
  indexnum = 0, distribution = NULL, weight, label, psindex
  = NULL)
}
\arguments{
  \item{paramDist}{The object that store the parameter
  list, if this parameter is missing, or object does not
  exist, the function will create a new object.}

  \item{type}{A character string that specifies the type of
  the parameter, it takes the value of one of 'indicator',
  'signal', 'enter', 'exit', 'order'.}

  \item{indexnum}{Tells the sequence within the type, (If
  the type is 'signal', indexnum =2 tells the function to
  update the 2nd signal in the strategy)}

  \item{distribution}{Distribution of the parameter, can be
  any function that return a vector of value. See detail.
  (A numerical example: 1:10 or sample(1:20,6)}

  \item{weight}{The weight of each value in the
  distribution, default value will be all equally
  weighted.}

  \item{label}{string label to apply to the parameter
  distribution}

  \item{psindex}{specify the index within the parameter
  distribution object, it is used to make change/ repalce a
  parameter distribution in the object.}
}
\value{
  The returned object is a structure contains the
  distribution of parameters, is of the same type as the
  input parameter paramDist (if provided), the function
  update the input paramDist object and return the updated
  one. Usually the returned value is pass to the same
  object as parameter. See details.
}
\description{
  Each call to the function will set/update the
  distribution of ONE parameter in the strategy.
}
\details{
  When call the function, the user must know the parameter
  strcture of the strategy, function getParameterTable can
  be used to layout the parameter structure of a certain
  strategy. Parameter distribution object for one strategy
  usually won't work for another strategy, because
  different strategies has different parameter structure.
  Type of the parameter and the sequence in that type is
  needed to specify the exact parameter in THAT STRATEGY.

  The parameter of the function 'distribution' is a list
  contains vector of values named WITH THE NAME OF THE
  PARAMETER, the values can be any type (integer,
  characters, etc) matches with the leagal value of that
  parameter. For example: distribution=list(nFast=(10:30))
  or distribution=list(relationship=c('gt','gte'))
}
\examples{
\dontrun{
#(For complete demo see parameterTestMACD.R)
tPD2<-setParameterDistribution(tPD2,'indicator',indexnum=1,distribution=list(nFast=(10:30)),label='nFast')
tPD2<-setParameterDistribution(tPD2,'indicator',indexnum=1,distribution=list(nSlow=(20:40)),label='nSlow')
tPD2<-setParameterDistribution(tPD2,'signal',indexnum=1,distribution=list(relationship=c('gt','gte')),label='sig1.gtgte')
}
}
\author{
  Yu Chen
}

