\name{setParameterDistribution}
\alias{setParameterDistribution}
\title{Function used to create an object that contains the distribution of parameters to be generated from, before testing parameters of a strategy.}
\usage{
  setParameterDistribution(paramDist = NULL, type = NULL,
    indexnum = 0, distribution = NULL, weight, label,
    psindex = NULL)
}
\arguments{
  \item{paramDist}{The object that store the parameter
  list, if this parameter is missing, or object does not
  exist, the function will return a new object.}

  \item{type}{A character string that specifies the type of
  the parameter, it takes the value in one of 'indicator',
  'signal', 'enter', 'exit', 'order'.}

  \item{indexnum}{Tells the sequence within the type, (for
  example: type = 'signal', indexnum =2 tells the function
  to update the 2nd signal in the strategy)}

  \item{distribution}{Distribution of the parameter, can be
  any function that returns a vector of value. See detail.
  (A numerical example: 1:10 or sample(1:20,6)}

  \item{weight}{The weight of each value in the
  distribution, if missing, the default value of all equal
  weights will be taken.}

  \item{label}{A string label to apply to the parameter
  distribution}

  \item{psindex}{A number specify the index within the
  parameter distribution object, it is used to make change/
  repalce a parameter distribution in the object.}
}
\value{
  The returned object is a structure contains the
  distribution of parameters, if the input argument
  'paramDist' is provided, the function update the input
  paramDist object and return the updated one. When specify
  the distribution of several parameters, usually the first
  returned object is passed to the next several call of the
  function as input argument 'paramDist'. See example.
}
\description{
  Each call to the function will set/update the
  distribution of ONE parameter in the 'parameter
  distribution object' that is associated with a specific
  strategy.
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

  The parameter 'distribution' is a list contains vector of
  values NAMED WITH THE NAMES OF THE PARAMETERS, the values
  can be any type (integer, characters, etc) but must match
  with the leagal value of that parameter. For example:
  distribution=list(nFast=(10:30)) or
  distribution=list(relationship=c('gt','gte'))
}
\examples{
\dontrun{
#(For complete demo see parameterTestMACD.R)
tPD2<-setParameterDistribution(tPD2,'indicator',indexnum=1,
		distribution=list(nFast=(10:30)),label='nFast')
tPD2<-setParameterDistribution(tPD2,'indicator',indexnum=1,
		distribution=list(nSlow=(20:40)),label='nSlow')
tPD2<-setParameterDistribution(tPD2,'signal',indexnum=1,
		distribution=list(relationship=c('gt','gte')),label='sig1.gtgte')
}
}
\author{
  Yu Chen
}

