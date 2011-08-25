\name{setParameterConstraint}
\alias{setParameterConstraint}
\title{Function to construct parameter constraint object.}
\usage{
  setParameterConstraint(paramConstraintObj = list(),
  constraintLabel, paramList, relationship)
}
\arguments{
  \item{paramConstraintObj}{the ParameterConstraint object
  to be updated on, if missing, funtion will create a new
  one.}

  \item{constraintLabel}{string label to apply to the
  constraint}

  \item{paramList}{the two name of the prameters as a list
  contains two strings.}

  \item{relationship}{relationship between the 1st
  parameter and 2nd one. ('gt' means 1st parameter > 2nd
  parameter)}
}
\description{
  Function to construct parameter constraint object.
}
\examples{
#(For complete demo see parameterTestMACD.R)
#In a MACD strategy, we want to fast macd calcuated from less time periods (days) than slow macd signal:
\dontrun{
pConstraint2<-setParameterConstraint(constraintLabel='macdPC',paramList=c('nFast','nSlow'),relationship='lt')
}
#The object pConstraint2 then can be used in applyParameter function to specify the constrains between parameters.
}
\author{
  Yu Chen
}

