\name{add.indicator}
\alias{add.indicator}
\title{add an indicator to a strategy...}
\usage{add.indicator(strategy, name, arguments, label, ..., enabled=TRUE,
    indexnum, store=FALSE)}
\description{add an indicator to a strategy}
\arguments{\item{strategy}{an object of type 'strategy' to add the indicator to}
\item{name}{name of the indicator, must correspond to an R function}
\item{arguments}{default arguments to be passed to an indicator function when executed}
\item{label}{arbitrary text label for indicator output, NULL default will be converted to '<name>.ind'}
\item{...}{any other passthru parameters}
\item{enabled}{TRUE/FALSE whether the indicator is enabled for use in applying the strategy, default TRUE}
\item{indexnum}{if you are updating a specific indicator, the index number in the $indicators list to update}
\item{store}{TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE}}
