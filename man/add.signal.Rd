\name{add.signal}
\alias{add.signal}
\title{add a signal to a strategy...}
\usage{add.signal(strategy, name, arguments, label, ..., enabled=TRUE, indexnum, store=FALSE)}
\description{add a signal to a strategy}
\arguments{\item{strategy}{an object of type 'strategy' to add the signal to}
\item{name}{name of the signal, must correspond to an R function}
\item{arguments}{default arguments to be passed to an signal function when executed}
\item{label}{arbitrary text label for signal output, NULL default will be converted to '<name>.sig'}
\item{...}{any other passthru parameters}
\item{enabled}{TRUE/FALSE whether the signal is enabled for use in applying the strategy, default TRUE}
\item{indexnum}{if you are updating a specific signal, the index number in the $signals list to update}
\item{store}{TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE}}
