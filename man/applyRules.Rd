\name{applyRules}
\alias{applyRules}
\title{apply the rules in the strategy to arbitrary market data...}
\usage{applyRules(strategy, mktdata, Dates, indicators, signals, ..., path.dep=TRUE)}
\description{apply the rules in the strategy to arbitrary market data}
\details{In typical usage, this function will be called via \code{\link{applyStrategy}}.  
In this mode, this function will be called twice, once with \code{path.dep=FALSE} 
and then again in stepping over the time indexes of the mktdata object.}
\seealso{\code{\link{add.rule}} \code{\link{applyStrategy}}}
\arguments{\item{strategy}{an object of type 'strategy' to add the rule to}
\item{mktdata}{an xts object containing market data.  depending on rules, may need to be in OHLCV or BBO formats, and may include indicator and signal information}
\item{Dates}{default NULL, list of time stamps to iterate over, ignored if \code{path.dep=FALSE}}
\item{indicators}{if indicator output is not contained in the mktdata object, it may be passed separately as an xts object or a list.}
\item{signals}{if signal output is not contained in the mktdata object, it may be passed separately as an xts object or a list.}
\item{...}{any other passthru parameters}
\item{path.dep}{TRUE/FALSE whether rule is path dependent, default TRUE, see Details}}
