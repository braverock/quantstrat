\name{sigThreshold}
\alias{sigThreshold}
\title{generate a threshold signal...}
\usage{sigThreshold(label, data, column, threshold=0, relationship=c("gt", "lt", "eq", "gte", "lte"))}
\description{generate a threshold signal}
\details{Many strategies, including RSI or MACD styles, make trading decisions when an indicator 
is over or under a specific threshold.  
This function generates the appropriate signal based on such a threshold.}
\arguments{\item{label}{text label to apply to the output}
\item{data}{data to apply comparison to}
\item{column}{named column to apply comparison to}
\item{threshold}{numeric threhold to test for}
\item{relationship}{one of c("gt","lt","eq","gte","lte") or reasonable alternatives}}
