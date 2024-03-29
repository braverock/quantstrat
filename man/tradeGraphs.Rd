% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tradeGraphs.R
\name{tradeGraphs}
\alias{tradeGraphs}
\title{Draw 3D graphs from tradeStats results using rgl}
\usage{
tradeGraphs(stats, free.params, params.filter = NULL, statistics, title = NULL)
}
\arguments{
\item{stats}{a data frame generated by tradeStats()}

\item{free.params}{a vector of length 2, containing the column names for the data to use on the x and z axes}

\item{params.filter}{- a regular expression to reduce dimensions by filtering on certain columns}

\item{statistics}{a vector containing the column names to produce graphs for}

\item{title}{an optional title to be printed above each graph}
}
\value{
invisible -- called for side-effect
}
\description{
Draw 3D graphs from tradeStats results using rgl
}
\examples{
\dontrun{
tradeGraphs (
     stats = stats,
     free.params = c("Param.indicator.1.nFast", "Param.indicator.2.nSlow"),
     params.filter = "Param.indicator.2.nSlow < 40 & Param.indicator.1.nFast > 5"
     statistics = c("Net.Trading.PL", "maxDrawdown", "Avg.Trade.PL", "Num.Trades")
     title = 'Luxor'
)
}
}
\author{
Jan Humme, rewritten by Chinmay Patil
}
