\name{applyIndicators}
\alias{applyIndicators}
\title{apply the indicators in the strategy to arbitrary market data}
\usage{
  applyIndicators(strategy, mktdata, parameters = NULL,
    ...)
}
\arguments{
  \item{strategy}{an object of type 'strategy' to add the
  indicator to}

  \item{mktdata}{an xts object containing market data.
  depending on indicators, may need to be in OHLCV or BBO
  formats}

  \item{parameters}{named list of parameters to be applied
  during evaluation of the strategy}

  \item{...}{any other passthru parameters}
}
\value{
  \code{mktdata} with indicators colums added.
}
\description{
  apply the indicators in the strategy to arbitrary market
  data
}
\examples{
\dontrun{
strategy("example", store=TRUE)
getSymbols("SPY", src='yahoo')
add.indicator('example', 'SMA', arguments=list(x=quote(Ad(SPY)), n=20))
str(getStrategy('example')$indicators)
out <- applyIndicators('example', SPY)
tail(out)
}
}

