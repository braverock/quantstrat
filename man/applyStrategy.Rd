\name{applyStrategy}
\alias{applyStrategy}
\title{apply the strategy to arbitrary market data...}
\usage{applyStrategy(strategy, portfolios, mktdata, ...)}
\description{apply the strategy to arbitrary market data}
\arguments{\item{strategy}{an object of type 'strategy' to add the indicator to}
\item{portfolios}{a list of portfolios to apply the strategy to}
\item{mktdata}{an xts object containing market data.  depending on indicators, may need to be in OHLCV or BBO formats, default NULL}
\item{...}{any other passthru parameters}}
