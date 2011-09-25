\name{applyStrategy}
\alias{applyStrategy}
\title{apply the strategy to arbitrary market data}
\usage{
  applyStrategy(strategy, portfolios, mktdata = NULL,
  parameters = NULL, ..., verbose = TRUE, symbols = NULL)
}
\arguments{
  \item{strategy}{an object of type 'strategy' to add the
  indicator to}

  \item{portfolios}{a list of portfolios to apply the
  strategy to}

  \item{mktdata}{an xts object containing market data.
  depending on indicators, may need to be in OHLCV or BBO
  formats, default NULL}

  \item{parameters}{named list of parameters to be applied
  during evaluation of the strategy, default NULL}

  \item{...}{any other passthru parameters}

  \item{verbose}{if TRUE, return output list}

  \item{symbols}{character vector identifying symbols to
  initialize a portfolio for, default NULL}
}
\description{
  if \code{mktdata} is NULL, the default, the mktdata
  variable will be populated for each symbol via a call to
  get (getSymbols??, not yet)
}

