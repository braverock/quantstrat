\name{applySignals}
\alias{applySignals}
\title{apply the signals in the strategy to arbitrary market data}
\usage{
  applySignals(strategy, mktdata, indicators = NULL,
    parameters = NULL, ...)
}
\arguments{
  \item{strategy}{an object of type 'strategy' to add the
  signal to}

  \item{mktdata}{an xts object containing market data.
  depending on signals, may need to be in OHLCV or BBO
  formats}

  \item{indicators}{if indicator output is not contained in
  the mktdata object, it may be passed separately as an xts
  object or a list.}

  \item{parameters}{named list of parameters to be applied
  during evaluation of the strategy}

  \item{...}{any other passthru parameters}
}
\description{
  apply the signals in the strategy to arbitrary market
  data
}

