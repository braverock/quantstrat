\name{initStrategy}
\alias{initStrategy}
\title{run standard and custom strategy initialization functions}
\usage{
  initStrategy(strategy, portfolio, symbols,
    get.Symbols = TRUE, init.Portf = TRUE,
    init.Acct = TRUE, init.Orders = TRUE, unique = TRUE,
    ...)
}
\arguments{
  \item{strategy}{object of type \code{strategy} to
  initialize data/containers for}

  \item{portfolio}{portfolio}

  \item{symbols}{symbols}

  \item{get.Symbols}{TRUE/FALSE, default TRUE:}

  \item{init.Portf}{TRUE/FALSE, default TRUE:}

  \item{init.Acct}{TRUE/FALSE, default TRUE:}

  \item{init.Orders}{TRUE/FALSE, default TRUE:}

  \item{unique}{TRUE/FALSE, default TRUE:}

  \item{\dots}{any other passtrhrough parameters}
}
\description{
  run standard and custom strategy initialization functions
}
\author{
  Garrett See, Brian Peterson
}

