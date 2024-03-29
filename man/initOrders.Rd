% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/orders.R
\name{initOrders}
\alias{initOrders}
\title{initialize order container}
\usage{
initOrders(portfolio = NULL, symbols = NULL, initDate = "1950-01-01", ...)
}
\arguments{
\item{portfolio}{text name of the portfolio to associate the order book with}

\item{symbols}{a list of identifiers of the instruments to be contained in the Portfolio.  The name of any associated price objects (xts prices, usually OHLC) should match these}

\item{initDate}{date (ISO8601) prior to the first close price given in mktdata, used to initialize the order book with a dummy order}

\item{\dots}{any other passthrough parameters}
}
\description{
This function sets up the order container by portfolio.
}
\details{
If no symbols list is provided (the default) the function will attempt 
to retrieve the symbols list from the portfolio in the trade blotter.
}
\concept{order book}
