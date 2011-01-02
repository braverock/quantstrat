\name{initOrders}
\alias{initOrders}
\title{initialize order container...}
\usage{initOrders(portfolio, symbols, initDate="1999-12-31", ...)}
\description{initialize order container}
\details{This function sets up the order container by portfolio.

If no symbols list is provided (the default) the function will attempt 
to retrieve the symbols list from the portfolio in the trade blotter.}
\arguments{\item{portfolio}{text name of the portfolio to associate the order book with}
\item{symbols}{a list of identfiers of the instruments to be contained in the Portfolio.  The name of any associated price objects (xts prices, usually OHLC) should match these}
\item{initDate}{date (ISO8601) prior to the first close price given in mktdata, used to initialize the order book with a dummy order}
\item{\dots}{any other passthrough parameters}}
