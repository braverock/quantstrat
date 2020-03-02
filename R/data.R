#' AAPL time series of daily OHLCVA bars
#'
#' A dataset containing the daily OHLCVA values of AAPL from 2017-01-03 to
#' 2018-12-28. The code to reproduce the dataset from yahoo is:
#' 
#' getSymbols(symbol,from=start_date, to = end_date, auto.assign = T, index.class = "POSIXct", src = 'yahoo')
#' for(i in symbols) assign(i, adjustOHLC(get(i),use.Adjusted=TRUE))
#' 
#' We have chosen to fix the dataset, since we have no control over changes at
#' the source, which would break our tests.
#' 
#' The variables are as follows:
#'
#' @format An xts object with 501 rows and 6 variables:
#' \describe{
#'   \item{Open}{Daily Open prices}
#'   \item{High}{Daily High prices}
#'   \item{Low}{Daily Low prices}
#'   \item{Close}{Daily Close prices}
#'   \item{Volume}{Aggregate Daily volume traded}
#'   \item{Adjusted}{Adjusted prices}
#' }
"AAPL"