\name{ruleSignal}
\alias{ruleSignal}
\title{default rule to generate a trade order on a signal}
\usage{
  ruleSignal(data = mktdata, timestamp, sigcol, sigval,
    orderqty = 0, ordertype, orderside = NULL,
    orderset = NULL, threshold = NULL, tmult = FALSE,
    replace = TRUE, delay = 1e-04, osFUN = "osNoOp",
    pricemethod = c("market", "opside", "active"),
    portfolio, symbol, ..., ruletype, TxnFees = 0,
    prefer = NULL, sethold = FALSE, label = "")
}
\arguments{
  \item{data}{an xts object containing market data.
  depending on rules, may need to be in OHLCV or BBO
  formats, and may include indicator and signal
  information}

  \item{timestamp}{timestamp coercible to POSIXct that will
  be the time the order will be inserted on}

  \item{sigcol}{column name to check for signal}

  \item{sigval}{signal value to match against}

  \item{orderqty}{numeric quantity of the desired order, or
  'all', modified by osFUN}

  \item{ordertype}{one of "market","limit","stoplimit",
  "stoptrailing", or "iceberg"}

  \item{orderside}{one of either "long" or "short", default
  NULL, see details}

  \item{orderset}{tag to identify an orderset; if one order
  of the set is filled, all others are canceled}

  \item{threshold}{numeric or function threshold to apply
  to trailing stop orders, default NULL, see Details}

  \item{tmult}{if TRUE, threshold is a percent multiplier
  for \code{price}, not a scalar to be added/subtracted
  from price.  threshold will be dynamically converted to a
  scalar at time of order entry}

  \item{replace}{TRUE/FALSE, whether to replace any other
  open order(s) on this portfolio symbol, default TRUE}

  \item{delay}{what delay to add to timestamp when
  inserting the order into the order book, in seconds}

  \item{osFUN}{function or text descriptor of function to
  use for order sizing, default \code{\link{osNoOp}}}

  \item{pricemethod}{determines how the order price will be
  calculated, see Details}

  \item{portfolio}{text name of the portfolio to place
  orders in}

  \item{symbol}{identifier of the instrument to place
  orders for.  The name of any associated price objects
  (xts prices, usually OHLC) should match these}

  \item{...}{any other passthru parameters}

  \item{ruletype}{one of
  "risk","order","rebalance","exit","entry", see
  \code{\link{add.rule}}}

  \item{TxnFees}{numeric fees (usually negative) or
  function name for calculating TxnFees (processing happens
  later, not in this function)}

  \item{prefer}{price method for getPrice}

  \item{sethold}{boolean, puts entry Rule processing on
  hold, default FALSE}

  \item{label}{rule label, default '', added by
  \code{\link{applyRules}}}
}
\description{
  The \code{ruleSignal} function is a default sample rule
  function for generating orders from a signal.  While some
  strategies may require custom rule functions, we've found
  that ruleSignal is sufficient for many strategies, with
  custom code more often found in indicators and rules.
}
\details{
  \code{pricemethod} may be one of \describe{
  \item{'market', 'opside', or 'active'}{ will use the
  'ask' price if you're buying and the 'bid' price if
  you're selling, crossing the market at the time of order
  entry to attempt to set an aggressive price to get the
  trade. } \item{'passive', 'work' or 'join'}{ which will
  join the 'bid' price if you are buying or join the 'ask'
  price if you are selling, passively working to make
  liquidity at the prevailing market price without crossing
  the market at time of order entry} \item{'maker'}{will
  create a pair of orders for both bid and offer, modeling
  market making activities by having orders on both sides.
  This will then create an Order.Set, and use the
  \code{threshold} to set the prices for these orders.} }

  If \code{threshold} is not numeric or \code{NULL} it
  should be the character string describing a function that
  can calculate a threshold. Ideally this will be a column
  lookup on a non-path-dependent indicator calculated in
  advance.

  If \code{orderside} is NULL, the function will attempt to
  calculate the side from the current position (if any),
  the order quantity, and the order type.
}
\seealso{
  \code{\link{osNoOp}} , \code{\link{add.rule}}
}

