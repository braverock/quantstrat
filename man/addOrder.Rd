\name{addOrder}
\alias{addOrder}
\title{add an order to the order book}
\usage{
  addOrder(portfolio, symbol, timestamp, qty, price,
    ordertype, side, orderset = "", threshold = NULL,
    status = "open", statustimestamp = "", prefer = "",
    delay = 1e-05, tmult = FALSE, replace = TRUE,
    return = FALSE, ..., TxnFees = 0, label = "")
}
\arguments{
  \item{portfolio}{text name of the portfolio to associate
  the order book with}

  \item{symbol}{identfier of the instrument to find orders
  for.  The name of any associated price objects (xts
  prices, usually OHLC) should match these}

  \item{timestamp}{timestamp coercible to POSIXct that will
  be the time to search for orders before this time}

  \item{qty}{numeric quantity of the order}

  \item{price}{numeric price at which the order is to be
  inserted}

  \item{ordertype}{one of "market","limit","stoplimit",
  "stoptrailing",or "iceberg"}

  \item{side}{one of either "long" or "short"}

  \item{threshold}{numeric threshold to apply to trailing
  stop orders and limit orders, default NULL}

  \item{status}{one of "open", "closed", "canceled", or
  "replaced", default "open"}

  \item{statustimestamp}{timestamp of a status update, will
  be blank when order is initiated}

  \item{delay}{what delay to add to timestamp when
  inserting the order into the order book, in seconds}

  \item{tmult}{if TRUE, threshold is a percent multiplier
  for \code{price}, not a scalar to be added/subtracted
  from price.  threshold will be dynamically converted to a
  scalar at time of order entry}

  \item{replace}{TRUE/FALSE, whether to replace any other
  open order(s) on this portfolio symbol, default TRUE}

  \item{return}{if TRUE, return the row that makes up the
  order, default FALSE (will assign into the environment)}

  \item{\dots}{any other passthru parameters}

  \item{TxnFees}{numeric fees (usually negative) or
  function name for calculating TxnFees (processing happens
  later, not in this function)}

  \item{label}{text label, default to '', set to rule label
  by \code{\link{ruleSignal}}}
}
\description{
  It is important to understand that all the order
  functionality included in \code{quantstrat} exists to
  more closely model a real trading environment both in
  backtesting and in production. Many backtesting systems
  make a set of assumptions about instant execution, and we
  have chosen not to do this in quantstrat, because real
  quantitative trading systems do not have instant
  execution.  They make decisions (the Rules) and then
  enter orders (the province of this function in
  backtesting), during which there is some \code{delay}
  between receiving the data that fires the Signal and
  Rule, and the time the order reaches the market, and then
  those orders \emph{MAY} become transactions if market
  prices and liquidity cooperate.
}
\details{
  By default, this function will locate and replace any
  'open' order(s) on the requested portfolio/symbol that
  have the same type and side. If you do not want open
  orders to be canceled and replaced with the new order,
  set \code{replace=FALSE}.

  We have modeled two types of stop orders, which should be
  sufficient to model most types of stops.

  We have modeled the simplest type, a 'stoplimit' order,
  which is just a limit order used to enter or exit a
  position at a specific price. Threshold multipliers have
  also been added for stoplimit.  These allow a threshold
  to be set as a multiplier of the current price. For
  example, to set a stoplimit order at the current price
  plus ten percent, you would set the threshold multiplier
  to 1.10.  The threshold multiplier (or scalar) on a
  stoplimit order will be converted to a price at order
  entry. There is no functional different between a regular
  'limit' order and a 'stoplimit' order once entered into
  the order book, but the distinction will likely be useful
  for reporting on when stops have been triggered.

  We have also modeled a 'stoptrailing' order, which may be
  used to model dynamic limit-based entry or exit. If you
  set \code{tmult=TRUE} on a stoptrailing order, the size
  of the threshold will be set as a difference between the
  multiplier times the price and the current price at order
  entry.  in this way, a 10% trailing entry (exit) will not
  change in size from the current price as the price
  changes.  It is effectively converted to a scalar at
  order entry.  While this functionality could change in
  the future, we believe this is more conservative than
  growing or shrinking the threshold distance from the
  current market price in relation to the threshold, and
  will result in fewer unintended consequences and more
  understandable behavior. Comments Welcome.

  The 'stop*' or 'iceberg' order types are the only order
  type that makes use of the order \code{threshold}. Scalar
  thresholds \code{tmult=FALSE} on stoplimit or
  stoptrailing orders will be added to the current market
  price to set the limit price.  In other worlds, a scalar
  threshold is the difference either positive or negative
  from the current price when the order is entered. With a
  stoptrailing order, the order may be moved (via
  cancel/replace) frequently.

  Some markets and brokers recognize a stop that triggers a
  market order, when the stop is triggered, a market order
  will be executed at the then-prevailing price.  We have
  not modeled this type of order.

  We have also added the 'iceberg' order type.  This order
  type should most often be paired with \code{delay} and
  \code{\link{osMaxPos}}.  The iceberg order when initially
  entered is treated like a limit order, with an optional
  threshold (which is applied at initial order entry, so be
  careful).  Right now, they will enter a new order at
  price+threshold upon any execution of the prior iceberg
  order.  This process could be infinite if
  \code{\link{osMaxPos}} or an equivalent order sizing
  function is not used to limit total position size. An
  order \code{delay} is also advisable to model the delay
  that occurs between getting the trade confirmation of the
  previous trade and entering the new order into the order
  book.

  If you ever wanted to move from a backtesting mode to a
  production mode, this function (and the linked funtion
  \code{\link{ruleOrderProc}}) would need to be replaced by
  functions that worked against your execution environment.
  Basically, the execution environment must provide three
  interfaces in a live trading environment:

  \enumerate{ \item a market data interface to provide
  updated market data, usually accessed in an event loop

  \item an order interface for sending orders (and
  canceling or updating them) to the market

  \item a fill interface that reports the transaction
  details when an order has been filled }

  Conversion to a live trading environment will also likely
  require a new version of \code{\link{applyStrategy}} to
  provide the event loop interface and interact with
  \code{mktdata}.
}
\seealso{
  getOrderBook

  updateOrders
}
\concept{
  backtest

  order book
}

