\name{updateOrders}
\alias{updateOrders}
\title{update an order or orders}
\usage{
  updateOrders(portfolio, symbol, timespan,
    ordertype = NULL, side = NULL, qtysign = NULL,
    oldstatus = "open", newstatus, statustimestamp,
    orderset = NULL)
}
\arguments{
  \item{portfolio}{text name of the portfolio to associate
  the order book with}

  \item{symbol}{identfier of the instrument to find orders
  for.  The name of any associated price objects (xts
  prices, usually OHLC) should match these}

  \item{timespan}{xts-style character timespan to be the
  period to find orders of the given status and ordertype}

  \item{ordertype}{one of NULL,
  "market","limit","stoplimit", or "stoptrailing" default
  NULL}

  \item{side}{one of NULL, "long" or "short", default NULL}

  \item{qtysign}{one of NULL, -1,0,1 ; could be useful when
  all qty's are reported as positive numbers and need to be
  identified other ways, default NULL}

  \item{set}{a tag identifying the orderset}

  \item{oldstatus}{one of NULL, "open", "closed",
  "canceled", or "replaced", default "open"}

  \item{newstatus}{one of "open", "closed", "canceled", or
  "replaced"}

  \item{statustimestamp}{timestamp of a status update, will
  be blank when order is initiated}
}
\description{
  When an order gets filled, it should have its status
  moved to 'closed'.
}
\details{
  When an order is updated with a new order, the order
  status should change to 'replaced' with a StatusTime that
  is the same as the one for the new order.  This could
  happen in the case of a traditional Cancel/Replace,
  because of a trailing stop, or in the case of a partial
  fill that needs to enter a replaced order for the
  remainder.

  When a risk event or over-limit event happens, typically
  open orders will be 'canceled'. Possibly new orders will
  be added to close open positions. Many models will also
  want to run a process at the close of market that will
  cancel all open orders.
}
\seealso{
  addOrder

  getOrders

  getOrderBook
}
\concept{
  order book
}

