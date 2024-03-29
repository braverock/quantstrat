% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rules.R
\name{applyRules}
\alias{applyRules}
\title{apply the rules in the strategy to arbitrary market data}
\usage{
applyRules(
  portfolio,
  symbol,
  strategy,
  mktdata,
  indicators = NULL,
  signals = NULL,
  parameters = NULL,
  ...,
  path.dep = TRUE,
  rule.order = NULL,
  rule.subset = NULL,
  debug = FALSE
)
}
\arguments{
\item{portfolio}{text name of the portfolio to associate the order book with}

\item{symbol}{identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these}

\item{strategy}{an object of type 'strategy' to add the rule to}

\item{mktdata}{an xts object containing market data.  depending on rules, may need to be in OHLCV or BBO formats, and may include indicator and signal information}

\item{indicators}{if indicator output is not contained in the mktdata object, it may be passed separately as an xts object or a list.}

\item{signals}{if signal output is not contained in the mktdata object, it may be passed separately as an xts object or a list.}

\item{parameters}{named list of parameters to be applied during evaluation of the strategy,default NULL, only needed if you need special names to avoid argument collision}

\item{...}{any other passthru parameters}

\item{path.dep}{TRUE/FALSE whether rule is path dependent, default TRUE, see Details}

\item{rule.order}{default NULL, use at your own risk to adjust order of rule evaluation}

\item{rule.subset}{ISO-8601 subset for period to execute rules over, default NULL}

\item{debug}{if TRUE, return output list}
}
\description{
In typical usage, this function will be called via \code{\link{applyStrategy}}.  
In this mode, this function will be called twice, once with \code{path.dep=FALSE} 
and then again in stepping over the time indexes of the mktdata object.
}
\details{
This function, because of its path dependent nature and the order of rule 
evaluation discussed in \code{\link{add.rule}}, will likely take up most of the 
execution time of a strategy backtest.

Individual rule functions may need to use <<- to place \code{hold} and \code{holdtill}
variables into play.  These would be most likely implemented by risk rules.  When
\code{hold==TRUE}, any open oders will still be processed (orders are \emph{NOT} 
canceled automatically, but no new orders may be entered.  \code{type='risk'}
rules will still function during a hold.  Note that hold must be set via a custom
rule.  We tend to set hold in an order or risk rule. 

\code{quantstrat} has a significant amount of logic devoted to handling 
path-dependent rule execution.  Most of that code/logic resides in this
function.  

This function, along with \code{\link{ruleOrderProc}}, \code{\link{addOrder}}, and 
\code{\link{applyStrategy}} will likely need to be replaced to connect to a live 
market infrastructure.
}
\section{Dimension Reduction for Performance}{

In evaluation of path-dependent rules, the simplest method, 
and the one we used initially, is to check the rules on every observation 
in the time series of market data.  
There are cases where this will still be required, but we hope to limit them as much as possible.
Looping in \R is generally discouraged, and on high frequency data for 
strategy evaluation it can produce completely unacceptable results.

The solution we've employed is to utilize a state machine to evaluate the rules only 
when deemed necessary.
This approach makes use of what we know about the strategy and
the orders the strategy places (or may place) to reduce the dimensionality of the problem.

As discussed in \code{\link{add.rule}}, the first step in this dimension 
reduction is to look for places in the time series where signals may cause the strategy to 
enter or change orders.  This creates an index of timestamps that must be evaluated.
This index should be significantly shorter than the full number of observations.    
\code{quantstrat} will always run \code{applyRules} on each of these indices
where we've previously figured out that the strategy might want to do something.

The next step in dimension reduction works on the order book.  
If there are open orders, we need to figure out when they might get filled.  
For market orders, this is the next observation.  For limit orders, we can 
locate the index timestamps after the order is placed to see when the
order might cross.  We will add this index to the list of indices to be 
evaluated.  There is of course no guarantee that the order will still be 
open at that time, that trading will not be on \code{hold} because of a risk rule, 
or that something else hasn't interfered.  Adding the index to the list only tells
the loop inside \code{applyRules} that rules (including order processing rules) 
need to be checked at that index, to see if anything needs to happen.

For trailing orders, the picture is somewhat more complicated.  Trailing orders
\emph{may} move on each new observation, per the method described in 
\code{\link{addOrder}}. To speed up evaluation of when such an
order may cross, we need to combine the possible crossing logic for 
the limit orders, above, with some additional logic to handle the 
trailing orders. We begin by evaluating when the order price might 
be moved. We then examine the market data between the current index and 
the point at which the order may move. if there is a (possible) cross, 
we insert that index into the indices for examination.  If not, we insert 
the index of the next probable move.

It should be noted that this dimension reduction methodology does 'look ahead'
in the data.  This 'look ahead' is only done \emph{after} the order has been 
entered in the normal path-dependent process, and only to insert new indices for 
evaluation, and so should not introduce biases.
}

\seealso{
\code{\link{add.rule}} \code{\link{applyStrategy}}
}
