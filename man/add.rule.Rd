\name{add.rule}
\alias{add.rule}
\title{add a rule to a strategy...}
\usage{add.rule(strategy, name, arguments, parameters, label, type=c(NULL,
    "risk", "order", "rebalance", "exit", "enter"), ..., enabled=TRUE,
    indexnum, path.dep=TRUE, timespan, store=FALSE)}
\description{add a rule to a strategy}
\details{Rules will be processed in a very particular manner, so it bears going over.

First, rules are either path dependent or non-path-dependent.  Path dependent rules 
will be processed in every time increment for the \code{mktdata} passed into
\code{\link{applyStrategy}}.  Non path dependent rules will likely be quite rare in real life, 
and will be applied after indicators and signals, and before path-dependent rules are processed.

All rules have a \code{type}.  These may be any of:
\itemize{
\item{risk}{ rules that check and react to risk of positions, may stop all other rule execution temporarily or permanently}
\item{order}{ rules for order processing of any open orders at time t, always path-dependent}
\item{rebalance}{ rules executed specifically in a portfolio context, unnecessary in univariate strategies}
\item{exit}{ rules to determine whether to exit a position}
\item{enter}{ rules to determine whether to enter or increase a position}
}  
The rules will be executed by type, in the order listed above.  
Multiple rules of each type may be defined, as with signals and indicators, 
they will be executed in order by index number with any other rules sharing the same 
type.

The rule execution order was constructed because path-dependent rules may modify   
the ability of rules that have not fired yet to be evaluated.  For example, a 
risk rule may flatten (close out) an entire position and put new orders 
on hold, effectively stopping all further execution of the strategy.  
Another example would be a rebalancing rule function that would enter 
orders to rebalance the portfolio, and would hold other strategy processing 
until the rebalancing period was over.

The \code{timespan} parameter will limit rule execution by time of day using 
time based subsetting.  See ISO-8601 specification and xts documentation for 
more details.  Note that these are only applicable to intra-day execution, 
and will remain that way barring patches (tests and documentatioon) from 
interested parties.  The subsetting may (will likely) work with normal 
ISO/xts subset ranges, but consider it unsupported. 

We anticipate that rules will be the portion of a strategy most likely to 
not have suitable template code included with this package, as every strategy 
and environment are different, especially in this respect.  
We will attempt to provide enough examples and generic rules to give strategy
authors a place to start.}
\arguments{\item{strategy}{an object of type 'strategy' to add the rule to}
\item{name}{name of the rule, must correspond to an R function}
\item{arguments}{named list of default arguments to be passed to an rule function when executed}
\item{parameters}{vector of strings naming parameters to be saved for apply-time definition}
\item{label}{arbitrary text label for rule output, NULL default will be converted to '<name>.rule'}
\item{type}{one of "risk","order","rebalance","exit","enter", see Details}
\item{...}{any other passthru parameters}
\item{enabled}{TRUE/FALSE whether the rule is enabled for use in applying the strategy, default TRUE}
\item{indexnum}{if you are updating a specific rule, the index number in the $rules[type] list to update}
\item{path.dep}{TRUE/FALSE whether rule is path dependent, default TRUE, see Details}
\item{timespan}{an xts/ISO-8601 style \emph{time} subset, like "T08:00/T15:00", see Details}
\item{store}{TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE}}
