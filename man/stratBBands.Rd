\name{stratBBands}
\docType{data}
\alias{stratBBands}
\alias{BBands}
\title{Bollinger Bands Strategy}
\description{
    Strategy object implementing a Bollinger Bands strategy.
}
\section{Indicators}{
    This strategy uses only a single indicator, comprised of the TTR function \code{BBands}.
    Parameters for this indicator include number of MA periods, and the size of the bands in standard deviations.
}
\section{Signals}{
    The classic Bollinger Bands strategy depends on crossover events (signals) for the upper and lower band and the midpoint.
    \describe{
        \item{Cl.gt.UpperBand}{type \code{\link{sigCrossover}}, if the Close price is greater than the upper band value.}
        \item{Cl.lt.LowerBand}{type \code{\link{sigCrossover}}, if the Close price is less than the lower band value.}
        \item{Cross.mid}{type \code{\link{sigCrossover}}, if the price at any point crossed the midpoint moving average.}
        
    }
}
\section{Rules}{
    In this strategy, each signal has a corresponding entry or exit rule.  
    \describe{
        \item{enter}{\itemize{
            \item type \code{\link{ruleSignal}}, enter a sell order at market on \code{Cl.gt.UpperBand} signal.
            \item type \code{\link{ruleSignal}}, enter a buy order at market on \code{Cl.lt.LowerBand} signal.
        }}
        
        \item{exit}{\itemize{
            \item type \code{\link{ruleSignal}}, enter a market order to close any open position at market on \code{Cross.mid} signal.
        }}
    }
}
\section{Notes}{
    This strategy may be improved in practice by: 
    \itemize{
        \item utilizing trailing entry or exit orders
        
        \item using a different smoothing mechanism other than SMA
        
        \item the addition of stop-loss rules
    }
}
\usage{data('stratBBands')}
\references{
    \url{http://www.investopedia.com/articles/trading/07/bollinger.asp}
    
    \url{http://www.mysmp.com/technical-analysis/bollinger-bands.html}
    
    \url{http://trading-strategies.netfirms.com/trading/bollingerbandtactics.htm}
}
\keyword{datasets}
\keyword{ ts } 