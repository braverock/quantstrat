\name{stratFaber}
\docType{data}
\alias{stratFaber}
\alias{Faber}
\title{Faber market timing strategy}
\description{
    The \cite{Faber(2007)} article proposes a very simple quantitative market-timing model.  They 
    test the model in sample on the US stock market since 1900 before testing
    it out-of-sample in twenty other markets.
}
\details{    
    The \cite{Faber(2007)} article discusses a 200-day simple moving average, which is proposed
    in Jeremy Seigel's book "Stocks for the Long Run" \cite{(1994)} for timing the DJIA.  He 
    concludes that a simple market timing strategy improves the absolute and
    risk adjusted returns over a buy-and-hold strategy.  After all transaction
    costs are included, the timing strategy falls short on the absolute return,
    but still provides a better risk-adjusted return.  Siegel also tests timing on  
    the Nasdaq composite since 1972 and finds better absolute and risk adjusted
    returns.
    
    The article implements a simpler version of the 200-day SMA, opting for a
    10-month SMA.  Monthly data is more easily available for long periods of time,
    and the lower granularity should translate to lower transaction costs.  
    
    The rules of the system are relatively simple:
    \enumerate{
        \item Buy when monthly price > 10-month SMA
        
        \item Sell and move to cash when monthly price < 10-month SMA
    
        \item All entry and exit prices are on the day of the signal at the close.
    
        \item All data series are total return series including dividends, updated monthly. 
        For the purposes of this demo, we only use price returns.
    
        \item Cash returns are estimated with 90-day commercial paper.  Margin rates for
        leveraged models are estimated with the broker call rate.  Again, for the
        purposes of this demo strategy, we ignore interest and leverage (though these can be modeled in the framework).
    
        \item commissions, and slippage are excluded (though they can be modeled in the framework).
        
        \item taxes are excluded.
    
    }
        
    This simple strategy is different from well-known trend-following systems in
    three respects.  First, there's no shorting.  Positions are converted to cash on
    a 'sell' signal, rather than taking a short position. Second, the entire position
    is put on at trade inception.  No assumptions are made about increasing position
    size as the trend progresses.  Third, there are no stops.  If the trend reverts
    quickly, this system will wait for a sell signal before selling the position.

}
\section{Indicators}{
    This strategy uses only a single indicator, comprised of the TTR function \code{SMA}.
    Parameters for this indicator include only the number of MA periods.
}
\section{Signals}{
    The Faber strategy depends on two crossover events (signals) around the SMA.
    \describe{
        \item{Cl.gt.SMA}{type \code{\link{sigCrossover}}, if the Close price is greater than the SMA value.}
        \item{Cl.lt.SMA}{type \code{\link{sigCrossover}}, if the Close price is less than the SMA value.}        
    }
}
\section{Rules}{
    In this strategy, each signal has a corresponding entry or exit rule.  
    \describe{
        \item{enter}{type \code{\link{ruleSignal}}, enter a buy order at market when the price crosses above the SMA using the \code{Cl.gt.SMA} signal.}
       
        \item{exit}{type \code{\link{ruleSignal}}, enter a sell order at market when the price crosses below the SMA using the \code{Cl.lt.SMA} signal.}
    }
}
\section{Notes}{
    This strategy may be improved in practice by: 
    \itemize{
        \item utilizing trailing entry or exit orders
        
        \item using a different smoothing mechanism other than SMA
        
        \item the addition of stop-loss rules
        
        \item the addition of some other indicator of value
    }
}
\usage{data('stratFaber')}
\references{
    Faber, Mebane T., "A Quantitative Approach to Tactical Asset Allocation." 
    Journal of Risk Management (Spring 2007).
    
    Siegel, Jeremy J. Stocks for the Long Run : 
    The Definitive Guide to Financial Market Returns and Long-Term 
    Investment Strategies (4th ed.). 436 pp. McGraw-Hill. 2007. ISBN 9780071494700. (earlier editions 1994, 1998, 2002)
}
\keyword{datasets}
\keyword{ ts } 