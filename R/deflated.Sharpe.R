###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2017
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################



#' Calculate a Deflated Sharpe Ratio using number of trials and portfolio moments
#'
#' Per Bailey and Lopex de Prado (2014), construct a Deflated Sharpe Ratio and 
#' associated p-value based on an observed Sharpe ratio and information drawn
#' from a series of trials (e.g. parameter optimization or other strategies tried
#' before the candidate strategy)
#' 
#' @param portfolios string name of portfolio, or optionally a vector of portfolios, see DETAILS
#' @param ... any other passtrhrough parameters
#' @param strategy optional strategy specification that would contain more information on the process, default NULL
#' @param trials optional number of trials,default NULL
#' @param audit optional audit environment containing the results of parameter optimization or walk forward, default NULL
#' @param env optional environment to find market data in, if required.
#'
#' @return
#' 
#' a \code{data.frame} containing:
#' 
#' \itemize{
#'   \item{original observed Sharpe ratio}
#'   \item{deflated Sharpe ratio}
#'   \item{p-value of the deflated Sharpe ratio}
#'   \item{number of trials used for adjustment}
#' }
#' 
#' this object may change in the future, and may be classed so that we can include more information
#' 
#' @references 
#' 
#' Bailey, David H, and Marcos Lopez de Prado. 2014. "The Deflated Sharpe Ratio: 
#' Correcting for Selection Bias, Backtest Overfitting and Non-Normality." 
#' Journal of Portfolio Management 40 (5): 94-107. 
#' http://www.davidhbailey.com/dhbpapers/deflated-sharpe.pdf
#'
#' @author Brian G. Peterson
#' @importFrom TTR ROC
#' @seealso \code{\link{SharpeRatio.haircut}}
#' @rdname SharpeRatio.deflated
#' @aliases deflatedSharpe SharpeRatio.deflated
#' @export deflatedSharpe
#' @export SharpeRatio.deflated
deflatedSharpe <- SharpeRatio.deflated <- function( portfolios
                                                  , ...
                                                  , strategy=NULL
                                                  , trials=NULL
                                                  , audit=NULL
                                                  , env=.GlobalEnv)
{ 
  #check inputs
  if(length(portfolios==1)&&is.null(audit)){
    stop("Not enough information to calculate.  \n",
         "Need either \n",
         "  - multiple portfolios \n",
         "  - single portfolio plus audit environment \n")
  }
  
  if(is.null(strategy)&&is.null(trials)){
    stop("Not enough information to calculate.  \n",
         "Need either \n",
         "  - strategy object with trials slot \n",
         "  - explicit number of trials \n")
  }
  
  #initialize things we'll need:
  if(!is.null(strategy)){
    if(!is.strategy((strategy))){
      s<-try(getStrategy(strategy))
      if(inherits(s,"try-error"))
        stop ("You must supply an object of type 'strategy'.")
    } else {
      s <- strategy
    }
    s_trials<-s$trials
    if(!is.null(trials) && s_trials>trials){
      trials <- s_trials
    }
    if(is.null(trials)) trials <- s_trials
  }
  if(trials==0 || !is.numeric(trials))
    stop("You must supply a numeric number of trials or a strategy with trials included")
  
  #loop over portfolios
  dailySt<-list()
  for(portfolio in portfolios){
    if(!is.null(audit)){
      if(!is.environment(audit)){
        stop("audit parameter should be an environment containing trial portfolios")
      } else{
        # run dailyStats on all (matching) portfolios if there
        pvec <- ls(pattern = paste0('portfolio.',portfolio),name = audit)
        if(length(pvec)){
          if(length(pvec)>trials) trials <- trials + length(pvec)
          # run dailyStats on all (matching) portfolios if there
          dailySt <- c(dailySt,lapply(pvec, function(x){ dailyStats(x,perSymbol = FALSE, method='moment', envir=audit) }))
          dailySt <- do.call(rbind,dailySt)
        }
        # put target portfolio first
        dailySt <- rbind(dailyStats(portfolios[1],perSymbol = FALSE, method='moment'),dailySt)
        rownames(dailySt) <- c(portfolios[1],pvec)
      }
    } else {
      #if we don't have an audit environment, we just need stats on all the portfolios
      dailySt <- c(dailySt,lapply(portfolios, function(x){ dailyStats(x,perSymbol = FALSE, method='moment') }))
      dailySt <- do.call(rbind,dailySt)
      rownames(dailySt) <- portfolios
    }
  }
  
  #dailySt contains a list of dailyStats, we might need to combine a little more fully, unsure
  
  #take periodicty and number of observations from target portfolio
  p<-.getPortfolio(portfolios[1])
  
  freq <- periodicity(p$summary)$scale
  periodsInYear <- switch (freq,
                           "daily" = 252,
                           "weekly" = 52,
                           "monthly" = 12,
                           "quarterly" = 4,
                           "yearly" = 1,
                           "hourly" = 5796
                          )

  numPeriods <- nrow(p$summary)
  sharpe <- dailySt[1,'Ann.Sharpe'] # assumes target portfolio is the first one
  skew   <- dailySt[1,'Skewness']
  kurt   <- dailySt[1,'Kurtosis']
  
  # need variance of the trials
  varTrials <- var(na.omit(ROC(dailySt$Total.Net.Profit))) #must be in returns!
  
  
  .deflatedSharpe(sharpe, nTrials=trials, varTrials, skew, kurt, numPeriods, periodsInYear) 
} 

#' internal implementation of Deflated Sharpe
#'
#' @param sharpe candidate (annualized) Sharpe Ratio
#' @param nTrials numeric number or trials
#' @param varTrials variance of Sharpe ratios of the trials
#' @param skew skewness of the candidate 
#' @param kurt non-excess kurtosis
#' @param numPeriods total periods in the backtest
#' @param periodsInYear number of periods in a year, default 252 (daily)
#'
#' @author Ilya Kipnis, Brian G. Peterson
#' @references 
#' https://quantstrattrader.wordpress.com/2015/09/24/
#' @rdname SharpeRatio.deflated
.deflatedSharpe <- function(sharpe, nTrials, varTrials, skew, kurt, numPeriods, periodsInYear=252) {
  emc <- 0.5772156649 # Euler-Mascheroni constant
  maxZ_t1 <- (1 - emc) * qnorm(1 - 1/nTrials)
  maxZ_t2 <- emc * qnorm(1 - 1/nTrials * exp(-1))
  maxZ <- maxZ_t1 + maxZ_t2
  sr0 <- sqrt(varTrials * 1/periodsInYear) * maxZ
  
  numerator <- (sharpe/sqrt(periodsInYear) - sr0)*sqrt(numPeriods - 1)
  
  skewnessTerm <- 1 - skew * sharpe/sqrt(periodsInYear)
  kurtosisTerm <- (kurt-1)/4*(sharpe/sqrt(periodsInYear))^2
  
  denominator <- sqrt(skewnessTerm + kurtosisTerm)
  
  dsr_adj <- numerator/denominator
  result  <- pnorm(dsr_adj)
  dsr     <- sharpe * result
  pval    <- 1-result
  
  return(data.frame(obs.Sharpe=sharpe, max.Sharpe=maxZ, deflated.Sharpe=dsr, p.value=pval, nTrials=nTrials))
}
