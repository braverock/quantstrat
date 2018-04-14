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


#' calculate degrees of freedom used by a strategy and available from test data
#' 
#' Degrees of freedom are effectively the number of observations in the testing 
#' set which are "free to vary".  In trading strategies, following Pardo(2008, p. 130-131)
#' this typically means the total number of observations in the market data to be 
#' tested minus the number of observations used by indicators, signals, and rules.
#' 
#' We start by removing one degree of freedom for each 'decision point' in
#' the strategy, e.g. each indicator, signal process, rule, parameter set,
#' parameter constraint.  This is a conservative approach that recognizes that 
#' the more complex a strategy is, the higher the probability it may be overfit.
#' So we treat added complexity as removing degrees of freedom in the analysis.
#'
#' If the strategy does not contain parameter sets, then all numeric variables in
#' the strategy will be considered as removing degrees of freedom.  This is again
#' slightly more conservative than a strict reading might suggest.  For example, 
#' a method argument that could range from 1:6, if you choose 6, doesn't necessarily
#' look at 6 market observations.  On the other hand, a standard deviation 
#' parameter may actually look at *all* trailing observations, arguably using 
#' all the degrees of freedom, so only counting 2 degrees of freedom for the 
#' stddev arg in an indicator is being generous.  We're trying to strike a 
#' reasonable balance, erring towards being moderately conservative.
#' 
#' For strategies containing parameter sets, we will examine the parameter
#' combinations after applying all parameter sets and constraints.  If
#' \code{paramset.method=='max'}, we will take the paramset that has the highest
#' total value, and remove that many degrees of freedom. If \code{paramset.method=='sum'},
#' we will take an even more conservative view, and consider the sum of degrees 
#' of freedom consumed by all examined parameter set combinations.  The default,
#' \code{paramset.method=='trial'}, falls in between, utilizing the same number
#' of degrees of freedom as the 'max' paramset, and subtracting one additional 
#' degree of freedom for each trial recorded. 
#'  
#' Collecting observations for market data is easy for lower frequencies, and 
#' may not be worth doing for higher frequencies.  If the \code{portfolios}
#' argument is not NULL, this function will check the symbols list, and then
#' attempt to load market data from \code{env} to count the number of observations.
#' With low frequency data, this is likely to work fine.  With high frequency data, 
#' it is quite possible that all the data will not be in memory at once, so the
#' number of observations counted will not contain all the data.  In practice,
#' the user should be aware of this, and take appropriate action, such as doing
#' those calculations by hand.  Also, as a practical matter, it may make very 
#' little difference as a percentage of available degrees of freedom, since
#' the higher the frequency of the data, the smaller the percentage of the data 
#' likely to be consumed by the strategy specification.
#' 
#' The \% degrees of freedom may be considered as the percentage of the observations
#' that may be used for other inference.  Typically, a number greater than 95\% 
#' will be desired.  Raising the available degrees of freedom may be accomplished 
#' by multiple methods:
#' 
#' \itemize{
#'    \item{increase the length of market data consumed by the backtest}
#'    \item{increase the number of symbols examined}
#'    \item{decrease the number of free parameters}
#'    \item{decrease the ranges of the parameters examined}
#' }
#' 
#' @param strategy an object of type 'strategy' or the name of a stored strategy to examine
#' @param portfolios portfolios to examine for symbols to use for observations, default NULL, see Details.
#' @param ... any other passthru parameters
#' @param paramset.method one of 'trial', or 'max', or 'sum' to determine how to count, see Details.
#' @param env environment to look in for market data, default .GlobalEnv
#' @param verbose default TRUE
#'
#' @return
#' 
#' an object of type \code{dof} containing:
#' 
#' \describe{
#'    \item{strategy}{string name of the strategy}
#'    \item{dp}{decision points in the strategy specification}
#'    \item{idf}{degrees of freedom consumed by numeric arguments to indicators}
#'    \item{psdf}{degrees of freedom consumed by numeric parameter sets}
#'    \item{strategy.dfc}{total degrees of freedom consumed by strategy specification}
#'    \item{portfolios}{character vector of portfolios examined for symbols}
#'    \item{symbols}{character vector of symbols taken from portfolios}
#'    \item{symbol.obs}{named list by symbol, containing observations for each}
#'    \item{mktdata.obs}{total number of degrees of freedom collected from market data observations}
#'    \item{deg.freedom}{total degrees of freedom}
#'    \item{pct.deg.freedom}{percent degrees of freedom, calculated as deg.freedom/mktdata.obs}
#'    \item{call}{call used when calling \code{degrees.of.freedom}}
#' }
#' 
#' @references 
#' Pardo, Robert. The Evaluation and Optimization of Trading Strategies. John Wiley & Sons. 2nd Ed., 2008.
#' @aliases degreesOfFreedom dof degrees.of.freedom
#' @rdname degrees.of.freedom
#' @export degrees.of.freedom
#' @export dof
#' @export degreesOfFreedom
degrees.of.freedom <- degreesOfFreedom <- dof <- function(strategy, 
                                                          portfolios=NULL, 
                                                          ..., 
                                                          paramset.method=c('trial','max','sum'), 
                                                          env=.GlobalEnv,
                                                          verbose=TRUE)
{
  ret<-list()
  
  if (!is.strategy(strategy)) {
    s<-try(getStrategy(strategy))
    if(inherits(s,"try-error"))
      stop ("You must supply an object of type 'strategy'.")
  } else {
    s <- strategy
  }
  
  # calculate degrees of freedom used by strategy
  
  # start with all the used slots, each decision point uses a degree of freedom
  dp <- 0
  dp <- dp + length(s$indicators)
  dp <- dp + length(s$signals)
  dp <- dp + length(s$rules)
  
  # count degrees of freedom from numeric indicator arguments
  idf <- 0
  for (ind in 1:length(s$indicators)){
    if(length(s$indicators[[ind]]$arguments)){
      for (arg in 1:length(s$indicators[[ind]]$arguments)){
        if(is.numeric(s$indicators[[ind]]$arguments[[arg]])){
          idf <- idf + sum(na.omit(s$indicators[[ind]]$arguments[[arg]]))
        }
      }
    }
  }
  
  # calculate degrees of freedom used by paramsets, if they exist
  psdf <- 0 
  if(length(s$paramsets)){
    paramset.method <- paramset.method[1]
    paramset.labels <- ls(s$paramsets)
    for (paramset.label in paramset.labels){
      distributions   <- s$paramsets[[paramset.label]]$distributions
      constraints     <- s$paramsets[[paramset.label]]$constraints
      dp <- dp + length(distributions)
      dp <- dp + length(constraints)
      
      param.combos    <- expand.distributions(distributions)
      param.combos    <- apply.constraints(constraints, distributions, param.combos)
      param.combos$psums <- apply(param.combos,1,function(x)sum(na.omit(as.numeric(x))))
      if(paramset.method == 'max' || paramset.method  == 'trial'){
        psdf <- psdf + max(na.omit(param.combos$psums))
        if(paramset.method=='trial'){
          psdf <- psdf + s$trials
        }
      } else if (paramset.method == 'sum') {
        psdf <- psdf + sum(na.omit(param.combos$psums))
      }
    }
  }
  # sum degrees of freedom consumed by the strategy, its indicators, and paramsets
  strategy.dfc     <- dp+idf+psdf
  
  ret$strategy     <- s$name
  ret$dp           <- dp
  ret$idf          <- dp
  ret$psdf         <- psdf
  ret$strategy.dfc <- strategy.dfc
  
  # calculate degrees of freedom granted by symbols in portfolios
  if(!is.null(portfolios)){
    # get symbols from all portfolios
    symbols<-character()
    ret$portfolios <- portfolios
    for(portfolio in portfolios){
      p <- try(.getPortfolio(portfolio))
      if(!inherits(p,"try-error")){
        symbols <- c(symbols,ls(p$symbols))
      }
    }
    symbols <- unique(symbols)
    ret$symbols <- symbols
    ret$symbol.obs <- list()
    # get number of observations
    mktdataobs <- 0
    for(symbol in symbols){
      tmpobs <- try(nrow(get(symbol,pos=env)))
      if(is.numeric(tmpobs)){
        mktdataobs <- mktdataobs + tmpobs
        ret$symbol.obs[[symbol]]<-tmpobs
      }
    }
    ret$mktdata.obs <- mktdataobs
  }

  ret$deg.freedom     <- ret$mktdata.obs - ret$strategy.dfc
  ret$pct.deg.freedom <- ret$deg.freedom/ret$mktdata.obs
  ret$call <- match.call()
  
  return(structure(ret, class='dof'))
} # end degrees of freedom fn


#' print method for strategy degrees of freedom object
#'
#' @param x an object of type \code{dof} to be printed 
#' @param ... any other passthough parameters
#'
#' @seealso \code{\link{degrees.of.freedom}}
#' @method print dof
#' @export
print.dof <- function(x, ...) 
{
  cat('\n',
              "Degrees of freedom report for strategy:",x$strategy,'\n',
              "Total market observations:",x$mktdata.obs,'\n',
              "Degrees of freedom consumed by strategy:",x$strategy.dfc,'\n',
              "Total degrees of freedom remaining:",x$deg.freedom,'\n',
              "% Degrees of Freedom: ",round(100*x$pct.deg.freedom,2),'%','\n')
  
  invisible(x)
}
