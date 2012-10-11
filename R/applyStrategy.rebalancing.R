#' apply the strategy to arbitrary market data, with periodic rebalancing
#' 
#' This function is the wrapper that holds together the execution of a strategy with rebalancing rules.
#' 
#' @param strategy an object of type 'strategy' to add the indicator to
#' @param portfolios a list of portfolios to apply the strategy to
#' @param mktdata an xts object containing market data.  depending on indicators, may need to be in OHLCV or BBO formats, default NULL
#' @param parameters named list of parameters to be applied during evaluation of the strategy, default NULL
#' @param ... any other passthru parameters
#' @param verbose if TRUE, return output list
#' @param symbols character vector identifying symbols to initialize a portfolio for, default NULL
#' @param initStrat whether to use (experimental) initialization code, default FALSE
#' @param updateStrat whether to use (experimental) wrapup code, default FALSE
#' @export
#' @seealso \code{\link{strategy}},  \code{\link{applyIndicators}}, 
#'  \code{\link{applySignals}}, \code{\link{applyRules}},
#'  \code{\link{initStrategy}}, \code{\link{applyStrategy}}
applyStrategy.rebalancing <- function(strategy , 
                                      portfolios, 
                                      mktdata=NULL , 
                                      parameters=NULL, 
                                      ..., 
                                      verbose=TRUE, 
                                      symbols=NULL, 
                                      initStrat=FALSE, 
                                      updateStrat=FALSE ) 
{
   
    ret<-list()
    
	if (!is.strategy(strategy)) {
    	s<-try(getStrategy(strategy))
    	if(inherits(strategy,"try-error"))
    	    stop ("You must supply an object of type 'strategy'.")
    } else {
        s <- strategy
    }
    
    # check rebalancing rules here to see whether we need to bother, and call applyRules if we don't
    if(length(s$rules[['rebalance']])>=1){
        #initialize the rebalancing periods
        periods<-NULL
        for (rule in s$rules[['rebalance']]){
            if(isTRUE(rule$path.dep)){ # only apply to path dependent rule
                # check for sigcol, sigval, otherwise use all
                if(is.null(rule$arguments$rebalance_on)){
                    warning(paste(rule$label,'does not have a rebalance_on period defined! Mayhem may ensue!'))
                } else {
                    periods<-c(periods,rule$arguments$rebalance_on)
                }
            }
        }
        periods<-unique(periods)
    } else {
        stop('no rebalance rules detected, use applyStrategy instead, it will be faster')
        return(applyStrategy(strategy=s , 
                portfolios=portfolios, 
                mktdata=mktdata , 
                parameters=parameters, 
                verbose=verbose, 
                symbols=symbols, 
                initStrat=initStrat, 
                updateStrat=updateStrat, 
                ... )
        )
    }

    if (missing(mktdata)) load.mktdata=TRUE else load.mktdata=FALSE
    
    for (portfolio in portfolios) {

		# initStrategy
        if(isTRUE(initStrat)) initStrategy(strategy=s, portfolio, symbols, ...=...)
        
   		ret[[portfolio]]<-list() # this is slot [[i]] which we will use later
        pobj<-getPortfolio(portfolio)
        symbols<-names(pobj$symbols)

        st<-new.env()
        #should be able to use this directly
        #assign(st,paste(s$name,'mktdata',sep='.'),pos=.strategy)
        
        if(length(periods)>1){ warning('no guarantee multiple-periodicity rebalancing will work just yet, patches welcome.') }
        st$periods<-periods
        # get the rebalancing periods list for this portfolio
        plist<-list()
        for( period in periods ) {
            from<-as.POSIXlt(index(pobj$summary)[1],tz=indexTZ(pobj$summary))
            # this sequence should work pretty generically
            plist[[period]]<-seq(from=from, to=as.POSIXlt(Sys.Date()), by = period)
            #TODO FIXME sort out a more robust 'to' parameter for this
        }
        st$plist<-plist
        
        if (length(plist) >1) pindex<-lapply(plist,c)
        else pindex<-plist[[1]]
        
        pindex<-xts(1:length(pindex),order.by=pindex)
        pindex<-index(pindex)
        st$rebalance_index<-pindex
        
        #first do the path-independent stuff for indicators and signals
        for (symbol in symbols){
            sret<-list()
            if(isTRUE(load.mktdata)) mktdata <- get(symbol)

            #loop over indicators
            sret$indicators <- applyIndicators(strategy=s , mktdata=mktdata , parameters=parameters, ... )
            #this should be taken care of by the mktdata<<-mktdata line in the apply* fn
            if(inherits(sret$indicators,"xts") & nrow(mktdata)==nrow(sret$indicators)){
                mktdata<-sret$indicators
            }
            
            #loop over signal generators
            sret$signals <- applySignals(strategy=s, mktdata=mktdata, sret$indicators, parameters=parameters, ... )
            #this should be taken care of by the mktdata<<-mktdata line in the apply* fn
            if(inherits(sret$signals,"xts") & nrow(mktdata)==nrow(sret$signals)){
                mktdata<-sret$signals    
            }
            # store mktdata
            assign(symbol,mktdata,pos=st) #creates an object named for 'symbol' in the 'st' environment
            
            sret$rules<-list()
            ret[[portfolio]][[symbol]]<-sret
            #TODO capture rebalance periods here?
        
        } # end path-independent loop over indicators and signals by symbol
        
        #now we need to do the endpoints loop. 
        for(i in 2:length(pindex)){
            #the proper endpoints for each symbol will vary, so we need to get them separately, and subset each one
            for (symbol in symbols){
                sret<-ret[[portfolio]][[symbol]]

                mktdata<-get(symbol,pos=st)
                #now subset
                md_subset<-mktdata[as.POSIXct(index(mktdata))>pindex[i-1]&as.POSIXct(index(mktdata))<=pindex[i]]
                if(nrow(mktdata)<1) next()
                #applyRules to this subset for this instrument  
                sret$rules$pathdep<-c(sret$rules$pathdep,
                                      applyRules(portfolio=portfolio, symbol=symbol, strategy=s, mktdata=md_subset, Dates=NULL, indicators=sret$indicators, signals=sret$signals, parameters=parameters,  ..., path.dep=TRUE))
                
                ret[[portfolio]][[symbol]]<-sret
            } #end loop over symbols for this sub-period
            
            #now call the rebalancing rules
            #to nest different rebalancing periods, we need to check if the pindex 'i' is in specific rebalance_on periods
            # specifically, we need to check if *this* index is in st$plist$period
            for(period in names(st$plist)){
                if(i %in% st$plist[[period]]){
                    #this index is a rebalancing index for period
                    #call the rebalance rules for this period
                    #still need to separate the rules by rebalancing period, this will call them all
                    ruleProc(s$rules$rebalance,timestamp=pindex[i], path.dep=TRUE, 'rebalance', ..., mktdata=md_subset, parameters=parameters)
                }
            }
        }
        
        # updateStrat
        if(isTRUE(updateStrat)) updateStrategy(strategy, portfolio, Symbols=symbols, ...=...)
        
    }
    
    if(verbose) return(ret)
}


###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2012
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
