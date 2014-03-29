#' apply the strategy to arbitrary market data, with periodic rebalancing
#' 
#' This function is the wrapper that holds together the execution of a strategy with rebalancing rules.
#' 
#' @param strategy an object of type 'strategy' or the name of a stored strategy to apply
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
#'  
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
    # TODO request add suite of strategies capability to rebalancing tests, 
    #      might be here or in a new applyStrategySuite fn (req from Chinmay)
    
    ret<-list()
    
    if (!is.strategy(strategy)) {
        s<-try(getStrategy(strategy))
        if(inherits(s,"try-error"))
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
        symbols<-ls(pobj$symbols)

        st<-new.env()
        #should be able to use this directly
        #assign(st,paste(s$name,'mktdata',sep='.'),pos=.strategy)
        
        # initialize rebalancing period variables
        if(length(periods)>1){ warning('no guarantee multiple-periodicity rebalancing will work just yet, patches welcome.') }
        st$periods<-periods
        plist<-list()

        #first do the path-independent stuff for indicators and signals
        for (symbol in symbols){

            sret<-list()
            if(isTRUE(load.mktdata)) mktdata <- get(symbol)

            if(isTRUE(load.mktdata) || length(plist)==0) {
                # get the rebalancing periods list for this portfolio
                for( period in periods ) {
                    plist[[period]] <- c(as.POSIXct(index(mktdata)[endpoints(mktdata, period)]), plist[[period]])
                }
            }

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

        st$plist<-plist
        
        # combine plist into one sorted index
        pindex <- unique(sort(do.call(c, c(plist, use.names=FALSE))))
        st$rebalance_index<-pindex
        
        #now we need to do the endpoints loop. 
        for(i in 2:length(pindex)){
            
            #the proper endpoints for each symbol will vary, so we need to get
            #them separately, and subset each one
            for (symbol in symbols){
                #sret<-ret[[portfolio]][[symbol]]
                mktdata<-get(symbol,pos=st)
                #now subset
                md_subset<-mktdata[as.POSIXct(index(mktdata))>pindex[i-1]&as.POSIXct(index(mktdata))<=pindex[i]]
                if(nrow(md_subset)<1) {
                    next()
                } else {
                    #applyRules to this subset for this instrument  
                    sret$rules$pathdep<-rbind(sret$rules$pathdep,
                      applyRules(portfolio=portfolio, 
                                 symbol=symbol, 
                                 strategy=s, 
                                 mktdata=md_subset, 
                                 Dates=NULL, 
                                 indicators=sret$indicators, 
                                 signals=sret$signals, 
                                 parameters=parameters,  
                                 ..., 
                                 path.dep=TRUE)
                    )
                }
                
                #ret[[portfolio]][[symbol]]<-sret
                
                #now call the rebalancing rules
                #to nest different rebalancing periods, we need to check if the pindex 'i' is in specific rebalance_on periods
                # specifically, we need to check if *this* index is in st$plist$period
                for(period in names(st$plist)){
                    if(pindex[i] %in% st$plist[[period]]){
                        
                        #print(pindex[i])
                        curIndex <- pindex[i] #make sure pindex is available as curIndex 
                      
                        #this index is a rebalancing index for period
                        #call the rebalance rules for this period
                        #still need to separate the rules by rebalancing period, this will call them all

                        ruleProc(s$rules$rebalance,
                                timestamp=pindex[i], 
                                path.dep=TRUE, 
                                ruletype='rebalance', 
                                ..., 
                                mktdata=md_subset, 
                                parameters=parameters,
                                portfolio=portfolio,
                                symbol=symbol)
                    }
                }
            } #end loop over symbols for this sub-period
        }
        
        # updateStrat
        if(isTRUE(updateStrat)) updateStrategy(s, portfolio, Symbols=symbols, ...=...)
        
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
