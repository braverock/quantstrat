#' add a rule to a strategy
#' 
#' Rules will be processed in a very particular manner, so it bears going over.
#' 
#' First, rules are either path dependent or non-path-dependent.  Path dependent rules 
#' will be processed in every time increment for the \code{mktdata} passed into
#' \code{\link{applyStrategy}}.  Non path dependent rules will likely be quite rare in real life, 
#' and will be applied after indicators and signals, and before path-dependent rules are processed.
#' 
#' All rules have a \code{type}.  These may be any of:
#' \itemize{
#'   \item{risk}{ rules that check and react to risk of positions, may stop all other rule execution temporarily or permanently}
#'   \item{order}{ rules for order processing of any open orders at time t, always path-dependent}
#'   \item{rebalance}{ rules executed specifically in a portfolio context, unnecessary in univariate strategies}
#'   \item{exit}{ rules to determine whether to exit a position}
#'   \item{enter}{ rules to determine whether to enter or increase a position}
#' }  
#' The rules will be executed by type, in the order listed above.  
#' Multiple rules of each type may be defined, as with signals and indicators, 
#' they will be executed in order by index number with any other rules sharing the same 
#' type.
#' 
#' The rule execution order was constructed because path-dependent rules may modify   
#' the ability of rules that have not fired yet to be evaluated.  For example, a 
#' risk rule may flatten (close out) an entire position and put new orders 
#' on hold, effectively stopping all further execution of the strategy.  
#' Another example would be a rebalancing rule function that would enter 
#' orders to rebalance the portfolio, and would hold other strategy processing 
#' until the rebalancing period was over.
#' 
#' The \code{timespan} parameter will limit rule execution by time of day using 
#' time based subsetting.  See ISO-8601 specification and xts documentation for 
#' more details.  Note that these are only applicable to intra-day execution, 
#' and will remain that way barring patches (tests and documentation) from 
#' interested parties.  The subsetting may (will likely) work with normal 
#' ISO/xts subset ranges, but consider it unsupported. 
#' 
#' The \code{name} parameter should be a character string naming the function
#' to be called in the \code{\link{applyRules}} loop. The \code{add.rule} 
#' function will then call \code{\link{match.fun}}, ands store the actual function 
#' in your strategy object.  
#' This will avoid lookups via \code{\link{match.fun}} at \code{\link{applyRules}} time, 
#' and may provide a significant speed increase on higher frequency data (20\% or more).
#' 
#' We anticipate that rules will be the portion of a strategy most likely to 
#' not have suitable template code included with this package, as every strategy 
#' and environment are different, especially in this respect.  
#' We will attempt to provide enough examples and generic rules to give strategy
#' authors a place to start.
#' 
#' For quantstrat to be able to (largly) vectorize the execution of path-dependent 
#' rule evaluation, the rule function is presumed to have a function signature 
#' like that of \code{\link{ruleSignal}}, specifically the arguments \code{sigcol} 
#' and \code{sigval}.  If these are present and function in a manner similar to 
#' \code{\link{ruleSignal}} we can do some preprocessing to significantly reduce the 
#' dimensionality of the index we need to loop over.  The speedup is the ratio of 
#' (symbols*total observations)/signal observations, so it can be significant for many strategies.
#'    
#' @param strategy an object of type 'strategy' to add the rule to
#' @param name name of the rule, must correspond to an R function
#' @param arguments named list of default arguments to be passed to an rule function when executed
#' @param parameters vector of strings naming parameters to be saved for apply-time definition
#' @param label arbitrary text label for rule output, NULL default will be converted to '<name>.rule'
#' @param type one of "risk","order","rebalance","exit","enter", see Details
#' @param ... any other passthru parameters
#' @param enabled TRUE/FALSE whether the rule is enabled for use in applying the strategy, default TRUE
#' @param indexnum if you are updating a specific rule, the index number in the $rules[type] list to update
#' @param path.dep TRUE/FALSE whether rule is path dependent, default TRUE, see Details 
#' @param timespan an xts/ISO-8601 style \emph{time} subset, like "T08:00/T15:00", see Details
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @export
add.rule <- function(strategy, name, arguments, parameters=NULL, label=NULL, type=c(NULL,"risk","order","rebalance","exit","enter"), ..., enabled=TRUE, indexnum=NULL, path.dep=TRUE, timespan=NULL, store=FALSE) {
    if(!is.strategy(strategy)) stop("You must pass in a strategy object to manipulate")
    type=type[1]
    if(is.null(type)) stop("You must specify a type")
	if(is.na(charmatch(type,c("risk","order","rebalance","exit","enter","pre","post")))) stop(paste("type:",type,' must be one of "risk", "order", "rebalance", "exit", "enter", "pre", or "post"'))
    tmp_rule<-list()
    if(!is.function(name)) {
        if(!is.function(get(name))){
            if(!is.function(get(paste("sig",name,sep='.')))){
                message(paste("Skipping rule",name,"because there is no function by that name to call"))
                next()      
            } else {
                name<-paste("sig",rule$name,sep='.')
            }
        }
        fn<-match.fun(name)
    } else {
        fn <- name
    }
    
    tmp_rule$name<-fn
    tmp_rule$type<-type
    tmp_rule$enabled<-enabled
    if (!is.list(arguments)) stop("arguments must be passed as a named list")
	if(is.null(label)) label = paste(name,"rule",sep='.')
    tmp_rule$label<-label
    tmp_rule$arguments<-arguments
	if(!is.null(parameters)) tmp_rule$parameters = parameters
	if(!is.null(timespan)) tmp_rule$timespan = timespan
	tmp_rule$path.dep<-path.dep
	if(length(list(...))) tmp_rule<-c(tmp_rule,list(...))
	
    tmp_rule$call<-match.call()
    class(tmp_rule)<-'trade_rule'
    if(!hasArg(indexnum) | (hasArg(indexnum) & is.null(indexnum))) indexnum = length(strategy$rules[[type]])+1
    strategy$rules[[type]][[indexnum]]<-tmp_rule
    
    if (store) assign(strategy$name,strategy,envir=as.environment(.strategy))
    else return(strategy)
}

#' apply the rules in the strategy to arbitrary market data 
#' 
#' In typical usage, this function will be called via \code{\link{applyStrategy}}.  
#' In this mode, this function will be called twice, once with \code{path.dep=FALSE} 
#' and then again in stepping over the time indexes of the mktdata object.
#' 
#' Individual rule functions may need to use <<- to place \code{hold} and \code{holdtill}
#' variables into play.  These would be most likely implemented by risk rules.
#' 
#' \code{quantstrat} has a significant amount of logic devoted to handling 
#' path-dependent rule execution.  Most of that code/logic resides in this
#' function.  
#' 
#' This function, along with \code{\link{ruleOrderProc}}, \code{\link{addOrder}}, and 
#' \code{\link{applyStrategy}} will likely need to be replaced to connect to a live 
#' market infrastructure. 
#' 
#' \section{Dimension Reduction for Performance}{
#' In evaluation of path-dependent rules, the simplest method, 
#' and the one we used initially, is to check the rules on every observation 
#' in the time series of market data.  
#' There are cases where this will still be required, but we hope to limit them as much as possible.
#' Looping in \R is generally discouraged, and on high frequency data for 
#' strategy evaluation it can produce completely unacceptable results.
#' 
#' The solution we've employed makes use of what we know about the strategy and
#' the orders the strategy places (or may place) to reduce the dimensionality of the problem.
#' 
#' As discussed in \code{\link{add.rule}}, the first step in this dimension 
#' reduction is to look for places in the time series where signals may cause the strategy to 
#' enter or change orders.  This creates an index of timestamps that must be evaluated.
#' This index should be significantly shorter than the full number of observations.    
#' \code{quantstrat} will always run \code{applyRules} on each of these indices
#' where we've previously figured out that the strategy might want to do something.
#' 
#' The next step in dimension reduction works on the order book.  
#' If there are open orders, we need to figure out when they might get filled.  
#' For market orders, this is the next observation.  For limit orders, we can 
#' locate the index timestamps after the order is placed to see when the
#' order might cross.  We will add this index to the list of indices to be 
#' evaluated.  There is of course no guarantee that the order will still be 
#' open at that time, that trading will not be on \code{hold} because of a risk rule, 
#' or that something else hasn't interfered.  Adding the index to the list only tells
#' the loop inside \code{applyRules} that rules (including order processing rules) 
#' need to be checked at that index, to see if anything needs to happen.
#' 
#' For trailing orders, the picture is somewhat more complicated.  Trailing orders
#' \emph{may} move on each new observation, per the method described in 
#' \code{\link{addOrder}}. To speed up evaluation of when such an
#' order may cross, we need to combine the possible crossing logic for 
#' the limit orders, above, with some additional logic to handle the 
#' trailing orders. We begin by evaluating when the order price might 
#' be moved. We then examine the market data between the current index and 
#' the point at which the order may move. if there is a (possible) cross, 
#' we insert that index into the indices for examination.  If not, we insert 
#' the index of the next probably move.
#' 
#' It should be noted that this dimension reduction methodology does 'look ahead'
#' in the data.  This 'look ahead' is only done \emph{after} the order has been 
#' entered in the normal path-dependent process, and so should not introduce biases.     
#' }
#' @param portfolio text name of the portfolio to associate the order book with
#' @param symbol identfier of the instrument to find orders for.  The name of any associated price objects (xts prices, usually OHLC) should match these
#' @param strategy an object of type 'strategy' to add the rule to
#' @param mktdata an xts object containing market data.  depending on rules, may need to be in OHLCV or BBO formats, and may include indicator and signal information
#' @param Dates default NULL, list of time stamps to iterate over, ignored if \code{path.dep=FALSE}
#' @param indicators if indicator output is not contained in the mktdata object, it may be passed separately as an xts object or a list.
#' @param signals if signal output is not contained in the mktdata object, it may be passed separately as an xts object or a list.
#' @param parameters named list of parameters to be applied during evaluation of the strategy,default NULL, only needed if you need special names to avoid argument collision
#' @param ... any other passthru parameters
#' @param path.dep TRUE/FALSE whether rule is path dependent, default TRUE, see Details 
#' @seealso \code{\link{add.rule}} \code{\link{applyStrategy}} 
#' @export
applyRules <- function(portfolio, symbol, strategy, mktdata, Dates=NULL, indicators=NULL, signals=NULL, parameters=NULL,   ..., path.dep=TRUE) {
    # TODO check for symbol name in mktdata using Josh's code:
    # symbol <- strsplit(colnames(mktdata)[1],"\\.")[[1]][1]
    
    # TODO handle indicator and signal lists as well as indicators/signals that were cbound to mktdata

    # ported from IBrokers thanks to Jeff
    # environment for data to be stored/accessed during applyRules execution
    # an example of this functionality is for the "symbols" variable
    # that can be set (by default) to display contract names
    .Data <- new.env()
    #get.Data <- function(x) get(x,.Data)
    #assign.Data <- function(x, value) assign(x, value, .Data)
    #remove.Data <- function(x) remove(x, .Data)
    get.dindex <- function() get("dindex",pos=.Data) # inherits=TRUE)
    assign.dindex <- function(dindex) {
        dindex<-sort(unique(dindex))
        #print(dindex)
        assign("dindex", dindex, .Data)
    }
    
    
    if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object of type 'strategy'.")
    } 
    ret <- NULL
    nargs <-list(...)
    if(length(nargs)==0) nargs=NULL
    if (length('...')==0 | is.null('...')) {
        rm('...')
        nargs=NULL
    }
    
    ruleProc <- function (ruletypelist,timestamp=NULL, path.dep, ruletype, ...){
        for (rule in ruletypelist){
            #TODO check to see if they've already been calculated
            if (!rule$path.dep==path.dep) next()
            if(!is.function(rule$name)) {
                if(!is.function(get(rule$name))){
                    if(!is.function(get(paste("sig",rule$name,sep='.')))){
                        message(paste("Skipping rule",rule$name,"because there is no function by that name to call"))
                        next()      
                    } else {
                        rule$name<-paste("sig",rule$name,sep='.')
                    }
                }   
            }
            
            if(!isTRUE(rule$enabled)) next()
            
			# check to see if we should run in this timepan
			if(!is.null(rule$timespan) & nrow(mktdata[rule$timespan]==0)) next()
			
            # see 'S Programming' p. 67 for this matching
            if(is.function(rule$name)) fun <- rule$name
            else fun<-match.fun(rule$name)
            
            nargs <-list(...)
            if(length(nargs)==0) nargs=NULL
            if (length('...')==0 | is.null('...')) {
                rm('...')
                nargs=NULL
            }

            .formals  <- formals(fun)
            
            if(hasArg(prefer)) .formals$prefer=match.call(expand.dots=TRUE)$prefer
            
            onames <- names(.formals)
            rule$arguments$timestamp = timestamp
			rule$arguments$ruletype  = ruletype
            pm <- pmatch(names(rule$arguments), onames, nomatch = 0L)
            # if (any(pm == 0L)) message(paste("some arguments stored for",rule$name,"do not match"))
            names(rule$arguments[pm > 0L]) <- onames[pm]
            .formals[pm] <- rule$arguments[pm > 0L]

			# now add arguments from parameters
			if(length(parameters)){
				pm <- pmatch(names(parameters), onames, nomatch = 0L)
				names(parameters[pm > 0L]) <- onames[pm]
				.formals[pm] <- parameters[pm > 0L]
			}

            #now add dots
            if (length(nargs)) {
                pm <- pmatch(names(nargs), onames, nomatch = 0L)
                names(nargs[pm > 0L]) <- onames[pm]
                .formals[pm] <- nargs[pm > 0L]
            }
            .formals$... <- NULL
            
            tmp_val<-do.call(fun,.formals)

            mktdata <<- mktdata
            ret <<- ret
            hold <<- hold #TODO FIXME hold processing doesn't work unless custom rule has set it with <<-

            #print(tmp_val)
        } #end rules loop
    } # end sub process function

    #we could maybe do something more sophisticated, but this should work
    if(isTRUE(path.dep)){
        Dates=unique(time(mktdata)) # should this be index() instead?  
        
        dindex<-vector()
        assign.dindex(dindex)
        #pre-process for dimension reduction here
        for ( type in names(strategy$rules)){
            # check if there's anything to do
            if(length(strategy$rules[[type]])>=1){
                for (rule in strategy$rules[[type]]){
                    if(isTRUE(rule$path.dep)){ # only apply to path dependent rule
                        # check for sigcol, sigval, otherwise use all
                        if(is.null(rule$arguments$sigcol) | is.null(rule$arguments$sigval) ){
                            assign.dindex(1:length(Dates))
                        } else {
                            assign.dindex(c(get.dindex(),which(mktdata[,rule$arguments$sigcol] == rule$arguments$sigval)))   
                        }
                    }
                }
            }    
        }
        dindex<-get.dindex()
        if(length(dindex)==0) dindex=1
        
    } else {
        Dates=''
        dindex=1
    }
    
    hold=FALSE
    holdtill=first(time(Dates))-1 # TODO FIXME make holdtill default more robust?

	mktinstr<-getInstrument(symbol)
    
    curIndex<-1

    
    nextIndex<-function(curIndex,...){
        if (!isTRUE(path.dep)){
            curIndex = FALSE
            return(curIndex)
        } 

        dindex<-get.dindex()
        tidx=FALSE
        nidx=FALSE
        neworders=NULL
        
        orderbook <- getOrderBook(portfolio)
        ordersubset <- orderbook[[portfolio]][[symbol]]
        
        oo.idx <- getOrders(portfolio=portfolio, symbol=symbol, status="open",which.i=TRUE) #, timespan=timespan, ordertype=ordertype,which.i=TRUE)
        if(length(oo.idx)==0){
            #print(curIndex)
            curIndex<-dindex[first(which(dindex>curIndex))] #this worked
            #print(curIndex)
            #this ??may?? be faster and more accurate if index insn't sorted
            #curIndex<-min(dindex[which(dindex>curIndex)])        
        
            if(is.na(curIndex) || curIndex > length(index(mktdata))) curIndex=FALSE
        } else { # open orders, 
            isOHLCmktdata <- is.OHLC(mktdata)
            isBBOmktdata  <- is.BBO(mktdata)
            #check for open orders at curIndex
            timespan<-paste(timestamp,"::",sep='')
            if(nrow(ordersubset[oo.idx,][timespan])==0){
                # no open orders between now and the next index
                curIndex<-dindex[first(which(dindex>curIndex))]
                if (is.na(curIndex) || curIndex > length(index(mktdata))) curIndex=FALSE
                return(curIndex) # no open orders, skip ahead
            } else {
                if(!length(grep('market',ordersubset[oo.idx,'Order.Type']))==0 || hasArg('prefer')) {
                    #if any type is market
                    #TODO right now, 'prefer' arguments loop through all observations.  we could probably change the code below on finding price to handle prefer, but not sure it matters
                    # set to curIndex+1
                    curIndex<-curIndex+1
                    if (is.na(curIndex) || curIndex > length(index(mktdata))) curIndex=FALSE
                    return(curIndex) # move to next index, a market order in this index would have trumped any other open order
                } 
                if (!length(grep('limit',ordersubset[oo.idx,'Order.Type']))==0){ # process limit orders
                    #else limit
                    #print("limit")
                    limitorders<-grep('limit',ordersubset[oo.idx,'Order.Type'])
                    for (lorder in limitorders){
                        dindex<-get.dindex()
                        tmpqty<-as.numeric(ordersubset[oo.idx[lorder],'Order.Qty'])
                        tmpprice<-as.numeric(ordersubset[oo.idx[lorder],'Order.Price'])
                        if(tmpqty>0){
                            #buying
                            relationship="gte"
                            if(isBBOmktdata) {
                                col<-first(colnames(mktdata)[has.Ask(mktdata,which=TRUE)])
                            } else if (isOHLCmktdata) {
                                col<-first(colnames(mktdata)[has.Lo(mktdata,which=TRUE)])
                            } else {
                                # We should never hit this code, but it should help us find any edge cases
                                # like perhaps we need a has.Price check
                                stop("no price discernable in applyRules")
                            }
                        } else {
                            #selling
                            relationship="lte"
                            if(isBBOmktdata) {
                                col<-first(colnames(mktdata)[has.Bid(mktdata,which=TRUE)])
                            } else if (isOHLCmktdata) {
                                col<-first(colnames(mktdata)[has.Hi(mktdata,which=TRUE)])
                            } else {
                                # We should never hit this code, but it should help us find any edge cases
                                stop("no price discernable in applyRules")
                            }
                        }
                        # use sigThreshold
                        cross<-sigThreshold(label='tmplimit',column=col,threshold=tmpprice,relationship=relationship)
                        if(any(cross[timespan])){
                            # find first index that would cross after this index
                            newidx <- curIndex + which(cross[timespan])[1] - 1  #curIndex/timestamp was 1 in the subset, we need a -1 offset?
                            # insert that into dindex
                            assign.dindex(c(dindex,newidx))                  
                        } else{
                            # no cross, move ahead
                            nidx=TRUE
                        }
                    } # end loop over open limit orders
                    #tidx<-TRUE
                } 
                if (!length(grep('trailing',ordersubset[oo.idx,'Order.Type']))==0){ # process trailing orders
                    #print("trailing")
                    #else process trailing
                    trailorders<-grep('trailing',ordersubset[oo.idx,'Order.Type'])
                    #print(curIndex)
                    for (torder in trailorders){
                        dindex<-get.dindex()
                        firsttime<-NULL
                        neworders<-NULL
                        onum<-oo.idx[torder]
                        orderThreshold <- as.numeric(ordersubset[onum,'Order.Threshold'])
                        tmpqty<-as.numeric(ordersubset[onum,'Order.Qty'])
                        tmpprice<-as.numeric(ordersubset[onum,'Order.Price'])
                        tmpidx<-as.character(index(ordersubset[onum,])) #this is the time the order was entered
                        #print(tmpidx)
                        if(isBBOmktdata) {
                            if(tmpqty > 0){ # positive quantity 'buy'
                                prefer='offer'
                            } else {
                                prefer='bid'
                            }
                        } else if (isOHLCmktdata) {
                            prefer='close'
                        } 
                        dindex<-get.dindex()
                        if(is.null(firsttime)) firsttime<-timestamp
                        nextidx<-dindex[first(which(dindex>curIndex))]
                        if(length(nextidx)){
                            nextstamp<-(as.character(index(mktdata[nextidx,])))
                            #print(nextstamp)
                            timespan<-paste(firsttime,"::",nextstamp,sep='')
                            #get the subset of prices
                            mkt_price_series <-getPrice(mktdata[timespan],prefer=prefer)
                            col<-first(colnames(mkt_price_series))
                            orderloop<-TRUE
                        } else {
                            orderloop<-FALSE
                        }
                        if(tmpqty > 0){ # positive quantity 'buy'
                            move_order <- ifelse( (mkt_price_series+orderThreshold) < tmpprice, TRUE, FALSE )
                            #this ifelse creates a logical xts vector 
                            relationship="gte"
                        } else {  # negative quantity 'sell'
                            move_order <- ifelse( (mkt_price_series+orderThreshold) > tmpprice, TRUE, FALSE )
                            relationship="lte"
                        }
                        tmpidx<-NULL
                        if(any(move_order)){
                            dindex<-get.dindex()
                            #print(firsttime)
                            # find first index where we would move an order
                            orderidx<-first(which(move_order)) 
                            if(is.null(tmpidx)) tmpidx<-as.character(index(move_order[orderidx,]))
                            trailspan<-paste(firsttime,"::",tmpidx,sep='')
                            #make sure we don't cross before then 
                            # use sigThreshold
                            cross<-sigThreshold(data=mkt_price_series, label='tmptrail',column=col,threshold=tmpprice,relationship=relationship)
                            # find first index that would cross after this index
                            if (any(cross[trailspan])){
                                newidx <- curIndex + which(cross[trailspan])[1] - 1  #curIndex/firsttime was 1 in the subset, we need a -1 offset?
                                newidx <- index(mktdata[index(which(cross[trailspan])[1]),which.i=TRUE])
                                # insert that into dindex
                                assign.dindex(c(dindex,newidx))
                            } else {
                                #if we don't cross, do this
                                moveidx<-index(mktdata[index(move_order[orderidx,]),which.i=TRUE])
                                assign.dindex(c(dindex,moveidx))
                            }    
                        } # end any(move_order) check                            
                    } # end loop over open trailing orders
                } # end else for trailing orders
            } # end else clause for open orders in this timespan    
        } # end any open orders closure
        if(nidx) {
            curIndex <- curIndex+1
        } else {
            dindex<-get.dindex()
            curIndex<-dindex[first(which(dindex>curIndex))]
        }
        if (is.na(curIndex) || curIndex > length(index(mktdata))) curIndex=FALSE
        return(curIndex)
    }
        
    while(curIndex){
        timestamp=Dates[curIndex]    

        # check to see if we need to release a hold
        if(isTRUE(hold) & holdtill<timestamp){
            hold=FALSE
            holdtill=NULL
        }
        for ( type in names(strategy$rules)){
            switch( type ,
                    pre = {
                        if(length(strategy$rules[[type]])>=1){
                            ruleProc(strategy$rules$pre,timestamp=timestamp, path.dep=path.dep, mktdata=mktdata,portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, ...)
                        }
                    },
                    risk = {
                        if(length(strategy$rules$risk)>=1){
                            ruleProc(strategy$rules$risk,timestamp=timestamp, path.dep=path.dep, mktdata=mktdata,portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, ...)
                        }
                    },
                    order = {
                        if(isTRUE(hold)) next()
                        if(length(strategy$rules[[type]])>=1) {
                            ruleProc(strategy$rules[[type]],timestamp=timestamp, path.dep=path.dep, mktdata=mktdata,portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, ...)
                        } else {
                            #(mktdata, portfolio, symbol, timestamp, slippageFUN=NULL)
                            if (isTRUE(path.dep)){
                                timespan<-paste("::",timestamp,sep='')
                            } else timespan=NULL
                            ruleOrderProc(portfolio=portfolio, symbol=symbol, mktdata=mktdata, timespan=timespan, ...)
                        }
                    },
                    rebalance =, exit = , enter = , entry = {
                        if(isTRUE(hold)) next()
                        if(type=='exit'){
                            # must have a position for an exit rules to fire
                            if (getPosQty(Portfolio=portfolio,Symbol=symbol,Date=timestamp)==0) next()
                        }
                        if(length(strategy$rules[[type]])>=1) {
                            ruleProc(strategy$rules[[type]],timestamp=timestamp, path.dep=path.dep, mktdata=mktdata,portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, ...)
                        }
                    },
                    post = {
                        #TODO do we process for hold here, or not?
                        if(length(strategy$rules$post)>=1) {
                            ruleProc(strategy$rules$post,timestamp=timestamp, path.dep=path.dep, mktdata=mktdata,portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, ...)
                        }
                    }
            ) # end switch
        } #end type loop
        if(isTRUE(path.dep)) curIndex<-nextIndex(curIndex, ...)
        else curIndex=FALSE
    } # end Dates while loop

    mktdata<<-mktdata
    if(is.null(ret)) {
        return(mktdata)
    }
    else return(ret)
}

###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2010
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
