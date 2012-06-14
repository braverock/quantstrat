#' add a rule to a strategy
#' 
#' Rules will be processed in a very particular manner, so it bears going over.
#' 
#' First, rules are either path dependent or non-path-dependent.  Path dependent rules 
#' will be processed in every time increment for the \code{mktdata} passed into
#' \code{\link{applyStrategy}}.  Non path dependent rules will likely be quite rare in real life, 
#' and will be applied after indicators and signals, and before path-dependent rules are processed.
#' 
#' 
#' All rules have a \code{type}.  These may be any of:
#' \describe{
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
#' @param storefun TRUE/FALSE whether to store the function in the rule, default TRUE.  setting this option to FALSE may slow the backtest, but makes \code{\link{debug}} usable
#' @return if \code{strategy} was the name of a strategy, the name. It it was a strategy, the updated strategy. 
#' @export
add.rule <- function(strategy, name, arguments, parameters=NULL, label=NULL, type=c(NULL,"risk","order","rebalance","exit","enter"), ..., enabled=TRUE, indexnum=NULL, path.dep=TRUE, timespan=NULL, store=FALSE, storefun=TRUE) {
    if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object or the name of an object of type 'strategy'.")
        store=TRUE
    } 
    type=type[1]
    if(is.null(type)) stop("You must specify a type")
	if(is.na(charmatch(type,c("risk","order","rebalance","exit","enter","pre","post")))) stop(paste("type:",type,' must be one of "risk", "order", "rebalance", "exit", "enter", "pre", or "post"'))
    tmp_rule<-list()
    if(!is.function(name) && isTRUE(storefun)) {
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
    strategy$name
}

#' apply the rules in the strategy to arbitrary market data 
#' 
#' In typical usage, this function will be called via \code{\link{applyStrategy}}.  
#' In this mode, this function will be called twice, once with \code{path.dep=FALSE} 
#' and then again in stepping over the time indexes of the mktdata object.
#' 
#' This function, because of its path dependent nature and the order of rule 
#' evaluation discussed in \code{\link{add.rule}}, will likely take up most of the 
#' execution time of a strategy backtest.
#' 
#' Individual rule functions may need to use <<- to place \code{hold} and \code{holdtill}
#' variables into play.  These would be most likely implemented by risk rules.  When
#' \code{hold==TRUE}, any open oders will still be processed (orders are \emph{NOT} 
#' canceled automatically, but no new orders may be entered.  \code{type='risk'}
#' rules will still function during a hold.  Note that hold must be set via a custom
#' rule.  We tend to set hold in an order or risk rule. 
#' 
#' \code{quantstrat} has a significant amount of logic devoted to handling 
#' path-dependent rule execution.  Most of that code/logic resides in this
#' function.  
#' 
#' This function, along with \code{\link{ruleOrderProc}}, \code{\link{addOrder}}, and 
#' \code{\link{applyStrategy}} will likely need to be replaced to connect to a live 
#' market infrastructure. 
#' 
#' 
#' @section Dimension Reduction for Performance:
#' In evaluation of path-dependent rules, the simplest method, 
#' and the one we used initially, is to check the rules on every observation 
#' in the time series of market data.  
#' There are cases where this will still be required, but we hope to limit them as much as possible.
#' Looping in \R is generally discouraged, and on high frequency data for 
#' strategy evaluation it can produce completely unacceptable results.
#' 
#' The solution we've employed is to utilize a state machine to evaluate the rules only 
#' when deemed necessary.
#' This approach makes use of what we know about the strategy and
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
#' the index of the next probable move.
#' 
#' It should be noted that this dimension reduction methodology does 'look ahead'
#' in the data.  This 'look ahead' is only done \emph{after} the order has been 
#' entered in the normal path-dependent process, and only to insert new indices for 
#' evaluation, and so should not introduce biases.
#'      
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
#' @param rule.order default NULL, use at your own risk to adjust order of rule evaluation
#' @seealso \code{\link{add.rule}} \code{\link{applyStrategy}} 
#' @export
applyRules <- function(portfolio, 
                        symbol, 
                        strategy, 
                        mktdata, 
                        Dates=NULL, 
                        indicators=NULL, 
                        signals=NULL, 
                        parameters=NULL,   
                        ..., 
                        path.dep=TRUE,
                        rule.order=NULL) {
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
    
    Dates=unique(index(mktdata)) # should this be index() instead?  
    
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
            
            # check to see if we should run in this timespan
            if(!is.null(rule$timespan) && nrow(mktdata[rule$timespan][timestamp])==0) next()

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
            
            onames <- names(.formals)
            rule$arguments$timestamp = timestamp
            rule$arguments$ruletype  = ruletype
            rule$arguments$label = rule$label
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

	    # any rule-specific prefer-parameters should override global prefer parameter
	    if(!is.null(rule$arguments$prefer)) .formals$prefer = rule$arguments$prefer
            
            tmp_val<-do.call(fun,.formals)

            mktdata <<- mktdata
            ret <<- ret
            hold <<- hold #TODO FIXME hold processing doesn't work unless custom rule has set it with <<-
            holdtill <<- holdtill 
            
            #print(tmp_val)
        } #end rules loop
    } # end sub process function ruleProc

    #we could maybe do something more sophisticated, but this should work
    if(isTRUE(path.dep)){
        dindex<-c(1,length(Dates))# -1) # set the dimension reduction/loop jumping index vector
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
                            if(is.null(rule$timespan)) {
                                assign.dindex(c(get.dindex(),which(mktdata[,rule$arguments$sigcol] == rule$arguments$sigval)))
                            } else {
                                assign.dindex(c(get.dindex(),which(merge(.xts(,.index(mktdata)),mktdata[rule$timespan,rule$arguments$sigcol]) == rule$arguments$sigval)))
                            }
                        }
                    }
                }
            }    
        }
        dindex<-get.dindex()
        if(length(dindex)==0) dindex=1
        
        #for debugging, set dindex to all index values:
        #assign.dindex(1:length(index(mktdata)))
        #print(dindex)
    } else {
        Dates=''
        dindex=1
    } # end dindex initialization
    
    nextIndex<-function(curIndex,...){
        if (!isTRUE(path.dep)){
            curIndex = FALSE
            return(curIndex)
        } 

        dindex<-get.dindex()
        #message(dindex," in nextIndex(), at ",curIndex)

        hasmktord <- FALSE
        nidx=FALSE
        neworders=NULL
        
        orderbook <- getOrderBook(portfolio)
        ordersubset <- orderbook[[portfolio]][[symbol]]
        
        oo.idx <- getOrders(portfolio=portfolio, symbol=symbol, status="open",which.i=TRUE) #, timespan=timespan, ordertype=ordertype,which.i=TRUE)
        if(length(oo.idx)==0){
            nidx=FALSE
        } else { # open orders, 
            isOHLCmktdata <- is.OHLC(mktdata)
            isBBOmktdata  <- is.BBO(mktdata)
            #check for open orders at curIndex
            timespan<-paste(timestamp,"::",sep='') #no check to see if timestamp came through dots? Does it come from the search path? -gsee
            if(nrow(ordersubset[oo.idx,][timespan])==0){
                # no open orders between now and the next index
                nidx=FALSE
            } else {
                if(!length(grep('market',ordersubset[oo.idx,'Order.Type']))==0 ) {
                    # if block above had a prefer exclusion, as below:
                    # || hasArg('prefer')
                    # 'prefer' arguments would loop through all observations.  
                    # we could probably change the code below on finding price to handle prefer, but not sure it matters
                                
                    #if any type is market    
                    # set to curIndex+1
                    #curIndex<-curIndex+1
                    if (is.na(curIndex) || (curIndex + 1) > length(index(mktdata))) curIndex=FALSE
                    hasmktord <- TRUE                    
                    #return(curIndex) # move to next index, a market order in this index would have trumped any other open order
                } 
                if (!length(grep('limit',ordersubset[oo.idx,'Order.Type']))==0){ # process limit orders
                    #else limit
                    #print("limit")
                    stoplimitorders <- grep('stoplimit',ordersubset[oo.idx,'Order.Type'])
                    limitorders<-grep('limit',ordersubset[oo.idx,'Order.Type'])
                    limitorders <- limitorders[-stoplimitorders]

                    for (slorder in stoplimitorders) {
                        dindex <- get.dindex()
                        tmpqty <- ordersubset[oo.idx[slorder],'Order.Qty']
                        if (tmpqty=='all') tmpqty<-osNoOp(timestamp=timestamp, orderqty=tmpqty, portfolio=portfolio, symbol=symbol,ruletype='exit' )
                        if (tmpqty==0) {
                            #no position, so do some sleight of hand to figure out when the index may be needed
                            side <- ordersubset[oo.idx[slorder],'Order.Side']
                            if(side=='long') tmpqty=-1
                            else tmpqty=1
                        }
                        tmpqty<-as.numeric(tmpqty)
                        tmpprice <- as.numeric(ordersubset[oo.idx[slorder],'Order.Price'])
                        if (tmpqty > 0) { #buy if mktprice moves above stoplimitorder price
                            relationship='gte'  #if the Ask or Hi go above threshold our stop will be filled
                            if(isBBOmktdata) {
                                col<-first(colnames(mktdata)[has.Ask(mktdata,which=TRUE)])
                            } else if (isOHLCmktdata) {
                                col<-first(colnames(mktdata)[has.Hi(mktdata,which=TRUE)])
                            } else { #univariate or something built with fn_SpreadBuilder  
                                col<-first(colnames(mktdata)[grep(prefer, colnames(mktdata))])
                                # perhaps we need a has.Price check
                            }
                            if (is.na(col)) stop("no price discernable for stoplimit in applyRules")
                        } else { #sell if mktprice moves below stoplimitorder price
                            relationship="lte" #if Bid or Lo go below threshold, our stop will be filled
                            if(isBBOmktdata) {
                                col<-first(colnames(mktdata)[has.Bid(mktdata,which=TRUE)])
                            } else if (isOHLCmktdata) {
                                col<-first(colnames(mktdata)[has.Lo(mktdata,which=TRUE)])
                            } else {
                                col<-first(colnames(mktdata)[grep(prefer, colnames(mktdata))])
                            }    
                            if (is.na(col)) stop("no price discernable for stoplimit in applyRules")                            
                        } 
                        cross<-sigThreshold(label='tmpstop',column=col,threshold=tmpprice,relationship=relationship)
                        if(any(cross[timespan])){
                            # find first index that would cross after this index
                            newidx <- curIndex + which(cross[timespan])[1] - 1
                            # insert that into dindex
                            assign.dindex(c(get.dindex(),newidx))                  
                        }
                    }

                    for (lorder in limitorders){
                        dindex<-get.dindex()
                        tmpqty<-ordersubset[oo.idx[lorder],'Order.Qty']
                        if (tmpqty=='all') tmpqty<-osNoOp(timestamp=timestamp, orderqty=tmpqty, portfolio=portfolio, symbol=symbol,ruletype='exit' )
#                        #don't think this block is needed for resular limit orders: BGP
#                        if (tmpqty==0) {
#                            #no position, so do some sleight of hand to figure out when the index may be needed
#                            side <- ordersubset[oo.idx[lorder],'Order.Side']
#                            if(side=='long') tmpqty=-1
#                            else tmpqty=1
#                        }
                        tmpqty<-as.numeric(tmpqty)
                        
                        tmpprice<-as.numeric(ordersubset[oo.idx[lorder],'Order.Price'])
                        
                        if(tmpqty>0){
                            #buying
                            relationship="lte" #look for places where Mkt Ask <= our Bid
                            if(isBBOmktdata) {
                                col<-first(colnames(mktdata)[has.Ask(mktdata,which=TRUE)])
                            } else if (isOHLCmktdata) {
                                col<-first(colnames(mktdata)[has.Lo(mktdata,which=TRUE)])
                            } else {
                                col<-first(colnames(mktdata)[grep(prefer, colnames(mktdata))])
                            }    
                            if (is.na(col)) stop("no price discernable for stoplimit in applyRules")
                        } else {
                            #selling
                            relationship="gte" #look for places where Mkt Bid >= our Ask
                            if(isBBOmktdata) {
                                col<-first(colnames(mktdata)[has.Bid(mktdata,which=TRUE)])
                            } else if (isOHLCmktdata) {
                                col<-first(colnames(mktdata)[has.Hi(mktdata,which=TRUE)])
                            } else {
                                col<-first(colnames(mktdata)[grep(prefer, colnames(mktdata))])
                            }    
                            if (is.na(col)) stop("no price discernable for stoplimit in applyRules")
                        }
                        # use sigThreshold
                        cross<-sigThreshold(label='tmplimit',column=col,threshold=tmpprice,relationship=relationship)
                        if(any(cross[timespan])){
                            # find first index that would cross after this index
                            newidx <- curIndex + which(cross[timespan])[1] #- 1  #curIndex/timestamp was 1 in the subset, we need a -1 offset?
                            #if there are is no cross curIndex will be incremented on line 496
                            # with curIndex<-min(dindex[dindex>curIndex]).                            
                            #we cannot get filled at this timestamp. The soonest we could get filled is next timestamp...
                            #see also that market order increments curIndex before returning it. Going by the docs,
                            #I think this is by design. i.e. no instant fills. -gsee
                            
                            # insert that into dindex
                            assign.dindex(c(get.dindex(),newidx))                  
                        } else{
                            # no cross, move ahead
                            # nidx=TRUE #WHY WAS THIS HERE?
                        }
                    } # end loop over open limit orders
                } # end if for limit order handling
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
                        tmpqty<-ordersubset[onum,'Order.Qty']
                        if (tmpqty=='all') tmpqty<-osNoOp(timestamp=timestamp, orderqty=tmpqty, portfolio=portfolio, symbol=symbol,ruletype='exit' )
                        if (tmpqty==0) {
                            #no position, so do some sleight of hand to figure out when the index may be needed
                            side <- ordersubset[onum,'Order.Side']
                            if(side=='long') tmpqty=-1
                            else tmpqty=1
                        }
                        tmpqty<-as.numeric(tmpqty)
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
                        nextidx<-min(dindex[dindex>curIndex])
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
                                assign.dindex(c(get.dindex(),newidx))
                            } else {
                                #if we don't cross, do this
                                moveidx<-index(mktdata[index(move_order[orderidx,]),which.i=TRUE])
                                assign.dindex(c(get.dindex(),moveidx))
                            }    
                        } # end any(move_order) check                            
                    } # end loop over open trailing orders
                } # end if for trailing orders
            } # end else clause for any open orders in this timespan    
        } # end any open orders closure

        if(curIndex){
            if(hasmktord) { 
                curIndex <- curIndex+1
                dindex<-get.dindex()
            } else {
                dindex<-get.dindex()
                if (any(dindex > curIndex)) {
                    curIndex<-min(dindex[dindex>curIndex]) 
                } else curIndex <- FALSE
            }
        }
        
        if (is.na(curIndex) || curIndex > length(index(mktdata))) curIndex=FALSE

        #debug line
        #print(curIndex)
        
        return(curIndex)
    } # end function nextIndex
        
    hold=FALSE
    holdtill=first(time(Dates))-1 # TODO FIXME make holdtill default more robust?
    
    mktinstr<-getInstrument(symbol)
    
    curIndex<-1
    
    while(curIndex){
        timestamp=Dates[curIndex]    

        # check to see if we need to release a hold
        if(isTRUE(hold) & holdtill<timestamp){
            hold=FALSE
            holdtill=NULL
        }
        # evaluate the rule types in the order listed in the documentation
        # thanks to Aleksandr Rudnev for tracking this down (R-SIG-Finance, 2011-01-25)
        if(is.null(rule.order)){
            types <- sort(factor(names(strategy$rules), levels=c("pre","risk","order","rebalance","exit","enter","entry","post")))
        } else {
            print("Be aware that order of operations matters, and poor choises in rule order can create unintended consequences.")
            types <- rule.order
        }
        for ( type in types ) {
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
#                        if(type=='exit'){
#                            if(length(strategy$rules$exit)==length(grep('market',strategy$rules$exit))){
#                                # all exit orders are of type 'market'
#                                # so we must have a position for exit rules to fire / be evaluated
#                                if (getPosQty(Portfolio=portfolio,Symbol=symbol,Date=timestamp)==0) next()
#                            }
#                        }
                        if(length(strategy$rules[[type]])>=1) {
                            ruleProc(strategy$rules[[type]],timestamp=timestamp, path.dep=path.dep, mktdata=mktdata,portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, ...)
                        }
                        if(isTRUE(path.dep) && length(getOrders(portfolio=portfolio, symbol=symbol, status="open", timespan=timestamp,which.i=TRUE))) {
                            ## TODO FIXME this doesn't appear to work correctly
                            # we opened orders in this timestamp, make sure to increment dindex w/ curIndex+1 so the order slot gets checked next index ?
                            #browser()
                            #assign.dindex(c(get.dindex(),curIndex+1))
                            #
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
        if(isTRUE(path.dep)) curIndex<-nextIndex(curIndex, ...) #timestamp comes from environment, not dots? -gsee
        else curIndex=FALSE
    } # end index while loop

    mktdata<<-mktdata
    if(is.null(ret)) {
        return(mktdata)
    }
    else return(ret)
}

###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2011
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, 
# Jeffrey Ryan, Joshua Ulrich, and Garrett See 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################

