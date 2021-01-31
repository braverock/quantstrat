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
#'   \item{chain}{ rules executed upon fill of an order corresponding to the label of the parent rule identified by the \code{parent} arg. }
#' } 
#'  
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
#' (symbols\*total observations)/signal observations, so it can be significant for many strategies.
#' 
#' @param strategy an object of type 'strategy' to add the rule to
#' @param name name of the rule, must correspond to an R function
#' @param arguments named list of default arguments to be passed to an rule function when executed
#' @param parameters vector of strings naming parameters to be saved for apply-time definition
#' @param label arbitrary text label for rule output, NULL default will be converted to '<name>.rule'
#' @param type one of "risk","order","rebalance","exit","enter","chain" see Details
#' @param parent the label of the parent rule for a chain rule
#' @param ... any other passthru parameters
#' @param enabled TRUE/FALSE whether the rule is enabled for use in applying the strategy, default TRUE
#' @param indexnum if you are updating a specific rule, the index number in the $rules[type] list to update
#' @param path.dep TRUE/FALSE whether rule is path dependent, default TRUE, see Details 
#' @param timespan an xts/ISO-8601 style \emph{time} subset, like "T08:00/T15:00", see Details
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @param storefun TRUE/FALSE whether to store the function in the rule, default TRUE.  setting this option to FALSE may slow the backtest, but makes \code{\link{debug}} usable
#' @return if \code{strategy} was the name of a strategy, the name. It it was a strategy, the updated strategy. 
#' @export
add.rule <- function(strategy
                     , name
                     , arguments
                     , parameters=NULL
                     , label=NULL
                     , type=c(NULL,"risk","order","rebalance","exit","enter","chain")
                     , parent=NULL
                     , ...
                     , enabled=TRUE
                     , indexnum=NULL
                     , path.dep=TRUE
                     , timespan=NULL
                     , store=FALSE
                     , storefun=TRUE) {
    if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object or the name of an object of type 'strategy'.")
        store=TRUE
    } 
    type=type[1]
    if(is.null(type)) stop("You must specify a type")
    if(is.na(charmatch(type,c("risk","order","rebalance","exit","enter","chain","pre","post")))) stop(paste("type:",type,' must be one of "risk", "order", "rebalance", "exit", "enter", "chain", "pre", or "post"'))
    tmp_rule<-list()
    if(!is.function(name) && isTRUE(storefun)) {
        if(exists(name, mode="function")) {
            fn <- get(name, mode="function")
        } else {
            rule.name <- paste("rule", name, sep=".")
            if(exists(rule.name, mode="function")) {
                fn <- get(rule.name, mode="function")
                name <- rule.name
            } else {
                message("Skipping rule ", name,
                        " because there is no function by that name to call")
            }
        }
    } else {
        fn <- name
    }

    tmp_rule$name<-fn
    tmp_rule$type<-type
    if(type == 'chain')
    {
        if(is.null(parent)) stop("You must specify the label of the parent rule if ruletype=='chain'")
        tmp_rule$parent<-parent
    }
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

    #increment trials
    strategy$trials <- strategy$trials+1
    
    if (store) assign(strategy$name,strategy,envir=as.environment(.strategy))
    else return(strategy)
    strategy$name
}

#' enable a rule in the strategy
#'
#' function to make it easy to enable (or disable) a specific rule in a strategy
#'
#' @param strategy an object of type 'strategy' which contains the rule
#' @param type one of "risk","order","rebalance","exit","enter","chain"
#' @param label the label for the rule; grep will be used to match, so multiple rules may be enabled (disabled) as a result
#' @param enabled TRUE/FALSE whether the rule is enabled for use in applying the strategy, default TRUE
#' @param store TRUE/FALSE whether to store the updated strategy in the .strategy environment, or return it.  default FALSE
#' @seealso \code{\link{add.rule}} \code{\link{applyStrategy}} 
#' @export

enable.rule <- function(strategy, type=c(NULL,"risk","order","rebalance","exit","enter","chain"), label, enabled=TRUE, store=FALSE)
{
    if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object or the name of an object of type 'strategy'.")
        store=TRUE
    } 

    for(i in 1:length(strategy$rules[[type]]))
        if(grepl(label, strategy$rules[[type]][[i]]$label))
            strategy$rules[[type]][[i]]$enabled <- enabled

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
#' @param indicators if indicator output is not contained in the mktdata object, it may be passed separately as an xts object or a list.
#' @param signals if signal output is not contained in the mktdata object, it may be passed separately as an xts object or a list.
#' @param parameters named list of parameters to be applied during evaluation of the strategy,default NULL, only needed if you need special names to avoid argument collision
#' @param ... any other passthru parameters
#' @param path.dep TRUE/FALSE whether rule is path dependent, default TRUE, see Details 
#' @param rule.order default NULL, use at your own risk to adjust order of rule evaluation
#' @param debug if TRUE, return output list
#' @param rule.subset ISO-8601 subset for period to execute rules over, default NULL
#' @seealso \code{\link{add.rule}} \code{\link{applyStrategy}} 
#' @export
applyRules <- function(portfolio, 
                        symbol, 
                        strategy, 
                        mktdata, 
                        indicators=NULL, 
                        signals=NULL, 
                        parameters=NULL,   
                        ..., 
                        path.dep=TRUE,
                        rule.order=NULL,
                        rule.subset=NULL,
                        debug=FALSE) {
    # TODO check for symbol name in mktdata using Josh's code:
    # symbol <- strsplit(colnames(mktdata)[1],"\\.")[[1]][1]
    
    # TODO handle indicator and signal lists as well as indicators/signals that were cbound to mktdata

    # ensure no duplicate index values in mktdata
    if(any(diff(.index(mktdata)) == 0)) {
        warning("'mktdata' index contains duplicates; calling 'make.index.unique'")
        mktdata <- make.index.unique(mktdata)
    }

    # we need to know the last point we should be evaluating rules for
    # we at least need to use this in assign.dindex, maybe elsewhere
    if(!is.null(rule.subset)){
      first.index <- which(index(mktdata)==first(index(mktdata[rule.subset])))
      last.index  <- which(index(mktdata)==last(index(mktdata[rule.subset])))
    } else {
      first.index <- 1
      last.index  <- which(index(mktdata)==last(index(mktdata)))
    }
  
    # ported from IBrokers thanks to Jeff
    # environment for data to be stored/accessed during applyRules execution
    # an example of this functionality is for the "symbols" variable
    # that can be set (by default) to display contract names
    .Data <- new.env()
    #get.Data <- function(x) get(x,.Data)
    #assign.Data <- function(x, value) assign(x, value, .Data)
    #remove.Data <- function(x) remove(x, .Data)
    get.dindex <- function() get("dindex",pos=.Data) # inherits=TRUE)
    assign.dindex <- function(dindex, lindex=last.index) {
        dindex <- unique(dindex)
        if(!isOrdered(dindex))
            dindex <- sort(dindex)
        dindex <- dindex[dindex <= lindex]
        #print(dindex)
        assign("dindex", dindex, .Data)
    }
    
    
    if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object of type 'strategy'.")
    } 
    ret <- NULL
    
    Dates <- index(mktdata)
    
    #we could maybe do something more sophisticated, but this should work
    if(isTRUE(path.dep)){ #initialize the dimension reduction index (dindex)
        dindex<-c(first.index,last.index) # set the dimension reduction/loop jumping index vector
        assign.dindex(dindex)
        #pre-process for dimension reduction here
        for ( type in names(strategy$rules)){
            if(type=='rebalance') next()
            # check if there's anything to do
            if(length(strategy$rules[[type]])>=1){
                for (rule in strategy$rules[[type]]){
                    sigcol <- rule$arguments$sigcol
                    sigval <- rule$arguments$sigval
                    # ensure mktdata contains sigcol
                    if (!is.null(sigcol) && !sigcol %in% colnames(mktdata)) {
                        stop("mktdata does not contain 'sigcol': ", sigcol)
                    }
                    if(isTRUE(rule$path.dep)){ # only apply to path dependent rule
                        # check for sigcol, sigval, otherwise use all
                        if(is.null(sigcol) || is.null(sigval)) {
                            if(is.null(rule$timespan)) {
                                assign.dindex(1:length(Dates))
                            } else {
                                assign.dindex(c(get.dindex(), which(.index(mktdata) %in% .index(mktdata[rule$timespan]))))
                            }
                        } else {
                            if(is.null(rule$timespan)) {
                                assign.dindex(c(get.dindex(),which(mktdata[, sigcol] == sigval)))
                            } else {
                                assign.dindex(c(get.dindex(),which(merge(.xts(,.index(mktdata)),mktdata[rule$timespan, sigcol]) == sigval)))
                            }
                        }
                    }
                }
            }    
        }
        dindex<-get.dindex()
              
        #rule subsetting will decrease the periods we evaluate rules for
        if(!is.null(rule.subset)){
          assign.dindex(c(mktdata[rule.subset,which.i=TRUE][1]
                          ,dindex[which(dindex %in% mktdata[rule.subset,which.i=TRUE])]))
          dindex <- get.dindex()
          #print('included indices:')
          #print(dindex)
        }
        
        if(length(dindex)==0) return(NULL) # not sure if NULL will cause other issues...
        
        #for debugging, set dindex to all index values:
        #assign.dindex(1:length(index(mktdata)))
        #print(dindex)
    } else {
        Dates=''
        dindex=first.index
    } # end dindex initialization

    # Find the next index the market will cross a resting order's price
    # or if we need to move a trailing order. Returns a named list.
    dindexOrderProc <- function(Order, mktPrices, curIndex) {
        out <- list()

        # get order information
        orderQty <- Order[1L,'Order.Qty']
        if (orderQty=='all' || orderQty=='trigger' || orderQty=='0'){
            # no position, so figure out when the index may be needed
            side <- Order[1L,'Order.Side']
            if(side=='long')
                orderQty <- -1
            else
                orderQty <- 1
        }
        orderQty <- as.numeric(orderQty)
        orderPrice <- as.numeric(Order[1L,'Order.Price'])
        orderType <- Order[1L,'Order.Type']

        # default mktPrice
        mktPrice <- mktPrices[[orderType]]$price

        # process order
        if (orderQty > 0) {  # buying
            # determine relationship
            relationship <-
                switch(orderType,
                limit = if(mktPrices$isOHLC) 'lt' else 'lte',      # will be filled if market Ask/Lo go below orderPrice
                stoptrailing = 'gte',                              # look for places where Mkt Bid >= our Ask
                stoplimit = if(mktPrices$isOHLC) 'gt' else 'gte')  # will be filled if market Ask/Hi go above orderPrice

            if(mktPrices$isOHLC || mktPrices$isBBO)                # get buy market price for this order type, if it exists
                mktPrice <- mktPrices[[orderType]]$posQty
                low_mktPrice <- mktPrices[[orderType]]$negQty      # used for determining if stoptrailing order price needs to be adjusted due to new low
                high_mktPrice <- mktPrices[[orderType]]$posQty     # used for determining if stoptrailing order trades
        } else {             # selling
            # determine relationship
            relationship <-
                switch(orderType,
                limit = if(mktPrices$isOHLC) 'gt' else 'gte',      # will be filled if market Bid/Hi go above orderPrice
                stoptrailing = 'lte',                              # look for places where Mkt Ask <= our Bid
                stoplimit = if(mktPrices$isOHLC) 'lt' else 'lte')  # will be filled if market Bid/Lo go below orderPrice

            if(mktPrices$isOHLC || mktPrices$isBBO)                # get sell market price for this order type, if it exists
                mktPrice <- mktPrices[[orderType]]$negQty
                low_mktPrice <- mktPrices[[orderType]]$negQty      # used for determining if stoptrailing order trades
                high_mktPrice <- mktPrices[[orderType]]$posQty     # used for determining if stoptrailing order price needs to be adjusted due to new high
        }
        # ensure we have a mktPrice
        if (is.null(mktPrice) || (length(mktPrice) == 1L && is.na(mktPrice)))
            stop("no price discernable for ", orderType, " in applyRules")

        # use .firstCross to find the location of the first orderPrice that crosses mktdata[,col]
        # *after* curIndex, since that is the soonest we can get filled.
        if(orderType %in% "stoptrailing") {
            if(orderQty > 0) {
                out$cross <- .firstCross(high_mktPrice, orderPrice, relationship, start=curIndex+1L)
            } else if(orderQty < 0) {
                out$cross <- .firstCross(low_mktPrice, orderPrice, relationship, start=curIndex+1L)
            }
        } else {
            out$cross <- .firstCross(mktPrice, orderPrice, relationship, start=curIndex+1L)
        }
            

        # check if trailing order needs to be moved
        out$move_order <- FALSE
        if(orderType %in% "stoptrailing") {
            orderThreshold <- as.numeric(Order[1L,'Order.Threshold'])
            if(orderQty > 0) {
                relationship <- "lt"
                newOrderPrice <- orderPrice - abs(orderThreshold)
                mktPrice <- low_mktPrice                           # use low price for determining if stoptrailing order price needs to change
            } else {
                relationship <- "gt"
                newOrderPrice <- orderPrice + abs(orderThreshold)
                mktPrice <- high_mktPrice                          # use high price for determining if stoptrailing order price needs to change
            }
            out$move_order <- .firstCross(mktPrice, newOrderPrice, relationship, start=curIndex+1L)
        }
        out
    }

    nextIndex <- function(curIndex, ..., mktPrices){
        if (!isTRUE(path.dep)){
            curIndex = FALSE
            return(curIndex)
        } 

        hasmktord <- FALSE
        nidx=FALSE
        neworders=NULL
        
        orderbook <- getOrderBook(portfolio)
        ordersubset <- orderbook[[portfolio]][[symbol]]
        
        oo.idx <- getOrders(portfolio=portfolio, symbol=symbol, status="open",which.i=TRUE) #, timespan=timespan, ordertype=ordertype,which.i=TRUE)
        if(length(oo.idx)==0){
            nidx=FALSE
        } else { # open orders, 
            #check for open orders at curIndex
            timespan<-paste(timestamp,"::",sep='') #no check to see if timestamp came through dots? Does it come from the search path? -gsee
            if(nrow(ordersubset[oo.idx,][timespan])==0 &&                   # prior open orders already in dindex; no need to recheck
               !any(ordersubset[oo.idx,"Order.Type"] %in% "stoptrailing"))  # ... but trailing orders may need to move
            {
                # no open orders between now and the next index
                nidx=FALSE
            } else {

                openOrderSubset <- ordersubset[oo.idx,]

                # process open market orders
                if(any('market'==openOrderSubset[,'Order.Type']))
                {
                    # if there are any market orders, set hasmktord to TRUE
                    # other orders still need to be processed? -JMU
                    hasmktord <- TRUE
                }

                # process open resting, but non-trailing orders
                # - dindex can be updated after processing all open orders
                openOrders <- which(openOrderSubset[,'Order.Type'] %in% c("limit","stoplimit"))
                if(length(openOrders) > 0) {
                    # dindexOrderProc$cross will be nrow(x) if there's no cross, and nrow(x) is always in dindex
                    newIndex <- sapply(openOrders, function(i) dindexOrderProc(openOrderSubset[i,], mktPrices, curIndex)$cross)
                    assign.dindex(c(get.dindex(),newIndex))
                }

                # process open trailing orders
                # - dindex should be updated after processing each open trailing order,
                #   regardless of trailing order type (only stoptrailing is currently implemented)
                openOrders <- which(openOrderSubset[,'Order.Type'] %in% "stoptrailing")
                for(openOrder in openOrders)
                {
                    # determine timespan we should search for trailing order executions
                    dindex <- get.dindex()
                    dindexNext <- dindex[.firstCross(dindex, curIndex, "gt")]

                    newIndex <- dindexOrderProc(openOrderSubset[openOrder,], mktPrices, curIndex)

                    # update dindex if order is moved or filled
                    if(newIndex$move_order < dindexNext || newIndex$cross < dindex[length(dindex)]) {
                        assign.dindex(c(dindex, min(newIndex$move_order, newIndex$cross, na.rm=TRUE)))
                    }
                } # end loop over open trailing orders
            } # end else clause for any open orders in this timespan
        } # end any open orders closure

        if(curIndex){
            if(hasmktord) {
                curIndex <- curIndex+1  # why isn't this put into dindex? -JMU
            } else {
                dindex<-get.dindex()
                dindexNext <- dindex[.firstCross(dindex, curIndex, "gt")]
                if (dindexNext < dindex[length(dindex)]) {
                    curIndex <- dindexNext
                } else {
                    curIndex <- FALSE
                }
            }
        }
        
        if (is.na(curIndex) || curIndex > length(Dates)) curIndex=FALSE

        #debug line
        #print(paste('curIndex ==', curIndex))
        
        return(curIndex)
    } # end function nextIndex
        
    hold=FALSE
    holdtill=first(time(Dates))-1 # TODO FIXME make holdtill default more robust?
    mktinstr<-getInstrument(symbol)
    curIndex<-first.index
    if(nrow(mktdata)>1)
        freq <- periodicity(mktdata)  # run once and pass to ruleOrderProc
    else {
        # For applyStrategy.rebalancing, which could run on a 1-row subset of mktdata
        # This should work for all index classes in ruleOrderProc's default switch expression
        freq <- structure(list(difftime = structure(NA, units="secs", class="difftime"), 
            frequency=1, start=start(mktdata), end=end(mktdata), units="secs", 
            scale="seconds", label="second"), class="periodicity")
    }
    
    # do order price subsetting outside of nextIndex and curIndex loop
    # this avoids repeated [.xts calls; and mktPrices is never altered, so copies aren't made
    if(is.BBO(mktdata)) {
        mktPrices <- list(
          stoplimit = list(
              posQty = mktdata[,has.Ask(mktdata,which=TRUE)[1]],
              negQty = mktdata[,has.Bid(mktdata,which=TRUE)[1]]),
          limit = list(
              posQty = mktdata[,has.Ask(mktdata,which=TRUE)[1]],
              negQty = mktdata[,has.Bid(mktdata,which=TRUE)[1]]),
          stoptrailing = list(
              posQty = getPrice(mktdata, prefer='offer')[,1],
              negQty = getPrice(mktdata, prefer='bid')[,1]))
    } else if (is.OHLC(mktdata)) {
        mktPrices <- list(
          stoplimit = list(
              posQty = mktdata[,has.Hi(mktdata,which=TRUE)[1]],
              negQty = mktdata[,has.Lo(mktdata,which=TRUE)[1]]),
          limit = list(
              posQty = mktdata[,has.Lo(mktdata,which=TRUE)[1]],
              negQty = mktdata[,has.Hi(mktdata,which=TRUE)[1]]),
          stoptrailing = list(
              posQty = getPrice(mktdata, prefer='high')[,1],
              negQty = getPrice(mktdata, prefer='low')[,1]))
    } else { # univariate or something built with fn_SpreadBuilder
        prefer <- if(hasArg("prefer")) match.call(expand.dots=TRUE)$prefer else NULL
        mktPrices <- list(
          stoplimit = list(
              price = getPrice(mktdata, prefer=prefer)[,1]),
          limit = list(
              price = getPrice(mktdata, prefer=prefer)[,1]),
          stoptrailing = list(
              price = getPrice(mktdata, prefer=prefer)[,1]))
    }
    mktPrices$isOHLC <- is.OHLC(mktdata)
    mktPrices$isBBO <- is.BBO(mktdata)

    while(curIndex){
        timestamp=Dates[curIndex]    
        
        #print(paste('timestamp',timestamp,'first',first(index(mktdata)),'last',last(index(mktdata))))
        
        # check to see if we need to release a hold
        if(isTRUE(hold) & holdtill<timestamp){
            hold=FALSE
            holdtill=NULL
        }
        # evaluate the rule types in the order listed in the documentation
        # thanks to Aleksandr Rudnev for tracking this down (R-SIG-Finance, 2011-01-25)
        if(is.null(rule.order)){
            types <- sort(factor(names(strategy$rules), levels=c("pre","risk","order","rebalance","exit","enter","chain","post")))
        } else {
            print("Be aware that order of operations matters, and poor choices in rule order can create unintended consequences.")
            types <- rule.order
        }
        for ( type in types ) {
            switch( type ,
                    pre = {
                        if(length(strategy$rules[[type]])>=1){
                            ruleProc(strategy$rules$pre,timestamp=timestamp, path.dep=path.dep, mktdata=mktdata,portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, parameters=parameters, curIndex=curIndex, ...)
                        }
                    },
                    risk = {
                        if(length(strategy$rules$risk)>=1){
                            ruleProc(strategy$rules$risk,timestamp=timestamp, path.dep=path.dep, mktdata=mktdata,portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr,parameters=parameters, curIndex=curIndex, ...)
                        }
                    },
                    order = {
                        if(length(strategy$rules[[type]])>=1) {
                            ruleProc(strategy$rules[[type]],timestamp=timestamp, path.dep=path.dep, mktdata=mktdata,portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, parameters=parameters, curIndex=curIndex, ...)
                        } else {
                            #(mktdata, portfolio, symbol, timestamp, slippageFUN=NULL)

                            if (isTRUE(path.dep))
                                timespan <- format(timestamp, "::%Y-%m-%d %H:%M:%OS6") #may be unecessary
                            else
                                timestamp=NULL
                            #print(paste(curIndex,timestamp))
                            closed.orders <- ruleOrderProc(portfolio=portfolio, symbol=symbol, mktdata=mktdata, timestamp=timestamp, periodicity=freq, curIndex=curIndex, ...)
                        }
                    },
                    chain = {
                        if(!is.null(closed.orders))
                        {
                            # determine which closed orders are chained to an entry
                            chain.rules <- strategy$rules[[type]]
                            chain.rule.names <- sapply(chain.rules, '[[', 'parent')
                            closed.chain <- closed.orders[closed.orders$Rule %in% chain.rule.names]
                            # loop over each closed order and call ruleProc() on each rule
                            for(i in seq_len(nrow(closed.chain))) {
                                rules <- chain.rules[chain.rule.names %in% closed.chain$Rule[i]]
                                for(j in seq_along(rules)) {
                                    # call ruleProc in a loop, since it doesn't look like chain.price would be subset correctly
                                    
                                    txns <- getTxns(Portfolio=portfolio, Symbol=symbol, Dates=timestamp)
                                    txn.price <- last(txns$Txn.Price)	# last() because there may be more than one txn at this timestamp

                                    #ruleProc(rules[j], timestamp=timestamp, path.dep=path.dep, mktdata=mktdata, portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, parameters=list(chain.price=as.numeric(closed.chain$Order.Price[i]), ...))
                                    ruleProc(rules[j], timestamp=timestamp, path.dep=path.dep, mktdata=mktdata, portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, parameters=list(chain.price=txn.price), curIndex=curIndex)
                                }
                            }
                        }
                    },
                    exit = , enter = {
                        if(isTRUE(hold)) next()

                        if(isTRUE(path.dep)) openOrdersLen <- length(getOrders(portfolio=portfolio, symbol=symbol, status="open", timespan=timestamp,which.i=TRUE))

                        if(length(strategy$rules[[type]])>=1) {
                            ruleProc(strategy$rules[[type]],timestamp=timestamp, path.dep=path.dep, mktdata=mktdata,portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, parameters=parameters, curIndex=curIndex, ...)
                        }
                        if(isTRUE(path.dep) && length(getOrders(portfolio=portfolio, symbol=symbol, status="open", timespan=timestamp,which.i=TRUE)) != openOrdersLen) {
                            assign.dindex(c(get.dindex(),curIndex+1))
                        }
                    },
                    post = {
                        #TODO do we process for hold here, or not?
                        if(length(strategy$rules$post)>=1) {
                            ruleProc(strategy$rules$post,timestamp=timestamp, path.dep=path.dep, mktdata=mktdata,portfolio=portfolio, symbol=symbol, ruletype=type, mktinstr=mktinstr, parameters=parameters, curIndex=curIndex, ...)
                        }
                    }
            ) # end switch
        } #end type loop
        if(isTRUE(path.dep)) curIndex <- nextIndex(curIndex, ..., mktPrices=mktPrices) #timestamp comes from environment, not dots? -gsee
        else curIndex=FALSE
    } # end index while loop

    if(isTRUE(debug)){
      mktdata<<-mktdata
      
      if(is.null(ret)) {
        return(mktdata)
      }
      else return(ret)      
    } else {
      return(NULL)
    }   
} #end applyRules

# private function ruleProc, used by applyRules and applyStrategy.rebalancing
ruleProc <- function (ruletypelist,timestamp=NULL, path.dep, ruletype, ..., parameters=NULL){
    
    for (rule in ruletypelist){
        #TODO check to see if they've already been calculated
        if (!rule$path.dep==path.dep) next()

        if(is.function(rule$name)) {
            ruleFun <- rule$name
        } else {
            if(exists(rule$name, mode="function")) {
                ruleFun <- get(rule$name, mode="function")
            } else {
                rule.name <- paste("rule", rule$name, sep=".")
                if(exists(rule.name, mode="function")) {
                    ruleFun <- get(rule.name, mode="function")
                    rule$name <- rule.name
                } else {
                    message("Skipping rule ", rule$name,
                            " because there is no function by that name to call")
                }
            }
        }

        if(!isTRUE(rule$enabled)) next()
        
        # check to see if we should run in this timespan
        if(!is.null(rule$timespan)) {
            # Get row index of timestamp for faster subsetting
            if(hasArg(curIndex))
                curIndex <- eval(match.call(expand.dots=TRUE)$curIndex, parent.frame())
            else
                curIndex <- timestamp
            if(nrow(mktdata[curIndex][rule$timespan])==0)
                next()
        }
        
        # modify a few things
        rule$arguments$timestamp = timestamp
        rule$arguments$ruletype  = ruletype
        rule$arguments$label = rule$label

        # replace default function arguments with rule$arguments
        .formals <- formals(rule$name)
        .formals <- modify.args(.formals, rule$arguments, dots=TRUE)
        # now add arguments from parameters
        .formals <- modify.args(.formals, parameters, dots=TRUE)
        # now add dots
        .formals <- modify.args(.formals, NULL, ..., dots=TRUE)
        # remove ... to avoid matching multiple args
        .formals$`...` <- NULL
        
        # any rule-specific prefer-parameters should override global prefer parameter
        if(!is.null(rule$arguments$prefer)) .formals$prefer = rule$arguments$prefer
        
        # evaluate rule in applyRules' environment
        tmp_val <- do.call(ruleFun, .formals, envir=parent.frame(1))
                
#            print(paste('tmp_val ==', tmp_val))
    } #end rules loop
} # end sub process function ruleProc

###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2015
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, 
# Jeffrey Ryan, Joshua Ulrich, and Garrett See 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
