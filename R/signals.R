
#' add a signal to a strategy
#' 
#' This adds a signal definition to a strategy object. 
#' 
#' Signals denote times at which the strategy \emph{may} want to 
#' take action.  Common signals types from the literature include 
#' crossovers, thresholds, or other interactions between your \code{mktdata}
#' and your indicators.
#' 
#' if \code{label} is not supplied,  NULL default will be converted to '<name>.sig'
#' if the signal function returns one named column, we use that, and ignore the label.  
#' If the signal function returns multiple columns, the label will be 
#' \code{\link{paste}}'d to either the returned column names or the 
#' respective column number.
#' 
#' @param strategy an object (or the name of an object) of type 'strategy' to add the signal to
#' @param name name of the signal, must correspond to an R function
#' @param arguments named list of default arguments to be passed to an signal function when executed
#' @param parameters vector of strings naming parameters to be saved for apply-time definition,default NULL, only needed if you need special names to avoid argument collision
#' @param label arbitrary text label for signal output, default NULL
#' @param ... any other passthru parameters
#' @param enabled TRUE/FALSE whether the signal is enabled for use in applying the strategy, default TRUE
#' @param indexnum if you are updating a specific signal, the index number in the $signals list to update
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @return if \code{strategy} was the name of a strategy, the name. It it was a strategy, the updated strategy. 
#' @seealso 
#' \code{\link{applySignals}}
#' \code{\link{add.indicator}}
#' \code{link{add.rule}}
#' \code{\link{sigComparison}}
#' \code{\link{sigCrossover}}
#' \code{\link{sigFormula}}
#' \code{\link{sigPeak}}
#' \code{\link{sigThreshold}}
#'  
#' @export
add.signal <- function(strategy, name, arguments, parameters=NULL, label=NULL, ..., enabled=TRUE, indexnum=NULL, store=FALSE) {
    if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object or the name of an object of type 'strategy'.")
        store=TRUE
    } 
    tmp_signal<-list()
    tmp_signal$name<-name
    if(is.null(label)) label = paste(name,"sig",sep='.')
    tmp_signal$label<-label
    tmp_signal$enabled<-enabled
    if (!is.list(arguments)) stop("arguments must be passed as a named list")
	arguments$label=label
    tmp_signal$arguments<-arguments
	if(!is.null(parameters)) tmp_signal$parameters = parameters
	if(length(list(...))) tmp_signal<-c(tmp_signal,list(...))

    if(!hasArg(indexnum) | (hasArg(indexnum) & is.null(indexnum))) indexnum = length(strategy$signals)+1
    tmp_signal$call<-match.call()
	class(tmp_signal)<-'strat_signal'
    strategy$signals[[indexnum]]<-tmp_signal

    #increment trials
    strategy$trials <- strategy$trials+1
    
    if (store) assign(strategy$name,strategy,envir=as.environment(.strategy))
    else return(strategy)
    strategy$name
}

#' apply the signals in the strategy to arbitrary market data
#' 
#' This funcion is called internally by \code{\link{applyStrategy}} in normal 
#' operation, but it is also useful for nanual testing during development.
#'  
#' If you are using this function to test your strategy, note that the 'mktdata' 
#' argument should likely contain the output of \code{\link{applyIndicators}}.
#' 
#' @param strategy an object of type 'strategy' to add the signal to
#' @param mktdata an xts object containing market data.  depending on signals, may need to be in OHLCV or BBO formats
#' @param indicators if indicator output is not contained in the mktdata object, it may be passed separately as an xts object or a list.
#' @param parameters named list of parameters to be applied during evaluation of the strategy
#' @param ... any other passthru parameters
#' @export
applySignals <- function(strategy, mktdata, indicators=NULL, parameters=NULL, ...) {
    #TODO add Date subsetting

    # TODO check for symbol name in mktdata using Josh's code:
    # symbol <- strsplit(colnames(mktdata)[1],"\\.")[[1]][1]

    # TODO handle indicator lists as well as indicators that were cbound to mktdata

    # ensure no duplicate index values in mktdata
    if(any(diff(.index(mktdata)) == 0)) {
        warning("'mktdata' index contains duplicates; calling 'make.index.unique'")
        mktdata <- make.index.unique(mktdata)
    }

    if (!is.strategy(strategy)) {
        strategy<-try(getStrategy(strategy))
        if(inherits(strategy,"try-error"))
            stop ("You must supply an object of type 'strategy'.")
    } 
    ret <- NULL
    
    for (signal in strategy$signals){
        #TODO check to see if they've already been calculated

        if(is.function(signal$name)) {
            sigFun <- signal$name
        } else {
            if(exists(signal$name, mode="function")) {
                sigFun <- get(signal$name, mode="function")
            } else {
                sig.name <- paste("sig", signal$name, sep=".")
                if(exists(sig.name, mode="function")) {
                    sigFun <- get(sig.name, mode="function")
                    signal$name <- sig.name
                } else {
                    message("Skipping signal ", signal$name,
                            " because there is no function by that name to call")
                }
            }
        }

        if(!isTRUE(signal$enabled)) next()
        
        # replace default function arguments with signal$arguments
        .formals <- formals(signal$name)
        .formals <- modify.args(.formals, signal$arguments, dots=TRUE)
        # now add arguments from parameters
        .formals <- modify.args(.formals, parameters, dots=TRUE)
        # now add dots
        .formals <- modify.args(.formals, NULL, ..., dots=TRUE)
        # remove ... to avoid matching multiple args
        .formals$`...` <- NULL

        tmp_val <- do.call(sigFun, .formals)
		
		#add label
		if(is.null(colnames(tmp_val)))
			colnames(tmp_val) <- seq(ncol(tmp_val))
		if(!identical(colnames(tmp_val),signal$label)) 
			colnames(tmp_val) <- paste(colnames(tmp_val),signal$label,sep='.')
		
		if (nrow(mktdata)==nrow(tmp_val) | length(mktdata)==length(tmp_val)) {
            # the signal returned a time series, so we'll name it and cbind it
            mktdata<-cbind(mktdata,tmp_val)
        } else {
            # the signal returned something else, add it to the ret list
            if(is.null(ret)) ret<-list()
            ret[[signal$name]]<-tmp_val
        }
        #print(tmp_val)
    } #end signals loop
    mktdata<<-mktdata
    if(is.null(ret)) {
        return(mktdata)
    }
    else return(ret)
}


#' generate comparison signal
#' 
#' Currently, this function compares two columns.  
#' Patches to compare an arbitrary number of columns would be gladly accepted.
#' 
#' Comparison will be applied from the first to the second column in the \code{columns} vector.
#' 
#' Relationship 'op' means 'opposite' side.  Reasonable attempt will be made to match.
#' 
#' @param label text label to apply to the output
#' @param data data to apply comparison to
#' @param columns named columns to apply comparison to
#' @param relationship one of c("gt","lt","eq","gte","lte","op") or reasonable alternatives
#' @param offset1 numeric offset to be added to the first column prior to comparison
#' @param offset2 numeric offset to be added to the second column prior to comparison
#' @export
sigComparison <- function(label,data=mktdata, columns, relationship=c("gt","lt","eq","gte","lte"), offset1=0, offset2=0) {
    relationship=relationship[1] #only use the first one
    if (length(columns)==2){
        ret_sig=NULL
        if (relationship=='op'){
            # (How) can this support "Close"? --jmu
            if(columns[1] %in% c("Close","Cl","close"))
                stop("Close not supported with relationship=='op'")
            switch(columns[1],
                    Low =, 
                    low =, 
                    bid = { relationship = 'lt' },
                    Hi  =,
                    High=,
                    high=,
                    ask = {relationship = 'gt'}
            )
        }

        colNums <- match.names(columns,colnames(data))

		opr <- switch( relationship,
					   	gt = , '>' = '>', 
					 	lt =, '<' = '<', 
					 	eq =, "==" =, "=" = "==",
					 	gte =, gteq =, ge =, ">=" = ">=",
					 	lte =, lteq =, le =, "<=" = "<="
					 )

		ret_sig <- do.call( opr, list(data[,colNums[1]]+offset1, data[,colNums[2]]+offset2))

    } else {
        stop("comparison of more than two columns not supported, see sigFormula")
    }
    colnames(ret_sig)<-label
    return(ret_sig)
}

#' generate a crossover signal
#' 
#' This will generate a crossover signal, which is a dimension-reduced version 
#' of a comparison signal \code{\link{sigComparison}}.
#' 
#' It will return TRUE on the period in which there is a crossover in the 
#' direction specified by \code{relationship}, and NA otherwise.
#' 
#' If you want all the information, use a comparison instead.
#'  
#' @param label text label to apply to the output
#' @param data data to apply crossover to
#' @param columns named columns to apply crossover of the first against the second
#' @param relationship one of c("gt","lt","eq","gte","lte") or reasonable alternatives
#' @param offset1 numeric offset to be added to the first column prior to comparison
#' @param offset2 numeric offset to be added to the second column prior to comparison
#' @export
sigCrossover <- function(label,data=mktdata, columns, relationship=c("gt","lt","eq","gte","lte"), offset1=0, offset2=0) {
    ret_sig = FALSE
    lng<-length(columns)
    for (i in 1:(lng-1)) {
        ret_sig = suppressWarnings(ret_sig | diff(sigComparison(label=label,data=data,columns=columns[c(i,lng)],relationship=relationship,offset1=offset1, offset2=offset2))==1)
    }
    is.na(ret_sig) <- which(!ret_sig)
    colnames(ret_sig)<-label
    return(ret_sig)
}

#' signal function for peak/valley signals
#' 
#' This function tests to see if the mktdata or indicator \code{column} observations 
#' on either side are lower(higher) creating a local peak(bottom).
#' @param label text label to apply to the output
#' @param data data to apply comparison to
#' @param column named column to apply comparison to
#' @param direction one of "peak" or "bottom" to calculate  peaks for
#' @export
sigPeak <- function(label,data,column, direction=c("peak","bottom")){
    if(!is.numeric(column)){
        colNum<-match.names(column,colnames(data))   
    } else colNum<-column
    direction=direction[1] # only use the first]
    #(Lag(IBM[,4],2)<Lag(IBM[,4],1)) & Lag(IBM[,4],1) >IBM[,4]
    switch(direction,
           "peak"   = { ret_sig <- Lag(data[,colNum],2) < Lag(data[,colNum],1) & Lag(data[,colNum],1) > data[,colNum] } ,
           "bottom" = ,
           "valley" = { ret_sig <- Lag(data[,colNum],2) > Lag(data[,colNum],1) & Lag(data[,colNum],1) < data[,colNum] }
    )
    colnames(ret_sig)<-paste(label,direction,"sig",sep='.')
    return(ret_sig)
}

#' generate a threshold signal
#' 
#' Many strategies, including RSI or MACD styles, make trading decisions when an indicator 
#' is over or under a specific threshold.  
#' This function generates the appropriate signal based on such a threshold.
#' 
#' @param label text label to apply to the output
#' @param data data to apply comparison to
#' @param column named column to apply comparison to
#' @param threshold numeric threshold to test for
#' @param relationship one of c("gt","lt","eq","gte","lte") or reasonable alternatives
#' @param cross if TRUE, will return TRUE only for the first observation to cross the threshold in a run
#' @export
sigThreshold <- function(label, data=mktdata, column, threshold=0, relationship=c("gt","lt","eq","gte","lte"),cross=FALSE) {
    relationship=relationship[1] #only use the first one
    ret_sig=NULL
    colNum <- match.names(column, colnames(data))
    switch(relationship,
            '>' =,
            'gt' = {ret_sig = data[,colNum] > threshold},
            '<' =,
            'lt' = {ret_sig = data[,colNum] < threshold},
            'eq'     = {ret_sig = data[,colNum] == threshold}, #FIXME any way to specify '='?
            'gte' =,
            'gteq'=,
            'ge'     = {ret_sig = data[,colNum] >= threshold}, #FIXME these fail with an 'unexpected =' error if you use '>='
            'lte' =,
            'lteq'=,
            'le'     = {ret_sig = data[,colNum] <= threshold}
    )
    if(isTRUE(cross)) ret_sig <- diff(ret_sig)==1
    if(!missing(label))  # colnames<- copies; avoid if possible
        colnames(ret_sig)<-label
    return(ret_sig)
}

#' @useDynLib quantstrat, .registration=TRUE, .fixes="C_"
.firstCross <- function(Data, threshold=0, relationship, start=1) {
    rel <- switch(relationship[1],
            '>'    =  ,
            'gt'   = 1,
            '<'    =  ,
            'lt'   = 2,
            '=='   =  ,
            'eq'   = 3,
            '>='   =  ,
            'gte'  =  ,
            'gteq' =  ,
            'ge'   = 4,
            '<='   =  ,
            'lte'  =  ,
            'lteq' =  ,
            'le'   = 5,
            '!='   =  ,
            'ne'   =  ,
            'neq'  = 6)
    .Call(C_firstCross, Data, threshold, rel, start)
}

#' generate a signal from a formula
#' 
#' This code takes advantage of some base R functionality that can treat an R object (in this case the internal mktdata object in quantstrat) as an environment or 'frame' using \code{\link{parent.frame}}.  
#' This allows the columns of the data to be addressed without any major manipulation, simply by column name.  In most cases in quantstrat, this will be either the price/return columns, or columns added by indicators or prior signals.
#' The formula will return TRUE/FALSE for each row comparison as a time series column which can then be used for rule execution.  The \code{formula} will be evaluated using \code{\link{eval}} as though in an if statement. 
#' 
#' This code is adapted from the approach used by Vijay Vaidyanthan in his PAST(AAII/SIPRO) code to construct arbitrary, formulaic, comparisons.  Many thanks to Vijay for sharing his expertise.
#'  
#' @param label text label to apply to the output
#' @param data data to apply formula to
#' @param formula a logical expression like that used in an if statement, will typically reference column names in \code{mktdata}
#' @param cross if TRUE, will return TRUE only for the first observation to match the formula in a run
#' @export
sigFormula <- function(label, data=mktdata, formula ,cross=FALSE){
	# Vijay's PAST/AAII/SIPRO example
	# fieldVals <- try(eval(parse(text=expression), data))
	ret_sig=NULL
	ret_sig <- try(.xts(eval(parse(text=formula), as.list(data)),index=.index(data)))
	if(is.xts(ret_sig)){
		if(isTRUE(cross)) ret_sig <- diff(ret_sig)==1
		colnames(ret_sig)<-label
	}
	return(ret_sig)
}

#' generate a signal on a timestamp
#' 
#' This will generate a signal on a specific timestamp or at a specific time every day, week, weekday, etc.
#' 
#' @param label text label to apply to the output
#' @param data data to apply formula to
#' @param timestamp either a POSIXct-based object, or a character string denoting a 24-hour time (e.g. "09:00", "16:00")
#' @param on only used if \code{timestamp} is character; passed to \code{\link[xts]{split.xts}}, therefore \code{on}
#' may be a character describing the time period as listed in \code{\link[xts]{endpoints}}, or a vector coercible to
#' factor (e.g. \code{\link[xts]{.indexday}})
#' @export
sigTimestamp <- function(label, data=mktdata, timestamp, on="days") {

   # default label
   if(missing(label))
     label <- "timestamp"

   # initialize ret$timestamp to the index of mktdata
   ret <- .xts(logical(nrow(data)), .index(data), dimnames=list(NULL,label))

   # default if timestamp and on are missing
   if(missing(timestamp) && missing(on)) {
     ret[end(data)] <- TRUE
     return(ret)
   }

   # timestamp can be a time-based timestamp
   if(is.timeBased(timestamp)) {
     after.sig <- .firstCross(index(data), timestamp, "lt")
     if(length(after.sig) != 0)
       ret$timestamp[after.sig] <- TRUE
   } else
   # timestamp can be a timestamp of day
   if(is.character(timestamp)) {
     time.str <- paste("T00:00/T",timestamp,sep="")
     funDay <- function(x, time.str) last(x[time.str])
     funOn  <- function(y, time.str) lapply(y, funDay, time.str)
     after.sig <- do.call(rbind, lapply(split(ret, on), funDay, time.str))
     if(nrow(after.sig) != 0)
       ret[index(after.sig)] <- TRUE
   } else {
     stop("don't know how to handle 'timestamp' of class ", class(timestamp))
   }
   return(ret)
}

#' Signal Analysis With Parmeter Optimization
#' 
#' This function allows user to analyze signals independently from rules.
#' 
#' This function is similar to \code{applyParameter} but includes additionally \code{applySignal} wrapped within it. To use it, the user 
#' will need to initially define the distribution and constraints by using \code{add.distribution} and \code{add.distribution.constraint}.
#' 
#' More generally, the function is designed to generate signals across various parameter combinations defined within the distribution and
#' constraints. Afterwards it extract the next N day price changes for further anlaysis.
#' 
#' The parameter \code{on} allows user to calculate post signal returns in different frequencies. Different signals have different timeframes effectiveness. 
#' \code{forward.days} lets the user select the number of post signal days to analyze results. 
#' 
#' To determine the goodness of a parameter set relative to all others, we let the user specify their own objective function (via parameter \code{obj.func})
#' that will be used to calculate an objective value representing each parameter. These will then be sorted from best to worst. The
#' objective function will take in a single matrix \code{ret.mat} of price changes. Each row represents an individual signal
#' while each column represents periods/post signal. 
#' 
#' @param strategy.st an object of type 'strategy' to add the indicator to
#' @param paramset.label a label uniquely identifying the paramset within the strategy
#' @param portfolio.st text name of the portfolio to associate the order book with
#' @param sigval signal value to match against
#' @param sigcol column name to check for signal
#' @param on the periods endpoints to find as a character string 
#' @param forward.days days to exit post signal
#' @param cum.sum whether to use cumsum on price changes
#' @param include.day.of.signal whether to include the day of signal generation
#' @param obj.fun objective function for determining goodness of each paramset
#' @param decreasing if \code{TRUE} (the default), larger objective function values are better
#' @param mktdata market data
#' @param verbose whether to output processing messages
#' @author Michael Guan
#' @return
#' A list of objects that contains the results
#' \describe{
#'    \item{signals}{List of signals named by parameter combination. Each object within the list is a XTS object with columns being assets and rows being signals.}
#'    \item{sigret.by.paramset}{List of list. Sorted by parameter combination and then by asset. Contains matrix of next day returns; columns are period (N day look ahead) and rows are signals}
#'    \item{sigret.by.asset}{List of list. Sorted by Asset and then by parameter combinations. Contains same matrix as sigret.by.paramset}
#'    \item{signal.stats}{List of signal statistics as calculated via custom user defined fitness function. Sorted by assets.}
#' }
#' @seealso 
#' \code{\link{add.distribution}}
#' \code{\link{add.distribution.constraint}}
#' @export

apply.paramset.signal.analysis<-function(strategy.st, paramset.label, portfolio.st, sigcol,sigval,
                                         on,forward.days,cum.sum=TRUE,include.day.of.signal,
                                         obj.fun,decreasing=TRUE,mktdata=NULL,verbose=TRUE){
  
  must.have.args(match.call(), c('strategy.st', 'paramset.label', 'portfolio.st')) #
  if(missing(obj.fun))
    stop("'obj.fun' must be provided in order to rank paramset signals")
  
  strategy <- must.be.strategy(strategy.st) 
  must.be.paramset(strategy, paramset.label)   
  portfolio <- .getPortfolio(portfolio.st)
  
  distributions <- strategy$paramsets[[paramset.label]]$distributions
  constraints <- strategy$paramsets[[paramset.label]]$constraints
  
  param.combos <- expand.distributions(distributions)
  param.combos <- apply.constraints(constraints, distributions, param.combos)
  rownames(param.combos) <- NULL  # reset rownames
  
  symbols = ls(portfolio$symbols)
  
  # list()
  # ...$ Paramset1 Signals = xts object; columns = by asset
  # ...$ Paramset2 Signals = xts object; columns = by asset
  .sig.list<-list() #list of signals as xts objects, each element in list contains n columns of signals for each asset in universe
  
  
  # list() by paramset
  # ...$ Paramset1
  #     ...$ Asset1
  #     ...$ Asset2
  # ...$ Paramset2
  #     ...$ Asset1
  #     ...$ Asset2
  .sig.ret.by.paramset = list() 
  
  # list() by asset
  # ...$ Asset1:
  #     ...paramset1
  #     ...paramset2
  # ...$ Asset2:
  #     ...$ paramset1
  #     ...$ paramset2
  .sig.ret.by.asset = list() 
  for(sym in symbols) .sig.ret.by.asset[[sym]] = list()
  
  # Loop through parameter by row to get signals
  #   Loop through each symbol
  # TODO: parallelize it
  for(i in 1:nrow(param.combos)){  # param.combo = param.combos[1,]
    param.combo <- param.combos[i,,drop=FALSE]
    
    if(verbose)cat("Applying Parameter Set: ",toString(param.combo),'\n')  
    
    # Attached Paramset to Strategy Object
    strategy <- install.param.combo(strategy, param.combo, paramset.label) 
    
    # Generate Indicators and Signals for Given Paramset
    name.ref = paste('paramset.',gsub(", ",".",toString(param.combo)),sep='')
    .sig.list[[name.ref]] = applyIndicatorSignals(strategy, portfolio.st, mktdata, sigcol)
    
    # Construct Post Signal Returns, loop through signal for each asset
    signal.ret.list.by.asset <- list() # post signal returns by asset
    for(j in 1:length( symbols  )){
      
      # Extract Post Signal Price Deltas for Given Asset Signals [can't do apply() here, unless force a dim on it inside function]
      signal.ret.list.by.asset[[symbols[j]]] = post.signal.returns(signals=.sig.list[[name.ref]][,paste(symbols[j],'.signal',sep='')],
                                                                   sigval=sigval,on=on,forward.days=forward.days,
                                                                   cum.sum=cum.sum,include.day.of.signal=include.day.of.signal)
      
      # Store Post Signal Price Deltas for Given Asset
      .sig.ret.by.asset[[symbols[j]]][[name.ref]] = signal.ret.list.by.asset[[symbols[j]]]
    }
    
    # Store Post Signal Price Deltas for Given Paramset
    .sig.ret.by.paramset[[name.ref]] = signal.ret.list.by.asset
  }
  
  # Generate Statistics by Asset
  signal.stats<-list()
  for(sym in symbols){
    signal.stats[[sym]] = signal.generate.statistics(post.ret=.sig.ret.by.asset[[sym]], obj.fun=obj.fun, decreasing=decreasing)
  }
  
  results = list()
  results$signals = .sig.list
  results$sigret.by.paramset = .sig.ret.by.paramset
  results$sigret.by.asset = .sig.ret.by.asset
  results$signal.stats = signal.stats
  return(results)
}

#' Calculate Indicators and Signals for a Strategy
#'
#' Calculate signals given indicators and signal definitions.
#'
#' @param strategy an object (or the name of an object) type 'strategy' to add the indicator to
#' @param portfolio text name of the portfolio to associate the order book with
#' @param mktdata market data
#' @param sigcol String name of signal to use for analysis 
#' @param ... any other passthru parameters
#' @author Michael Guan
#' @return \code{xts} object with columns representing signals for each symbol
#' @seealso 
#' \code{\link{apply.paramset.signal.analysis}}
#' \code{\link{applyIndicators}}
#' \code{\link{applySignals}}
#' @export

applyIndicatorSignals<-function(strategy, portfolio, mktdata, sigcol, ...){
  
  # Get Portfolio & Symbols
  pobj <- .getPortfolio(portfolio)
  symbols <- ls(pobj$symbols)
  
  # Loop Through asset by asset and combine signal columns in to single xts object
  .signals<-c()
  for(symbol in symbols){ 
    mktdata <- get(symbol) #get market data for current iteration symbol
    
    # Apply Indicators and Signals for Given Asset
    temp <- applyIndicators(strategy = strategy, mktdata = mktdata, ...)
    temp <- applySignals(strategy = strategy, mktdata = temp, ...)
    
    sig.col = temp[,sigcol]
    sig.col[which(is.na(sig.col) == TRUE),] = 0
    colnames(sig.col) <- paste(symbol,'.signal',sep='')
    .signals = merge.xts(.signals,sig.col)    #cbind,merge.xts,or merge? 
  }
  
  return(.signals)
}


#' Signal Objective Function Calculation
#'
#' This function takes a list of matrix of post signal price changes and 
#' applies an user defined objective function on it. 
#'                           
#' @param post.ret \code{matrix} of parameter set of post signal price deltas
#' @param obj.fun custom objective function for measuring signal goodness
#' @param decreasing if \code{TRUE} (the default), larger objective function values are better
#' @author Michael Guan
#' @return objective function values
#' @seealso 
#' \code{\link{apply.paramset.signal.analysis}}
#' @export

signal.generate.statistics<-function(post.ret, obj.fun=NULL, decreasing=TRUE){
  if(is.null(obj.fun)) stop("Must define an objective function for signal sorting.")
  obj.fun = match.fun(obj.fun)
  results = list()
  obj.values = matrix(NA,nrow=length(post.ret),ncol=1)
  row.names = c()
  
  for(j in 1:length(names(post.ret))){
    obj.value = obj.fun(post.ret[[names(post.ret)[j]]])
    if(length(obj.value)!=1) stop('Custom objective function must return only a single value.')
    obj.values[j,]=obj.value
    row.names=c(row.names,names(post.ret)[j])
  }
  rownames(obj.values) = row.names
  
  results[['obj']] = apply(obj.values,2,sort,decreasing=decreasing) #Sort
  
  return(results)
}


#' Generate Post Signal Returns
#'
#' This function collects and aggregates post signal price changes for N days forward.
#' 
#' @param signals xts object with signals, one column
#' @param sigval signal value to match against
#' @param on the periods endpoints to find as a character string
#' @param forward.days number of days to look forward after signal (days to exit post signal)
#' @param cum.sum \code{TRUE},\code{FALSE}; cumulative sum of price changes
#' @param include.day.of.signal whether to analyze the return on signal day
#' @param mktdata market data
#' @author Michael Guan
#' @return \code{matrix} of post signal price changes; rows = nth signal, column = nth period since signal
#' @seealso 
#' \code{\link{apply.paramset.signal.analysis}}
#' @export

post.signal.returns<-function(signals,sigval,on=NULL,forward.days,cum.sum=TRUE,
                              include.day.of.signal=FALSE,mktdata=NULL){
  
  # Incremental Index Values / Label Generation
  if(include.day.of.signal == TRUE){
    days.increment = seq(0,forward.days)
  }else{
    days.increment = seq(1,forward.days+1)
  }
  name.ref = paste0("Period.", head(days.increment, -1))
  
  # Get Relevant Market Data
  if(is.null(mktdata)){
    symbol = gsub('.signal','',colnames(signals))
    mktdata = get(symbol)
  }
  
  if(!is.null(on)){
    # Determine Number of Periods in Given Frequency ie) 1 week has 5 days if signal frequency = days
    days.in.period = cbind(na.omit(signals),na.omit(signals)[endpoints(na.omit(signals),on),])[,2]
    days.in.period[which(days.in.period == 0),] = 1
    days.in.period[is.na(days.in.period),]=0
    days.in.period = max(rle(as.vector(days.in.period))$lengths) + 1
  }else{
    days.in.period = 1
  }
  
  # Get Price Delta 
  ret = tryCatch({
    ret = diff(Cl(mktdata))
  }, error = function(e){
    stop('Market Data does not contain a row with Close. Try to set and aggregate data on to a higher frequency.')
  })
  ret[1,] = 0
  
  # Align Mkt Data and Signals
  signals = signals[index(ret),]
  # Get Indexes of Signals
  idx = which(as.vector(signals)==sigval)
  
  # Take out signals that cause "out of bounds" exception
  idx.cancel = idx[which((nrow(signals) - idx) < max((days.increment * days.in.period)))]
  if(length(idx.cancel)!=0){
    signals[idx.cancel,]=0  
    idx = idx[-which(idx %in% idx.cancel)]  
  }
  
  if(length(idx) == 0)stop("There are no signals for analysis. Try reducing the number of look ahead periods.")
  
  # Daily return since signal; each column = days since signal; each row = each signal
  signal.ret = matrix(NA,nrow=length(idx),ncol=length(name.ref)) #daily return since signal
  
  # Extract Returns
  for(j in 1:length(idx)){
    
    signal.ret[j,] = tryCatch({
      na.omit(diff( getPrice(mktdata[idx[j] + (days.increment * days.in.period)  ,]) ))
    }, error = function(e){
      cat('')
      stop('Not enough forward data to evaluate post signal returns.')
    })
  }
  
  colnames(signal.ret) = name.ref
  
  if(cum.sum == TRUE) signal.ret = t(apply(signal.ret,1,cumsum)) # cumulative sum of price changes yields equity drift
  #matplot(signal.ret,type='l')
  return(signal.ret)
}



# post.signal.returns<-function(signals,sigval,on='weeks',forward.days,cum.sum=TRUE,
#                               include.day.of.signal=FALSE,mktdata=NULL){
#   
#   # Get Relevant Market Data
#   if(is.null(mktdata)){
#     symbol = gsub('.signal','',colnames(signals))
#     mktdata = get(symbol)
#   }
#   
#   # Transform Market data to Specific Frequency 
#   if(!is.null(on)){
#     # Find the Dates for Mktdata in the frequency of interest
#     # Find the Dates of Signals, merge the two to get the final dates of interest
#     
#     sig.date.idx = index(signals)[which(signals == sigval)]  
#     mktdata.temp = mktdata[endpoints(x=mktdata,on=on),]
#     
#     mktdata.idx = index(cbind(mktdata.temp,signals))
#     mktdata = mktdata[mktdata.idx,]
#     signals = signals[mktdata.idx,]
#   }
#   
#   # Get Price Delta 
#   ret = tryCatch({
#     ret = diff(Cl(mktdata))
#   }, error = function(e){
#     stop('Market Data does not contain a column with Close. Try to set and aggregate data on to a higher frequency.')
#   })
#   ret[1,] = 0
#   
#   # Align Mkt Data and Signals
#   signals = signals[index(ret),]
#   
#   # Take out signals that cause "out of bounds" exception
#   signals[(nrow(signals)-forward.days):nrow(signals),]=0  
#   
#   # Get Indexes of Signals
#   idx = which(as.vector(signals)==sigval)
#   
#   if(length(idx) == 0) stop("There are no signals for analysis.")
#   
#   # Incremental Index Values / Label Generation
#   if(isTRUE(include.day.of.signal)){
#     days.increment = c(0,seq(1,forward.days))
#     name.ref = sapply( days.increment ,function(x){paste("Period",x,sep='.')})
#   }else{
#     days.increment = seq(1,forward.days)
#     name.ref = sapply( days.increment ,function(x){paste("Period",x,sep='.')})
#   }
#   
#   # Daily return since signal; each column = days since signal; each row = each signal
#   signal.ret = matrix(NA,nrow=length(idx),ncol=length(name.ref)) #daily return since signal
#   
#   # Extract return 
#   for(j in 1:length(idx))signal.ret[j,] = ret[idx[j] + days.increment  ,]
#   
#   colnames(signal.ret) = name.ref
#   
#   if(isTRUE(cum.sum)) signal.ret = t(apply(signal.ret,1,cumsum)) # cumulative sum of price changes yields equity drift
#   
#   return(signal.ret)
# }


#' Signal Objective Function
#' 
#' Simple example of objective function that can can be used to measure the effectiveness of various parameter combinations.
#' 
#' Calculates the slope of the Cumulative Equity line. Higher the better.
#' 
#' It is important to note that all objective functions are called within \code{signal.generate.statistics}. What gets
#' passed in to it as parameter is an matrix of post signal price changes or cumulative equity for each signal. The matrix
#' is of N-by-M dimensions. N being the row representing the Nth signal. M being the column representing Mth period.  
#' 
#' @param x Return matrix, each row represents price deltas for a single signal, each column represents periods
#' @author Michael Guan
#' @return Single Objective Value
#' @seealso 
#' \code{\link{apply.paramset.signal.analysis}}
#' @export

signal.obj.slope<-function(x){
  mu = colMeans(x)
  lm.r = lm(mu~seq(length(mu))) 
  obj = coef(lm.r)[2]
  return(obj)
}


#' Visualization of Signal Across Lookback
#'
#' This function takes a list of  matrix of post signal price changes and 
#' plots boxplots. Note function plots whatever is given to it therefore when 
#' there are lots paramsets, it is best to plot a smaller portion so the 
#' information can be displayed clearly.
#'                          
#' @param signals list of paramset forward looking price changes by asset
#' @param rows number of rows for plot
#' @param columns number of columns for plot
#' @param mai A numerical vector of the form c(bottom, left, top, right) which gives the margin size specified in inches.
#' @param mgp The margin line (in mex units) for the axis title, axis labels and axis line. Note that mgp[1] affects title whereas mgp[2:3] affect axis. The default is c(3, 1, 0).
#' @param xlab a title for the x axis
#' @param ylab a title for the y axis
#' @param cex.main The magnification to be used for main titles relative to the current setting of cex.
#' @param xaxt A character which specifies the x axis type. Specifying "n" suppresses plotting of the axis. The standard value is "s": for compatibility with S values "l" and "t" are accepted but are equivalent to "s": any value other than "n" implies plotting.
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of cex.
#' @param h the y-value(s) for horizontal line(s).
#' @param hlinecol A specification for the default plotting color. See section \sQuote{Color Specification}.
#' @param ... any other passthru parameters
#' @author Michael Guan
#' @export

signal.plot<-function(signals,rows=NULL,columns=NULL,mai = c(0.1,0.4,0.2,0.1), mgp = c(1,1,0),
                       xlab='',ylab='',cex.main=0.6,xaxt='n',cex.axis=0.5,h=0,hlinecol='red',...){
  
  if(is.null(signals)) stop('No signals to plot')
  
  # Determine Plot layout
  if(is.null(rows) | is.null(columns)){
    list.len = length(signals)
    rows = ceiling(list.len / as.numeric(substring(toString(list.len),1,1)))
    columns = ceiling(list.len / rows)  
  }
  
  # Set up display parameters
  plt = par(mfrow=c(rows,columns),mai=mai,mgp=mgp)
  
  # Generate Boxplot for each paramset
  for(plt in names(signals)){
    boxplot(x=signals[[plt]],main=plt,xlab=xlab,ylab=ylab,
            cex.main=cex.main,xaxt=xaxt,cex.axis=cex.axis,...)
    abline(h=h,col=hlinecol)
  }
  
}

#' Visualization of Signal Across Lookback with Beanplots
#'
#' This function is similar to \code{signal.plot} but uses beanplots 
#' instead of barplots. Requires 'beanplot' package
#'                          
#' @param signals list of paramset forward looking price changes by asset
#' @param rows number of rows for plot
#' @param columns number of columns for plot
#' @param mai A numerical vector of the form c(bottom, left, top, right) which gives the margin size specified in inches.
#' @param mgp The margin line (in mex units) for the axis title, axis labels and axis line. Note that mgp[1] affects title whereas mgp[2:3] affect axis. The default is c(3, 1, 0).
#' @param xlab a title for the x axis
#' @param ylab a title for the y axis
#' @param cex.main The magnification to be used for main titles relative to the current setting of cex.
#' @param xaxt A character which specifies the x axis type. Specifying "n" suppresses plotting of the axis. The standard value is "s": for compatibility with S values "l" and "t" are accepted but are equivalent to "s": any value other than "n" implies plotting.
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of cex.
#' @param h the y-value(s) for horizontal line(s).
#' @param hlinecol A specification for the default plotting color. See section \sQuote{Color Specification}.
#' @param ... any other passthru parameters
#' @author Michael Guan
#' @return plot
#' @export
 
beanplot.signals<-function(signals,rows=NULL,columns=NULL,mai = c(0.1,0.4,0.2,0.1), mgp = c(1,1,0),
                           xlab='',ylab='',cex.main=0.6,xaxt='n',cex.axis=0.5,
                           h=0,hlinecol='red',...){
  if(!requireNamespace('beanplot', quietly=TRUE)) stop("The 'beanplot' package is required to use this function")
  
  if(is.null(signals)) stop('No signals to plot')

  # Determine Plot layout
  if(is.null(rows) | is.null(columns)){
    list.len = length(signals)
    rows = ceiling(list.len / as.numeric(substring(toString(list.len),1,1)))
    columns = ceiling(list.len / rows)  
  }
  
  plt = par(mfrow=c(rows,columns),mai=mai,mgp=mgp)
  
  for(plt in names(signals)){
    beanplot(data.frame(signals[[plt]]),xlab=xlab,ylab=ylab,main=plt,
             cex.main = cex.main,xaxt=xaxt,cex.axis=cex.axis,...)
    abline(h=h,col=hlinecol)
  }
}


#' Visualization of Single Signal 
#' 
#' This function employs \code{plotSimpleGamlss} in package \code{gamlss.util}.
#'                          
#' @param signal list of paramset forward looking price changes by asset
#' @param x.val he values of the explanatory variable where we want to see the distribution
#' @param val this parameter determines how the plotted distribution is shown, increase/decrease it if the distribution is not shown properly
#' @param ylim the y limits in the plot
#' @param xlim the x limits in the plot
#' @param mai A numerical vector of the form c(bottom, left, top, right) which gives the margin size specified in inches.
#' @param h the y-value(s) for horizontal line(s).
#' @param ... any other passthru parameters
#' @author Michael Guan
#' @return plot
#' @export

distributional.boxplot<-function(signal,x.val=seq(1, 50, 5),val=10,ylim=c(-5, 5),
                                 xlim=c(0, 50),mai=c(1,1,0.3,0.5),h=0,...){
  
  if(is.null(signal)) stop('No signals to plot')

  if(!requireNamespace('gamlss', quietly=TRUE)) stop("The 'gamlss' package is required to use this function")
  if(!requireNamespace('gamlss.util', quietly=TRUE)) stop("The 'gamlss.util' package is required to use this function")
  
  # Reformat data
  n.row = nrow(signal)
  ind.var = matrix(signal)
  dep.var = signal * NA
  for(i in 1:ncol(dep.var)) dep.var[,i] = i
  dep.var = matrix(dep.var)
  
  mat.data = cbind(dep.var,ind.var)
  colnames(mat.data) = c('Period','Change')
  mat.data = data.frame(mat.data)
  par(mai=mai)
  
  # Regression
  m1 <- gamlss(Change~Period, data=mat.data)
  
  # Visualization
  tryCatch({
    plotSimpleGamlss(y=Change,x=Period, model=m1,data=mat.data, 
                     x.val=x.val, # where to plot the dist
                     val=val, # size of dist
                     ylim=ylim, xlim=xlim)
    abline(h=0,lty=3,...)  
  },error=function(e){cat('gamlss package currently doesnt 
                            support encapsulation of their 
                            plotting function. Pending Patch. \n')})
    
}

#' Visualization of Signal Path
#' 
#' This function creates a rChart - hplot that is interactive. It displays 
#'                          
#' @param data signal data
#' @param main plot title
#' @author Michael Guan
#' @return plot
#' @examples
#' \dontrun{
#' signal.plot.path(results$sigret.by.asset$RTH$paramset.1.5[1:10,])
#' }
#' @export
signal.path.plot<-function(data,main='Cumulative Return Paths'){
  if(!requireNamespace('rCharts', quietly=TRUE)) stop("The 'rCharts' package is required to use this function")
  if(!requireNamespace('reshape2', quietly=TRUE)) stop("The 'reshape2' package is required to use this function")
  
  data = t(data)
  n.row = nrow(data)
  ind.var = matrix(data)
  dep.var = ind.var * NA
  dep.var[] = rep(seq(1,n.row,1),ncol(data))
  
  groups = ind.var * NA
  groups[]=melt(sapply( seq(1,ncol(data),1),function(x)rep(x,n.row)))[,2]
  
  
  mat = cbind(dep.var,ind.var,groups)
  colnames(mat) = c('Periods','Returns','groups')
  mat = data.frame(mat)
  h2a = hPlot(x = "Periods", y = "Returns",group='groups', data = mat, 
              type = "line",showInLegend = FALSE,title=main)
  
  h2a$legend(FALSE)
  h2a
}


#TODO Going Up/Going Down maybe better implemented as slope/diff() indicator, then coupled with threshold signal 
#TODO set/reset indicator/signal for n-periods since some other signal is set, or signal set for n periods

###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2015
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
