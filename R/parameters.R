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

label2index <- function(strategy, type, label)
{
    switch(type,
        indicator = {
            specs <- strategy$indicators
        },
        signal = {
            specs <- strategy$signals
        },
        enter = {
            specs <- strategy$rules$enter
        },
        exit = {
            specs <- strategy$rules$exit
        },
        order = {
            specs <- strategy$rules$order
        },
        chain = {
            specs <- strategy$rules$chain
        },
        {
            stop(paste(type, ': no such type in strategy', strategy))
        }
    )

    i = 1
    for(spec in specs)
    {
        if(spec$label == label)
            return(i)
        i <- i + 1
    }
    stop(paste(label, ': no such label with type', type, 'in strategy', strategy))

    print(paste('====== index ==', i))
}

# Functions for parameter generating and testing.
# 
# Author: Yu Chen
###############################################################################

#retrieve the needed parameters and existing values after add*

#' Extract the parameter structure from a strategy object. (deprecated)
#' 
#' Users can use this function to extract the parameters used in a strategy, and use the output as a reminder/ cheatsheet
#' when they create the parameter distribution or parameter constraints. But it's not required to run to specify the distribution or constraints of parameters. 
#' 
#' @examples
#' # When strategy object stratMACD has already been created by demo macd.R:
#' # following line will return object x that contains the parameter information.
#' \dontrun{
#' x<-getParameterTable(stratMACD)
#' }
#' @return
#' A list of objects that contains the parameter structure  information
#' \describe{
#'    \item{paramNameList}{the list of parameters used in the strategy, for printing or viewing as a table.}
#'    \item{strategyName}{ string name of the strategy}
#'    \item{structure}{the detailed paramter structure in the input strategy, can be used when user wants to look into more details of the parameter structure.}
#' }
#' @param strategy The strategy object.
#' @author Yu Chen
getParameterTable<-function (strategy) #,staticSwitch)
{
    
    tmp_paramTable<-list()
    nofi=0
    indexnum=0
    for (indicator in strategy$indicators ){ 
        #   .formals  <- formals(fun) #yc here get the prameters needed for that function.
        #   print(.formals)
        nofi=nofi+1 
        indexnum=indexnum+1
        #
        fun<-match.fun(indicator$name) #yc here get the function of the indicator
        tmp_paramTable[[nofi]]<-list()
        #tmp_paramTable[[nofi]]<-formals(fun)       
        tmp_paramTable[[nofi]]$paramType<-'indicator'
        tmp_paramTable[[nofi]]$paramEnabled<-indicator$enabled
        tmp_paramTable[[nofi]]$indexnum=indexnum
        tmp_paramTable[[nofi]]$label<-indicator$label
        tmp_paramTable[[nofi]]$args<-formals(fun)
        
    }
    
    indexnum=0
    for (signal in strategy$signals ){ 
        
        
        nofi=nofi+1
        indexnum=indexnum+1
        
        fun<-match.fun(signal$name)
        tmp_paramTable[[nofi]]<-list()
        
        tmp_paramTable[[nofi]]$paramType<-'signal'
        tmp_paramTable[[nofi]]$paramEnabled<-signal$enabled
        tmp_paramTable[[nofi]]$indexnum=indexnum
        tmp_paramTable[[nofi]]$label<-signal$label
        tmp_paramTable[[nofi]]$args<-formals(fun)
        
    }
    
    for (rule in strategy$rules ){
        indexnum=0
        for (trule in rule){
            
            
            
            nofi=nofi+1
            indexnum=indexnum+1
            
            fun<-match.fun(trule$name) 
            tmp_paramTable[[nofi]]<-list()
            
            tmp_paramTable[[nofi]]$paramType<-trule$type
            tmp_paramTable[[nofi]]$paramEnabled<-trule$enabled
            tmp_paramTable[[nofi]]$indexnum=indexnum
            tmp_paramTable[[nofi]]$label<-trule$label
            tmp_paramTable[[nofi]]$args<-formals(fun)
            tmp_paramTable[[nofi]]$timespan<-trule$timespan
            
        }
        
    }
    #data.frame(c(paramStructure[[6]][1:4],param.name.=names(paramStructure[[6]]$args)))
    paramPack<-list()
    for (i in 1:length(tmp_paramTable)){
        
        paramPack$paramNameList[[i]]<-data.frame(c(tmp_paramTable[[i]][1:4],param.=names(tmp_paramTable[[i]]$args)))
        
    }
    
    #tmp_paramTable$strategyName<-strategy$name
    paramPack$strategyName<-strategy$name
    paramPack$structure<-tmp_paramTable
    
    return(paramPack)
    
}


#' Function used to create an object that contains the distribution of parameters to be generated from, before testing parameters of a strategy. (deprecated)
#' 
#' 
#' Each call to the function will set/update the distribution of ONE parameter in the 'parameter distribution object' that is associated with a specific strategy.  
#' 
#' Parameter distribution object for one strategy usually won't work for another strategy, because different strategies has different parameter structure.
#' Type of the parameter and the sequence in that type is needed to specify the exact parameter in THAT STRATEGY.
#' 
#' The parameter 'distribution' is a list contains vector of values NAMED WITH THE NAMES OF THE PARAMETERS, the values can be any type (integer, characters, etc) but must match with the legal value of that parameter.
#' For example: distribution=list(nFast=(10:30)) or distribution=list(relationship=c('gt','gte'))
#' 
#' @examples 
#' \dontrun{
#' #(For complete demo see parameterTestMACD.R)
#' tPD2<-setParameterDistribution(tPD2, strat, component.type='indicator', component.label='FastSMA', 
#'      distribution=list(nFast=(10:30)), label='nFast')
#' tPD2<-setParameterDistribution(tPD2, strat, component.type='indicator', component.label='SlowSMA', 
#'      distribution=list(nSlow=(20:40)), label='nSlow')
#' tPD2<-setParameterDistribution(tPD2, strat, component.type='signal', component.label='go.long', 
#'      distribution=list(relationship=c('gt', 'gte')), label='sig1.gtgte')
#' }
#' 
#' @param paramDist The object in which the parameter list is stored; if this parameter is missing, or object does not exist, the function will return a new object.
#' @param strategy The strategy object
#' @param component.type A character string that specifies the type of the strategy component that contains the parameter, it takes the value in one of 'indicator', 'signal', 'enter', 'exit', 'order', 'chain'.
#' @param component.label The label that uniquely identifies the strategy component that contains the parameter
#' @param distribution Distribution of the parameter, can be any function that returns a vector of value. See detail. (A numerical example: 1:10 or sample(1:20,6)
#' @param weight The weight of each value in the distribution, if missing, the default value of all equal weights will be taken.
#' @param label A string label to apply to the parameter distribution
#' @param psindex A number specify the index within the parameter distribution object, it is used to make change/ repalce a parameter distribution in the object.
#' @return The returned object is a structure contains the distribution of parameters, if the input argument 'paramDist' is provided, the function update the input paramDist object and return the updated one. When specify the distribution of several parameters, usually the first returned object is passed to the next several call of the function as input argument 'paramDist'. See example. 
#' @author Yu Chen
setParameterDistribution<-function(paramDist=NULL, strategy, component.type, component.label, distribution=NULL, weight, label, psindex=NULL) #All is needed,  set to illegal values
{
    .Deprecated(new = 'add.distribution', msg = 'old parameter code deprecated, please use add.distribution instead')
  
    missing.msg <- ': missing in call to setParameterDistribution'

    if(!hasArg(strategy))
        stop(paste('strategy', missing.msg))
    if(!hasArg(component.type))
        stop(paste('component.type', missing.msg))
    if(!hasArg(component.label))
        stop(paste('component.label', missing.msg))
    if(!hasArg(distribution))
        stop(paste('distribution', missing.msg))

    if(!component.type %in% c("indicator","signal","enter","exit","order","chain"))
        stop("Type must be a string in: indicator, signal, enter, exit, order", "chain")

    if(!is.list(distribution) || length(distribution) != 1)
        stop("distribution must be passed as a named list of length 1")

    if(!hasArg(paramDist) || !exists(as.character(substitute(paramDist))))
    {
        paramDist<-list()
        print('Object for parameter distribution initialized...')
    }

    indexnum <- label2index(strategy, component.type, component.label)

    tmp_paramDist<-list()
    tmp_paramDist$type<-component.type
    tmp_paramDist$indexnum<-indexnum
    tmp_paramDist$distribution<-distribution
    
    if(missing(label))
    {
        tmp_paramDist$label<-paste('Param',component.type,indexnum,names(distribution),sep='.')
    }
    else
    {
        tmp_paramDist$label<-label
    }

    if(!hasArg(weight)) weight<-sample(1/length(distribution[[1]]),length(distribution[[1]]),replace=TRUE)

    tmp_paramDist$weight<-weight

    if(!hasArg(psindex) || (hasArg(psindex) && is.null(psindex)))
        psindex = length(paramDist)+1

    #class(tmp_paramDist)<-'parameter_distribution'

    #TODO put an check to see if the component.type/indexnum exist already.
    paramDist[[psindex]]<-tmp_paramDist

    return(paramDist)
}

#' Generate parameter sets for a specific strategy, test the strategy on each set of parameters, output result package. (deprecated)
#' 
#' The function do several things in one call, to test different parameters on a strategy. It generates parameter sets based on specified distribution (a defined parameter distribution object generated by setParameterDistribution function) and constraints (A defined parameter constraint object generated by setParameterConstraint function), 
#' apply the generated parameter sets to the specified strategy and return the results package, put the generated portfolio objects and account objects in .blotter environment.
#' 
#' Other than the returned result pack, the function puts the generated portfolio and account objects in the .blotter environment.
#' The names of those objects will be extension of the names created by initial strategy/portfolios are created.
#' For example, in macd.R demo, account.macd and portfolio.macd are created in .blotter environment. After calling the applyParameter function and did the 
#' parameter test, series of similar objects are created with names account.macd.p.1, account.macd.p.2 ... and portfolio.macd.p.1, portfolio.macd.p.2 ...
#' 
#' @return 
#' In the returned result pack as a list of object:
#' 
#' \describe{
#'      \item{statsTable}{is the summary table show stats from all the runs}  
#'      \item{eachRun}{contains the details of test run with each parameter set, including portfolio name, account name, strategy used, parameters used, stats of the single run, object in .blotter but as a list named blotterl} 
#'      \item{paramTable}{is parameter samples used, in a table for print}
#'      \item{paramConstrainTable}{is the constraints apply to the parameters, in a table for print}
#'      \item{parameterDistribution}{is the parameter distribution passed in as argument}
#'      \item{parameterConstraints}{is the constraints apply to the parameters, passed in as argument}
#' }
#' 
#' @section Support for parallel execution:
#' The function supports parallel execution, user only need to initial the parallel package and wrap up afterwards.
#' The function will automaticly use the number of registered parallel sessions to run testing, See example.
#' 
#' @examples 
#' \dontrun{
#' require(foreach)
#' require(doSMP)
#' workers <- startWorkers(2)
#' registerDoSMP(workers)
#' 
#' #PUT ALL CODE RELATED TO QUANTSTRAT HERE
#'      #Example to call the function:  (For complete demo see parameterTestMACD.R)
#'      x<-applyParameter(strategy=stratMACD,portfolios=portfolio.st,parameterPool=tPD2,
#'                   method='random',sampleSize=20,parameterConstraints=pConstraint2)
#'      #or 
#'      x<-applyParameter(strategy=stratMACD,portfolios=portfolio.st,parameterPool=tPD2,
#'                   method='expand',parameterConstraints=pConstraint2)
#' 
#' stopWorkers(workers)
#' rmSessions(all=TRUE)
#' }
#' 
#' @param strategy The strategy to test paramters to.
#' @param portfolios The character name of the portfolios to apply to.
#' @param parameterPool The object that created by setParameterDistribution function, which includes all the parameter legal values and distribution/weights.
#' @param parameterConstraints The object created by setParameterConstraint function that specifies the constraints between each parameters,
#' @param method Takes string 'expand' or 'random', specify how to generate samples of parameters. 'expand' will do all possible combinations of the parameter sets, 
#' @param sampleSize Used when method=='random', specify how many parameter sets to generate and run test of.
#' @param verbose if verbose TRUE or 1+, will print a lot of debug info, default FALSE
#' @param \dots any other passthru parameters
#' @seealso \code{\link{setParameterDistribution}}, \code{\link{setParameterConstraint}}
#' 
#' @author Yu Chen
applyParameter<-function(strategy,portfolios,parameterPool,parameterConstraints,method,sampleSize,verbose=FALSE,...)
{
    .Deprecated("apply.paramsets","The original parameter code in applyParameter hase been deprecated.  Use apply.paramsets instead.")  
  
    #need to create combination of distribution values in each slot of the parameterPool
    
    
    initialPortf<-.getPortfolio(portfolios)
    symbols<-ls(initialPortf$symbols)

    # TODO: we likely want to search for first date, not (arbitrarily?)
    # choose the first symbol
    firstsymbol<-first(symbols)
    initDate<-time(first(initialPortf$symbols[[firstsymbol]]$posPL))
    
    limits<-list()
    for(symbol in ls(initialPortf$symbols))
        limits[[symbol]]<-initialPortf$symbols[[symbol]]$PosLimit

    tmp_strategy<-strategy
    
    results<-list()
    results$stats<-NULL
    
    if (!is.strategy(tmp_strategy)) {
        tmp_strategy<-try(getStrategy(tmp_strategy))
        if(inherits(tmp_strategy,"try-error"))
            stop ("You must supply an object of type 'strategy'.")
    } 
    
    out<-list()
    paramdist<-list()
    paramweight<-list()
    paramLabel<-list()
    lvmatch<-list()
    
    for (i in 1:length(parameterPool)){
        
        distr<-parameterPool[[i]]
        #paramdist[[i]]<-distr$distribution[[1]]
        paramdist[[paste('Param',distr$type,distr$indexnum,names(distr$distribution),sep='.')]]<-distr$distribution[[1]]
        paramweight[[paste('ParamWt',distr$type,distr$indexnum,names(distr$distribution),sep='.')]]<-distr$weight
        #paramdist[[paste(i)]]<-distr$distribution[[1]]
        
        #Build label<->var name match.
        lvmatch$label[i]<-distr$label
        lvmatch$varName[i]<-paste('Param',distr$type,distr$indexnum,names(distr$distribution),sep='.')
        
    }
    
    paramLabel<-data.frame(lvmatch,stringsAsFactors=FALSE)
    
    #TODO make it take sample size etc.
    
    applyConstraints <- function(param.table, constraints)
    {
        for(constraint in constraints)
        {
            constraint.fulfilled <- paramConstraint(
                    label=constraint$constraintLabel,
                    data=param.table,
                    columns=merge(paramLabel,data.frame(constraint$paramList),by="label")$varName, #has to keep the order.
                    relationship=constraint$relationship
            )
            param.table <- param.table[which(constraint.fulfilled==TRUE),]
        }
        return(param.table)
    }
    
    if (method=='expand') 
    {
        paramTable <- expand.grid(paramdist, stringsAsFactors=FALSE)

        paramTable <- applyConstraints(paramTable, parameterConstraints)
    }
    else if (method=='random')
    {
        if (missing(sampleSize)) {stop ("sampleSize is needed")} 
        #paramTable<-data.frame()
        
        #genSample update the paramTable with more sample rows; this is a recursive function
        genSample<-function(iparamTable,paramdist,tsampleSize,remainSize)
        {
            if (missing(remainSize) ) remainSize=tsampleSize
            
            tparamTable<-data.frame()
            
            for( i in 1:length(paramdist))
            {
                ireplace<-(length(paramdist[i])<tsampleSize)
                
                if (nrow(tparamTable)==0)
                {
                    tparamTable<-data.frame(sample(paramdist[[i]],remainSize,prob=paramweight[[i]],replace=ireplace),stringsAsFactors=FALSE)
                }   
                else
                {
                    tparamTable<-cbind(tparamTable,data.frame(sample(paramdist[[i]],remainSize,prob=paramweight[[i]],replace=ireplace),stringsAsFactors=FALSE))
                }                                       
            }
            
            names(tparamTable)<-names(paramdist)
            
            paramTable <- applyConstraints(tparamTable, parameterConstraints)
            
            iparamTable<-rbind(iparamTable,tparamTable)
            iparamTable<-unique(iparamTable)
            
            if (nrow(iparamTable)<tsampleSize)
            {
                iparamTable<-genSample(iparamTable,paramdist,tsampleSize,remainSize=tsampleSize-nrow(iparamTable))          
            }
            
            names(iparamTable)<-names(paramdist)
            return(iparamTable)
        } #end define function
        
        paramTable<-NULL
        paramTable<-genSample(paramTable,paramdist,sampleSize)      
    }
    
    strategyList<-list()
    if(verbose >=1) print("ParamTable generated")
    
    instruments<-as.list(FinancialInstrument:::.instrument)
    getSymbols<-as.list(.getSymbols)
    blotter<-as.list(.blotter)
    
    #Pack all symbols downloaded in .GlobalEnv
    symbols<-names(.getSymbols)
    
    testPackListPRL<-foreach (i = 1:nrow(paramTable), .export=c('instruments',symbols,'getSymbols','blotter','tmp_strategy'),.verbose=verbose,...=...) %dopar% 
            
            {
                #if(verbose)
                print(paste('===> now starting parameter test', i))

                require(quantstrat, quietly=TRUE)
                
                # loops must be run with an empty .blotter environment each, or .blotter appears to accumulate portfolios and accounts
                # and passes them from one loop to the next on each CPU - JH July 2012
                if (getDoParRegistered() && getDoParWorkers()>1)
                {
                    rm(list=ls(pos=.blotter), pos=.blotter)
                    gc(verbose=verbose)
                }

                testPack<-list()
                
                #Pass environments needed.
                loadInstruments(instruments)
                .getSymbols<-as.environment(getSymbols)
                
                #Unpack symbols to worker. change later.
                #seems need to go through assign, rather than just .export the names...
                
                for (sym in symbols) {
                    assign(sym, eval(as.name(sym)), .GlobalEnv)
                }
                
                #Create a copy of strategy object, so not to lock up on the sameone.
                PLtmp_strategy<-tmp_strategy
                
                #Extract parameter from table and construct PLtmp_strategy.
                for (j in 1:ncol(paramTable))
                {
                    set.param.values <- function(param.list, new.values)
                    {
                        pnamepos<-pmatch(names(param.list),names(new.values),nomatch=0L)

                        if( any(pnamepos>0))
                        {
                            #FIXME: any matching args will be set to 1st param
                            param.list[which(pnamepos>0)]<-new.values[1]
                        }
                        else
                        {
                            param.list<-append(param.list, new.values)
                        }
                        param.list
                    }

                    tmp_arg<-parameterPool[[j]]$distribution[1] #Just get the list form with name
                    tmp_arg[[1]]<-paramTable[i,j]
                    
                    tmp_index<-parameterPool[[j]]$indexnum

                    switch(parameterPool[[j]]$type,
                            'indicator'=
                            {
                                PLtmp_strategy$indicators[[tmp_index]] = set.param.values(PLtmp_strategy$indicators[[tmp_index]], tmp_arg)
                                PLtmp_strategy$indicators[[tmp_index]]$arguments = set.param.values(PLtmp_strategy$indicators[[tmp_index]]$arguments, tmp_arg)
                            },
                            'signal'=
                            {
                                PLtmp_strategy$signals[[tmp_index]] = set.param.values(PLtmp_strategy$signals[[tmp_index]], tmp_arg)
                                PLtmp_strategy$signals[[tmp_index]]$arguments = set.param.values(PLtmp_strategy$signals[[tmp_index]]$arguments, tmp_arg)
                            },
                            'order'=
                            {
                                PLtmp_strategy$rules$order[[tmp_index]] = set.param.values(PLtmp_strategy$rules$order[[tmp_index]], tmp_arg)
                                PLtmp_strategy$rules$order[[tmp_index]]$arguments = set.param.values(PLtmp_strategy$rules$order[[tmp_index]]$arguments, tmp_arg)
                            },
                            'enter'=
                            {
                                PLtmp_strategy$rules$enter[[tmp_index]] = set.param.values(PLtmp_strategy$rules$enter[[tmp_index]], tmp_arg)
                                PLtmp_strategy$rules$enter[[tmp_index]]$arguments = set.param.values(PLtmp_strategy$rules$enter[[tmp_index]]$arguments, tmp_arg)
                            },
                            'exit'=
                            {
                                PLtmp_strategy$rules$exit[[tmp_index]] = set.param.values(PLtmp_strategy$rules$exit[[tmp_index]], tmp_arg)
                                PLtmp_strategy$rules$exit[[tmp_index]]$arguments = set.param.values(PLtmp_strategy$rules$exit[[tmp_index]]$arguments, tmp_arg)
                            },
                            'chain'=
                            {
                                PLtmp_strategy$rules$chain[[tmp_index]] = set.param.values(PLtmp_strategy$rules$chain[[tmp_index]], tmp_arg)
                                PLtmp_strategy$rules$chain[[tmp_index]]$arguments = set.param.values(PLtmp_strategy$rules$chain[[tmp_index]]$arguments, tmp_arg)
                            },
                    )
                } #loop j
                
#Initial portfolio for each test        
                #######################################################################################
                
                testPack$portfolio.st<-paste(portfolios,'p',i,sep='.')
                testPack$account.st<-paste(portfolios,'p',i,sep='.')
                
                rmpstr<-paste('portfolio',testPack$portfolio.st,sep=".")
                rmastr<-paste('account',testPack$account.st,sep=".")
                
                try(rm(list = rmpstr, pos = .blotter),silent=FALSE)
                try(rm(list = rmastr, pos = .blotter),silent=FALSE)
                try(rm(list=paste("order_book",testPack$account.st,sep="."),pos=.strategy),silent=FALSE)
                
                if(verbose >=1) print('Initial portf')
                
#               Decide not to remove the main obj from .blotter, incase of non-parallel run.
#               try(rm(list=paste("order_book",portfolios,sep='.'),pos=.strategy),silent=TRUE)
##              try(rm(paste("account",portfolio.st,sep='.'),paste("portfolio",portfolio.st,sep='.'),pos=.blotter),silent=TRUE)
#               try(rm(list=paste("account",portfolios,sep='.'),pos=.blotter))
#               try(rm(list=paste("portfolio",portfolios,sep='.'),pos=.blotter))
                
                try({initPortf(testPack$portfolio.st,symbols=symbols, initDate=initDate)})
                try({initAcct(testPack$account.st,testPack$portfolio.st, initDate=initDate)})
                try({initOrders(portfolio=testPack$portfolio.st,initDate=initDate)})
                
                for(symbol in names(limits))
                    addPosLimit(portfolio=testPack$portfolio.st, symbol=symbol, timestamp=initDate, maxpos=limits[[symbol]]$MaxPos[[1]])

# Apply strategy ######################################################################################
                if(verbose >=1) print("Apply strategy...")
                
                try(rm("PLtmp_strategy",pos=.strategy),silent=TRUE)
                
                if(verbose >=1) print(PLtmp_strategy$signals[[2]])
                
                assign("PLtmp_strategy1",PLtmp_strategy,envir=as.environment(.strategy))
                
                if(verbose)
                {
                    testPack$out<-try(applyStrategy(strategy=PLtmp_strategy , portfolios=testPack$portfolio.st ),...=...)
                    testPack$strategy<-PLtmp_strategy
                }
                else
                {
                    try(applyStrategy(strategy=PLtmp_strategy , portfolios=testPack$portfolio.st ),...=...)
                }
                
#   Update portfolio ######################################################################################
                
                #out<-try(applyStrategy(strategy=stratBBands , portfolios=portfolios ))
                #       try({
                #                   updatePortf(testPack$portfolio.st,Date=initDate)
                #                   updateAcct(testPack$account.st,Date=initDate)
                #                   updateOrders(portfolio=testPack$portfolio.st)
                #               })
                
                
#try(updatePortf(Portfolio=testPack$portfolio.st,Dates=paste('::',as.Date(Sys.time()),sep='')))
                updatePortf(Portfolio=testPack$portfolio.st,Dates=paste('::',as.Date(Sys.time()),sep=''))
                
                #no need to update account.
                #updateAcct(account.st,Dates=paste(startDate,endDate,sep="::")) 
                #updateEndEq(account.st,Dates=paste(startDate,endDate,sep="::"))
                #getEndEq(account.st,Sys.time())
                
                testPack$parameters<-paramTable[i,]
                
                testPack$stats<-tradeStats(Portfolios=testPack$portfolio.st)
                if(verbose)
                    testPack$blotterl<-as.list(.blotter)
#               testPack$blotter<-as.environment(.blotter)
#               testPack$blotterr<-.blotter
        
                return(testPack)
                
            }   # Loop i
            gc(verbose=verbose)
    
    
    for (k in 1: nrow(paramTable)){
        results$statsTable<-rbind(results$stats,cbind(testPackListPRL[[k]]$parameters,testPackListPRL[[k]]$stats))
        
        if(verbose)
        {
            print(names(testPackListPRL[[k]]$blotterl))

            for(nn in 1:length(testPackListPRL[[k]]$blotterl)){
#               if(verbose >=1) print(paste(names(testPackListPRL[[k]]$blotterl)[nn],'nnp',nn,sep='.'))
                assign(names(testPackListPRL[[k]]$blotterl[nn]),testPackListPRL[[k]]$blotterl[[nn]],envir=as.environment(.blotter))
            }
            names(testPackListPRL)[k]<-testPackListPRL[[k]]$portfolio.st
        }
    }
    
    
    results$paramTable<-paramTable

    if(verbose)
    {
        results$eachRun<-testPackListPRL
        results$paramConstrainTable<-data.frame(parameterConstraints)
            
        results$parameterDistribution<-parameterPool
        results$parameterConstraints<-parameterConstraints
    }
    return(results)
    
}


#' Internal function used in applyParameter function for process constraints on relationship between two parameter values. (deprecated)
#' 
#' Basically is the same as sigComparison function in signal.R written by Brian, with minor change.  
#' 
#' Currently, this function compares two columns.  
#' Patches to compare an arbitrary number of columns would be gladly accepted.
#' 
#' Comparison will be applied from the first to the second column in the \code{columns} vector.
#' 
#' Relationship 'op' means 'opposite' side.  Reasonable attempt will be made to match.
#' 
#' @param label text label to apply to the constraint
#' @param data data to apply comparison to
#' @param columns named columns to apply comparison to
#' @param relationship one of c("gt","lt","eq","gte","lte","op") or reasonable alternatives
paramConstraint <- function(label,data=mktdata, columns, relationship=c("gt","lt","eq","gte","lte", "op"))
{

  
    if(length(relationship) != 1)
        stop('paramConstraint: length(relationship)!=1')

    if(length(columns) != 2)
        stop('paramConstraint: length(columns)!=2')

    if(relationship == 'op')
    {
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
            gt =, '>' = '>', 
            lt =, '<' = '<', 
            eq =, "==" =, "=" = "==",
            gte =, gteq =, ge =, ">=" = ">=",
            lte =, lteq =, le =, "<=" = "<="
    )
    
    ret_sig <- NULL
    ret_sig$tname <- do.call(opr, list(data[,colNums[1]], data[,colNums[2]]))
    
    names(ret_sig)<-label
    return(data.frame(ret_sig))
}


#' Function to construct parameter constraint object. (deprecated)
#' 
#' Function to construct parameter constraint object. The returned value will be one of the inputs to the applyParameter function.
#'  
#' @examples
#' #(For complete demo see parameterTestMACD.R)
#' #In a MACD strategy, we want to fast macd calcuated from less time periods (days)
#' #than slow macd signal:
#' \dontrun{  
#' x<-setParameterConstraint(constraintLabel='macdPC',
#'      paramList=c('nFast','nSlow'),relationship='lt')
#' }
#' #The object x then can be used as one of the inputs to applyParameter function to specify the
#' #constraints between parameters. 
#' 
#' @param paramConstraintObj the ParameterConstraint object to be updated, if missing, funtion will create a new one.
#' @param constraintLabel string label to apply to the constraint.
#' @param paramList the two name of the prameters as a list contains two strings.
#' @param relationship relationship between the 1st parameter and 2nd one. ('gt' means 1st parameter > 2nd parameter).
#' @return The returned object is a structure contains the constraints on pairs of parameters, if the input argument 'paramConstraintObj' is provided, the function update the input paramConstraintObj object and return the updated one. When specify the constraints of several pairs of parameters, usually the first returned object is passed to the next several call of the function as input argument 'paramConstraintObj'. See example. 
#' @author Yu Chen
setParameterConstraint<-function(paramConstraintObj=list(),constraintLabel,paramList,relationship)
{
    if(!hasArg(paramConstraintObj) || !exists(as.character(substitute(paramConstraintObj))))
    {
        paramConstraintObj<-list()
        print('Parameter constraint object initialized...')     
    }
    else
    {
        if(!is.list(paramConstraintObj) || length(paramConstraintObj) != 1)
            stop("Parameter constrain object must be passed as a named list of length 1")
    }
    
    if(missing(constraintLabel))
    {
        constraintLabel<-paste("parameterConstraint",length(paramConstraintObj)+1)
    }

    tmp_PC<-list()
    tmp_PC$constraintLabel<-constraintLabel
    #names(paramList)<-"label"
    tmp_PC$paramList$label<-paramList
    tmp_PC$relationship<-relationship   
    
    paramConstraintObj[[paste(constraintLabel)]]<-tmp_PC
    return(paramConstraintObj)
}

