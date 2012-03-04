## add parameters to strategy objects: ALPHA CODE USE WITH CARE 
#add.parameter <- 
#		function (strategy, 
#				type = c('indicator','signal'), 
#				add.to.name,
#				method = c('lookup','lookup.range','calc'), 
#				arguments = NULL, 
#				label = NULL,
#				...,
#				store=FALSE) 
#{
#	if(!is.strategy(strategy)) stop("You must pass in a strategy object to manipulate")
#	# perhaps I should add parameters and parameter.args as arguments to the constructors...
#	
#	tmp.param<-list()
#	
#	type=type[1] #this should probably come out eventually
#	
#	method = method[1] #only use the first if the user didn't specify, or over-specified
#	
#	if(is.null(label)) {
#		label<-method
#	}
#	tmp.param$label <- label
#	tmp.param$method <- method
#	tmp.param$call <- match.call()
#	tmp.param$arguments <- arguments
#	class(tmp.param)<-'quantstrat.parameter'
#	
#	switch(type,
#			indicator = {type='indicators'},
#			signal = {type='signals'},
#			rule = {type='rules'}) #NOTE rules not supported yet, since they need a rule type too
#	
#	# need to think about how to create a 'parameters' list, and whether 
#	# it should be at the strategy level or lower down, on the individual 
#	# signal/indicator/rule
#	
#	if(!is.list(strategy[[type]][[add.to.name]]$parameters)){
#		strategy[[type]][[add.to.name]]$parameters <- list()
#	}
#	strategy[[type]][[add.to.name]][['parameters']][[method]] <- tmp.param
#	
#	if (store) assign(strategy$name,strategy,envir=as.environment(.strategy))
#	else return(strategy)
#}
#
#
## add parameters to strategy objects: ALPHA CODE USE WITH CARE 
#paramLookup <- function(strategy, symbol , type, name, parameter, ...) {
#	# should take in a strategy and parameter object, and return an argument list for 'symbol'
#	#as.pairlist(paramTable[,symbol]
#	paramTable<-get(paste(strategy,type,name,'table',pos=.strategy))
#	as.pairlist(paramTable[,symbol])
#}
#
## add parameters to strategy objects: ALPHA CODE USE WITH CARE 
#add.paramLookupTable <- function(strategy, type, name, paramTable){
#	assign(paste(strategy,type,name,'table',pos=.strategy),paramTable)
#}
#
## get parameterized arguments list out of the strategy environment
#getParams <- function (strategy, symbol, type, name)
#{
#	
#	params <- strategy[[type]][[name]]$parameters
#	param.ret<-list()
#	for (param in params) {
#		switch(param$method,
#				lookup = {param.ret<-c(param.ret,paramLookup(strategy,symbol,parameter=param))},
#				lookup.range = {},
#				calc = {},
#				{warning("parameter method",param$method,'not recognized for',type,name); next()}
#		)
#	}
#	# return an arguments list back to the 'apply*' fn
#	return(param.ret)
#}


###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2011
# Peter Carl, Dirk Eddelbuettel, Brian G. Peterson, Jeffrey Ryan, and Joshua Ulrich 
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################


# Functions for parameter generating and testing.
# 
# Author: Yu Chen
###############################################################################

#retreave the needed parameters and existing values after add*

#' Extract the parameter structure from a strategy object.
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
#' @export
getParameterTable<-function (strategy) #,staticSwitch)
{
	
	tmp_paramTable<-list()
	nofi=0
	indexnum=0
	for (indicator in strategy$indicators ){ 
		#	.formals  <- formals(fun) #yc here get the prameters needed for that function.
		#	print(.formals)
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
#
#
#getParameterInfo<-function(paramStructure){
#	paramInfo<-list()
#	for(paraLine in paramStructure){
#		paraInfo[[1]]<-paraLine$paramType
#	}
#}
#getParameterMatrix<-function(paraStructure){
#	
#}

#' Function used to create an object that contains the distribution of parameters to be generated from, before testing parameters of a strategy.
#' 
#' 
#' Each call to the function will set/update the distribution of ONE parameter in the 'parameter distribution object' that is associated with a specific strategy.  
#' 
#' When call the function, the user must know the parameter strcture of the strategy, function getParameterTable can be used to layout the parameter structure of a certain strategy.
#' Parameter distribution object for one strategy usually won't work for another strategy, because different strategies has different parameter structure.
#' Type of the parameter and the sequence in that type is needed to specify the exact parameter in THAT STRATEGY.
#' 
#' The parameter 'distribution' is a list contains vector of values NAMED WITH THE NAMES OF THE PARAMETERS, the values can be any type (integer, characters, etc) but must match with the leagal value of that parameter.
#' For example: distribution=list(nFast=(10:30)) or distribution=list(relationship=c('gt','gte'))
#' 
#' @examples 
#' \dontrun{
#' #(For complete demo see parameterTestMACD.R)
#' tPD2<-setParameterDistribution(tPD2,'indicator',indexnum=1,
#' 		distribution=list(nFast=(10:30)),label='nFast')
#' tPD2<-setParameterDistribution(tPD2,'indicator',indexnum=1,
#' 		distribution=list(nSlow=(20:40)),label='nSlow')
#' tPD2<-setParameterDistribution(tPD2,'signal',indexnum=1,
#' 		distribution=list(relationship=c('gt','gte')),label='sig1.gtgte')
#' }
#' 
#' @param paramDist The object that store the parameter list, if this parameter is missing, or object does not exist, the function will return a new object.
#' @param type A character string that specifies the type of the parameter, it takes the value in one of 'indicator', 'signal', 'enter', 'exit', 'order'.
#' @param indexnum Tells the sequence within the type, (for example: type = 'signal', indexnum =2 tells the function to update the 2nd signal in the strategy)  
#' @param distribution Distribution of the parameter, can be any function that returns a vector of value. See detail. (A numerical example: 1:10 or sample(1:20,6)
#' @param weight The weight of each value in the distribution, if missing, the default value of all equal weights will be taken.
#' @param label A string label to apply to the parameter distribution
#' @param psindex A number specify the index within the parameter distribution object, it is used to make change/ repalce a parameter distribution in the object.
#' @return The returned object is a structure contains the distribution of parameters, if the input argument 'paramDist' is provided, the function update the input paramDist object and return the updated one. When specify the distribution of several parameters, usually the first returned object is passed to the next several call of the function as input argument 'paramDist'. See example. 
#' @author Yu Chen
#' @export
setParameterDistribution<-function(paramDist=NULL,type=NULL,indexnum=0,distribution=NULL,weight,label,psindex=NULL){#All is needed, set to illegal values
	
	if(!hasArg(paramDist)||!exists(as.character(substitute(paramDist))) ){
		paramDist<-list()
		print('Object for parameter distribution initialized...')
	}
#	else{
	
		if (!is.list(distribution)|length(distribution)!=1) stop("distribution must be passed as a named list of length 1")
		if (!type %in% c("indicator","signal","enter","exit","order")) stop("Type must be a string in: indicator, signal, enter, exit, order")
		
		tmp_paramDist<-list()
		tmp_paramDist$type<-type
		tmp_paramDist$indexnum<-indexnum
		tmp_paramDist$distribution<-distribution
		
		if (missing(label)) {
			tmp_paramDist$label<-paste('Param',type,indexnum,names(distribution),sep='.')
		}
		else {tmp_paramDist$label<-label}
		
		
		if(!hasArg(weight)) weight<-sample(1/length(distribution[[1]]),length(distribution[[1]]),replace=TRUE)
		
		tmp_paramDist$weight<-weight
		
		if(!hasArg(psindex) | (hasArg(psindex) & is.null(psindex))) psindex = length(paramDist)+1
		#class(tmp_paramDist)<-'parameter_distribution'
		
		#TODO put an check to see if the type/indexnum exist already.
		paramDist[[psindex]]<-tmp_paramDist
#	}
	return(paramDist)
}

#' Generate parameter sets for a specific strategy, test the strategy on each set of parameters, output result package.
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
#' 		#Example to call the function:  (For complete demo see parameterTestMACD.R)
#' 		x<-applyParameter(strategy=stratMACD,portfolios=portfolio.st,parameterPool=tPD2,
#'                   method='random',sampleSize=20,parameterConstraints=pConstraint2)
#' 		#or 
#' 		x<-applyParameter(strategy=stratMACD,portfolios=portfolio.st,parameterPool=tPD2,
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
#' 
#' @seealso \code{\link{setParameterDistribution}}, \code{\link{setParameterConstraint}}
#' 
#' @author Yu Chen
#' @export
applyParameter<-function(strategy,portfolios,parameterPool,parameterConstraints,method,sampleSize){
	#need to create combination of distribution values in each slot of the parameterPool
	
	initialPortf<-getPortfolio(portfolios)
	stock.str<-names(initialPortf$symbols)
	initDate<-time(first(initialPortf$symbols[[1]]$posPL))
	
	tmp_strategy<-strategy
	
	testPackList<-list()
	testPackList$stats<-NULL
	
	testPackListPRLStructure<-list()
	testPackListPRLStructure$stats<-NULL
	
	
	
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
	
	
	
	if (method=='expand') 
	{
		paramTable<-expand.grid(paramdist, stringsAsFactors=FALSE)
	}
	else if (method=='random')
	{
		if (missing(sampleSize)) {stop ("sampleSize is needed")} 
		#paramTable<-data.frame()
		
		#genSample update the paramTable with more sample rows.
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
				else{
					tparamTable<-cbind(tparamTable,data.frame(sample(paramdist[[i]],remainSize,prob=paramweight[[i]],replace=ireplace),stringsAsFactors=FALSE))
				}										
			}
			
			names(tparamTable)<-names(paramdist)
			
			# put constraint test on tparamTable, before rbind
			for (k in 1:length(parameterConstraints))
			{
				constrintfill<-paramConstraint(label=parameterConstraints[[k]]$constraintLabel,
						data=tparamTable,
						columns=merge(paramLabel,data.frame(parameterConstraints[[k]]$paramList),by="label")$varName, #has to keep the order.
						relationship=parameterConstraints[[k]]$relationship)				
				
				
				#only keep the samples fulfill the constraints.
				tparamTable<-tparamTable[which(constrintfill==TRUE),]
			}
			
			
			iparamTable<-rbind(iparamTable,tparamTable)
			
			iparamTable<-unique(iparamTable)
			
#			print("nnnnnnnnnnnnnnnnnnnnnnn")
#			print(nrow(iparamTable))
			
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
	
	
	testPackList$paramTable<-paramTable
	testPackList$paramdist<-paramdist
	testPackList$paramweight<-paramweight
	testPackList$paramLabel<-paramLabel
	
	strategyList<-list()
	print("ParamTable generated")
	
	
	psize=nrow(paramTable)
	print(psize)
	
	
	
	instruments<-as.list(FinancialInstrument:::.instrument)
	getSymbols<-as.list(.getSymbols)
	blotter<-as.list(.blotter)
	
	#Pack all symbols downloaded in .GlobalEnv
	symbols<-names(.getSymbols)
	
	testPackListPRL<-foreach (i = 1:psize, .export=c('instruments',symbols,'getSymbols','blotter','tmp_strategy'),.verbose=TRUE) %dopar% 
			
			{
#				library(blotter)
#				require(FinancialInstrument)
				require(quantstrat)
				
				testPack<-list()
				
				#Pass environments needed.
                loadInstruments(instruments)
				.getSymbols<-as.environment(getSymbols)
#				CAN NOT BE HERE, OTHERWISE will have problem extract to outside .blooter.. #.blotter<-as.environment(blotter)
				
				#Unpack symbols to worker. change later.
				#seems need to go through assign, rather than just .export the names...
				
				for (sym in symbols) {
					assign(sym, eval(as.name(sym)), .GlobalEnv)
				}
				
				#Create a copy of strategy object, so not to lock up on the sameone.
				PLtmp_strategy<-tmp_strategy
				
				#Extract parameter from table and construct PLtmp_strategy.
				for (j in 1:ncol(paramTable)){
					
					tmp_arg<-parameterPool[[j]]$distribution[1] #Just get the list form with name
					#tmp_arg<-list(tmp_argName=paramTable[i,j])
					tmp_arg[[1]]<-paramTable[i,j]
					
					tmp_index<-parameterPool[[j]]$indexnum
					
					switch(parameterPool[[j]]$type,
							'indicator'={
								#merge.list uses another package. PLtmp_strategy$indicators[[tmp_index]]$arguments<-merge.list(targ1,tmp_arg)
								targ1<-PLtmp_strategy$indicators[[tmp_index]]$arguments
								
								pnamepos<-pmatch(names(targ1),names(tmp_arg),nomatch=0L)
								if( any(pnamepos>0)){
									#just change the argument value itself will do ?or still need add.indicator??
									PLtmp_strategy$indicators[[tmp_index]]$arguments[which(pnamepos>0)]<-tmp_arg[1]
								}
								else{
									PLtmp_strategy$indicators[[tmp_index]]$arguments<-append(targ1,tmp_arg)
									
								}
								#OR still need add.*??
								#pass_arg<-append(,tmp_arg)
								#PLtmp_strategy <- add.indicator(strategy = PLtmp_strategy,name=PLtmp_strategy$indicators[[tmp_index]]$name, arguments = pass_arg,indexnum=tmp_index)
							},
							'signal'={
								
								targ1<-PLtmp_strategy$signals[[tmp_index]]$arguments
								
								pnamepos<-pmatch(names(targ1),names(tmp_arg),nomatch=0L)
								if( any(pnamepos>0)){
									#just change the argument value itself will do ?or still need add.indicator??
									
									PLtmp_strategy$signals[[tmp_index]]$arguments[which(pnamepos>0)]<-tmp_arg[1]
								}
								else{
									PLtmp_strategy$signals[[tmp_index]]$arguments<-append(targ1,tmp_arg)
									
								}
								
#						pass_arg<-append(PLtmp_strategy$signal[[tmp_index]]$arguments,tmp_arg)
#						PLtmp_strategy <- add.signal(strategy = PLtmp_strategy,name=PLtmp_strategy$signal[[tmp_index]]$name,arguments = tmp_arg,indexnum=tmp_index)
								
							},
							'order'={
								targ1<-PLtmp_strategy$rules$order[[tmp_index]]$arguments
								
								pnamepos<-pmatch(names(targ1),names(tmp_arg),nomatch=0L)
								if( any(pnamepos>0)){
									#just change the argument value itself will do ?or still need add.indicator??
									PLtmp_strategy$rules$order[[tmp_index]]$arguments[which(pnamepos>0)]<-tmp_arg[1]
								}
								else{
									PLtmp_strategy$rules$order[[tmp_index]]$arguments<-append(targ1,tmp_arg)
									
								}
							},
							'enter'={
								targ1<-PLtmp_strategy$rules$enter[[tmp_index]]$arguments
								
								pnamepos<-pmatch(names(targ1),names(tmp_arg),nomatch=0L)
								if( any(pnamepos>0)){
									#just change the argument value itself will do ?or still need add.indicator??
									PLtmp_strategy$rules$enter[[tmp_index]]$arguments[which(pnamepos>0)]<-tmp_arg[1]
								}
								else{
									PLtmp_strategy$rules$enter[[tmp_index]]$arguments<-append(targ1,tmp_arg)
									
								}						
							},
							'exit'={
								targ1<-PLtmp_strategy$rules$exit[[tmp_index]]$arguments
								
								pnamepos<-pmatch(names(targ1),names(tmp_arg),nomatch=0L)
								if( any(pnamepos>0)){
									#just change the argument value itself will do ?or still need add.indicator??
									PLtmp_strategy$rules$exit[[tmp_index]]$arguments[which(pnamepos>0)]<-tmp_arg[1]
								}
								else{
									PLtmp_strategy$rules$exit[[tmp_index]]$arguments<-append(targ1,tmp_arg)
									
								}
							}
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
				
				print('Initial portf')
				
#				Decide not to remove the main obj from .blotter, incase of non-parallel run.
#				try(rm(list=paste("order_book",portfolios,sep='.'),pos=.strategy),silent=TRUE)
##				try(rm(paste("account",portfolio.st,sep='.'),paste("portfolio",portfolio.st,sep='.'),pos=.blotter),silent=TRUE)
#				try(rm(list=paste("account",portfolios,sep='.'),pos=.blotter))
#				try(rm(list=paste("portfolio",portfolios,sep='.'),pos=.blotter))
				
				try({initPortf(testPack$portfolio.st,symbols=stock.str, initDate=initDate)})
				try({initAcct(testPack$account.st,testPack$portfolio.st, initDate=initDate)})
				try({initOrders(portfolio=testPack$portfolio.st,initDate=initDate)})
				
# Apply strategy ######################################################################################
				print("Apply strategy...")
				
				try(rm("PLtmp_strategy",pos=.strategy),silent=TRUE)
				
				print(PLtmp_strategy$signals[[2]])
				
				assign("PLtmp_strategy1",PLtmp_strategy,envir=as.environment(.strategy))
				
				testPack$out<-try(applyStrategy(strategy=PLtmp_strategy , portfolios=testPack$portfolio.st ))
				testPack$strategy<-PLtmp_strategy
				
# 	Update portfolio ######################################################################################
				
				#out<-try(applyStrategy(strategy=stratBBands , portfolios=portfolios ))
				#		try({
				#					updatePortf(testPack$portfolio.st,Date=initDate)
				#					updateAcct(testPack$account.st,Date=initDate)
				#					updateOrders(portfolio=testPack$portfolio.st)
				#				})
				
				
#try(updatePortf(Portfolio=testPack$portfolio.st,Dates=paste('::',as.Date(Sys.time()),sep='')))
				updatePortf(Portfolio=testPack$portfolio.st,Dates=paste('::',as.Date(Sys.time()),sep=''))
				
				#no need to update account.
				#updateAcct(account.st,Dates=paste(startDate,endDate,sep="::")) 
				#updateEndEq(account.st,Dates=paste(startDate,endDate,sep="::"))
				#getEndEq(account.st,Sys.time())
				
				testPack$parameters<-paramTable[i,]
				
				testPack$stats<-tradeStats(Portfolios=testPack$portfolio.st)
				testPack$blotterl<-as.list(.blotter)
#				testPack$blotter<-as.environment(.blotter)
#				testPack$blotterr<-.blotter
		
				return(testPack)
				
			}	# Loop i
	
	
	for (k in 1: nrow(paramTable)){
		testPackListPRLStructure$statsTable<-rbind(testPackListPRLStructure$stats,cbind(testPackListPRL[[k]]$parameters,testPackListPRL[[k]]$stats))
		print(names(testPackListPRL[[k]]$blotterl))
		
		for(nn in 1:length(testPackListPRL[[k]]$blotterl)){
#			print(paste(names(testPackListPRL[[k]]$blotterl)[nn],'nnp',nn,sep='.'))
			assign(names(testPackListPRL[[k]]$blotterl[nn]),testPackListPRL[[k]]$blotterl[[nn]],envir=as.environment(.blotter))
		}
		names(testPackListPRL)[k]<-testPackListPRL[[k]]$portfolio.st
	}
	
	
	testPackListPRLStructure$eachRun<-testPackListPRL
	testPackListPRLStructure$paramTable<-paramTable
	testPackListPRLStructure$paramConstrainTable<-data.frame(parameterConstraints)
	
	testPackListPRLStructure$parameterDistribution<-parameterPool
	testPackListPRLStructure$parameterConstraints<-parameterConstraints
	
	return(testPackListPRLStructure)
	
}

#for(pname in names(as.list(ss$blotter))){
#	assign(paste(pname,'p',23,sep='.'),get(paste('ss$blotter$',pname,sep='')),envir=as.environment(.blotter))
#}


#' Internal function used in applyParameter function for process constraints on relationship between two parameter values. Basicly is the same as sigComparison function in signal.R written by Brian, with miner change.
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
paramConstraint <- function(label,data=mktdata, columns, relationship=c("gt","lt","eq","gte","lte")) {
	relationship=relationship[1] #only use the first one
#	print(columns)
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
		
		ret_sig$tname <- do.call( opr, list(data[,colNums[1]], data[,colNums[2]]))
		
	} else {
		stop("comparison of more than two columns not supported, see sigFormula")
	}
	names(ret_sig)<-label
	return(data.frame(ret_sig))
}


#' Function to construct parameter constraint object.
#' 
#' Function to construct parameter constraint object. The returned value will be one of the inputs to the applyParameter function.
#'  
#' @examples
#' #(For complete demo see parameterTestMACD.R)
#' #In a MACD strategy, we want to fast macd calcuated from less time periods (days)
#' #than slow macd signal:
#' \dontrun{  
#' x<-setParameterConstraint(constraintLabel='macdPC',
#' 		paramList=c('nFast','nSlow'),relationship='lt')
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
#' @export
setParameterConstraint<-function(paramConstraintObj=list(),constraintLabel,paramList,relationship)
{
	if(!hasArg(paramConstraintObj)||!exists(as.character(substitute(paramConstraintObj)))){
		paramConstraintObj<-list()
		print('Parameter constraint object initialized...')		
	}
	else{
		if (!is.list(paramConstraintObj)|length(paramConstraintObj)!=1) stop("Parameter constrain object must be passed as a named list of length 1")
		
	}
	
	if (missing(constraintLabel)) {constraintLabel<-paste("parameterConstraint",length(paramConstraintObj)+1)}
	tmp_PC<-list()
	tmp_PC$constraintLabel<-constraintLabel
	#names(paramList)<-"label"
	tmp_PC$paramList$label<-paramList
	tmp_PC$relationship<-relationship	
	
	paramConstraintObj[[paste(constraintLabel)]]<-tmp_PC
	return(paramConstraintObj)
}

