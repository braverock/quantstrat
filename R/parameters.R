#' add parameters to strategy objects: ALPHA CODE USE WITH CARE 
#' @param strategy 
#' @param type 
#' @param add.to.name 
#' @param method 
#' @param arguments 
#' @param label 
#' @param ... 
#' @param store 
#' @export
add.parameter <- 
		function (strategy, 
				type = c('indicator','signal'), 
				add.to.name,
				method = c('lookup','lookup.range','calc'), 
				arguments = NULL, 
				label = NULL,
				...,
				store=FALSE) 
{
	if(!is.strategy(strategy)) stop("You must pass in a strategy object to manipulate")
	# perhaps I should add parameters and parameter.args as arguments to the constructors...
	
	tmp.param<-list()
	
	type=type[1] #this should probably come out eventually
	
	method = method[1] #only use the first if the user didn't specify, or over-specified
	
	if(is.null(label)) {
		label<-method
	}
	tmp.param$label <- label
	tmp.param$method <- method
	tmp.param$call <- match.call()
	tmp.param$arguments <- arguments
	class(tmp.param)<-'quantstrat.parameter'
	
	switch(type,
			indicator = {type='indicators'},
			signal = {type='signals'},
			rule = {type='rules'}) #NOTE rules not supported yet, since they need a rule type too
	
	# need to think about how to create a 'parameters' list, and whether 
	# it should be at the strategy level or lower down, on the individual 
	# signal/indicator/rule
	
	if(!is.list(strategy[[type]][[add.to.name]]$parameters)){
		strategy[[type]][[add.to.name]]$parameters <- list()
	}
	strategy[[type]][[add.to.name]][['parameters']][[method]] <- tmp.param
	
	if (store) assign(strategy$name,strategy,envir=as.environment(.strategy))
	else return(strategy)
}


#' add parameters to strategy objects: ALPHA CODE USE WITH CARE 
#' @param strategy 
#' @param symbol 
#' @param type 
#' @param name 
#' @param parameter 
#' @param ... 
paramLookup <- function(strategy, symbol , type, name, parameter, ...) {
	# should take in a strategy and parameter object, and return an argument list for 'symbol'
	#as.pairlist(paramTable[,symbol]
	paramTable<-get(paste(strategy,type,name,'table',pos=.strategy))
	as.pairlist(paramTable[,symbol])
}

#' add parameters to strategy objects: ALPHA CODE USE WITH CARE 
#' @param strategy 
#' @param type 
#' @param name 
#' @param paramTable 
#' @export
add.paramLookupTable <- function(strategy, type, name, paramTable){
	assign(paste(strategy,type,name,'table',pos=.strategy),paramTable)
}

#' get parameterized arguments list out of the strategy environment
#' @param strategy 
#' @param symbol 
#' @param type 
#' @param name 
getParams <- function (strategy, symbol, type, name)
{
	
	params <- strategy[[type]][[name]]$parameters
	param.ret<-list()
	for (param in params) {
		switch(param$method,
				lookup = {param.ret<-c(param.ret,paramLookup(strategy,symbol,parameter=param))},
				lookup.range = {},
				calc = {},
				{warning("parameter method",param$method,'not recognized for',type,name); next()}
		)
	}
	# return an arguments list back to the 'apply*' fn
	return(param.ret)
}


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
# Author: CCD
###############################################################################

#retreave the needed parameters and existing values after add*

#' Extract the parameter structure from a strategy object.
#' @param strategy The name of the strategy
#' @param staticSwitch 
#' @author CCD
#' @export
getParameterTable<-function (strategy,staticSwitch){
	
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
		tmp_paramTable[[nofi]]<-formals(fun)		
		tmp_paramTable[[nofi]]$paramType<-'indicator'
		tmp_paramTable[[nofi]]$paramEnabled<-indicator$enabled
		tmp_paramTable[[nofi]]$indexnum=indexnum
	}
	
	indexnum=0
	for (signal in strategy$signals ){ 
		
		
		nofi=nofi+1
		indexnum=indexnum+1
		
		fun<-match.fun(signal$name) 
		tmp_paramTable[[nofi]]<-formals(fun)
		tmp_paramTable[[nofi]]$paramType<-'signal'
		tmp_paramTable[[nofi]]$paramEnabled<-signal$enabled
		tmp_paramTable[[nofi]]$indexnum=indexnum
	}
	
	for (rule in strategy$rules ){
		indexnum=0
		for (trule in rule){
			
			
			
			nofi=nofi+1
			indexnum=indexnum+1
			
			fun<-match.fun(trule$name) 
			tmp_paramTable[[nofi]]<-formals(fun)
			tmp_paramTable[[nofi]]$paramType<-trule$type
			tmp_paramTable[[nofi]]$paramEnabled<-trule$enabled
			tmp_paramTable[[nofi]]$indexnum=indexnum
		}
		
	}
	
	tmp_paramTable$strategyName<-strategy$name
	return(tmp_paramTable)
	
}


getParameterInfo<-function(paramStructure){
	paramInfo<-list()
	for(paraLine in paramStructure){
		paraInfo[[1]]<-paraLine$paramType
	}
}
getParameterMatrix<-function(paraStructure){
	
}

#' 
#' @param paramDist the object name that store the parameter list
#' @param type indicator/signal/
#' @param indexnum 
#' @param distribution 
#' @param weight 
#' @param psindex 
#' @author Yu Chen
#' @export
setParameterDistribution<-function(paramDist=NULL,type=NULL,indexnum=0,distribution=NULL,weight,label,psindex=NULL){#All is needed, set to illegal values
	
	if(!hasArg(paramDist)){
		paramDist<-list()
	}
	else{
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
	}
	return(paramDist)
}



#' Generate parameter sets and apply each parameter set to an existing strategy. 
#' @param strategy name of the strategy to apply paramters to.
#' @param portfolios name of the portfolio to apply to.
#' @param parameterPool a paramter set object include all the parameter legal values and distribution/weights.
#' @param method string 'expand' or 'random' how to generate samples of parameters.
#' @param sampleSize 
#' @author Yu Chen
#' @export
applyParameter<-function(strategy,portfolios,parameterPool,parameterConstrains,method,sampleSize){
	#need to create combination of distribution values in each slot of the parameterPool
	tmp_strategy<-strategy
	
	testPackList<-list()
	testPackList$stats<-NULL
	testPack<-list()
	
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
			
			#TODO put constraint test on tparamTable, before rbind
			for (k in 1:length(parameterConstrains))
			{
				
				#tt<-ParamConstrain(label="pc",data=tparamTable,columns=c("Param.indicator.1.sd","Param.indicator.1.n"),relationship="gt")
			
				constrintfill<-paramConstraint(label=parameterConstrains[[k]]$constraintLabel,
						data=tparamTable,
						columns=merge(paramLabel,data.frame(parameterConstrains[[k]]$paramList),by="label")$varName, #has to keep the order.
						relationship=parameterConstrains[[k]]$relationship)				
				
					
				#only keep the samples fulfill the constraints.
				tparamTable<-tparamTable[which(constrintfill==TRUE),]
			}
		
			
			iparamTable<-rbind(iparamTable,tparamTable)
			
			iparamTable<-unique(iparamTable)
			
			print("nnnnnnnnnnnnnnnnnnnnnnn")
			print(nrow(iparamTable))
			
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
	
	for (i in 1:nrow(paramTable)){
		print(paramTable[i,])
		for (j in 1:ncol(paramTable)){
			
			tmp_arg<-parameterPool[[j]]$distribution[1] #Just get the list form with name
			#tmp_arg<-list(tmp_argName=paramTable[i,j])
			tmp_arg[[1]]<-paramTable[i,j]
			
			
			#print(tmp_arg)
			
			tmp_index<-parameterPool[[j]]$indexnum
			
			
			
			switch(parameterPool[[j]]$type,
					'indicator'={
						#merge.list uses another package. tmp_strategy$indicators[[tmp_index]]$arguments<-merge.list(targ1,tmp_arg)
						targ1<-tmp_strategy$indicators[[tmp_index]]$arguments
						
						pnamepos<-pmatch(names(targ1),names(tmp_arg),nomatch=0L)
						if( any(pnamepos>0)){
							#just change the argument value itself will do ?or still need add.indicator??
							tmp_strategy$indicators[[tmp_index]]$arguments[which(pnamepos>0)]<-tmp_arg[1]
						}
						else{
							tmp_strategy$indicators[[tmp_index]]$arguments<-append(targ1,tmp_arg)
							
						}
						#OR still need add.*??
						#pass_arg<-append(,tmp_arg)
						#tmp_strategy <- add.indicator(strategy = tmp_strategy,name=tmp_strategy$indicators[[tmp_index]]$name, arguments = pass_arg,indexnum=tmp_index)
					},
					'signal'={
						
						targ1<-tmp_strategy$signals[[tmp_index]]$arguments
						
						pnamepos<-pmatch(names(targ1),names(tmp_arg),nomatch=0L)
						if( any(pnamepos>0)){
							#just change the argument value itself will do ?or still need add.indicator??
										
							tmp_strategy$signals[[tmp_index]]$arguments[which(pnamepos>0)]<-tmp_arg[1]
						}
						else{
							tmp_strategy$signals[[tmp_index]]$arguments<-append(targ1,tmp_arg)
							
						}
						
#						pass_arg<-append(tmp_strategy$signal[[tmp_index]]$arguments,tmp_arg)
#						tmp_strategy <- add.signal(strategy = tmp_strategy,name=tmp_strategy$signal[[tmp_index]]$name,arguments = tmp_arg,indexnum=tmp_index)
						
					},
					'order'={
						targ1<-tmp_strategy$rules$order[[tmp_index]]$arguments
						
						pnamepos<-pmatch(names(targ1),names(tmp_arg),nomatch=0L)
						if( any(pnamepos>0)){
							#just change the argument value itself will do ?or still need add.indicator??
							tmp_strategy$rules$order[[tmp_index]]$arguments[which(pnamepos>0)]<-tmp_arg[1]
						}
						else{
							tmp_strategy$rules$order[[tmp_index]]$arguments<-append(targ1,tmp_arg)
							
						}
					},
					'enter'={
						targ1<-tmp_strategy$rules$enter[[tmp_index]]$arguments
						
						pnamepos<-pmatch(names(targ1),names(tmp_arg),nomatch=0L)
						if( any(pnamepos>0)){
							#just change the argument value itself will do ?or still need add.indicator??
							tmp_strategy$rules$enter[[tmp_index]]$arguments[which(pnamepos>0)]<-tmp_arg[1]
						}
						else{
							tmp_strategy$rules$enter[[tmp_index]]$arguments<-append(targ1,tmp_arg)
							
						}						
					},
					'exit'={
						targ1<-tmp_strategy$rules$exit[[tmp_index]]$arguments
						
						pnamepos<-pmatch(names(targ1),names(tmp_arg),nomatch=0L)
						if( any(pnamepos>0)){
							#just change the argument value itself will do ?or still need add.indicator??
							tmp_strategy$rules$exit[[tmp_index]]$arguments[which(pnamepos>0)]<-tmp_arg[1]
						}
						else{
							tmp_strategy$rules$exit[[tmp_index]]$arguments<-append(targ1,tmp_arg)
							
						}
					}
			)
		} #loop j
		
#Initial portfolio for each test		
		#######################################################################################
		
#TODO will move out later, as parameter or put in a obj of TestPack.
		initDate='2010-12-31'
		initEq=1000000
		
		testPack$portfolio.st=paste(portfolios,'t',i,sep='.')
		testPack$account.st=paste(portfolios,'t',i,sep='.')
#		
		
		rmpstr=paste('portfolio',testPack$portfolio.st,sep=".")
		rmastr=paste('account',testPack$account.st,sep=".")
		
		try(rm(list = rmpstr, pos = .blotter),silent=FALSE)
		try(rm(list = rmastr, pos = .blotter),silent=FALSE)
		try(rm(list=paste("order_book",testPack$account.st,sep="."),pos=.strategy),silent=FALSE)
		
		
		try({
					initPortf(testPack$portfolio.st,symbols=stock.str, initDate=initDate)
					initAcct(testPack$account.st,testPack$portfolio.st, initDate=initDate)
					initOrders(portfolio=testPack$portfolio.st,initDate=initDate)
				})
		
# Apply strategy ######################################################################################
		print("Apply strategy...")
		
		try(rm("tmp_strategy",pos=.strategy),silent=TRUE)
		
		print(tmp_strategy$signals[[2]])
		
		assign("tmp_strategy1",tmp_strategy,envir=as.environment(.strategy))
		
		testPack$out<-try(applyStrategy(strategy=tmp_strategy , portfolios=testPack$portfolio.st ))
		testPack$strategy<-tmp_strategy
		
# 	Update portfolio ######################################################################################
		
#out<-try(applyStrategy(strategy=stratBBands , portfolios=portfolios ))
#		try({
#					updatePortf(testPack$portfolio.st,Date=initDate)
#					updateAcct(testPack$account.st,Date=initDate)
#					updateOrders(portfolio=testPack$portfolio.st)
#				})
		
		
		updatePortf(Portfolio=testPack$portfolio.st,Dates=paste('::',as.Date(Sys.time()),sep=''))
#? what to do with account?
#updateAcct(account.st,Dates=paste(startDate,endDate,sep="::")) 
#updateEndEq(account.st,Dates=paste(startDate,endDate,sep="::"))
#getEndEq(account.st,Sys.time())
		
		testPack$parameters<-paramTable[i,]
		
		testPack$stats<-tradeStats(Portfolios=testPack$portfolio.st)
		
		testPackList$stats<-rbind(testPackList$stats,cbind(testPack$parameters,testPack$stats))
		
		
# replaced -- testPackList[[i]]<-testPack
		testPackList[[paste(testPack$portfolio.st)]]<-testPack
		
		
	}	# Loop i
	
	
	return(testPackList)
	
}




#' Basicly is the same as sigComparison function in signal.R wrote by Brian, with miner change.
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
#' @export
paramConstraint <- function(label,data=mktdata, columns, relationship=c("gt","lt","eq","gte","lte")) {
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
		
		ret_sig$tname <- do.call( opr, list(data[,colNums[1]], data[,colNums[2]]))
		
	} else {
		stop("comparison of more than two columns not supported, see sigFormula")
	}
	names(ret_sig)<-label
	return(data.frame(ret_sig))
}


#' construct parameter constraint object
#' @param constrainlabel 
#' @param paramList 
#' @param relationship 
#' @author CCD
#' @export
setParameterConstraint<-function(paramConstraintObj=list(),constraintLabel,paramList,relationship)
{
	if(!hasArg(paramConstraintObj)){
		paramConstraintObj<-list()
		
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

