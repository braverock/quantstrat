###############################################################################
# R (http://r-project.org/) Quantitative Strategy Model Framework
#
# Copyright (c) 2009-2022
###############################################################################
#
# Authors: Brian G. Peterson
#
###############################################################################
#
#   Tools for Federov designs for backtest parameter experiments
#
###############################################################################


#' construct a full Federov design for a strategy parameter set 
#'
#' @param strategy an object of type `strategy` that contains a parameter set to construct a Federov design for
#' @param paramset.label label describing the paramset to use in the strategy object
#' @param ... any other passthrough parameters
#' @param printd if TRUE, print the design summary
#' @param returnlist if TRUE, return the list object describing the Federov design, else return just the `param.combos`
#' @param approximate if FALSE, use an exact design, will be slower but more accurate than if TRUE
#' @param center if TRUE, the default, center the parameters around a center value, see Details
#'
#' @return
#' @export
#'
#' @seealso optFederov
Federov.paramset <- function(strategy, paramset.label, ..., printd=FALSE, returnlist=FALSE, approximate=TRUE, center=TRUE){
  
  if(!requireNamespace('AlgDesign', quietly=TRUE)) stop("The 'AlgDesign' package is required to use this function")
  
  strategy <- must.be.strategy(strategy)
  must.be.paramset(strategy, paramset.label)
  
  distributions <- strategy$paramsets[[paramset.label]]$distributions
  constraints <- strategy$paramsets[[paramset.label]]$constraints
  param.combos <- expand.distributions(distributions)
  param.combos <- apply.constraints(constraints, distributions, param.combos)
  rownames(param.combos) <- NULL  # reset rownames
  
  param.design <- optFederov(~quad(.),data=param.combos,approximate=approximate, center=center)
  
  if(printd){
    print(param.design)
  }
  
  if(returnlist){
    return(param.design)
  } else {
    return(param.design$design[,-1])
  }
}

#' test a single parameter set to determine if it passes constraints
#'
#' @param param.vec a vector (named or unamed) containing a trial parameter set
#' @param ... any other passthrough parameters
#'
#' @return boolean -  TRUE if param.vec passes constraints, FALSE otherwise
#' @export
#'
#' @seealso Federov.paramset MonteCarlo.paramset apply.constraints
Federov.constraints <- function(param.vec, ...){
  # AlgDesign, unfortunately, uses un-named parameter vectors.  
  # We need the names to check the constraints.
  s <- get('strategy')  # might need to set pos=-2 or something here 
  s <- must.be.strategy(s)
  
  paramset.label <- try(get('paramset.label'))
  if(class(paramset.label)=='try-error') paramset.label <- first(names(s$paramsets))
  distributions <- s$paramsets[[paramset.label]]$distributions
  constraints <- s$paramsets[[paramset.label]]$constraints
  
  param.combo<-param.vec
  if(is.null(names(param.vec))){
    names(param.combo)<-names(s$paramsets[[paramset.label]]$distributions)
  }
  param.combo <- t(as.data.frame(param.combos))
  rownames(param.combo)<-NULL
  param.combo <- as.data.frame(param.combo)
  
  param.combos <- apply.constraints(constraints, distributions, param.combo)
  if(!nrow(param.combo)) return(FALSE) # failed constraints
  else return(TRUE) #passed constraints
}
