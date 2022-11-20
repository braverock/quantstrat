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


#' construct a full Federov design or a constrained Monte Carlo Federov design for a strategy parameter set 
#'
#' This function provides a wrapper for constructing Federov designs for
#' parameters by using the parameter set logic of quantstrat and the experiment
#' design functions from the `AlgDesign` package.  It will construct a full or
#' approximate Federov design via `method="Federov"` (not case sensitive) using
#' the `optFederov` function and will construct a constrained Monte Carlo
#' Federov design via `method="MonteCarlo"` using the `optMonteCarlo` function.
#' 
#' It is important to note from the beginning that while `method="Federov"` is
#' the default, that the closed form Federov design is not suitable when there
#' are constraints.  We will warn the user if there are constraints in the 
#' strategy specification and a full Federov design is chosen.  The function
#' also supports the option `constrain=TRUE` that will apply constraints to 
#' the full Federov design.  For some strategies with many constraints, this 
#' may result in a significantly unbalanced set.  This should still be OK as a 
#' starting point for optimization, but may hamper some statistical inference
#' about parameter interactions and lower the overall power of the design if
#' the unbalanced nature is severe.
#'  
#' For now, for constrained Monte Carlo Federov designs, we are not supporting 
#' factor or mixture models, though these designs are supported by the `AlgDesign`
#' package.  Patches welcome, or even just discussion of solid use cases.
#'  
#' The user may use \dots to pass through any additional parameters for use by 
#' `optFederov` or `optMonteCarlo`. For example, `nLevels`, `nCand`, `nRepeats`
#' may be candidates for finer grained control.  see `?AlgDesign::optMonteCarlo`
#' for details.  We have tried to make reasonable decisions based on the data
#' contained in the strategy object. `nLevels` defaults to the minimum of 5 
#' (though 3 is a reasonable and smaller choice) or the number of levels 
#' contained in the paramset for that variable.
#' 
#' @param strategy.st astring describing the name of an object of type `strategy` that contains a parameter set to construct a Federov design for
#' @param paramset.label label describing the paramset to use in the strategy object
#' @param ... any other passthrough parameters
#' @param method one of "Federov" or "MonteCarlo", see Details
#' @param printd if TRUE, print the design summary
#' @param returnlist if TRUE, return the list object describing the Federov design, else return just the `param.combos`
#' @param approximate if FALSE, use an exact design, will be slower but more accurate than if TRUE
#' @param center if TRUE, the default, center the parameters around a center value, see Details
#' @param constrain if TRUE, `apply.constraints` for the full Federov design, see Details
#' 
#' @return
#' @export
#'
#' @seealso optFederov
Federov.paramset <- function(strategy.st, 
                             paramset.label, 
                             ...,  
                             method='Federov',
                             printd=FALSE, 
                             returnlist=FALSE, 
                             approximate=TRUE, 
                             center=TRUE,
                             constrain=FALSE){
  
  if(!requireNamespace('AlgDesign', quietly=TRUE)) stop("The 'AlgDesign' package is required to use this function")
  else{
    require(AlgDesign,quietly = TRUE)
  }
  strategy <- must.be.strategy(strategy.st)
  must.be.paramset(strategy, paramset.label)
  
  distributions <- strategy$paramsets[[paramset.label]]$distributions
  constraints <- strategy$paramsets[[paramset.label]]$constraints
  
  if(length(constraints)&&method=='Federov'&&!isTRUE(constrain)) warning('The full Federov method will not honor constraints, consider using `method="MonteCarlo"` or `constrain=TRUE`.') 
  
  switch(method,
         federov =,
         Federov =
         {
           param.combos <- expand.distributions(distributions)
           param.combos <- apply.constraints(constraints, distributions, param.combos)
           rownames(param.combos) <- NULL  # reset rownames
           
           param.design <- optFederov(~quad(.),data=param.combos,approximate=approximate, center=center)
           
           if(length(constraints)>1&&isTRUE(constrain)) {
             param.design$design <- apply.constraints(constraints=constraints, distributions=distributions, param.combos=param.design$design[,-1])
           }
         },
         montecarlo =,
         MonteCarlo =
         {
           # TODO: construct the 'data' input data frame from the strategy object
           # optMonteCarlo has a required 'data' parameter which is a data frame with columns:
           # var:    The names of the variables.
           # low:    The lower limit of the range for each variable. Ignored for mixtures.
           # high:   The upper limit of the range for each variable. Ignored for mixtures.
           # center: The centering value for each variable. Ignored for mixtures.
           # nLevels:The number of levels for each variable. Ignored for mixture variables.
           # round:  The number of decimal digits for the levels. 
           #         The levels are randomly and uniformly chosen between low 
           #         and high, and this parameter controls the number of 
           #         trailing digits. The max value for mixture variables in 
           #         this vector is used to round all mixture variables.
           # factor: TRUE, FALSE depending on whether or not the variable is a 
           #         factor. 
           #         Note: other columns will be reset to conform to a nLevels factor.
           # mix:    TRUE if the variable is a mixture variable. This column may be omitted if there are no mixture variables.
           # 
           # The challenge with this design is that we need to keep everything 
           # lined up with how it is structured in the paramsets slot of the 
           # strategy object.
           
           df<-NULL
           
           if(!hasArg('round')) r <- 0
           else r<-round
           
           if(hasArg(nRepeats)) nRepeats<-nRepeats
           else nRepeats <- length(strategy$paramsets[[paramset.label]]$distributions)
           
           if(hasArg(nTrials)) nTrials<-nTrials
           else nTrials <- length(strategy$paramsets[[paramset.label]]$distributions)+10
           
           # TODO: grab some of the match.arg logic we use elsewhere to clean up passthroughs
               
           for(i in 1:length(strategy$paramsets[[paramset.label]]$distributions)){ 
             
             if(hasArg('nLevels')) nLevels<-nLevels
             else nLevels <- min(5,length(strategy$paramsets[[paramset.label]]$distributions[[i]]$variable.dist))
             
             rg <- range(strategy$paramsets[[paramset.label]]$distributions[[i]]$variable.dist)
             
             d <- data.frame(var=names(strategy$paramsets[[paramset.label]]$distributions[i]), 
                             low=min(strategy$paramsets[[paramset.label]]$distributions[[i]]$variable.dist),
                             high=max(strategy$paramsets[[paramset.label]]$distributions[[i]]$variable.dist),
                             center=(rg[1]+rg[2])/2,
                             nLevels=nLevels,
                             round=r,
                             factor=FALSE,
                             mix=FALSE 
             )
             
             if(is.null(df)) df<-d
             else df <- rbind(df,d)
           }
           
           param.design <- optMonteCarlo(~quad(.),
                                         data=df,
                                         constraints=Federov.constraints,
                                         args=printd,
                                         nRepeats=nRepeats,
                                        ) 
         }   
        )
  
  if(printd){
    print(param.design)
  }
  
  if(returnlist){
    return(param.design)
  } else {
    rownames(param.design$design) <- NULL # number as string rownames get in the way
    
    if(method=='Federov'){
      if(length(constraints)>1&&isTRUE(constrain)) return(param.design$design)
      else return(param.design$design[,-1]) # 'Proportion' in the first column!
    } else {
      return(param.design$design)
    } # end method return block
  } # end returnlist block
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
  s <- dynGet('strategy.st',inherits=TRUE)  # get was flaky here, pos seemed variable, use dynGet instead
  s <- must.be.strategy(s)
  
  paramset.label <- try(dynGet('paramset.label',inherits=TRUE))
  if(class(paramset.label)=='try-error') paramset.label <- first(names(s$paramsets))
  distributions <- s$paramsets[[paramset.label]]$distributions
  constraints <- s$paramsets[[paramset.label]]$constraints
  
  param.combo<-param.vec
  if(is.null(names(param.vec))){
    names(param.combo)<-names(s$paramsets[[paramset.label]]$distributions)
  }
  param.combo <- t(as.data.frame(param.combo))
  rownames(param.combo)<-NULL
  param.combo <- as.data.frame(param.combo)
  
  param.combos <- apply.constraints(constraints, distributions, param.combo)
  if(!nrow(param.combo)) return(FALSE) # failed constraints
  else return(TRUE) #passed constraints
}
