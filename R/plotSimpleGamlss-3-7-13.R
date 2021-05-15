# ported from https://github.com/cran/gamlss.util/blob/master/R/plotSimpleGamlss-3-7-13.R
#
# the function to plot y and x and fitted distribution for y for 
# a grid of x
# created 13-2-13 MD
#-------------------------------------------------------------------------------
# in this version the data argument is compulsory 
# TO DO 
# 
plotSimpleGamlss <- function(y, 
                             x,       # has to be in the data for prediction
                             model = NULL, # if a fitted model is available 
                             formula = NULL, # if fitted model not available use the gamlss formula
                             data = NULL, # this is colpulsory 
                             family = NULL, # it need it if no model is given
                             val = NULL, # controls howfar the horisontal plots should go
                             N = 1000, # how many poits to evaluate for the distribution curves 
                             x.val = quantile(x), # which values of x
                             ylim = c(min(y), max(y)),
                             xlim = c(min(x), max(x)), 
                             #                         ylab = paste(deparse(substitute(y))),
                             #                         xlab = paste(deparse(substitute(x))),
                             ...)
{
  args <- list(...)
  if (is.null(data)) stop("the data argument is required")  
  ## has to change
  ## if (!is.null(data)) {attach(data); on.exit(detach(data))}
  ylab <- deparse(substitute(y))
  xlab <- deparse(substitute(x))
  y <- if (!is.null(data)) get(deparse(substitute(y)), envir=as.environment(data)) else y
  x <- if (!is.null(data)) get(deparse(substitute(x)), envir=as.environment(data)) else x
  if (!is.null(model))
  {
    if (!is.gamlss(model)) stop("the model should be an gamlss object") 
    #   family <- model$family[1]
    #    fname  <- as.character(substitute(family))
    family <-  if(is.null(model$call$family)) as.gamlss.family(NO) 
    else as.gamlss.family(model$call$family)
    fname <- model$family[1]
  }
  else 
  {
    family <-  if(is.null(family)) as.gamlss.family(NO) 
    else as.gamlss.family(family)
    fname <- family$family[1] 
  }
  distype <- family$type
  nopar <- family$nopar
  rfun <- paste("r",fname,sep="")
  dfun <- paste("d",fname,sep="")
  rpdf <- eval(parse(text=rfun))
  pdf <- eval(parse(text=dfun))
  xv <- x.val #xv <- c(0.1,5,10,15,20)
  ran <- xlim
  with(data, scattersmooth(x,y, ylim=ylim, xlim=xlim, xlab=xlab, ylab=ylab, ...))
  #get formula for gamlss
  # whether to fit the model if not given
  if (!is.null(model))
  {
    lines(fitted(model)[order(x)]~x[order(x)],  type="l", col="black") 
    NewData <- eval(parse(text=paste(paste("data.frame(", paste(xlab,"=x.val", sep="")),")")))
    p <- predictAll(model, , newdata=NewData,  type="response", data=data)
  }
  else
  {
    form <- if (is.null(formula)) y~x else formula 
    m1 <- gamlss(formula=form, data=data, trace=FALSE, family=family, ...)
    if (!is.null(data) ) m1$call$data <- substitute(data)
    lines(fitted(m1)[order(x)]~x[order(x)],  type="l", col="black")
    NewData <- eval(parse(text=paste(paste("data.frame(", paste(xlab,"=x.val", sep="")),")")))
    p <- predictAll(m1, , newdata=NewData,  type="response", data=data)
  }  
  
  
  if (distype=="Continuous")
  {
    for (i in 1:length(xv))
    {
      xx <- rep(xv[i],N)
      switch(nopar,
             {
               yy <- rpdf(N, mu=p$mu[i])
               DD <- pdf(yy, mu=p$mu[i])
             },  
             {
               yy <- rpdf(N, mu=p$mu[i], sigma=p$sigma[i])
               DD <- pdf(yy, mu=p$mu[i], sigma=p$sigma[i])},
             {
               yy <- rpdf(N, mu=p$mu[i], sigma=p$sigma[i], nu=p$nu[i])
               DD <- pdf(yy, mu=p$mu[i], sigma=p$sigma[i], nu=p$nu[i])
             },
             {
               yy <- rpdf(N, mu=p$mu[i], sigma=p$sigma[i], nu=p$nu[i], tau=p$tau[i])
               DD <- pdf(yy, mu=p$mu[i], sigma=p$sigma[i], nu=p$nu[i], tau=p$tau[i])  
             })
      yyy  <- seq(ylim[1],ylim[2],length=N) 
      if (is.null(val) ) val <- (ran[2]-ran[1])*.25/max(DD)
      xxDD <- xx-val*DD
      lines(xx,yyy) 
      lines(xxDD[order(yy)],yy[order(yy)], col="black",  lwd=2)
      #points(xx,yy,cex=0.45, pch="+", col="blue4")
    }  
  }
  if (distype=="Discrete")
  {
    for (i in 1:length(xv))
    {
      xval <- rep(xv[i],N)
      yy  <-  0:(N-1) 
      switch(nopar,
             {
               DD <- pdf(yy, mu=p$mu[i])
             },  
             {
               DD <- pdf(yy, mu=p$mu[i], sigma=p$sigma[i])},
             {
               DD <- pdf(yy, mu=p$mu[i], sigma=p$sigma[i], nu=p$nu[i])
             },
             {
               DD <- pdf(yy, mu=p$mu[i], sigma=p$sigma[i], nu=p$nu[i], tau=p$tau[i])  
             })
      if (is.null(val) ) val <-5 
      xxDD <- xval-val*DD
      lines(xval,yy) 
      lines(yy~xxDD, type="s", lwd=2)
    }   
  }
  
}
