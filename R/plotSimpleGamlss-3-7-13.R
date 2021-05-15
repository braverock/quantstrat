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

#library(colorspace) is take off so it can go to gamlss
scattersmooth <- function(x, y, 
                          nbin = 100, 
                          lambda = 1, 
                          ndot = 500, 
                          csize = 0.3, 
                          ticks = TRUE,
                          xlim = c(min(x), max(x)), 
                          ylim = c(min(y), max(y)), 
                          show = TRUE,
                          save = FALSE,
                          data = NULL,
                          xlab = NULL,
                          ylab = NULL,
                          cols = heat.colors(10:200),
                          col.points = "blue",
                          ...)
{
  #-------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------
  smooth2d = function (H, lambda) {
    # 2-D exponential smoother
    if (length(lambda) == 1) lambda = c(lambda, lambda)
    # Build penalty matrices
    m <- nrow(H)
    n <- ncol(H)
    E1 <- diag(m)
    E2 <- diag(n)
    D1x <- diff(E1)
    D1y <- diff(E2)
    D2x <- diff(D1x)
    D2y <- diff(D1y)    
    Qx <- E1 + lambda[1]^2 * t(D2x) %*% D2x + 2 * lambda[1] * t(D1x) %*% D1x
    Qy <- E2 + lambda[2]^2 * t(D2y) %*% D2y + 2 * lambda[2] * t(D1y) %*% D1y
    # Smooth
    H <- t(solve(Qy, t(solve(Qx, H))))
    return(H)
  }
  #-------------------------------------------------------------------------------
  fillhist <- function(xb, yb, nb)
  {
    H <- matrix(rep(0, prod(nb)), nb[1], nb[2])
    for (i in 1:length(xb))
    {
      H[xb[i],yb[i]] <- H[xb[i],yb[i]] + 1
    }   
    H
  }
  #-------------------------------------------------------------------------------
  # main function starts here
  # check correct input
  xlab <- if (is.null(xlab))  deparse(substitute(x)) else xlab
  ylab <- if (is.null(ylab))  deparse(substitute(y)) else ylab
  y <- if (!is.null(data)) get(deparse(substitute(y)), envir=as.environment(data)) else y
  x <- if (!is.null(data)) get(deparse(substitute(x)), envir=as.environment(data)) else x
  # ## --------------
  #     if (!is.null(data)) 
  #            {
  #            YYY <- paste(paste(deparse(substitute(data)),"$", sep=""),deparse(substitute(y)), sep="" )
  #              y <- eval(parse(text=YYY))
  # #            XXX <- paste(paste(deparse(substitute(data)),"$", sep=""),deparse(substitute(x)), sep="" )
  #              x <- eval(parse(text=XXX))
  #            }
  if (length(x) != length(y))
    stop("lengths of x and y do not match")
  if ( (length(x) < 2) | (length(y) < 2) )
    stop("x and y should be vectors")
  if ( !is.numeric(x) | !is.numeric(y) )
    stop("x and y should contain numeric values")
  if ( all(!is.numeric(lambda)) | all(lambda < 0) | (length(lambda) > 2) )
    stop("lambda should be numeric and positive")
  if ( length(lambda) == 1 )
    lambda <- c(lambda, lambda)
  if (all(!is.numeric(nbin)) | all(nbin < 1) | (length(nbin) > 2) )
    stop("nbin should be a strictly positive integer")
  if ( length(nbin) == 1 )
    nbin <- c(nbin, nbin)
  if (!is.numeric(ndot) | (ndot < 0) | (length(ndot) > 1) )
    stop("ndot should be a non-negative integer")
  ndot <- floor(ndot)
  m <- length(x)
  # Put the x-values into bins
  xmin <- xlim[1]
  xmax <- xlim[2]
  dx <- (xmax - xmin) / (nbin[1] - 1)
  xbin <- floor(1 + (x - xmin) / dx)
  xgrid <- xmin + (1:nbin[1] - 0.5) * dx
  # Put the y-values into bins
  ymin <- ylim[1]
  ymax <- ylim[2]
  dy <- (ymax - ymin) / (nbin[2]-1)
  ybin <- floor(1 + (y - ymin) / dy)
  ygrid <- ymin + (1:nbin[2] - 0.5) * dy
  # Select point within boundaries
  sel <- 1 <= xbin & xbin <= nbin[1] & 1 <= ybin & ybin <= nbin[2]
  xbin <- xbin[sel]
  ybin <- ybin[sel]
  nmiss <- length(x) - sum(sel)
  # Create and smooth the unsmoothed histogram
  Hraw <- fillhist(xbin, ybin, nbin)
  Hsmooth <- smooth2d(Hraw, lambda) 
  if (ticks == FALSE) par(xaxt = 'n', yaxt = 'n')
  if (show) 
  {
    # cols =  #gray(0:n.col/n.col) 
    #heat_hcl(100:200)
    #          heat.colors(10:200)
    image(x = xgrid, y = ygrid, z = -Hsmooth, xlab = xlab, ylab = ylab, 
          col = cols)
    # Plot selection of dots
    if (ndot > 0) 
    {
      ndot <- min(m, ndot)
      sel <- sort.list(rnorm(m))[1:ndot]
      points(x[sel], y[sel], cex = csize, col = col.points, pch = 15)
    }
    box()   
  }
  sg <- list()
  if (save==TRUE) 
  {
    # Return results
    sg <- list(Hraw = Hraw, Hsmooth = Hsmooth, xgrid = xgrid, ygrid = ygrid, 
               xbin = xbin, ybin = ybin, nmiss = nmiss, seldots = sel)
    sg
  }
  else
  {
    return(invisible(sg))
  }
}
