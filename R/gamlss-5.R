# ported from https://github.com/cran/gamlss/blob/master/R/gamlss-5.R
#
# ## offsets are saved for the vcov() to work 1-5-13
# ## revisised Friday, March 19, 2010 to correct a bug reported by Dr Willem Vervoort
# ## major revision MS Wednesday, December 29, 2004 at 18:04
# ## to unify the output and tidy the input 
# .onAttach <- function(...)
# {
#   packageStartupMessage(paste(paste(" **********   GAMLSS Version",  packageDescription("gamlss")$Version), " ********** \n")
#                         ,appendLF = FALSE)
#   packageStartupMessage(paste("For more on GAMLSS look at",  packageDescription("gamlss")$URL))
#   packageStartupMessage("Type gamlssNews() to see new features/changes/bug fixes.\n")
# }
# # packageStartupMessage(cat(paste(" **********   GAMLSS Version",  packageDescription("gamlss")$Version), "********** \n")
# #                 ;cat(paste("For more on GAMLSS look at",  packageDescription("gamlss")$URL), "\n")
# #                 ;cat("Type gamlssNews() to see new features/changes/bug fixes.\n")
# #                 , appendLF = FALSE)  
# # cat(paste(" **********   GAMLSS Version",  packageDescription("gamlss")$Version), "********** \n")
# # cat(paste("For more on GAMLSS look at",  packageDescription("gamlss")$URL), "\n")
# # cat("Type gamlssNews() to see new features/changes/bug fixes.\n")
# #    }
# gamlssNews <- function() file.show(system.file("doc", "NEWS.txt", package="gamlss"))# Monday, September 5, 2005 MS
# #.onAttach <- function(libname, pkgname) {
# #    gamlssver <- if (as.numeric(R.version$major) < 2 && 
# #                 as.numeric(R.version$minor) < 9.0)
# #      package.description("gamlss")["Version"] else
# #    packageDescription("gamlss")$Version
# #    cat(paste("gamlss", gamlssver), "\n")
# #    cat("Type gamlssNews() to see new features/changes/bug fixes.\n")
# #}
# ##
# ##----------------------------------------------------------------------------------------
.gamlss.bi.list<-c("BI", "Binomial", "BB", "Beta Binomial", "ZIBI", "ZIBB", 
                   "ZABI", "ZABB", "DBI", "BItr", "BBtr",  "ZIBItr", "ZIBBtr", 
                   "ZABItr", "ZABBtr", "DBItr") # binomial denominators
.gamlss.multin.list<-c("MULTIN", "MN3", "MN4", "MN5")
##----------------------------------------------------------------------------------------
.gamlss.sm.list<-c("cs", "scs",                   # smoothing cubic splines  "s" vc"
                   "ps", "pb", "cy", "tp", "pvc", # penalised splines : ps, pb, cy tp pvc pbq
                   "pbm", "pbj", "pbo", "pbz",    # monotone jumps and going to zero
                   "pbc", "pbts", "pbp", "pbf",   # pb cycle pb predict
                   "pcat",                        # for categorical to reduce levels 
                   "pbq",                         # pb using Qfunction 
                   "gmrf",                        # Gaussian Markov random fields
                   "mrfa",  "mrf",                # Markov random fields
                   "so", "SOP",                   # seperation of Anisotropic penalties
                   "krig" ,                       # kriging
                   "lo",                          # loess                   
                   "random","re", "re4",          # random effect  "ra","rc","rash"
                   "fp","pp",                     # fractional poly         
                   "nl",                          # non-linear (.nl)         
                   "ri",                          # ridge regression      "ridge",
                   "boost",                       # boosting              
                   "fk",                          # free knot smoothing   
                   "own", "test", "test0","test1",# for testing new   
                   "arma",                        # time series  
                   "rw", "ar", "seas",            # time series             
                   "srw", "sar",                  # time series seasonal  
                   "la", "plag",                  # time series penalised lags, 
                   "tr",                          # decision trees  
                   "ga", "ba", "mm",              # gam - bam Simon Wood      
                   "nn",                          # neural netwoks           : nn
                   "sv",                          # suppor vector machines   : sv 
                   "ma",                          # mars                     : ma
                   "pcr",                         # principal component Reg  : pcr
                   "pc",                          # principal component regr : pc
                   "h2o",                         # h2o project 
                   "pa",                          # partial regression
                   "gnet", "lrs")                 # glmnet and lars

# note that predict only read 2 characters Monday, May 4, 2009 at 11:27
##----------------------------------------------------------------------------------------
# .gamlss.sm.list<-c( "ar", "arma",
#                    "cs", "cy", 
#                    "fk","fp",
#                    "ga",
#                    "krig",
#                    "la","lo",  
#                   "mrf", "mrfa", 
#                   "nl","nn",
#                   "ri", "rw", 
#                    "pb",  "pbj", "pbm", "pbo", "pbq","pbz",
#                    "pp","ps", "pvc", 
#                    "random","re",# random effect
#                    "sap", "sap3",  "seas",                      
#                   "test", "test0","test1","tr",                     
#                   "own")
##========================================================================================
##  Generalised Additive Models for Location Scale and Shape 
##   R-functions created
##   originaly created 6  Feb  2002. 
##   Last maijor revision  Saturday, Thursday, January 9, 2003 at 09:48
##   
##                y   the response variable 
##                X   [1,2,3,4]   design matrices
##                w   weights for weighed likelihood analysis (not as in GLM and GAM) 
##          *offset   offsets in linear predictor for mu, sigma,nu, tau
##           family   response family see for available GAMLSS families
##           *start   starting values for mu, sigma, nu or tau (optional)
##             *fix   whether the specific parameter should be remained 
##                    fixed in the fitting procedure
#========================================================================================
###  The gamlss function contains the sub-functions
###  RS() CG() and mixed()
###  
#----------------------------------------------------------------------------------------
# library(survival)
gamlss <- function(formula = formula(data),
                   sigma.formula = ~1,
                   nu.formula = ~1,
                   tau.formula = ~1,
                   family = NO(),
                   data,
                   weights = NULL, # for weighted likelihood analysis 
                   # (not the same as in GLM's) 
                   contrasts = NULL, # one type of contrasts for all  parameters 
                   method = RS(), # default algorithm
                   start.from = NULL, # starting from previous gamlss object  
                   mu.start = NULL, # starting from given values
                   sigma.start = NULL, 
                   nu.start = NULL,
                   tau.start = NULL,
                   mu.fix = FALSE,# whether the parameter is fixed 
                   sigma.fix = FALSE,
                   nu.fix = FALSE,
                   tau.fix = FALSE,
                   control = gamlss.control(...), 
                   i.control = glim.control(...),   # the inner circle control (GLIM)
                   ...)
{
  ## na.action = na.fail(), # both na.action and subset have been removed 
  ##                    because while there is only one data set there are usually  
  ##   subset = NULL,   four different model frames created therefore it is easier to apply 
  ##                    sub-setting and na.action to the whole data set not to the 
  ##                    frames 
  ##---------------------------------------------------------------------------------------
  ## require(stats) Thursday, June 10, 2004 at 09:58 MS
  #require(splines) # this will be removed with namespaces
  #----------------------------------------------------------------------------------------
  #gamlss.rc.list<-c("EX.rc","Exponential.rc") # the right censoring distribution list 
  #gamlss.bi.list<-c("BI", "Binomial", "BB", "Beta Binomial") # binomial denominators
  #.gamlss.multin.list<-c("MULTIN", "MN3", "MN4", "MN5")
  # ---------------------------------------------------------------------------------------
  # this is to replicate rqres within gamlss enviroment DS Friday, March 31, 2006 at 10:30
  rqres <- function (pfun = "pNO", 
                     type = c("Continuous", "Discrete", "Mixed"),
                     censored = NULL,  
                     ymin = NULL, 
                     mass.p = NULL, 
                     prob.mp = NULL,
                     y = y,
                     ... )
  { }
  body(rqres) <-  eval(quote(body(rqres)), envir = getNamespace("gamlss"))
  ##---------------------------------------------------------------------------------------
  ##---------------------------------------------------------------------------------------
  ## first the definition of the three algorithms
  ##---------------------------------------------------------------------------------------
  ## the mixing algorithm
  ##---------------------------------------------------------------------------------------
  ##---------------------------------------------------------------------------------------
  mixed <- function(n1=1,n2=20) 
  {
    conv <- RS(n.cyc=n1, no.warn = FALSE)
    conv <- CG(n.cyc=n2) 
    conv
  }
  ##---------------------------------------------------------------------------------------
  ## the Rigby and Stasinopoulos algorithm
  ##---------------------------------------------------------------------------------------
  RS <- function(n.cyc = control$n.cyc, no.warn = TRUE)
  {
    ## this is to emulate the GLIM iterative algorithm
    ## created by Mikis Stasinopoulos Monday, February 18, 2002 at 09:23 
    ##                    last change  Wednesday, February 23, 2005 at 20:23
    ##   where the step has changed to apply to the fitted values         
    glim.fit <- function(f,X,y,w,fv,os, step = 1,control = glim.control(), 
                         auto, gd.tol)
    { 
      cc <- control$cc          # convergence criterion-tolerance
      cyc <- control$cyc         # max. no. of cycles
      trace <- control$glm.trace       # whether to print 
      bf.cyc <- control$bf.cyc 
      bf.tol <- control$bf.tol
      bf.trace <- control$bf.trace
      itn <- 0
      lp <- eta <- f$linkfun(fv)
      dr <- f$dr(eta)    # dmu/deta
      dr <- 1/dr         # deta/dmu = 1 / (dmu/deta)
      di <- f$G.di(fv)   # deviance increment
      dv <- sum(w*di)    # the global deviance
      olddv <- dv+1         # the old global deviance
      dldp <- f$dldp(fv)   # u score
      d2ldp2 <- f$d2ldp2(fv) # second derivative of log-Likelihood
      d2ldp2 <- ifelse(d2ldp2 < -1e-15, d2ldp2,-1e-15) # added 26-10-07  
      wt <- -(d2ldp2/(dr*dr))#  -(d2l/dp2)/(1/(dmu/deta))^2=- (d2l/dp2)(dmu/eta)^2
      # we need to stop the weights to go to Infty
      wt <- ifelse(wt>1e+10,1e+10,wt) # Mikis 9-10-14 
      wt <- ifelse(wt<1e-10,1e-10,wt) 
      # wv <- (eta-os)+step*dldp/(dr*wt)
      wv <- (eta-os)+dldp/(dr*wt) # eta 
      if (family$type=="Mixed") wv <-ifelse(is.nan(wv),0,wv) ## TEST
      iterw <- FALSE
      who <- f$who
      smooth.frame <- f$smooth.frame
      s <- f$smooth
      ## starting the recycling    
      while ( abs(olddv-dv) > cc && itn < cyc ) # MS Wednesday, June 26, 2002 
      {
        itn <- itn+1 # the glim inner iteration number
        lpold <- lp
        sold <- s
        if (any(is.na(wt))||any(is.na(wv)) ) stop("NA's in the working vector or weights for parameter ", names(formals(f$valid)), "\n") 
        if (any(!is.finite(wt))||any(!is.finite(wv)) ) stop("Inf values in the working vector or weights for parameter ", names(formals(f$valid)), "\n") 
        if(length(who) > 0) 
        { 
          fit <- additive.fit(x=X,y=wv,w=wt*w,s=s,who=who,smooth.frame, maxit = bf.cyc, 
                              tol = bf.tol, trace = bf.trace)
          # lp <- fit$fitted.values 
          lp <- if (itn==1)  fit$fitted.values else step*fit$fitted.values+(1-step)*lpold
          #  s <- fit$smooth # test Wednesday, January 8, 2003 at 14:37 
          s <- if (itn==1) fit$smooth else step*fit$smooth+(1-step)*sold  
        }   
        else 
        { 
          fit <- lm.wfit(X,wv,wt*w,method="qr") 
          lp <- if (itn==1)  fit$fitted.values else step*fit$fitted.values+(1-step)*lpold
          # lp <- fit$fitted.values
        }
        ## method 1
        eta <- lp+os # fixed Wednesday, September 4, 2002 at 09:45 DS  
        ## own link
        fv <- f$linkinv(eta)
        ## own dist
        di <- f$G.di(fv) 
        olddv <- dv
        dv <- sum(w*di)  
        ## new for automatic steps MS BR Friday, April 15, 2005 at 18:50
        if (dv > olddv && itn >= 2 && auto==TRUE) 
        {
          for(i in 1:5) # MS Thursday, September 22, 2005 
          { 
            lp <- (lp+lpold)/2
            eta <- lp+os 
            fv <- f$linkinv(eta)
            di <- f$G.di(fv) 
            dv <- sum(w*di) 
            #  cat("try",i,"\n")
            if(length(who) > 0) s <- (s+sold)/2  
            if ((olddv-dv) > cc) break # MS Thursday, September 22, 2005 
          }
        }
        if ((dv > olddv+gd.tol ) && itn >= 2 && iterw==FALSE) 
        {
          warning("The deviance has increased in an inner iteration for ",
                  names(formals(f$valid)), "\n","Increase gd.tol and if persist, try different steps",  "\n", "or model maybe inappropriate") #
          iterw <-TRUE
        }                      
        if (is.na(!f$valid(fv)) ) # MS Saturday, April 6, 2002 at 18:06 
          stop( "fitted values in the inner iteration out of range")
        dr <- f$dr(eta) 
        dr <- 1/dr
        ## method 2
        dldp <- f$dldp(fv)   
        d2ldp2 <- f$d2ldp2(fv)
        d2ldp2 <-  ifelse(d2ldp2 < -1e-15, d2ldp2,-1e-15) # added 26-10-07   
        wt <- -(d2ldp2/(dr*dr)) 
        wt <- ifelse(wt>1e+10,1e+10,wt) # Mikis 9-10-14    
        wt <- ifelse(wt<1e-10,1e-10,wt) 
        # wv <- (eta-os)+step*dldp/(dr*wt)
        wv <- (eta-os)+dldp/(dr*wt)
        if (family$type=="Mixed") wv <-ifelse(is.nan(wv),0,wv) ## TEST  
        #   olddv <- dv
        #      dv <- sum(w*di)
        if(trace)
          cat("GLIM iteration ", itn, " for ",  names(formals(f$valid)), ": Global Deviance = ",
              format(round(dv, 4)), " \n", sep = "")
      } #end of while
      pen <- 0 # DS Thursday, November 21, 2002 at 23:04
      if(length(who) > 0) {pen<- sum(eta*wt*(wv-eta)) }
      c(fit, list(fv = fv, wv = wv, wt = wt, eta = eta, os = os, pen=pen)) #ms Saturday, December 4, 2004 
    }     
    ##-end of GLIM.fit------------------------------------------------------------
    
    ##---start RS-----------------------------------------------------------------
    ## getting the contol papameters
    c.crit <- control$c.crit
    # n.cyc <- control$n.cyc
    trace <- control$trace
    autostep <- control$autostep
    mu.step <- control$mu.step
    sigma.step <- control$sigma.step
    nu.step <- control$nu.step
    tau.step <- control$tau.step
    gd.tol <- control$gd.tol
    iter <- control$iter
    conv <- FALSE
    ## initial Gloval deviance
    G.dev.incr  <- eval(G.dev.expr)
    G.dev <- sum(w*G.dev.incr)
    G.dev.old <- G.dev+1
    ##----------------------------------------------------------------------------
    ## the outer iteration starts here
    while ( abs(G.dev.old-G.dev) > c.crit && iter < n.cyc )
    {
      # the mean submodel
      if ("mu"%in%names(family$parameters))
      {
        if  (family$parameter$mu==TRUE & mu.fix==FALSE)
        {
          #mu.old <- mu
          mu.fit <<- glim.fit(f = mu.object, X = mu.X, y = y, w = w,
                              fv = mu, os = mu.offset, step = mu.step,
                              control = i.control, gd.tol = gd.tol,
                              auto = autostep)
          mu <<- mu.fit$fv
          mu.object$smooth <- mu.fit$smooth  
        }
      }
      # the scale-dispersion submodel
      if ("sigma"%in%names(family$parameters))
      {
        if  ( family$parameter$sigma==TRUE & sigma.fix==FALSE) 
        {  
          #sigma.old <- sigma
          sigma.fit  <<- glim.fit(f = sigma.object, X = sigma.X, y = y, 
                                  w = w, fv = sigma, os = sigma.offset, 
                                  step = sigma.step, control = i.control,
                                  gd.tol = gd.tol, auto = autostep)
          sigma <<- sigma.fit$fv 
          sigma.object$smooth <- sigma.fit$smooth  
        }
      }
      # the nu submodel
      if ("nu"%in%names(family$parameters))
      {
        if ( family$parameter$nu==TRUE & nu.fix==FALSE)
        {
          # nu.old <- nu
          nu.fit <<- glim.fit(f = nu.object, X = nu.X, y = y,
                              w = w, fv = nu, os = nu.offset, 
                              step = nu.step, control = i.control, 
                              gd.tol = gd.tol, auto = autostep)
          nu <<- nu.fit$fv
          nu.object$smooth <- nu.fit$smooth  
        }
      }
      # the tau submodel
      if ("tau"%in%names(family$parameters))
      {
        if (family$parameter$tau ==TRUE & tau.fix==FALSE)
        { 
          # tau.old <- tau
          tau.fit <<- glim.fit(f = tau.object, X = tau.X, y = y,
                               w = w, fv = tau, os = tau.offset,
                               step = tau.step, control = i.control, 
                               gd.tol = gd.tol, auto = autostep)
          tau <<- tau.fit$fv
          tau.object$smooth <- tau.fit$smooth   
        }
      }
      #   the overall Global Deviance
      G.dev.old <- G.dev
      G.dev.incr  <- eval(G.dev.expr)
      G.dev <- sum(w*G.dev.incr)
      iter <- iter+1
      fiter <<- iter
      if(trace)
        cat("GAMLSS-RS iteration ", iter, ": Global Deviance = ",
            format(round(G.dev, 4)), " \n", sep = "")
      if (G.dev > (G.dev.old+gd.tol) && iter >1 )
        stop(paste("The global deviance is increasing", "\n", 
                   "Try different steps for the parameters or the model maybe inappropriate"))
    }
    if ( abs(G.dev.old-G.dev) < c.crit ) # MS Wednesday, June 11, 2003 at 11:58 
      #taken out (abs((G.dev-G.dev.old)/(0.1+abs(G.dev.old)))<c.crit&&iter<=n.cyc) 
      conv <- TRUE else FALSE
    if (!conv && no.warn )   warning("Algorithm RS has not yet converged")
    conv
  }
  ##=======================================================================================
  ## End of GAMLSS RS fitting algorithm
  ##=======================================================================================
  ##---------------------------------------------------------------------------------------
  ##---------------------------------------------------------------------------------------
  #========================================================================================
  ## the gamlss CG (Cole and Green) fitting algorithm
  ##=======================================================================================
  #----------------------------------------------------------------------------------------
  CG<-function(n.cyc=control$n.cyc) 
  {
    #========================================================================================
    #  getting the contol papameters
    #========================================================================================
    #----------------------------------------------------------------------------------------
    c.crit <- control$c.crit
    #n.cyc <- control$n.cyc
    trace <- control$trace
    mu.step <- control$mu.step
    sigma.step <- control$sigma.step
    nu.step <- control$nu.step
    tau.step <- control$tau.step
    gd.tol <- control$gd.tol
    autostep <- control$autostep
    iter <- control$iter
    conv <- FALSE
    i.c.crit <- i.control$cc
    i.n.cyc <- i.control$cyc 
    i.trace <- i.control$glm.trace 
    #  bf.cyc <- i.control$bf.cyc 
    bf.tol <- i.control$bf.tol
    bf.trace <- i.control$bf.trace
    first.iter  <- TRUE #ms Thursday, November 6, 2003 at 09:04
    
    G.dev.incr <- eval(G.dev.expr)                  
    G.dev <- sum(w*G.dev.incr)
    G.dev.old <- G.dev+1  
    # initial values for the variates used in the inner iteration 
    w.mu.sigma<-w.mu.nu<-w.mu.tau<-w.sigma.nu<-w.sigma.tau<-w.nu.tau<-rep(0,N)            
    eta.mu<-eta.old.mu<-eta.sigma<-eta.old.sigma<-eta.nu<-eta.old.nu<-rep(0,N)  
    eta.old.tau<-eta.tau<-rep(0,N) #???
    # here starts the outer iteration 
    #=======================================================================================        
    while ( abs(G.dev.old-G.dev) > c.crit && iter < n.cyc )    
    {
      i.iter <- 0
      ## for mu  
      if ("mu"%in%names(family$parameters))
      {      
        eta.mu <- eta.old.mu <- family$mu.linkfun(mu)    
        u.mu <- mu.object$dldp(mu=mu)
        u2.mu <- mu.object$d2ldp2(mu=mu)
        dr.mu <- family$mu.dr(eta.mu)
        dr.mu <- 1/dr.mu
        who.mu <- mu.object$who
        smooth.frame.mu <- mu.object$smooth.frame
        s.mu <- if(first.iter) mu.object$smooth else s.mu       
        w.mu <- -u2.mu/(dr.mu*dr.mu)
        z.mu <- (eta.old.mu-mu.offset)+mu.step*u.mu/(dr.mu*w.mu)
      }   
      ## for sigma
      if ("sigma"%in%names(family$parameters)) 
      {                    
        eta.sigma <- eta.old.sigma <- family$sigma.linkfun(sigma)    
        u.sigma <- sigma.object$dldp(sigma=sigma)
        u2.sigma <- sigma.object$d2ldp2(sigma=sigma)
        u2.mu.sigma <- family$d2ldmdd(sigma=sigma)  
        dr.sigma <- family$sigma.dr(eta.sigma)
        dr.sigma <- 1/dr.sigma
        who.sigma <- sigma.object$who
        smooth.frame.sigma <- sigma.object$smooth.frame
        s.sigma <- if(first.iter) sigma.object$smooth else s.sigma        
        w.sigma <- -u2.sigma/(dr.sigma*dr.sigma)
        w.mu.sigma <- -u2.mu.sigma/(dr.mu*dr.sigma)                     
        z.sigma <- (eta.old.sigma-sigma.offset)+sigma.step*u.sigma/
          (dr.sigma*w.sigma)
      } 
      ## for nu
      if ("nu"%in%names(family$parameters)) 
      {                    
        eta.nu <- eta.old.nu <- family$nu.linkfun(nu)    
        u.nu <- nu.object$dldp(nu=nu)
        u2.nu <- nu.object$d2ldp2(nu=nu)
        u2.mu.nu <- family$d2ldmdv(nu=nu)  
        u2.sigma.nu <- family$d2ldddv(nu=nu)  
        dr.nu <- family$nu.dr(eta.nu)
        dr.nu <- 1/dr.nu
        who.nu <- nu.object$who
        smooth.frame.nu <- nu.object$smooth.frame
        s.nu <- if(first.iter) nu.object$smooth else s.nu          
        w.nu <- -u2.nu/(dr.nu*dr.nu)
        w.mu.nu <- -u2.mu.nu/(dr.mu*dr.nu)
        w.sigma.nu <- -u2.sigma.nu/(dr.sigma*dr.nu)
        z.nu <- (eta.old.nu-nu.offset)+nu.step*u.nu/
          (dr.nu*w.nu)
      }
      ## for tau        
      if ("tau"%in%names(family$parameters)) 
      {                    
        eta.tau <- eta.old.tau <- family$tau.linkfun(tau)    
        u.tau <- tau.object$dldp(tau=tau)
        u2.tau <- tau.object$d2ldp2(tau=tau)
        u2.mu.tau <- family$d2ldmdt(tau=tau)  
        u2.sigma.tau <- family$d2ldddt(tau=tau)  
        u2.nu.tau <- family$d2ldvdt(tau=tau)  
        dr.tau <- family$tau.dr(eta.tau)
        dr.tau <- 1/dr.tau
        who.tau <- tau.object$who
        smooth.frame.tau <- tau.object$smooth.frame
        s.tau <- if(first.iter) tau.object$smooth else s.tau       
        w.tau <- -u2.tau/(dr.tau*dr.tau)
        w.mu.tau <- -u2.mu.tau/(dr.mu*dr.tau)
        w.sigma.tau <- -u2.sigma.tau/(dr.sigma*dr.tau)
        w.nu.tau <- -u2.nu.tau/(dr.nu*dr.tau)
        z.tau <- (eta.old.tau-tau.offset)+tau.step*u.tau/
          (dr.tau*w.tau)
      }        
      ## initial G. devinace for the inner iteration
      G.dev.in <- G.dev+1
      i.G.dev <- G.dev
      first.iter <- FALSE   
      ## the inner iteration starts here
      ##--------------------------------------------------------------------------------------
      while ( abs(G.dev.in -i.G.dev) > i.c.crit && i.iter < i.n.cyc )
      {     
        if ("mu"%in%names(family$parameters))
        {   
          if  (family$parameter$mu==TRUE & mu.fix==FALSE) 
          {
            adj.mu <- -(w.mu.sigma*(eta.sigma-eta.old.sigma)+
                          w.mu.nu*(eta.nu-eta.old.nu)+w.mu.tau*(eta.tau-eta.old.tau))/w.mu 
            wv.mu  <- z.mu+adj.mu
            if(length(who.mu) > 0) 
            { 
              mu.fit <<- additive.fit(x = mu.X, y = wv.mu, w = w.mu*w,s  = s.mu, 
                                      who = who.mu, smooth.frame = smooth.frame.mu, maxit = 1, 
                                      tol = bf.tol, trace = bf.trace)
              mu.fit$eta <<- eta.mu <- mu.fit$fitted.values+mu.offset
              mu.fit$fv <<-  mu <<- mu.object$linkinv(eta.mu)
              s.mu.old <- s.mu
              s.mu <- mu.fit$smooth 
              mu.fit$pen <<- sum(eta.mu*w.mu*(wv.mu-eta.mu))
              mu.fit$wv <<- wv.mu
              mu.fit$wt <<- w.mu
              mu.fit$os <<- mu.offset
              
            }   
            else 
            { 
              mu.fit <<- lm.wfit(x=mu.X,y=wv.mu,w=w.mu*w,method="qr") 
              mu.fit$eta <<- eta.mu <- mu.fit$fitted.values+mu.offset
              mu.fit$fv <<-   mu <<- mu.object$linkinv(eta.mu)
              mu.fit$wv <<- wv.mu
              mu.fit$wt <<- w.mu
              mu.fit$os <<- mu.offset      
            }
          }
        }     
        ##fit sigma 
        if ("sigma"%in%names(family$parameters))
        {  
          if  (family$parameter$sigma==TRUE & sigma.fix==FALSE) 
          {
            adj.sigma <- -(w.mu.sigma*(eta.mu-eta.old.mu)+
                             w.sigma.nu*(eta.nu-eta.old.nu)+w.sigma.tau*(eta.tau-eta.old.tau))/w.sigma 
            wv.sigma  <- z.sigma+adj.sigma
            if(length(who.sigma) > 0) 
            { 
              sigma.fit <<- additive.fit(x = sigma.X, y = wv.sigma, 
                                         w = w.sigma*w,s = s.sigma,
                                         who = who.sigma, 
                                         smooth.frame = smooth.frame.sigma, 
                                         maxit = 1,
                                         tol = bf.tol, trace = bf.trace)
              
              sigma.fit$eta <<- eta.sigma <- sigma.fit$fitted.values+sigma.offset
              sigma.fit$fv <<- sigma <<- sigma.object$linkinv(eta.sigma) 
              s.sigma.old <- s.sigma
              s.sigma <- sigma.fit$smooth
              sigma.fit$pen <<- sum(eta.sigma*w.sigma*(wv.sigma-eta.sigma))
              sigma.fit$wv <<- wv.sigma
              sigma.fit$wt <<- w.sigma
              sigma.fit$os <<- sigma.offset 
            }   
            else 
            { 
              sigma.fit <<- lm.wfit(x=sigma.X,y=wv.sigma,w=w.sigma*w,method="qr") 
              sigma.fit$eta <<- eta.sigma <- sigma.fit$fitted.values+sigma.offset
              sigma.fit$fv <<- sigma    <<- sigma.object$linkinv(eta.sigma) 
              sigma.fit$wv <<- wv.sigma
              sigma.fit$wt <<- w.sigma 
              sigma.fit$os <<- sigma.offset   
            }
          }   
        }
        ##fit nu 
        if ("nu"%in%names(family$parameters))
        {   
          if  (family$parameter$nu==TRUE & nu.fix==FALSE) 
          {
            adj.nu <- -(w.mu.nu*(eta.mu-eta.old.mu)+
                          w.sigma.nu*(eta.sigma-eta.old.sigma)+w.nu.tau*(eta.tau-eta.old.tau))/w.nu 
            wv.nu  <- z.nu+adj.nu
            if(length(who.nu) > 0) 
            { 
              nu.fit <<- additive.fit(x = nu.X,y = wv.nu, w = w.nu*w,s = s.nu, 
                                      who = who.nu, smooth.frame = smooth.frame.nu, 
                                      maxit = 1,
                                      tol = bf.tol, trace = bf.trace)
              nu.fit$eta <<- eta.nu <- nu.fit$fitted.values+nu.offset
              nu.fit$fv <<-    nu <<- nu.object$linkinv(eta.nu) 
              s.nu.old <- s.nu
              s.nu <- nu.fit$smooth 
              nu.fit$pen <<- sum(eta.nu*w.nu*(wv.nu-eta.nu)) 
              nu.fit$wv <<- wv.nu
              nu.fit$wt <<- w.nu 
              nu.fit$os <<- nu.offset  
            }   
            else 
            { 
              nu.fit <<- lm.wfit(x=nu.X,y=wv.nu,w=w.nu*w,method="qr") 
              nu.fit$eta <<- eta.nu <- nu.fit$fitted.values+nu.offset
              nu.fit$fv <<-    nu <<- nu.object$linkinv(eta.nu) 
              nu.fit$wv <<- wv.nu
              nu.fit$wt <<- w.nu 
              nu.fit$os <<- nu.offset      
            } 
          }
        }    
        ##fit tau
        if ("tau"%in%names(family$parameters))
        {        
          if  (family$parameter$tau==TRUE & tau.fix==FALSE) 
          {
            adj.tau <- -(w.mu.tau*(eta.mu-eta.old.mu)+
                           w.sigma.tau*(eta.sigma-eta.old.sigma)+w.nu.tau*(eta.nu-eta.old.nu))/w.tau 
            wv.tau  <- z.tau+adj.tau
            if(length(who.tau) > 0) 
            { 
              tau.fit <<- additive.fit(x = tau.X,y = wv.tau, w = w.tau*w,s = s.tau, 
                                       who = who.tau, smooth.frame = smooth.frame.tau, 
                                       maxit = 1,
                                       tol = bf.tol, trace = bf.trace)
              tau.fit$eta <<- eta.tau <- tau.fit$fitted.values+tau.offset
              tau.fit$fv <<-    tau <<- tau.object$linkinv(eta.tau)
              s.tau.old <- s.tau
              s.tau <- tau.fit$smooth 
              tau.fit$pen <<- sum(eta.tau*w.tau*(wv.tau-eta.tau)) 
              tau.fit$wv <<- wv.tau
              tau.fit$wt <<- w.tau 
              tau.fit$os <<- tau.offset  
            }   
            else 
            { 
              tau.fit <<- lm.wfit(x=tau.X,y=wv.tau,w=w.tau*w,method="qr") 
              tau.fit$eta <<- eta.tau <- tau.fit$fitted.values+tau.offset
              tau.fit$fv <<-    tau <<- tau.object$linkinv(eta.tau)
              tau.fit$wv <<- wv.tau
              tau.fit$wt <<- w.tau
              tau.fit$os <<- tau.offset        
            }
          }
        }                        
        G.dev.in <- i.G.dev
        G.dev.incr <- eval(G.dev.expr)   
        i.G.dev <- sum(w*G.dev.incr)
        i.iter <- i.iter+1     
        if(i.trace)
          cat("CG inner iteration ", iter, ": Global Deviance = ",
              format(round(i.G.dev, 4)), " \n", sep = "")           
        if (i.G.dev > (G.dev.in+gd.tol) && iter >1 )  
          stop(paste("The global deviance is increasing in the inner CG loop", "\n", 
                     "Try different steps for the parameters or the model maybe inappropriate"))  
      } 
      ## the inner iteration finish here 
      ##--------------------------------------------------------------------------------------
      G.dev.old <- G.dev
      G.dev.incr <- eval(G.dev.expr)   
      G.dev <- sum(w*G.dev.incr)
      # autostep<-FALSE
      ## new for automatic steps MS BR Friday, April 15, 2005 at 18:50
      if (G.dev > G.dev.old && iter >= 2 && autostep == TRUE) # && itn >= 2 ) 
      {
        for(i in 1:5)             
        { 
          if ("mu"%in%names(family$parameters))
          {
            eta.mu <- (eta.mu+eta.old.mu)/2
            mu <<- mu.object$linkinv(eta.mu) 
            if(length(who.mu) > 0) s.mu <- (s.mu+s.mu.old)/2 
          }
          if ("sigma"%in%names(family$parameters))
          {
            eta.sigma <- (eta.sigma+eta.old.sigma)/2
            sigma <<- sigma.object$linkinv(eta.sigma)
            if(length(who.sigma) > 0) s.sigma <- (s.sigma+s.sigma.old)/2  
          }
          if ("nu"%in%names(family$parameters))
          {
            eta.nu <- (eta.nu+eta.old.nu)/2
            nu <<- nu.object$linkinv(eta.nu) 
            if(length(who.nu) > 0) s.nu <- (s.nu+s.nu.old)/2 
          }     
          if ("tau"%in%names(family$parameters))
          {
            eta.tau <- (eta.tau+eta.old.tau)/2
            tau <<- tau.object$linkinv(eta.tau)
            if(length(who.tau) > 0) s.tau <- (s.tau+s.tau.old)/2  
          } 
          G.dev.incr <- eval(G.dev.expr)   
          G.dev <- sum(w*G.dev.incr)
          # cat("helow there \n")
          #  if(length(who) > 0) s <- (s+sold)/2  
          if (G.dev < G.dev.old) break
        }
      }
      iter <- iter+1  
      fiter <<-iter  
      if(trace)
        cat("GAMLSS-CG iteration ", iter, ": Global Deviance = ",
            format(round(G.dev, 4)), " \n", sep = "")
      if (G.dev > (G.dev.old+gd.tol) && iter >1 )  
        stop(paste("The global deviance is increasing in CG-algorithm ", "\n", 
                   "Try different steps for the parameters or the model maybe inappropriate"))  
      
    }   
    if ( abs(G.dev.old-G.dev) < c.crit ) conv <- TRUE else FALSE # MS June 11, 2003
    if (!conv)   warning("Algorithm CG has not yet converged")
    conv
    ##the outer iteration finish here 
    ##=======================================================================================
  }
  ##---------------------------------------------------------------------------------------
  ## the end of CG        
  ##---------------------------------------------------------------------------------------
  
  ##=======================================================================================
  ##---------------------------------------------------------------------------------------
  ## this function is used in for outputing the parameters
  ##---------------------------------------------------------------------------------------
  parameterOut <- function(what="mu", save)
  {
    out <- list()
    if(save==TRUE) 
    { 
      if(family$parameter[[what]]==TRUE && eval(parse(text=paste(what,".fix",sep="")))==FALSE)
      {
        out$fv <- eval(parse(text=what))
        out$lp <- eval(parse(text=(paste(what,".fit$eta", sep=""))))
        out$wv <- eval(parse(text=(paste(what,".fit$wv", sep=""))))
        out$wt <- eval(parse(text=(paste(what,".fit$wt", sep=""))))
        out$link <- eval(parse(text=(paste(what,".object$link", sep=""))))
        out$terms <- eval(parse(text=(paste(what,".terms", sep=""))))
        out$x <- eval(parse(text=(paste(what,".X", sep=""))))
        out$qr <- eval(parse(text=(paste(what,".fit$qr", sep="")))) 
        out$coefficients <- eval(parse(text=(paste(what,".fit$coefficients", sep="")))) 
        out$offset <- eval(parse(text=(paste(what,".fit$os",           sep="")))) 
        out$xlevels  <- .getXlevels( eval(parse(text=paste(what,".terms",sep="")))
                                     , eval(parse(text=paste(what,".frame",sep="")))) 
        # ms Sunday, June 13 2004  
        out$formula <- eval(parse(text=paste(what,".formula",sep="")))
        if(length(eval(parse(text=paste(what,".smoothers",sep="")))) > 0) 
        { out$df <- eval(parse(text=paste(what,".fit$nl.df",sep="")))+
          eval(parse(text=paste(what,".fit$rank",sep=""))) 
        out$nl.df <- eval(parse(text=paste(what,".fit$nl.df",sep="")))
        out$s  <- eval(parse(text=paste(what,".fit$smooth",sep=""))) 
        out$var <- eval(parse(text=paste(what,".fit$var",sep="")))  
        out$coefSmo <- eval(parse(text=paste(what,".fit$coefSmo",sep=""))) 
        out$lambda <- eval(parse(text=paste(what,".fit$lambda",sep="")))
        out$pen <- eval(parse(text=paste(what,".fit$pen",sep="")))            
        }
        else                       
        { out$df <- eval(parse(text=paste(what,".fit$rank",sep=""))) 
        out$nl.df <- 0
        out$pen  <- 0 #ms May 13, 2004 
        }
      }
      else
      {         out$fix <- eval(parse(text=paste(what,".fix",sep="")))
      out$df <- 0
      out$fv <- eval(parse(text=what))
      }
    } # if(save== not TRUE) 
    else
    {
      if(family$parameter[[what]]==TRUE && eval(parse(text=paste(what,".fix",sep="")))==FALSE)
      {
        if(length(eval(parse(text=paste(what,".smoothers",sep="")))) > 0) 
        { out$df <- eval(parse(text=paste(what,".fit$nl.df",sep="")))+
          eval(parse(text=paste(what,".fit$rank",sep=""))) 
        out$nl.df <- eval(parse(text=paste(what,".fit$nl.df",sep="")))
        out$terms <- eval(parse(text=(paste(what,".terms", sep=""))))
        out$formula <- eval(parse(text=paste(what,".formula",sep="")))
        }
        else                       
        {  
          out$df <- eval(parse(text=paste(what,".fit$rank",sep=""))) 
          out$nl.df <- 0
          out$terms <- eval(parse(text=(paste(what,".terms", sep=""))))
          out$formula <- eval(parse(text=paste(what,".formula",sep="")))
        }
      }
      else
      {     
        out$df <- 0
      }
    }
    out
  }
  ##=========================================================================================
  ##-----------------------------------------------------------------------------------------
  ## this function is used to extract the formula for the parameters other than mu
  ##-----------------------------------------------------------------------------------------
  ##=========================================================================================
  other.formula <- function(form)
  {
    dform <- formula(form)
    if (length(dform)==2)
    {
      dform[3] <- dform[2]      # taking 1 in position [3]
      dform[2] <- if (is(formula, "terms") )formula[[2]] else formula[2] # ms 31-12-08   # put y in position 2 
    }
    dform 
  }
  ##=========================================================================================
  ##-----------------------------------------------------------------------------------------
  ## this function is getting the smoothers at each parameter
  ##-----------------------------------------------------------------------------------------
  ##=========================================================================================
  get.smoothers <- function(term)
  {
    a <- attributes(term)   #
    smoothers <- a$specials #S convert variable pointers to term pointers
    if(length(smoothers) > 0)
    {
      smoothers <- smoothers[sapply(smoothers, length) > 0]
      # smoothersR <-smoothers 
      for(i in seq(along = smoothers))
      {
        tt <- smoothers[[i]]
        ff <- apply(a$factors[tt,  , drop = FALSE], 2, any)
        smoothers[[i]] <- if(any(ff)) seq(along = ff)[a$order == 1 & ff]
        else NULL
      }
    }
    smoothers
  }
  ##=========================================================================================
  ##-----------------------------------------------------------------------------------------
  ## this function creates the parameter objects
  ##-----------------------------------------------------------------------------------------
  ##=========================================================================================
  get.object <- function(what)
  {
    link <- eval(parse(text=(paste("family$",what,".link", sep=""))))
    linkfun <- eval(parse(text=(paste("family$",what,".linkfun", sep="")))) 
    linkinv <- eval(parse(text=(paste("family$",what,".linkinv", sep="")))) 
    dr <- eval(parse(text=(paste("family$",what,".dr", sep=""))))
    dldp <- switch( what,
                    "mu" = family$dldm,
                    "sigma" = family$dldd,
                    "nu" = family$dldv,
                    "tau" = family$dldt
    )  
    d2ldp2 <- switch( what,
                      "mu" = family$d2ldm2,
                      "sigma" = family$d2ldd2,
                      "nu" = family$d2ldv2,
                      "tau" = family$d2ldt2
    )    
    G.di <- family$G.dev.incr            
    valid <- eval(parse(text=(paste("family$",what,".valid", sep=""))))               
    object <- list(link = link, linkfun = linkfun, linkinv = linkinv, dr = dr,
                   dldp = dldp,  d2ldp2 = d2ldp2,     G.di = G.di, valid = valid)          
    if(length(eval(parse(text=(paste(what,".smoothers", sep=""))))) > 0) 
    {        #only if smoothing 
      parAttrTermlevels  <- eval(parse(text=(paste(what,".a$term.labels", sep=""))))
      boo <- unlist(eval(parse(text=(paste(what,".smoothers", sep="")))))
      who <- parAttrTermlevels[boo[ order(boo)]]
      smooth.frame <- eval(parse(text=(paste(what,".frame", sep="")))) 
      s <- matrix(0, N, length(who))
      dimnames(s) <- list(names(y), who)
      object$smooth <- s
      object$who <- who 
      object$smooth.frame <- smooth.frame
    } 
    object   
  }
  #==========================================================================================
  ##-----------------------------------------------------------------------------------------
  ##-----------------------------------------------------------------------------------------
  ##=========================================================================================
  ## here is where the proper gamlss function starts
  ##=========================================================================================
  ##-----------------------------------------------------------------------------------------
  ##       Save call for future reference
  gamlsscall <- match.call()  #   the function call
  ## checking for NA in the data 
  if(!missing(data)) 
  {
    if  (any(is.na(data)))   
      stop("The data contains NA's, use data = na.omit(mydata)")
  }  
  ##       Evaluate the model frame
  mnames <- c("", "formula", "data", "weights" ) #  "subset"  "na.action"
  cnames <- names(gamlsscall)  # get the names of the arguments of the call
  cnames <- cnames[match(mnames,cnames,0)] # keep only the ones that match with mnames
  mcall <- gamlsscall[cnames] # get in mcall all the relevant information but remember
  # that the first elenent will be NULL
  mcall[[1]] <- as.name("model.frame") # replace NULL with model.frame
  ##        Specials for smoothing
  mcall$formula <- if(missing(data)) terms(formula, specials = .gamlss.sm.list) 
  else terms(formula, specials = .gamlss.sm.list, data = data)    
  mu.frame <- eval(mcall, sys.parent())  # evalute the data.frame at the model.frame
  
  ##-----------------------------------------------------------------------------------------
  ## This part deals with the family 
  family <- as.gamlss.family(family)        # bring first the gamlss family
  G.dev.expr <- body(family$G.dev.inc)          #MS Thursday, April 11, 2002 at 10:34 
  #  nopar <- family$nopar # the number of parameters for the family  
  ##-----------------------------------------------------------------------------------------
  ## Now extract the model components using model.extra and model.matrix
  ## This part deals with the response variable 
  Y <- model.extract(mu.frame, "response") #extracting the y variable from the formula
  if(is.null(dim(Y)))                       # if y not matrix
    N <- length(Y) else N <- dim(Y)[1]   # calculate the dimension for y  
  # .gamlss.bi.list <-  if (exists("gamlss.bi.list",envir=.GlobalEnv)) 
  #                         get("gamlss.bi.list", envir=.GlobalEnv) else .gamlss.bi.list
  ## extracting now the y and the binomial denominator in case we use BI or BB
  if(any(family$family%in%.gamlss.bi.list)) 
  { 
    if (NCOL(Y) == 1) 
    {
      y <- if (is.factor(Y))  Y != levels(Y)[1] else Y
      bd <- rep(1, N)
      if (any(y < 0 | y > 1)) stop("y values must be 0 <= y <= 1")
    } 
    else if (NCOL(Y) == 2) 
    {
      if (any(abs(Y - round(Y)) > 0.001)) {
        warning("non-integer counts in a binomial GAMLSS!")
      }
      bd <- Y[,1] + Y[,2]
      y <-  Y[,1]
      if (any(y < 0 | y > bd)) stop("y values must be 0 <= y <= N") # MS Monday, October 17, 2005 
    } 
    else stop(paste("For the binomial family, Y must be", 
                    "a vector of 0 and 1's or a 2 column", "matrix where col 1 is no. successes", 
                    "and col 2 is no. failures"))
  }
  # multinomial checking
  else if(any(family$family%in%.gamlss.multin.list))
  {
    y <- if(is.factor(Y))   unclass(Y)
    else Y
  } 
  ## For censoring
  else if(is.Surv(Y))
  { 
    ## checking that the family is censored
    if (length(grep("censored",family$family[[2]]))==0) 
      stop(paste("the family in not a censored distribution, use cens()"))
    ## checking compatability of Surv object and censored distribution
    if (length(grep(attr(Y,"type"),family$family[[2]]))==0) 
      stop(paste("the Surv object and the censored distribution are not of the same type"))
    y <- Y
    #if (NCOL(Y) == 2)
    #   {
    #      #.event <- Y[,2]
    #      #    y  <- Y[,1]
    #      y <- Y
    #   } 
    #else if (NCOL(Y) == 2) 
    #stop("interval censored data are not implemented in gamlss yet")     
  }     
  else {y <- Y }
  ##-----------------------------------------------------------------------------------------
  ##checking the permissible y values      
  if (!family$y.valid(y)) # MS Thursday, June 20, 2002 at 16:30 
    stop( "response variable out of range")
  ##-----------------------------------------------------------------------------------------
  ## this part is used if start.from is used as argument
  ##------------start.from fitted model--------
  if(!is.null(start.from))
  {
    if (!is.gamlss(start.from))  
      stop(paste("The object in start.from is not a gamlss object", "\n", ""))
    mu.start <- NULL
    sigma.start <- NULL
    nu.start <- NULL
    tau.start <- NULL
    ##               location model
    if ("mu"%in%start.from$parameters)
    {
      mu.start <- start.from$mu.fv
    }
    ##               scale-dispersion submodel
    if ("sigma"%in%start.from$parameters) 
    {
      sigma.start <- start.from$sigma.fv
    }                
    ##               nu submodel
    if ("nu"%in%start.from$parameters)
    {
      nu.start <- start.from$nu.fv
    } 
    ##               tau submodel
    if ("tau"%in%start.from$parameters)
    {
      tau.start <- start.from$tau.fv 
    } 
  }
  ##-----------------------------------------------------------------------------------------
  ## checking the parameter.fix
  if (!is.logical(mu.fix)) stop("mu.fix should be logical TRUE or FALSE")
  if (!is.logical(sigma.fix)) stop("sigma.fix should be logical TRUE or FALSE")
  if (!is.logical(nu.fix)) stop("nu.fix should be logical TRUE or FALSE")
  if (!is.logical(tau.fix)) stop("tau.fix should be logical TRUE or FALSE")
  ##-----------------------------------------------------------------------------------------
  ## extract the weights
  w <- model.extract(mu.frame, weights) # weights for the likelihood
  if(is.null(w))   w <- rep(1, N)
  else if(any(w < 0)) stop("negative weights not allowed") #
  #   else if (!all(trunc(w)==w)) warning("weights should be integer values \n",
  #         " indicating number of observations with identical values \n") #
  ##=========================================================================================
  ##-----------------------------------------------------------------------------------------
  ##  Set up location-mean submodel:
  ##             mu.X   design matrix
  ##        mu.offset   offset in linear predictor
  ##         mu.start   starting values for mu (optional)
  ##-----------------------------------------------------------------------------------------
  mu.fit <- list() # MS Thursday, January 23, 2003 at 14:46
  mu.formula <- formula # ms Wednesday, December 29, 2004 
  mu.terms <- attr(mu.frame, "terms") #   it peeks up the terms attribute 
  mu.smoothers <- get.smoothers(mu.terms)  
  mu.a <- attributes(mu.terms)   #  from the model.frame
  mu.X <- model.matrix(mu.terms, mu.frame, contrasts) # the mean model matrix
  mu.offset <- model.extract(mu.frame, offset) # the mean-location offset
  if(is.null(mu.offset))     mu.offset <- rep(0,N)
  mu.object <- get.object("mu") 
  formals(mu.object$dldp, envir=new.env()) <- alist(mu = fv) # this is to get the right GLIM arguments 
  formals(mu.object$d2ldp2, envir=new.env()) <- alist(mu = fv) # 
  formals(mu.object$G.di, envir=new.env()) <- alist(mu = fv) #
  formals(mu.object$valid, envir=new.env()) <- alist(mu = fv) #
  ## initial values for mu
  if(!is.null(mu.start)) 
  { mu <- if(length(mu.start)>1) mu.start  else rep(mu.start,N)}
  else (eval(family$mu.initial)) #MS: Friday, March 29, 2002 at 11:27
  ##---------------------------------------------------------------------------------------
  ##  Set up dispersion-scale submodel:
  ##           sigma.X   design matrix
  ##           sigma.offset   offset in linear predictor
  ##       sigma.start   starting values for sigma (optional)
  ##---------------------------------------------------------------------------------------
  if ("sigma"%in%names(family$parameters)) 
  {
    orig.Envir  <- attr(mcall$formula, ".Environment")    # DS fix for Willem Thursday, March 18, 2010
    sigma.fit <- list() # MS Thursday, January 23, 2003 at 14:48    
    form.sigma <- other.formula(form = sigma.formula) 
    
    sigma.terms <- if(missing(data)) terms(form.sigma, specials = .gamlss.sm.list) 
    else terms(form.sigma, specials = .gamlss.sm.list, data = data)       
    mcall$formula <- sigma.terms
    attr(mcall$formula, ".Environment") <- orig.Envir # DS fix for Willem Thursday, March 18, 2010
    sigma.frame <- eval(mcall,sys.parent())    
    sigma.terms <- attr(sigma.frame, "terms")  
    sigma.smoothers <- get.smoothers(sigma.terms)  
    sigma.a <- attributes(sigma.terms)      
    sigma.X <- model.matrix(sigma.terms, sigma.frame, contrasts) 
    sigma.offset <- model.extract(sigma.frame, offset) 
    if(is.null(sigma.offset))   sigma.offset <- rep(0,N)
    sigma.object <- get.object("sigma")      
    formals(sigma.object$dldp, envir=new.env()) <- alist(sigma = fv) # 
    formals(sigma.object$d2ldp2, envir=new.env()) <- alist(sigma = fv) # 
    formals(sigma.object$G.di, envir=new.env()) <- alist(sigma = fv) #
    formals(sigma.object$valid, envir=new.env()) <- alist(sigma = fv) #
    formals(family$d2ldmdd, envir=new.env()) <- alist(sigma=sigma)#  ?? I do not think is needed
    ## initial values for sigma
    if(!is.null(sigma.start)) 
      sigma <- if(length(sigma.start)>1) sigma.start else rep(sigma.start,N)
    else eval(family$sigma.initial)  # 
  }
  ##-----------------------------------------------------------------------------------------
  ##  Set up for the 3rd parameter submodel:
  ##            nu.X   design matrix
  ##       nu.offset   offset in linear predictor
  ##        nu.start   starting values for nu (optional)
  ##-----------------------------------------------------------------------------------------
  if ("nu"%in%names(family$parameters))
  {
    nu.fit <- list() # MS Thursday, January 23, 2003 at 14:48    
    form.nu <- other.formula(form = nu.formula)  
    nu.terms <- if(missing(data)) terms(form.nu, specials = .gamlss.sm.list) 
    else terms(form.nu, specials = .gamlss.sm.list, data = data)
    mcall$formula <- nu.terms
    attr(mcall$formula, ".Environment") <- orig.Envir # DS fix for Willem Thursday, March 18, 2010
    nu.frame <- eval(mcall,sys.parent()) # ms Saturday, April 6, 2002 at 10:23   
    nu.terms <- attr(nu.frame, "terms")  
    nu.a <- attributes(nu.terms)   
    nu.smoothers <- get.smoothers(nu.terms)  
    nu.X <- model.matrix(form.nu, nu.frame, contrasts) 
    nu.offset <- model.extract(nu.frame, offset)
    if(is.null(nu.offset))  nu.offset <- rep(0,N)
    nu.object <- get.object("nu")      
    formals(nu.object$dldp, envir=new.env()) <- alist(nu = fv) # this is to get the right GLIM argument
    formals(nu.object$d2ldp2, envir=new.env()) <- alist(nu = fv) #    
    formals(nu.object$G.di, envir=new.env()) <- alist(nu = fv) #
    formals(nu.object$valid, envir=new.env()) <- alist(nu = fv) # 
    formals(family$d2ldmdv, envir=new.env()) <- alist(nu = nu)
    formals(family$d2ldddv, envir=new.env()) <- alist(nu = nu) 
    ## initial values for nu 
    if(!is.null(nu.start))
    {nu <- if(length(nu.start)>1) nu.start  else rep(nu.start,N)}
    else { eval(family$nu.initial) } # 
  }                    
  ##-----------------------------------------------------------------------------------------
  ##  Set up for the 4rd parameter submodel:
  ##            tau.X   design matrix
  ##       tau.offset   offset in linear predictor
  ##        tau.start   starting values for tau (optional)
  ##-----------------------------------------------------------------------------------------
  if ("tau"%in%names(family$parameters))  
  {   
    tau.fit <- list() # MS Thursday, January 23, 2003 at 14:48
    form.tau <- other.formula(form = tau.formula)  
    tau.terms <- if(missing(data)) terms(form.tau, specials = .gamlss.sm.list) 
    else terms(form.tau, specials = .gamlss.sm.list, data = data)
    mcall$formula <- tau.terms
    attr(mcall$formula, ".Environment") <- orig.Envir # DS fix for Willem Thursday, March 18, 2010
    tau.frame <- eval(mcall,sys.parent())
    tau.terms <- attr(tau.frame, "terms") # 
    tau.a <- attributes(tau.terms)   #
    tau.smoothers <- get.smoothers(tau.terms)  
    tau.X <- model.matrix(form.tau, tau.frame, contrasts) 
    tau.offset <- model.extract(tau.frame, offset) 
    if(is.null(tau.offset))  tau.offset <- rep(0,N) 
    tau.object <- get.object("tau")      
    formals(tau.object$dldp, envir=new.env()) <- alist(tau = fv) # 
    formals(tau.object$d2ldp2, envir=new.env()) <- alist(tau = fv) # 
    formals(tau.object$G.di, envir=new.env()) <- alist(tau = fv) #
    formals(tau.object$valid, envir=new.env()) <- alist(tau = fv) #
    formals(family$d2ldmdt, envir=new.env()) <- alist(tau = tau)
    formals(family$d2ldddt, envir=new.env()) <- alist(tau = tau)  
    formals(family$d2ldvdt, envir=new.env()) <- alist(tau = tau) 
    ## initial values for tau
    if(!is.null(tau.start)) 
    {tau <- if(length(tau.start)>1)  tau.start  else rep(tau.start,N)}
    else { eval(family$tau.initial) } # 
  } 
  ##-----------------------------------------------------------------------------------------
  ##-----------------------------------------------------------------------------------------
  ##=========================================================================================
  ##  Checking whether proper algorithm  (RS, CG or mixed) 
  ##=========================================================================================
  ##-----------------------------------------------------------------------------------------
  name.method <- substitute(method)
  name.method <- deparse(name.method[1])
  list.methods <- c("RS()","CG()","mixed()")
  i.method <- pmatch(name.method,list.methods,nomatch=0)
  if(!i.method) stop("Method must be RS(), CG() or mixed()")
  ##-----------------------------------------------------------------------------------------
  ## fitting the model
  fiter <- 0
  conv <- eval(substitute(method))
  method <- substitute(method)
  ##-----------------------------------------------------------------------------------------
  ##-----------------------------------------------------------------------------------------
  ##  Getting the GAMLSS object out
  ## ----------------------------------------------------------------------------------------
  ## first the general output 
  ## calculate the Global deviance again
  G.dev.incr  <- eval(G.dev.expr)
  G.dev <- sum(w*G.dev.incr)
  out <- list(family = family$family, parameters = names(family$parameters), 
              call = gamlsscall, y = y, control = control, weights = w, 
              G.deviance = G.dev, N = N, rqres = family$rqres, iter = fiter, 
              type = family$type, method = method, contrasts = contrasts) 
  #, na.action=na.act
  out$converged <- conv  
  out$residuals <- eval(family$rqres)
  noObs <- if(all(trunc(w)==w)) sum(w) else N 
  out$noObs <- noObs
  ## binomial denominator 
  if(any(family$family%in%.gamlss.bi.list))  out$bd <- bd        
  ##-----------------------------------------------------------------------------------------
  saveParam <- control$save
  ##  Output for mean model: ----------------------------------------------------------------
  if ("mu"%in%names(family$parameters))
  {
    out <- c(out, mu = parameterOut(what="mu", save=saveParam))
  }
  else
  {
    out$mu.df <- 0
  }
  ## define now the degrees of freedom for the fit and residuals
  out$df.fit <- out$mu.df
  out$pen <- out$mu.pen
  out$df.residual <- noObs-out$mu.df
  ## Output for dispersion model: ----------------------------------------------------------
  if ("sigma"%in%names(family$parameters))
  {
    out <- c(out, sigma = parameterOut(what="sigma", save=saveParam) )  
    out$df.fit <- out$mu.df + out$sigma.df
    out$pen <- out$mu.pen + out$sigma.pen
    out$df.residual <- noObs-out$mu.df-out$sigma.df
  }
  ##  output for nu ------------------------------------------------------------------------
  if ("nu"%in%names(family$parameters))
  {
    out <- c(out, nu = parameterOut(what="nu", save=saveParam) ) 
    out$df.fit <- out$mu.df+out$sigma.df+out$nu.df
    out$df.residual <- noObs-out$mu.df-out$sigma.df-out$nu.df
    out$pen <- out$mu.pen + out$sigma.pen + out$nu.pen
  }
  ##  output for tau -----------------------------------------------------------------------
  if ("tau"%in%names(family$parameters))
  {
    out <- c(out, tau = parameterOut(what="tau", save=saveParam) )       
    out$df.fit <- out$mu.df+out$sigma.df+out$nu.df+out$tau.df
    out$pen <- out$mu.pen + out$sigma.pen + out$nu.pen + out$tau.pen
    out$df.residual <- noObs-out$mu.df-out$sigma.df- out$nu.df -out$tau.df
  }
  ##=======================================================================================
  out$P.deviance <- out$G.deviance+out$pen # ms Thursday, May 13, 2004 
  out$aic <- G.dev+2*out$df.fit 
  out$sbc <- G.dev+log(noObs)*out$df.fit 
  #MS Thursday, April 22, 2004 at 11:26
  #  if ((ls(1,pattern="fiter")=="fiter")) rm(fiter, envir = as.environment(1)) 
  #MS Thursday, January 8, 2004 at 17:52
  class(out) <- c("gamlss","gam","glm","lm")
  out
}
#########################################################################################
##                          the END of gamlss                                         
#########################################################################################



#########################################################################################
##                         control gamlss                                            
#########################################################################################
gamlss.control <- function(c.crit = 0.001, n.cyc = 20, mu.step = 1, sigma.step=1, 
                           nu.step=1, tau.step=1, gd.tol = Inf, iter=0, trace = TRUE, 
                           autostep = TRUE, save = TRUE,  ...)
{
  ##  Control iteration for GAMLSS
  ##  Mikis Stasinopoulos Monday, March 25, 2002 at 16:17
  ## last change Thursday, October 12, 2006 save is added
  ##
  if(c.crit <= 0) {
    warning("the value of c.crit supplied is zero or negative the default value of 0.001 was used instead")
    c.crit <- 0.001}
  if(n.cyc < 1) {
    warning("the value of no cycles supplied is zero or negative the default value of 20 was used instead")
    n.cyc <- 20}
  if(iter < 0) {
    warning("the value of no iterations  supplied is  negative the default value of 0 was used instead")
    iter <- 0}
  if(mu.step > 1 | mu.step < 0) {
    warning("the value of mu.step supplied is less than zero or more than one the default value of 1 was used instead")
    mu.step <- 1}
  if(sigma.step > 1 | sigma.step < 0) {
    warning("the value of sigma.step supplied is less than zero or more than one the default value of 1 was used instead")
    sigma.step <- 1}
  if(nu.step > 1 | nu.step < 0) {
    warning("the value of nu.step supplied is less than zero or more than one the default value of 1 was used instead")
    nu.step <- 1}
  if(tau.step > 1 | tau.step < 0) {
    warning("the value of tau.step supplied is less than zero or more than one the default value of 1 was used instead")
    tau.step <- 1} 
  if(gd.tol < 0) {
    warning("the value of gd.tol supplied is less than zero the default value of Inf was used instead")
    gd.tol <- Inf}                            
  list(c.crit = c.crit, n.cyc = n.cyc, mu.step = mu.step, sigma.step = sigma.step, nu.step=nu.step, 
       tau.step = tau.step, gd.tol = gd.tol, iter = iter, trace = as.logical(trace)[1],
       autostep = as.logical(autostep)[1], save = as.logical(save)[1] )
}

##---------------------------------------------------------------------------------------
glim.control <- function(cc = 0.001, cyc = 50, glm.trace = FALSE,  bf.cyc = 30, #  step = 1,
                         bf.tol = 0.001, bf.trace = FALSE, ...)
{
  ##  Control iteration for GLIM
  ##  MS  Sunday, February 17, 2002 at 19:18
  ##
  if(cc <= 0) {
    warning("the value of cc supplied is zero or negative the default value of 0.001 was used instead")
    cc <- 0.001}
  if(bf.tol <= 0) {
    warning("the value of bf.tol supplied is zero or negative the default value of 0.001 was used instead")
    bf.tol <- 0.001}                
  if(cyc < 1) {
    warning("the value of cyc supplied is zero or negative the default value of 20 was used instead")
    cyc <- 20}
  if(bf.cyc < 1) {
    warning("the value of bf.cyc supplied is zero or negative the default value of 30 was used instead")
    bf.cyc <- 30}                
  #        if(step > 1 | step < 0) {
  # warning("the value of step supplied is less than zero or more than one the default value of 1 was used instead")
  #                step <- 1}        
  list(cc = cc, cyc = cyc,  glm.trace = as.logical(glm.trace)[1], bf.cyc = bf.cyc, # step = step,
       bf.tol = bf.tol, bf.trace = as.logical(bf.trace)[1] )# MS Thursday, January 8, 2004 at 17:53
}
##----------------------------------------------------------------------------------------