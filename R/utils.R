
must.have.args <- function(supplied.args, mandatory.args)
{
    msg <- ': argument(s) missing in call to function '

    missing.args <- NULL

    for(arg in mandatory.args)
    {
        if(length(grep(paste('^',arg,'$',sep=''), names(as.list(supplied.args)))) == 0)
        {
            if(is.null(missing.args))
                missing.args <- arg
            else
                missing.args <- paste(missing.args, ', ', arg)
        }
    }
    if(length(missing.args) > 0)
    {
        funcname <- as.character(sys.call(-1)[[1]])

        stop(paste(missing.args, msg, funcname, sep=''))
    }
}

must.be.strategy <- function(strategy)
{
    if(!is.strategy(strategy))
    {
        strategy<-try(getStrategy(strategy))

        if(inherits(strategy,"try-error"))
            stop(paste(strategy, ': not a strategy'))
    }
    return(strategy)
}

must.be.portfolio <- function(portfolio)
{
    if(!is.portfolio(portfolio))
    {
        portfolio<-try(getPortfolio(portfolio))

        if(inherits(portfolio,"try-error"))
            stop(paste(portfolio, ': not a portfolio'))
    }
}

