#' match names in data to a list of partial name matches
#' 
#' Often, the generic definition of a signal or indicator will include 
#' partial name matches.  In financial data, common partial matches include
#' 'Close', 'Open', and 'Volume', but there are many more.  
#' 
#' In complex data, additional name information may be added to column names
#' for example, a symbol or an indicator of some adjustment may be added.
#' 
#' This small utility exists to do the matching in a centralized location 
#' so that more robust error handling and reporting can be conducted.
#'    
#' @param data_names names for the data to be matched to
#' @param match_names names to match
#' @export
match.names <- function(match_names,data_names) {
    loc<-NULL
    for (mname in match_names){
        t<-grep(paste(mname,"$",sep=""),data_names)
        if(is.null(loc)) loc<-t
        else loc <- c(loc,t)
    }
    if ( !identical(length(loc),length(match_names)) ) {
        mstr<-paste(match_names,collapse=' ')
        dstr<-paste(data_names,collapse=' ')
        warning(paste("all columns not located in",mstr,"for",dstr))  
    }
    return(loc)
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
