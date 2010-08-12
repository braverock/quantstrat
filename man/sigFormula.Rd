\name{sigFormula}
\alias{sigFormula}
\title{generate a signal from a formula...}
\usage{sigFormula(label, data=mktdata, formula, cross=FALSE)}
\description{generate a signal from a formula}
\arguments{\item{label}{text label to apply to the output}
\item{data}{data to apply formula to}
\item{formula}{a logical expression like that used in an if statement, will typically reference column names in \code{mktdata}}
\item{cross}{if TRUE, will return TRUE only for the first observation to match the formula in a run}}
