\name{sigPeak}
\alias{sigPeak}
\title{signal function for peak/valley signals}
\usage{
  sigPeak(label, data, column,
    direction = c("peak", "bottom"))
}
\arguments{
  \item{label}{text label to apply to the output}

  \item{data}{data to apply comparison to}

  \item{column}{named column to apply comparison to}

  \item{direction}{one of "peak" or "bottom" to calculate
  peaks for}
}
\description{
  This function tests to see if the mktdata or indicator
  \code{column} observations on either side are
  lower(higher) creating a local peak(bottom).
}

