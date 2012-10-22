\name{add.distribution}
\alias{add.distribution}
\title{Adds a distribution to a paramset in a strategy}
\usage{
  add.distribution(strategy, paramset.label,
    component.type, component.label, variable,
    weight = NULL, label)
}
\arguments{
  \item{strategy:}{the name of the strategy object}

  \item{paramset.label:}{a label uniquely identifying the
  paramset within the strategy}

  \item{component.type:}{one of c('indicator', 'signal',
  'order', 'enter', 'exit', chain')}

  \item{component.label:}{a label identifying the
  component. must be unique per component type}

  \item{variable:}{the name of the variable in the
  component}

  \item{label:}{a label uniquely identifying the
  distribution within the paramset}
}
\description{
  Adds a distribution to a paramset in a strategy
}
\author{
  Jan Humme
}

