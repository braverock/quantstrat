[![Travis build status](https://travis-ci.org/braverock/quantstrat.svg?branch=master)](https://travis-ci.org/braverock/quantstrat)

# quantstrat - Quantitative Strategy Model Framework

Specify, build, and back-test quantitative financial trading and portfolio strategies.

## Installing

In order to install quantstrat from github, you will need to install devtools.

```
install.packages("devtools")
```

then

```
require(devtools)
install_github("braverock/quantstrat")
```

If you can run one of the demo files, you would have successfully installed blotter.

```
demo('bbands', ask=FALSE)
```

### Prerequisites

There are a few dependencies for _quantstrat_, namely:

* R (>= 3.0.0)
* quantmod
* xts(>= 0.10)
* blotter(>= 0.14.0)
* FinancialInstrument(>= 0.12.5)
* foreach(>= 1.4.0)

Imports:

* methods
* iterators
* zoo
* TTR
* MASS

Suggests:

* PerformanceAnalytics
* PortfolioAnalytics
* rgl
* testthat
* rCharts
* gamlss.util
* reshape2
* beanplot
* knitr
* rmarkdown

## Authors, Creators and Contributors

* Peter Carl [aut]
* Brian G. Peterson [aut, cre]
* Joshua Ulrich [aut]
* Jan Humme [aut]
* Yu Chen [ctb]
* Joe Dunn [ctb]
* Dirk Eddelbuettel [ctb]
* Michael Guan [ctb]
* Jeffrey A. Ryan [ctb]
* Garrett See [ctb]

## License

This project is licensed under GPL-3. See https://www.gnu.org/licenses/gpl-3.0.en.html for details.
