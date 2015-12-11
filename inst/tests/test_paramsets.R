require(testthat)
context("paramset warnings")

options(in_test=TRUE)

require(quantstrat)
strategy.st <- "paramset_test"
strategy(strategy.st, store=TRUE)
.timespans <- c("T08:00/T10:00")

add.distribution(strategy.st,
	paramset.label = 'Timespan',
	component.type = 'enter',
	component.label = 'EnterLONG',
	variable = list(timespan = .timespans),
	label = 'Timespan')

test_that("Replacing paramset distribution warns", expect_warning({
    add.distribution(strategy.st,
	    paramset.label = 'Timespan',
	    component.type = 'enter',
	    component.label = 'EnterLONG',
	    variable = list(timespan = .timespans),
	    label = 'Timespan')
}))

add.distribution.constraint(strategy.st,
    paramset.label = 'Timespan',
    distribution.label.1 = 'EnterLong',
    distribution.label.2 = 'EnterShort',
    operator = '==',
    label = 'Timespan')

test_that("Replacing paramset constraint warns", expect_warning({
    add.distribution.constraint(strategy.st,
	    paramset.label = 'Timespan',
	    distribution.label.1 = 'EnterLong',
	    distribution.label.2 = 'EnterShort',
	    operator = '==',
	    label = 'Timespan')
}))

test_that("Deleting unknown paramset warns", expect_warning({
  delete.paramset(strategy.st, "Timespans")
}))

