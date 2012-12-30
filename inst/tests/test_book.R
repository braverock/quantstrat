require(testthat)

######################## DEFINE VARIABLES TO BE TESTED #######

source('blue.R')

rule   = book$bluePort$spx[,'Rule']
status = book$bluePort$spx[,'Order.Status']
qty    = book$bluePort$spx[,'Order.Qty']



######################## RUN TEST SUITE ######################

context("Order Book is consistent ")

test_that("The first entry is not a trade", 
          { expect_that(as.character(rule[1]) =="", is_true()) })

test_that("The first trade is a long entry", 
          { expect_that(as.character(rule[3]) =="EnterLONG", is_true()) })

test_that("The first short exit order is rejected", 
          { expect_that(as.character(status[2]) =="rejected", is_true()) })

test_that("The first long signal is entered long", 
          { expect_that(as.character(qty[3]) =="100", is_true()) })

test_that("The first short signal is entered short", 
          { expect_that(as.character(qty[5]) =="-100", is_true()) })
