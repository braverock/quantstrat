require(testthat)
require(quantstrat)

test_package("quantstrat", filter="paramsets")
test_package("quantstrat", filter="osMaxPos")

