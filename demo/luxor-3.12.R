#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - August 2012
#
# Tested and found to work correctly using blotter r1143
#
# From Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#
# Figure 3.12: MAE graph in percentage terms

require('blotter')

data('luxor-p066', package='quantstrat', envir=.blotter)

chart.ME('luxor', type='MAE', scale='percent')
