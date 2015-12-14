#!/usr/bin/Rscript --vanilla
#
# Jan Humme (@opentrades) - August 2012
#
# Tested and found to work correctly using blotter r1457
#
# After Jaekle & Tamasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
#
# Walk forward chart sample

require('quantstrat')

chart.forward.training(paste0(path.package("quantstrat"),'/data/luxor.wfa.ples.RData'))

