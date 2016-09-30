# deal: https://www.r-project.org/conferences/DSC-2003/Proceedings/BottcherDethlefsen.pdf
install.packages('deal')
library(deal)
setwd('~/Documents/a-mmm-lab/a-ml/data')
cdata = read.csv('5-crack-surface-normal.csv', header=T)
names(cdata)
netdata = cdata[c(6,12,26,31)]
?drawnetwork
netdata.nw = network(netdata)
# Breaks b/c there's only continuous, figure out how to stick a dummy var in or something
nw.prior = jointprior(netdata.nw)