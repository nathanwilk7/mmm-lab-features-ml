# Data Exploration

rm(list=ls())
setwd('~/Documents/a-mmm-lab/a-ml/data')
cdata = read.csv('11-delta-taylor-factor.csv', header=T)

# a ton of these are very small, why?
# answer: There are a ton of points on crack surface 0 that have 
#  a very small dadN
# hist(cdata$dadN)
# 
# small.dadN = subset(cdata, cdata$dadN < .0005)
# hist(small.dadN$dadN)
# hist(small.dadN$crack_id)
# summary(small.dadN$crack_id)
# small.dadN.not.zero = subset(small.dadN, small.dadN$crack_id > 0)
# hist(small.dadN.not.zero$dadN)
# hist(small.dadN.not.zero$crack_id)
# summary(small.dadN.not.zero$crack_id)

# remove crack_id = 0
cdata = subset(cdata, cdata$crack_id > 0)
cdata = subset(cdata, cdata$on_crack_front == 'True') # 1 is for true
summary(cdata)

hist(cdata$signed_delta_taylor_factor)
plot(cdata$signed_delta_taylor_factor, cdata$dadN)
lm = lm(dadN~signed_delta_taylor_factor, cdata)
summary(lm)
abline(2.840e-03,-2.516e-04)
hist(cdata$curvature_distance)
plot(cdata$curvature_distance, cdata$dadN)
hist(cdata$previous_dadN)
hist(cdata$dadN)
plot(cdata$previous_dadN, cdata$dadN)
lm = lm(dadN~curvature_distance, data=cdata)
summary(lm)
abline(2.756e-03, -1.874e-04)
zeros.col = rep(0, length(cdata$dadN))
cdata = subset(cdata, cdata$theta > 1)
cdata = subset(cdata, cdata$theta <= 1.1)
plot(cdata$theta, cdata$dadN)
hist(cdata$dadN)
plot(cdata$dadN, cdata$dadN)
hist(cdata$beta)
plot(cdata$beta, cdata$dadN)
hist(cdata$change_in_z)
plot(cdata$change_in_z, cdata$dadN)
hist(cdata$crack_id)
plot(cdata$crack_id, cdata$dadN)
hist(cdata$theta)
plot(cdata$theta, cdata$dadN)
hist(cdata$x)
plot(cdata$x, cdata$dadN)
hist(cdata$y)
plot(cdata$y, cdata$dadN)
hist(cdata$z)
plot(cdata$z, cdata$dadN)
hist(cdata$grain_id)
hist(cdata$nearest_grain_boundary_x)
plot(cdata$distance_to_grain_boundary, cdata$dadN)
hist(cdata$taylor_factor)
plot(cdata$taylor_factor, cdata$dadN)

# Combining paraview for vis
library(lattice)
cloud(cdata$dadN~cdata$taylor_factor*cdata$distance_to_grain_boundary)
cloud(cdata$dadN~cdata$taylor_factor*cdata$distance_to_grain_boundary, 
      screen = list(x = 135, y = 45, z = 0))

cdata['on_crack_front'] = NULL

my.scale=function(df)
{
  for (i in 1:ncol(df))
  {
    new.col = (df[,i] - mean(df[,i])) / sd(df[,i])
    if (i == 1)
    {
      scaled.df = data.frame(new.col)
    }
    else
    {
      scaled.df = data.frame(scaled.df, new.col)
    }
  }
  colnames(scaled.df) = colnames(df)
  return(scaled.df)
}
cdata = my.scale(cdata)
zeros.col = rep(0, length(cdata$dadN))
cdata.write = cbind(cdata, zeros.col)
write.csv(cdata.write, 'vis/cdata-scaled.csv')
plot(cdata$curvature_distance, cdata$dadN)
summary(lm)
heatmap.data = cbind(cdata$distance_to_grain_boundary, cdata$taylor_factor, cdata$dadN)
my.heatmap <- heatmap(heatmap.data, Rowv=NA, Colv=NA, 
                      col = heat.colors(256), scale="column", 
                      margins=c(5,10))
library(gplots)

my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
col_breaks = c(seq(-3,-1,length=100), # for red
               seq(-.999,.999,length=100),  # for yellow
               seq(1,3,length=100)) # for green

heatmap.2(heatmap.data,
          cellnote = heatmap.data,  # same data set for cell labels
          main = "Correlation", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(12,9),     # widens margins around plot
          col=my_palette,       # use on color palette defined earlier
          breaks=col_breaks,    # enable color transition at specified limits
          dendrogram="row",     # only draw a row dendrogram
          Colv="NA")            # turn off column clustering
