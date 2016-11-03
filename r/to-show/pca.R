# 3-pca
#rm(list=ls())
setwd('~/Documents/a-mmm-lab/a-ml/data')
cdata = read.csv('6-beta.csv', header=T)

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

real.data.missing = data.frame(cdata$distance_to_grain_boundary, cdata$unit_vector_to_grain_boundary_x, cdata$unit_vector_to_grain_boundary_y, cdata$unit_vector_to_grain_boundary_z, 
                               cdata$phi1, cdata$Phi, cdata$phi2, cdata$misorientation, cdata$scaled_angle)

real.data.b = real.data.missing[complete.cases(real.data.missing),]
real.data = my.scale(real.data.b)
pc = prcomp(real.data, scale=T)
plot(pc, type='l')
summary(pc)

pc.2 = prcomp(real.data, scale=T, tol = .75)
summary(pc.2)
plot(pc.2)

library(ggfortify)
autoplot(pc.2)





# real.data.missing = data.frame(cdata$dadN, cdata$beta, cdata$change_in_z, 
#                       cdata$crack_id, cdata$theta, cdata$x, cdata$y, 
#                       cdata$z, cdata$grain_id, cdata$nearest_grain_boundary_x,
#                       cdata$nearest_grain_boundary_y, cdata$nearest_grain_boundary_z, 
#                       cdata$nearest_grain_boundary_id, cdata$distance_to_grain_boundary, 
#                       cdata$vector_to_grain_boundary_x, cdata$vector_to_grain_boundary_y, 
#                       cdata$vector_to_grain_boundary_z, cdata$phi1, cdata$Phi, cdata$phi2, 
#                       cdata$nearest_grain_phi1, cdata$nearest_grain_Phi, cdata$nearest_grain_phi2, 
#                       cdata$misorientation, cdata$normal_x, cdata$normal_y, cdata$normal_z, 
#                       cdata$scaled_angle)