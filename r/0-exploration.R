setwd('~/Documents/a-mmm-lab/a-ml/data')
crack_data = read.csv('5-crack-surface-normal.csv', header=T)
names(crack_data)
attach(crack_data)

# Histograms
hist(crack_id)
hist(crack_data$theta)

hist(crack_data$x)
hist(crack_data$y)
hist(crack_data$z) # The crack surface tended to go more down than up
hist(crack_data$nearest_grain_boundary_x)
hist(crack_data$nearest_grain_boundary_y)
hist(crack_data$nearest_grain_boundary_z)

# The growth rate is somewhat normally distributed but tails off to the
#  right more gradually than expected or the mean is a bit left of expected.
hist(crack_data$dadN3Dline)

hist(crack_data$grain_id)
hist(crack_data$nearest_grain_boundary_id)

# By far, most grain boundaries are within just a few microns...
hist(crack_data$distance_to_grain_boundary)
hist(crack_data$magnitude_of_vector_to_grain_boundary)

hist(crack_data$vector_to_grain_boundary_x)
hist(crack_data$vector_to_grain_boundary_y)
hist(crack_data$vector_to_grain_boundary_z)

# Why are these more uniformly distributed?
hist(crack_data$unit_vector_to_grain_boundary_x)
hist(crack_data$unit_vector_to_grain_boundary_y)
hist(crack_data$unit_vector_to_grain_boundary_z)

# Why do so many grains share the same phi orientations?
hist(crack_data$phi1)
hist(crack_data$Phi)
hist(crack_data$phi2)

# These are interestingly different from the starting phis
hist(crack_data$nearest_grain_phi1)
hist(crack_data$nearest_grain_Phi)
hist(crack_data$nearest_grain_phi2)

hist(crack_data$misorientation)
hist(crack_data$normal_x)
hist(crack_data$normal_y)
hist(crack_data$normal_z)

# The nearest grain boundary tends to be in the way
hist(crack_data$raw_angle)
hist(crack_data$scaled_angle)

summary(crack_data)

# The third and last cracks grew especially much
plot(crack_data$crack_id, dadN3Dline)

# Theta being near 0 and pi may be related to growth
plot(crack_data$theta, dadN3Dline)

plot(crack_data$x, dadN3Dline)
plot(crack_data$y, dadN3Dline)

# Shows that the z direction is obviously a great indicator of growth.
#  I wonder if we should also have polar coordinates saved?
plot(crack_data$z, dadN3Dline)

# Most grains seems to have huge ranges of growth rates. As grain numbers
# go up, they grow more
plot(crack_data$grain_id, dadN3Dline)
plot(crack_data$nearest_grain_boundary_id, dadN3Dline)

plot(crack_data$nearest_grain_boundary_x, dadN3Dline)
plot(crack_data$nearest_grain_boundary_y, dadN3Dline)
plot(crack_data$nearest_grain_boundary_z, dadN3Dline)

# Seems to be highest in the middle...? That seems super backward
plot(crack_data$vector_to_grain_boundary_x, dadN3Dline)
plot(crack_data$vector_to_grain_boundary_y, dadN3Dline)
plot(crack_data$vector_to_grain_boundary_z, dadN3Dline)

# Possible negative relationship
plot(crack_data$distance_to_grain_boundary, dadN3Dline)
plot(crack_data$magnitude_of_vector_to_grain_boundary, dadN3Dline)

# Outliers stop before getting too low
plot(crack_data$unit_vector_to_grain_boundary_x, dadN3Dline)
plot(crack_data$unit_vector_to_grain_boundary_y, dadN3Dline)

# Same but too high
plot(crack_data$unit_vector_to_grain_boundary_z, dadN3Dline)

# Start and nearest are very similar, why?
# Possible trend between extremes of phi2 and more growth
plot(crack_data$phi1, dadN3Dline)
plot(crack_data$Phi, dadN3Dline)
plot(crack_data$phi2, dadN3Dline)

plot(crack_data$nearest_grain_phi1, dadN3Dline)
plot(crack_data$nearest_grain_Phi, dadN3Dline)
plot(crack_data$nearest_grain_phi2, dadN3Dline)

# Possibly more misorientation means less growth?
plot(crack_data$misorientation, dadN3Dline)

plot(crack_data$normal_x, dadN3Dline)
plot(crack_data$normal_y, dadN3Dline)

# Why are all the normal_z's about the same?
plot(crack_data$normal_z, dadN3Dline)

# Doesn't appear to be much trend here but there may be a negative one
plot(crack_data$scaled_angle, dadN3Dline)
plot(crack_data$raw_angle, dadN3Dline)

lm.fit=lm(dadN3Dline~distance_to_grain_boundary,data=crack_data)
lm.fit
summary(lm.fit)

confint(lm.fit)
plot(distance_to_grain_boundary, dadN3Dline)
abline(lm.fit,lwd=3,col='red')
par(mfrow=c(2,2))
plot(lm.fit)

lm.fit=lm(dadN3Dline~.,data=crack_data)
summary(lm.fit)

lm.fit=lm(dadN3Dline~scaled_angle+distance_to_grain_boundary,
          data=crack_data)
summary(lm.fit)

lm.fit=lm(dadN3Dline~distance_to_grain_boundary)
summary(lm.fit)
