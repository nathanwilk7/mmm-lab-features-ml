# Set up data

rm(list=ls())
setwd('~/Documents/a-mmm-lab/a-ml/data')
data = read.csv('6-beta.csv', header=T)

# User parameters

#set.seed(351734895)
num.samples = 100000
num.folds = 5
costs = c(.0001, .001, .01, .1, 1, 10, 100, 1000)
gammas = c(.0001, .001, .01, .1, 1, 10, 100, 1000)

# Calculated parameters

num.test.samples = num.samples %/% num.folds
num.tune.samples = num.test.samples
num.train.samples = num.samples - (num.test.samples + num.tune.samples)

folds = 1:num.folds
theta.interval = pi / num.folds

num.models = 1 # length(costs) * length(gammas)

# We don't want this column because it's not numeric

data['on_crack_front'] = NULL

# Scale function

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

library(tree)

# Store results

cv.tune.mses <- vector(mode = "list", length = num.folds)
cv.test.mses <- vector(mode = "list", length = num.folds)
cv.best.index <- vector(mode = "list", length = num.folds)
cv.to.plot <- vector(mode = "list", length = num.folds)

# The Cross Validation Loop

for (fold in folds)
{
  print(fold)
  
  # Calculate the beginning and end of the test section and get the test data
  test.theta.begin = fold * theta.interval - theta.interval
  test.theta.end = fold * theta.interval
  test.data = data[data[,'theta'] > test.theta.begin & data[,'theta'] < test.theta.end,]
  
  # Choose the tune section and get the tune data
  tune.section.id = sample(folds[-fold], 1)
  tune.theta.begin = tune.section.id * theta.interval - theta.interval
  tune.theta.end = tune.section.id * theta.interval
  tune.data = data[data[,'theta'] > tune.theta.begin & data[,'theta'] < tune.theta.end,]
  
  # Get the train data, which is everything that's not test/tune
  train.data = data[(data[,'theta'] < test.theta.begin & data[,'theta'] < tune.theta.begin |
                       data[,'theta'] < test.theta.begin & data[,'theta'] > tune.theta.end |
                       data[,'theta'] > test.theta.end & data[,'theta'] < tune.theta.begin |
                       data[,'theta'] > test.theta.end & data[,'theta'] > tune.theta.end),]
  
  # Sample from the data
  test.samples = test.data[sample(nrow(test.data), num.test.samples), ]
  tune.samples = tune.data[sample(nrow(tune.data), num.tune.samples), ]
  train.samples = train.data[sample(nrow(train.data), num.train.samples), ]
  
  # Scale the sampled data
  test.samples.scaled = my.scale(test.samples)
  tune.samples.scaled = my.scale(tune.samples)
  train.samples.scaled = my.scale(train.samples)
  
  # Set up the training data
  train.input = data.frame(train.samples.scaled$distance_to_grain_boundary, train.samples.scaled$unit_vector_to_grain_boundary_x, train.samples.scaled$unit_vector_to_grain_boundary_y, train.samples.scaled$unit_vector_to_grain_boundary_z, 
                           train.samples.scaled$phi1, train.samples.scaled$Phi, train.samples.scaled$phi2, train.samples.scaled$misorientation, train.samples.scaled$scaled_angle)
  colnames(train.input) = c('distance_to_grain_boundary', 'unit_vector_to_grain_boundary_x', 'unit_vector_to_grain_boundary_y', 'unit_vector_to_grain_boundary_z', 'phi1', 'Phi', 'phi2', 'misorientation', 'scaled_angle')
  train.dadN = train.samples.scaled$dadN
  train.beta = train.samples.scaled$beta
  train.cz = train.samples.scaled$change_in_z
  
  # Set up the tuning data
  tune.input = data.frame(tune.samples.scaled$distance_to_grain_boundary, tune.samples.scaled$unit_vector_to_grain_boundary_x, tune.samples.scaled$unit_vector_to_grain_boundary_y, tune.samples.scaled$unit_vector_to_grain_boundary_z, 
                          tune.samples.scaled$phi1, tune.samples.scaled$Phi, tune.samples.scaled$phi2, tune.samples.scaled$misorientation, tune.samples.scaled$scaled_angle)
  colnames(tune.input) = c('distance_to_grain_boundary', 'unit_vector_to_grain_boundary_x', 'unit_vector_to_grain_boundary_y', 'unit_vector_to_grain_boundary_z', 'phi1', 'Phi', 'phi2', 'misorientation', 'scaled_angle')
  tune.dadN = tune.samples.scaled$dadN
  tune.beta = tune.samples.scaled$beta
  tune.cz = tune.samples.scaled$change_in_z
  
  # Set up the testing data
  test.input = data.frame(test.samples.scaled$distance_to_grain_boundary, test.samples.scaled$unit_vector_to_grain_boundary_x, test.samples.scaled$unit_vector_to_grain_boundary_y, test.samples.scaled$unit_vector_to_grain_boundary_z, 
                          test.samples.scaled$phi1, test.samples.scaled$Phi, test.samples.scaled$phi2, test.samples.scaled$misorientation, test.samples.scaled$scaled_angle)
  colnames(test.input) = c('distance_to_grain_boundary', 'unit_vector_to_grain_boundary_x', 'unit_vector_to_grain_boundary_y', 'unit_vector_to_grain_boundary_z', 'phi1', 'Phi', 'phi2', 'misorientation', 'scaled_angle')
  test.dadN = test.samples.scaled$dadN
  test.beta = test.samples.scaled$beta
  test.cz = test.samples.scaled$change_in_z
  
  models <- vector(mode = "list", length = num.models)
  i = 1
  
  print('Creating Models')
  
  # Basic tree model
  model = tree(train.dadN~., train.input)
  
  # Pruned tree model
  model = cv.tree(model)
  models[[i]] = model
  # for (my.cost in costs)
  # {
  #   for (my.gamma in gammas)
  #   {
  #     model <- svm(train.dadN~distance_to_grain_boundary+unit_vector_to_grain_boundary_x+unit_vector_to_grain_boundary_y+unit_vector_to_grain_boundary_z+phi1+phi2+misorientation+scaled_angle, data=train.input, kernel ="radial", 
  #                  cost=my.cost, gamma=my.gamma)
  #     models[[i]] = model
  #     i = i + 1
  #   }
  # }
  
  train.mse.dadN <- vector(mode = "list", length = num.models)
  tune.mse.dadN <- vector(mode = "list", length = num.models)
  
  minimum.tune.mse = 999
  minimum.index = 1
  
  print('Evaluating Models')
  for (i in 1:num.models)
  {
    train.pred.dadN = predict(models[[i]], train.input)
    train.mse = sum((train.pred.dadN - train.dadN)^2) / length(train.dadN)
    tune.pred.dadN = predict(models[[i]], tune.input)
    tune.mse = sum((tune.pred.dadN - tune.dadN)^2) / length(tune.dadN)
    
    train.mse.dadN[i] = train.mse
    tune.mse.dadN[i] = tune.mse
    
    if (tune.mse < minimum.tune.mse)
    {
      minimum.tune.mse = tune.mse
      minimum.index = i
    }
  }
  # Save the best error and index of parameters
  cv.tune.mses[fold] = minimum.tune.mse
  cv.best.index[fold] = minimum.index
  cv.to.plot[[fold]] = matrix(tune.mse.dadN, nrow=length(gammas), ncol=length(costs))
  
  test.pred.dadN = predict(models[[minimum.index]], test.input)
  test.mse.dadN = sum((test.pred.dadN - test.dadN)^2) / length(test.dadN)
  cv.test.mses[fold] = test.mse.dadN
}

# Print Test MSEs

my.sum = 0
for (i in 1:length(cv.test.mses))
{
  my.sum = my.sum + cv.test.mses[[i]]
}
result = my.sum / length(cv.test.mses)
print(result)

# Print all Test MSE

for (i in 1:length(cv.test.mses))
{
  print(cv.test.mses[[i]])
}

library(lattice)

unlist(cv.test.mses)
levelplot(cv.to.plot[[5]], at=c(.8,.85,.9,1,1.05,1.1), xlab='Gammas', ylab='Costs')
