# dadN
plot(test.errors.gamma.dadN, type='l')
abline(h=mean.error.dadN, col='red', lwd=3)

plot(test.errors.cost.dadN, type='l')
abline(h=mean.error.dadN, col='red', lwd=3)

# Beta
plot(test.errors.gamma.beta, type='l')
abline(h=mean.error.beta, col='red', lwd=3)

plot(test.errors.cost.beta, type='l')
abline(h=mean.error.beta, col='red', lwd=3)

# Change in Z
plot(test.errors.gamma.z, type='l')
abline(h=mean.error.z, col='red', lwd=3)

plot(test.errors.cost.z, type='l')
abline(h=mean.error.z, col='red', lwd=3)
