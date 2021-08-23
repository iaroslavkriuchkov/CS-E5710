x <- seq(0, 1, length.out = 10000)
m = .2
sigmasq = .01

alpha = m * (m * (1 - m) / sigmasq - 1)
beta = alpha * (1 - m) / m

plot(x, dbeta(x, alpha, beta))

sample1000 = rbeta(1000, alpha, beta, ncp=0)
hist(sample1000)

mean(sample1000)
var(sample1000)
left_bound = mean(sample1000) - quantile(sample1000, probs = 0.95)
right_bound = mean(sample1000) + quantile(sample1000, probs = 0.95)
