#######################
########Simulation#####
#######################

data <- replicate(1000, sum(sample(c("H","T"), 4, replace = TRUE)=="H"))
table(data)/1000
barplot(table(data)/1000)

barplot(table(rbinom(1000, size = 4, prob = 1/2)))
#?Hypergeometric(N=10, n=5, k=6)
rhyper(100, m = 6, n = 4, k = 5) #m white balls, n black balls, k number of balls to draw

rpois(100, lambda = 3)

hist(runif(10000, min = 2, max = 5))

hist(rexp(1000, rate = 2))

hist(rnorm(1000, mean = 2, sd = 3))

#arbitary given discrete distribution: x = {}, probablity={}
arbi <- replicate(1000, sample(c(1,3,6,10), 1, replace = TRUE, prob = c(0.3,0.1,0.2,0.4)))
barplot(table(arbi)/1000)

wb = sample(c("W","B"), 50, replace = TRUE, prob = c(0.8, 0.2))
mean(wb == "W")

#white if 0.6 is the truth
data60 <- replicate(1000, rbinom(1, 50, 0.6)/50)
hist(data60)
points(mean(wb=="W"),0, pch=20, col = "red")
mean(data60>mean(wb=="W"))
#prove by contradiction
#inconsistant with ...
#since there is only ...


###############
#Sample distribution of sample mean
################Central limit theorem########

#x_i ~ poisson(lambda = 5), i = 1,2,...50

#want to know Xbar_50 distribution
hist(replicate(1000, mean(rbinom(500, 50, 0.3))))
hist(replicate(1000, mean(rpois(500,5))))
     
     