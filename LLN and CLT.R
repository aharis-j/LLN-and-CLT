# Probability of observing "heads"
p <- 0.3

####################################
########### LLN Example ############
####################################

# A sequence of sample sizes used to show convergence of sample mean to mu
samp.seq <- seq(1, 20001, by = 100)

# Calculating sample means for each sample size contained in samp.seq
mean.seq <- vector("numeric", length = length(samp.seq))
for(i in samp.seq){
  mean.seq[which(samp.seq == i)] <- mean(ifelse(runif(i) <= p, 1, 0))
}

# Generating plot showing convergence of sample mean to mu
plot(samp.seq, mean.seq, type = "l", main = "Values of sample mean for different sample sizes", xlab = "sample size", ylab = "sample mean", xlim = c(11, 20000), ylim = c(0.25, 0.35), cex = 0.5)
points(samp.seq, mean.seq, pch = 19, cex = 0.5)
abline(h = p, col = "red", lwd = 2)

####################################
########### CLT Example ############
####################################

# Plotting asymptotic normal distributions for sample sizes 201, 501, 1001, 20001
time.seq <- seq(0.2, 0.4, by = 0.00001)
plot(time.seq, dnorm(time.seq, p, sqrt(p*(1-p)/samp.seq[which(samp.seq == 1001)])), type = "n", main = "Distribution of sample mean", xlab = "probability of heads", ylab = "density", ylim = c(0, 120))
lines(time.seq, dnorm(time.seq, p, sqrt(p*(1-p)/samp.seq[which(samp.seq == 201)])), col = "blue")
lines(time.seq, dnorm(time.seq, p, sqrt(p*(1-p)/samp.seq[which(samp.seq == 501)])), col = "purple")
lines(time.seq, dnorm(time.seq, p, sqrt(p*(1-p)/samp.seq[which(samp.seq == 1001)])), col = "orange")
lines(time.seq, dnorm(time.seq, p, sqrt(p*(1-p)/samp.seq[which(samp.seq == 20001)])), col = "red")
legend("topright", c(expression("n = 201"), expression("n = 501"), expression("n = 1001"), expression("n = 20001")), lty = c("solid", "solid", "solid", "solid"), col = c("blue", "purple", "orange", "red"))      

# Generating histograms of sample means: 1000 replications for each sample size 10 and 1000
# No. of replications
rep <- 1000
# sample size 10
samp <- 10
# vector of sample means
mean.samp <- vector("numeric", length = rep)

for(i in 1:rep){
  mean.samp[i] <- mean(ifelse(runif(samp) <= p, 1, 0))
}
# Histogram associated with sample size 10
hist1 <- hist(mean.samp, freq = F)

# sample size 1000
samp <- 1000
for(i in 1:rep){
  mean.samp[i] <- mean(ifelse(runif(samp) <= p, 1, 0))
}
# Histogram associated with sample size 1000
hist2 <- hist(mean.samp, freq = F)

# Plotting histograms
plot(hist1, freq = F, col = rgb(0, 0, 1, alpha = 1), xlim = c(0, 0.8), ylim = c(0, 30), main = "Histogram of sample mean", xlab = "probability of heads")
lines(seq(0, 1, by = 0.001), dnorm(seq(0, 1, by = 0.001), p, sqrt(p*(1-p)/10)), col = "orange", lwd = 2)
plot(hist2, freq = F, col = rgb(1, 0, 0, alpha = 1), add = T)
lines(seq(0, 1, by = 0.001), dnorm(seq(0, 1, by = 0.001), p, sqrt(p*(1-p)/1000)), col = "purple", lwd = 2)
legend("topright", c(expression("n = 10"), expression("normal distr. (n = 10)"), expression("n = 1000"), expression("normal distr. (n = 1000)")), lty = c("solid", "solid", "solid", "solid"), col = c("blue", "orange", "red", "purple"), lwd = c(4, 2, 4, 2))