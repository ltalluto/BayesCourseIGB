library(RColorBrewer)

xxpos <- seq(0,500, 0.1)
yyg <- dgamma(xxpos, 0.1, 0.1)
xx <- seq(-300,300,0.1)
yyn <- dnorm(xx, 0, 1000)
yyu <- dunif(xx, -2000, 2000)

xxb <- seq(0,1,0.01)
yyb <- dbeta(xxb, 1,1)

par(mfrow=c(1,3), mar=c(4,4,0.2,0.2), bty='n', cex.axis=1.4, cex.lab=1.6)
plot(xxpos, yyg, type='l', col = "#c6b6e5", lwd=3.5, xlim=c(0,50), xlab=expression(theta), ylab="Density")
text(10, 0.5, expression(bold("Gamma(0.1, 0.1)")), pos=4, cex=1.7)

plot(xx, yyn, type='l', col = "#c6b6e5", lwd=3.5, xlim=c(-300,300), xlab=expression(theta), yaxt='n', ylab='', ylim=c(0,0.0005))
axis(side=2, labels=F)
text(-200, 0.00042, expression(bold("Normal(0, 1000)")), pos=4, col="#6c44bb", cex=1.7)
lines(xx, yyu, lwd=3.5, col="#c96840")
text(-100, 0.0002, expression(bold("Uniform(-2000, 2000)")), pos=4, col="#c96840", cex=1.7)

plot(xxb, yyb, type='l', col = "#c6b6e5", lwd=3.5, xlim=c(0,1), xlab=expression(theta), ylab="", ylim=c(0,1.2), yaxt='n')
axis(side=2, labels=F)
text(0.2, 1.05, expression(bold("Beta(1, 1)")), pos=4, cex=1.7)



## weakly informative
yyexp <- dexp(xxpos, 0.1)
yyhcau <- dcauchy(xxpos, 0, 5)
yyn2 <- dnorm(xx, 0, 5)
yyc2 <- dcauchy(xx, 0, 5)
yyb2 <- dbeta(xxb, 2,2)

par(mfrow=c(1,3), mar=c(4,4,0.2,0.2), bty='n', cex.axis=1.4, cex.lab=1.6)
plot(xxpos, yyexp, type='l', col = "#c6b6e5", lwd=3.5, xlim=c(0,50), xlab=expression(theta), ylab="Density")
text(8, 0.08, expression(bold("Exponential(0.1)")), pos=4, cex=1.7, col="#6c44bb")
lines(xxpos, yyhcau, lwd=3.5, col="#c96840")
text(15, 0.04, expression(bold("Half Cauchy(0, 5)")), pos=4, col="#c96840", cex=1.7)

plot(xx, yyn2, type='l', col = "#c6b6e5", lwd=3.5, xlim=c(-50,50), xlab=expression(theta), yaxt='n', ylab='', ylim=c(0,0.08))
axis(side=2, labels=F)
text(-45, 0.04, expression(bold("Normal(0, 5)")), pos=4, col="#6c44bb", cex=1.7)
lines(xx, yyc2, lwd=3.5, col="#c96840")
text(5, 0.04, expression(bold("Cauchy(0, 5)")), pos=4, col="#c96840", cex=1.7)

plot(xxb, yyb2, type='l', col = "#c6b6e5", lwd=3.5, xlim=c(0,1), xlab=expression(theta), ylab="", ylim=c(0,1.8), yaxt='n')
axis(side=2, labels=F)
text(0.2, 0.8, expression(bold("Beta(2, 2)")), pos=4, cex=1.7)


# likelihood
par(mfrow=c(1,1), mar=c(4,5,0.2,0.2), bty='n', cex.axis=1.4, cex.lab=1.6)
xx <- seq(0,1,0.005)
ylik <- dbinom(6, 49, xx)
plot(xx, ylik, type='l', col = "#c6b6e5", lwd=3.5, xlim=c(0,0.4), xlab=expression(theta), ylab=expression("pr(n=6,k=49|"~theta~")"))
abline(v = 0.087, lwd=3.5, col="#c96840")


## priors
cols <- brewer.pal(6, 'OrRd')
prmean <- 0.087
yprior1 <- dbeta(xx, 1, 1)
# yprior2 <- dbeta(xx, 0.435, 0.435/prmean - 0.435)
yprior3 <- dbeta(xx, 1.74, 1.74/prmean - 1.74)
yprior4 <- dbeta(xx, 4.263, 4.263/prmean - 4.263)
yprior5 <- dbeta(xx, 8.526, 8.526/prmean - 8.526)

plot(xx, ylik/max(ylik), type='l', col = "#c6b6e5", lwd=3.5, xlim=c(0,0.4), xlab=expression(theta), ylab=expression("pr(n=6,k=49|"~theta~")"))
lines(xx, yprior1/3*max(yprior1), col = cols[2], lwd=3.5)
# text(0.25, 0.2, expression(bold("Beta(1, 1)")), pos=4, col="#c96840", cex=1.4)
lines(xx, yprior3/(1.2*max(yprior3)), col = cols[4], lwd=3.5)
# text(0.25, 0.2, expression(bold("Beta(1.74, 18)")), pos=4, col="#c96840", cex=1.4)
# text(0.25, 0.16, expression(bold("n = 20")), pos=4, col="#c96840", cex=1.4)
lines(xx, yprior4/(1.2*max(yprior4)), col = cols[5], lwd=3.5)
# text(0.25, 0.2, expression(bold("Beta(4.3, 44.7)")), pos=4, col="#c96840", cex=1.4)
# text(0.25, 0.16, expression(bold("n = 49")), pos=4, col="#c96840", cex=1.4)
lines(xx, yprior5/(1.2*max(yprior5)), col = cols[6], lwd=3.5)
text(0.25, 0.2, expression(bold("Beta(8.5, 89.5)")), pos=4, col="#c96840", cex=1.4)
text(0.25, 0.16, expression(bold("n = 98")), pos=4, col="#c96840", cex=1.4)


par(mfrow=c(1,1), mar=c(4,5,0.2,0.2), bty='n', cex.axis=1.4, cex.lab=1.6)
plot(xx, ylik/max(ylik), type='l', col = "#c6b6e5", lwd=3.5, xlim=c(0,0.4), xlab=expression(theta), ylab=expression("pr(n=6,k=49|"~theta~")"))
lines(xx, yprior3/(1.2*max(yprior3)), col = cols[4], lwd=3.5)
ypost <- dbeta(xx, 1.74 + 6, (1.74/prmean - 1.74) + 49 - 6)
lines(xx, ypost/(1.2*max(ypost)), col = cols[6], lwd=3.5)