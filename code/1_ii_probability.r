library(ggplot2)
zomb <- data.frame(z = factor(c(rep('not a zombie', 70), rep('zombie',30))))
hist(as.numeric(zomb))

ggplot(zomb) + aes(zomb$z) + geom_histogram(stat='count') + xlab("") + ylab("")

prz <- 0.3
zomb2 <- data.frame(z = c(rep(0, 100*dbinom(0,2,prz)), rep(1, 100*dbinom(1,2,prz)), rep(2, 100*dbinom(2,2,prz))))

ggplot(zomb2) + geom_histogram(aes(x=z, y=..density..), binwidth=1, colour="black", fill="#6666aa", lwd=0.5) + xlab("Number of Zombies") + ylab("Probability")