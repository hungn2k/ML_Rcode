who = read.csv('D:/My Youtube course/R software/WHO.csv')

str(who)
summary(who)

plot(who$LifeExpectancy, who$GNI)

plot(who$LifeExpectancy, who$GNI, main="Bieu do phan tan",
     xlab = 'Life Expectancy',
     ylab="GNI")

plot(who$LifeExpectancy, who$GNI, main="Bieu do phan tan",
     xlab = 'Life Expectancy',
     ylab="GNI",
     xlim=c(40,100),
     ylim=c(0,100000))

plot(who$LifeExpectancy, who$GNI, main="Bieu do phan tan",
     xlab = 'Life Expectancy',
     ylab="GNI",
     xlim=c(40,100),
     ylim=c(0,100000),
     col="red",
     pch = 17)

?points

plot(who$LifeExpectancy, who$GNI, main="Bieu do phan tan",
     xlab = 'Life Expectancy',
     ylab="GNI",
     xlim=c(40,100),
     ylim=c(0,100000),
     col="red",
     pch = 21)

?plot

x = c(10,21, 25,33,45,12,34,61)

plot(x, type = 'p')
plot(x, type='p', ylim=c(0,70))
plot(x, type='l', ylim=c(0,70))
plot(x, type='b', ylim=c(0,70))

plot(x, type='b', ylim=c(0,70),col='green', lty = 6, lwd=1.5)

?line


hist(who$LifeExpectancy, breaks=20, freq=FALSE)
hist(who$LifeExpectancy, breaks=10, freq=FALSE)
hist(who$LifeExpectancy, breaks=10, freq=TRUE)
abline(v=mean(who$LifeExpectancy))

abline(v=mean(who$LifeExpectancy), lwd=2, col="red")




par(mfrow= c(1,2))
hist(subset(who, Region== "Europe")$LifeExpectancy)
hist(subset(who, Region== "Africa")$LifeExpectancy)

par(mfrow= c(2,1))
hist(subset(who, Region== "Europe")$LifeExpectancy)
hist(subset(who, Region== "Africa")$LifeExpectancy)

par()

?par



