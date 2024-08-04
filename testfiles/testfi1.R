getwd()
Auto <- read.csv("Promotion.csv",header=T)
Auto
str()
str(Auto)
head(Auto)
names(Auto)
plot(Auto)
plot(Auto$Rev)
plot(Auto$Prom)
plot(Auto$Prom, Auto$Rev)
par(mfrow=c(4,4))
plot(Auto$Prom, Auto$Rev, xlab = "new test",ylab = "uey", main = "test work")


plot(Auto)


sum_cyl<-table(Auto$Rev)
par(mfrow=c(2,4))
barplot(sum_cyl,col = "red")


hist(Auto$Rev)
hist(Auto$Rev,col=2)
plot(Auto$Rev,Auto$Prom)


pairs(~ Rev + Prom, Auto)
