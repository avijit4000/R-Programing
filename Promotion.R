data<- read.csv("Promotion.csv")
head(data)

model <- lm(Rev~Prom,data)

summary(model)

new<- data.frame(Prom=5)
predict(model, newdata = new)

predict(model, newdata= new, interval = "prediction")
predict(model, newdata= new, interval = "confidence")

confint(model, level = 0.95)

par(mfrow= c(2,2))
plot(model)

data$Prom_ln <- log(data$Prom)

mode12 <- lm( Rev ~ Prom_ln, data)
summary(mode12)


par(mfrow=c(2,2))
plot(mode12)
anova(mode12)

rs<- rstandard(mode12)
which (rs< -3 | rs >3 )
ht <- hatvalues(mode12)
which(ht >0.16)
ck <- cooks.distance(mode12)
which(ck>1)



