dibatic2<-read.csv("Pima.te.csv")
dibatic2


str(dibatic2)
dibatic2$type<-ifelse(dibatic2$type=="Yes",1,0)
prop.table(table(dibatic2$type))

library(caTools)
set.seed(123)
split<- sample.split(dibatic2[,8],SplitRatio = 0.8)

traing<- subset(dibatic2,split="TRUE")
testing<- subset(dibatic2,split="FALSE")
str(traing);str(testing)

prop.table(table(dibatic2$type))
prop.table(table(traing$type))
prop.table(table(testing$type))


model<- glm(type~.,traing, family=binomial)
summary(model)


with(model,null.deviance~deviance)
with(model,df.null~df.residual)

with(mdoel,pchisq(null.deviance~di))
