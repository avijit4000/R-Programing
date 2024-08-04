mtcars
str(mtcars)


m1<-lm(mpg~1, data=mtcars)
summary(m1)

m2 <- lm(mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb, data = mtcars)
summary(m2)


library(car)

vif(m2)


  x<-step(m1, scope=list(lower=m1,upper=m2),direction = "forward", text="F",trace=TRUE)
  
  x<- step(m1,scope = list(lower=m1,upper=m2),direction = "both",text="F",trace = TRUE)