getwd()

library(dplyr)

college_tedata <- read.csv("College-test.csv", strip.white = TRUE, stringsAsFactors = TRUE)

head(college_tedata)


#Q1. D
#Q2. A
#Q3. B

#Q4.  
mengrarat <- mean(college_tedata$Grad.Rate[1:50])
print(mengrarat)
#Q4. B

#Q5.
las <- median(college_tedata$Enroll[-100:,])
print(las)
#Q5. C

#Q6 
collen <- sum(college_tedata$Enroll<1000)
print(collen)

#Q6. 602

#Q7.
collboophd <- college_tedata$Name[college_tedata$PhD>50 & college_tedata$Books<100]
print(collboophd)
#Q7.B

#Q8. 
grarat <- college_tedata$Grad.Rate[college_tedata$Name=="University of North Texas"]
print(grarat)
#Q8.D


#Q9.c
collne <- sum(college_tedata$Private=="No" & college_tedata$Top10perc>60)
print(collne)


#Q10. 
connero <- sum(college_tedata$Enroll>200)
print(connero)

#Q12.
connero <- sum(college_tedata$$Terminal<75)
print(connero)

#Q13.
