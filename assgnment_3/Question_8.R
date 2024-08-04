
college <- read.csv("College.csv")


rownames(college) <- college[,1]
college <- college[,-1]

summary(college)
pairs(college[, 1:10])
 
boxplot(Outstate ~ Private, data = college, xlab = "Private", ylab = "Outstate", main = "Outstate Tuition by Private/Public College")

Elite <- rep("No", nrow(college))
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)


summary(college$Elite)
plot(college$Elite, college$Outstate, xlab = "Elite", ylab = "Outstate Tuition", main = "Outstate Tuition by Elite/Non-Elite College")

par(mfrow = c(2, 2))

hist(college$Apps, main = "Applications", xlab = "Number of Applications", breaks = 20)
hist(college$Enroll, main = "Enrollment", xlab = "Number of Enrolled Students", breaks = 20)
hist(college$Outstate, main = "Outstate Tuition", xlab = "Outstate", breaks = 20)
hist(college$PhD, main = "Percent of Faculty with PhD", xlab = "Percent", breaks = 20)






