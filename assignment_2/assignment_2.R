# For Q1 to Q12 use “College.csv” (Read with strip.white=T & stringsAsFactor=T)

library(dplyr)   # Loaded library

college_data <- read.csv("College.csv", strip.white = TRUE, stringsAsFactors = TRUE)  # Read the College.csv file

head(college_data) # Display the first few rows of the dataset

# Q1.	Calculate the median of the first 15 rows of the variable Enroll?

median_enroll <- median(college_data$Enroll[1:15])  # Calculate the median of the first 15 rows of the variable Enroll

# Print the result
print(median_enroll)

  
# Q2.	Calculate the mean of variable "Enroll".

mean_enroll <- mean(college_data$Enroll) # Calculate the mean of the variable Enroll

# Print the result
print(mean_enroll)


# Q3.	How many colleges have Grad.Rate greater than 50?

grad_rate<-sum(college_data$Grad.Rate>50)
print(grad_rate)

# Q4.	What is the class of variable "Private"?
class_priv<-class(college_data$Private) 
print(class_priv)
  
# Q5.	Calculate the mean of the last 20 rows of the variable “Apps"
mean_data<-mean(college_data$Apps[1:20])
print(mean_data)

# Q6.	How many observations have more than 70 books and 10 PhD?
observation<-sum(college_data$Books>70 & college_data$PhD>10)
print(observation)

# Q7.	What is the difference between mean of ‘Enroll’ for Private and Public colleges?
enrole_private<-mean(college_data$Enroll[college_data$Private=="Yes"])
enrole_publicc<-mean(college_data$Enroll[college_data$Private=="No"])
differnce<- enrole_publicc - enrole_private
print(differnce)

# Q8.	How many PhD students are there in Marymount University?
ma<-college_data[college_data$Name=="Marymount University",]
ne<-ma$PhD
print(ne)

# Q9.	Acceptance Ratio is defined as Accept/Apps. Append the field acceptance ratio to the college dataframe and save df as college_new.
# Calculate Acceptance Ratio
college_data$Acceptance.Ratio <- college_data$Accept / college_data$Apps
write.csv(college_data, "college_new.csv", row.names = FALSE)
head(college_data)


# Q10.	How many colleges have acceptance ratio greater than the mean of acceptance ratio of all colleges?
mean_acceptance_ratio <- mean(college_data$Acceptance.Ratio, na.rm = TRUE)
num_colleges_acceptance_gt_mean <- sum(college_data$Acceptance.Ratio > mean_acceptance_ratio, na.rm = TRUE)
print(num_colleges_acceptance_gt_mean)


# Q11.	Which college has minimum enrolments?
num <- which.min(college_data$Enroll)
enmen <- college_data$Name[num]
print(enmen)

# Q12.	How many private colleges have more than 600 books?
collda <- sum(college_data$Books>600 & college_data$Private=="Yes")


print(collda)

# For Q13 to Q20 use “cars.txt” file

library(dplyr)
cardata <- read.csv("cars.csv", strip.white = TRUE, stringsAsFactors = TRUE)
print(cardata)



# Q13.	Append a variable hp2 defined as hp/2 to the data-frame. 
hp2<-cardata$hp/2
print(hp2)
print(head(hp2))


  # Q14.	What is the frequency distribution of the "brand" variable in the "cars" data frame?
brand_freq <- table(cars$brand)
print(brand_freq)

# Q15.	Create a data frame named "cars.rsub" by subsetting the data for the first 50 rows of the "cars" data frame
# Assuming 'cars' is your original data frame
# Subset the first 50 rows
cars.rsub <- cars[1:50, ]
print(head(cars.rsub))
print(cars.rsub)

# Q16.	Create a data frame "cars.csub" by subsetting the "cars" data frame from the first 3 columns?
cars.csub <- cars[, 1:3]
print(str(cars.csub))


# Q17.	Select rows 2, 5, and 10, and columns 1, 3, and 6 from the "cars" data frame and store it in a new data frame "car_out"
selected_rows <- c(2, 5, 10)
selected_cols <- c(1, 3, 6)
car_out <- cars[selected_rows, selected_cols]
print(car_out)


# Q18.	How many cars have greater than or equal to 106 hp?
cars_greater_than_106hp <- subset(cars, hp >= 106)
num_cars_greater_than_106hp <- nrow(cars_greater_than_106hp)


# Print the column names of the 'cars' data frame
print(colnames(cars))


# Q19.	How many cars manufactured in US have more than 100 Hp?
us_cars_gt_100hp <- subset(cars, country == "US" & hp > 100)
num_us_cars_gt_100hp <- nrow(us_cars_gt_100hp)
print(num_us_cars_gt_100hp)

# Q20.	How many cars have weight less than the mean weight of all cars?
mean_weight <- mean(cars$weight)
cars_lt_mean_weight <- subset(cars, weight < mean_weight)
num_cars_lt_mean_weight <- nrow(cars_lt_mean_weight)
print(num_cars_lt_mean_weight)



