sink("Output_Sara_Luppi.txt", split = TRUE)

#LIBRARIES----
library(psych)
library(summarytools)
library(lavaan)
library(semPlot)
library(ggplot2)
library(dplyr)
library(emmeans)

data <- read.csv("Data_Sara_Luppi.csv")

#Deleting rows that I do not need
data <- data[-c(1,2),]

#Doing some adjustments to variable names
data <- rename(data, attention_check = similarity_5,
               condition = FL_9_DO,
               field_other = field_2_TEXT)

#Doing some adjustments to levels
#I consider business students even those who didn't select the "Business or related" option as their field of studies, but included words such as economics, marketing and communication
data$field_other <- tolower(data$field_other)
data[grepl(("economics"), data$field_other), 41] <- "Business or related"
data[grepl(("marketing"), data$field_other), 41] <- "Business or related"
data[grepl(("communication"), data$field_other), 41] <- "Business or related"

#Deleting columns that I do not need
data <- data[,-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 42, 43)]

#DATA QUALITY CHECK----
dim(data)

#I only keep participants that answered correctly to the attention check
data <- data[data$attention_check == "Somewhat agree",]
dim(data)

#I only keep participants over 18
data <- data[data$age >= 18,]

#I check for a sufficient level of education (at least high school diploma)
table(data$edu)
dim(data)

#Recoding----
#For familiarity
data[data == 'Not familiar at all'] <- 1
data[data == 'Slightly familiar'] <- 2
data[data == 'Moderately familiar'] <- 3
data[data == 'Very familiar'] <- 4
data[data == 'Extremely familiar'] <- 5

#For similarity and cohesiveness
data[data == 'Strongly disagree'] <- 1
data[data == 'Somewhat disagree'] <- 2
data[data == 'Neither agree nor disagree'] <- 3
data[data == 'Somewhat agree'] <- 4
data[data == 'Strongly agree'] <- 5

#For gender and education
data[data == "Female"] <- 0
data[data == "Male"] <- 1
data[data == "Prefer not to say"] <- NA

data[data == "High school"] <- 1
data[data == "Bachelor's degree"] <- 2
data[data == "Master's degree"] <- 3
data[data == "PhD"] <- 4
data[data == "Professional degree"] <- NA

#Converting type to numeric
data[, 1:21] <- apply(data[, 1:21], 2, as.numeric)
data$edu <- as.numeric(data$edu)
                      
dfSummary(data)

#Straightliners---- 
data$sd <- apply(data[,1:19], 1 , sd)

table(data$sd == 0)
table(is.na(data$sd))
#There are no straightliners

#SCALES CONSTRUCTION----

#Reliability - Cronbach's Alpha----

#X (similarity)
round(cor(data[, 11:14]), 1)
psych::alpha(data[, 11:14])
#The reliability for X is 0.9 [0.87,0.94]

#Z (cohesiveness)
round(cor(data[, 16:19]), 1)
psych::alpha(data[, 16:19])
#The reliability for Z is 0.029 [0.07,0.51], but there are negatively formulated items that need to be reversed
#Reverse negative items
data$cohesiveness_2r <- 6 - data$cohesiveness_2
round(cor(data[, c(16,27,18,19)]), 1)
psych::alpha(data[, 16:19], check.keys = TRUE)
#Now the reliability of Z is 0.84 [0.78,0.89]

#M (fit)
round(cor(data[, 2:7]), 1)
psych::alpha(data[, 2:7])
#The reliability for M is 0.96 [0.94,0.97]

#Y (attitude)
round(cor(data[, 8:10]), 1)
psych::alpha(data[, 8:10])
#The reliability for Y is 0.96 [0.95,0.98]

#Unweighted Scales----
data$scale_x <- (data$similarity_1 + data$similarity_2 + data$similarity_3 + data$similarity_4)/4
data$scale_m <- (data$fit_1 + data$fit_2 + data$fit_3 + data$fit_4 + data$fit_5 + data$fit_6)/6
data$scale_y <- (data$attitude_1 + data$attitude_2 + data$attitude_3)/3
data$scale_z <- (data$cohesiveness_1 + data$cohesiveness_2 + data$cohesiveness_3 + data$cohesiveness_4)/4

#ANOVA----
#Create categorical effect coded variables for similarity and cohesiveness

data$dummy_x <- ifelse(data$condition == "sponsor1"
                       | data$condition == "sponsor3", 1, -1) #similarity
data$dummy_z <- ifelse(data$condition == "sponsor1"
                       | data$condition == "sponsor2", 1, -1) #cohesiveness

# Compute the analysis of variance for similarity (X)
group_by(data, dummy_x) %>%
  summarise(count = n(),
            mean = mean(scale_x, na.rm = TRUE),
            sd = sd(scale_x, na.rm = TRUE))

res.aov.x <- aov(scale_x ~ dummy_x*dummy_z, data = data) # mc for similarity
summary(res.aov.x)

# Compute the analysis of variance for cohesiveness (Z)
group_by(data, dummy_z) %>%
  summarise(count = n(),
            mean = mean(scale_z, na.rm = TRUE),
            sd = sd(scale_z, na.rm = TRUE))

res.aov.z <- aov(scale_z ~ dummy_x*dummy_z, data = data) # mc for cohesiveness
summary(res.aov.z)

#There are differences between conditions, both in similarity and cohesiveness

round(cor(data[,c("dummy_x","dummy_z","scale_m","scale_y", "scale_x", "scale_z")]),2)

#REGRESSION ANALYSIS----

#Model 1 (without covariates and without interaction)----
set.seed(123)
model1  <- "
			scale_m ~ a1*dummy_x + a2*dummy_z          
			scale_y ~ b1*scale_m + c1*dummy_x + c2*dummy_z 
			#indirect and total effects   
			a1b1         := a1*b1
			a2b1         := a2*b1
			cprime       := c1           
			totaleffect  := a1*b1+a2*b1 + c1  
			"

fit1 <- sem(model1, data = data, se = "bootstrap", bootstrap = 10000)
summary(fit1, ci = T, rsquare = T)

#Model 2 (without covariates and with interaction)----
set.seed(123)
model2  <- "
			scale_m ~ a1*dummy_x + a2*dummy_z + a3*dummy_x:dummy_z                
			scale_y ~ b1*scale_m + c1*dummy_x + c2*dummy_z +c3*dummy_x:dummy_z
			#indirect and total effects   
			a1b1         := a1*b1
			a2b1         := a2*b1
			a3b1         := a3*b1
			cprime       := c1
			totaleffect  := a1*b1+a2*b1+a3*b1 + c1  
			"

fit2 <- sem(model2, data=data, se = "bootstrap", bootstrap = 10000)
summary(fit2, ci = T, rsquare = T)

#Model 3 (with covariates)----
data$business <- ifelse(data$field == "Business or related", 1, -1)
round(cor(data[,c("dummy_x", "dummy_z", "scale_m", "scale_y", "familiarity", "business")]), 2)
set.seed(123)
model3  <- "                       
			scale_m ~ a1*dummy_x + a2*dummy_z + a3*dummy_x:dummy_z                
			scale_y ~ b1*scale_m + c1*dummy_x + c2*dummy_z +c3*dummy_x:dummy_z + familiarity + business
			#indirect and total effects   
			a1b1         := a1*b1
			a2b1         := a2*b1
			a3b1         := a3*b1
			cprime       := c1
			totaleffect  := a1*b1+a2*b1+a3*b1 + c1       
			"         
fit3 <- sem(model3, data = data, se = "bootstrap", bootstrap = 10000)        
summary(fit3, ci = T, rsquare = T)

#Model 4 (with a new variable that incorporates fit and attitude) ----
data$scale_new <- (data$fit_1 + data$fit_2 + data$fit_3 + data$fit_4 + data$fit_5 + data$fit_6 + data$attitude_1 + data$attitude_2 + data$attitude_3)/9
round(cor(data[,c("dummy_x", "dummy_z", "scale_new")]), 2)

set.seed(123)
model4  <- "
			scale_new ~ a1*dummy_x + a2*dummy_z + a3*dummy_x:dummy_z                
			"
fit4 <- sem(model4, data = data, se = "bootstrap", bootstrap = 10000)
summary(fit4, ci = T, rsquare = T)

#RANDOMIZATION CHECK----
#For familiarity
group_by(data,condition)%>%
  summarise(count = n(),
            mean = mean(familiarity, na.rm = TRUE),
            sd = sd(familiarity, na.rm = TRUE))

res.aov.f <- aov(familiarity ~ condition, data = data)
summary(res.aov.f)

# For business
group_by(data,condition)%>%
  summarise(count = n(),
            mean = mean(business, na.rm = TRUE),
            sd = sd(business, na.rm = TRUE))

res.aov.b <- aov(business ~ condition, data = data)
summary(res.aov.b)

#For age
group_by(data,condition)%>%
  summarise(count = n(),
            mean = mean(age, na.rm = TRUE),
            sd = sd(age, na.rm = TRUE))

res.aov.age <- aov(age ~ condition, data = data)
summary(res.aov.age)

#For education
group_by(data,condition)%>%
  summarise(count = n(),
            mean = mean(edu, na.rm = TRUE),
            sd = sd(edu, na.rm = TRUE))

res.aov.edu <- aov(edu ~ condition, data = data)
summary(res.aov.edu)

#For gender
group_by(data,condition)%>%
  summarise(count = n(),
            mean = mean(gender, na.rm = TRUE),
            sd = sd(gender, na.rm = TRUE))

res.aov.gender <- aov(gender ~ condition, data = data)
summary(res.aov.gender)

#Spotlight Analysis----
catcat <- lm(scale_m ~ dummy_x*dummy_z, data=data)
summary(catcat)

emcatcat <- emmeans(catcat, ~ dummy_x*dummy_z)
summary(emcatcat)

contrast(emcatcat, "revpairwise", by = "dummy_x", adjust = "none")
contrast(emcatcat, "revpairwise", by = "dummy_z", adjust = "none")

sink()