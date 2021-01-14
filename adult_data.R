library(dplyr)
data_adult<-read.csv("C:/Users/ravi/Desktop/adult.csv/adult.csv")
data_adult <- select(data_adult ,-occupation , -native.country ,-capital.gain ,-fnlwgt , -relationship ,-capital.loss ,-relationship )
glimpse(data_adult)
#check for continuous variable
continuous <-select_if(data_adult, is.numeric)
summary(continuous)
#to check outliers
# Histogram with kernel density curve
library(ggplot2)
ggplot(continuous, aes(x = hours.per.week)) +
  geom_density(alpha = .2, fill = "#FF6666")
top_one_percent <- quantile(data_adult$hours.per.week, .98)
top_one_percent
#calculate top 1 percentile 99 percentile that is 80 hrs meand 99% people are working for less than 80hr/week so drop these 80 values
data_adult_drop <-data_adult %>%
  filter(hours.per.week<top_one_percent)
dim(data_adult_drop)
#standardize numerical data
library(caret)
preproc1 <- preProcess(continuous, method=c("center", "scale"))
data_adult_rescale <- predict(preproc1, data_adult_drop)
summary(data_adult_rescale)
head(data_adult_rescale)
#check factor variables
factor <- data.frame(select_if(data_adult_rescale, is.factor))
ncol(factor)
head(factor)
#bar plot of work class
a=table(factor$ workclass)
a
barplot(a,
        col=rainbow(2),
        legend=rownames(a),
        main="Barplot of  workclass",
        xlab=" workclass",
        ylab="Count")
library(ggplot2)
# Create graph for each column
graph <- lapply(names(factor),
                function(x) 
                  ggplot(factor, aes(get(x))) +
                  geom_bar() +
                  theme(axis.text.x = element_text(angle = 90)))
graph

#recast education
recast_data <- data_adult_rescale %>%
  mutate(education = factor(ifelse(education == "Preschool" | education == "10th" | education == "11th" | education == "12th" | education == "1st-4th" | education == "5th-6th" | education == "7th-8th" | education == "9th", "dropout", ifelse(education == "HS-grad", "HighGrad", ifelse(education == "Some-college" | education == "Assoc-acdm" | education == "Assoc-voc", "Community",
                                                                                                                                                                                                                                                                                            ifelse(education == "Bachelors", "Bachelors",
                                                                                                                                                                                                                                                                                                   ifelse(education == "Masters" | education == "Prof-school", "Master", "PhD")))))))

head(recast_data)
# how many years required to acquire degrees
recast_data %>%
  group_by(education) %>%
  summarize(average_educ_year = mean(educational.num),
            count = n()) %>%
  arrange(average_educ_year)

# recast marital status
recast_data <- recast_data %>%
  mutate(marital.status = factor(ifelse(marital.status == "Never-married" | marital.status == "Married-spouse-absent", "Not_married", ifelse(marital.status == "Married-AF-spouse" | marital.status == "Married-civ-spouse", "Married", ifelse(marital.status == "Separated" | marital.status == "Divorced", "Separated", "Widow")))))
table(recast_data$marital.status)

# graph hued by the income
# gender with income
ggplot(recast_data, aes(x = gender, fill = income)) +
  geom_bar(position = "fill") +
  theme_classic()
# race with income
ggplot(recast_data, aes(x = race, fill = income)) +
  geom_bar(position = "fill") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))
# gender with hours per week
ggplot(recast_data, aes(x = gender, y = hours.per.week)) +
  geom_boxplot() +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue") +
  theme_classic()
# education with hours per week 
# Plot distribution working time by education
ggplot(recast_data, aes(x = hours.per.week)) +
  geom_density(aes(color = education), alpha = 0.5) +
  theme_classic()
# anova test for checking group homegenity (p<0.05 , groups are different )
anova <- aov(hours.per.week~education, recast_data)
summary(anova)
# correlation
# Convert data to numeric
corr <- data.frame(lapply(recast_data, as.integer))
summary(corr)
# Plot the graph
rplot<-cor(corr) 
rplot
corrplot(rplot, method="number")
corrplot(rplot, method="number", addCoef.col = "black")

#splitting the data
library(caTools)
set.seed(1)
sample=sample.split(recast_data$income,SplitRatio = 0.80)
train_set=subset(recast_data,sample==TRUE)
test_set=subset(recast_data,sample==FALSE)
dim(train_set)
dim(test_set)

#model
logit <- glm(income~., data = train_set, family = 'binomial')
summary(logit )
library(rcompanion)

nagelkerke(logit)
logit$aic


#probit model
probit <- glm(income~. , data = train_set, family = 'binomial'(link="probit"))
summary(probit)

#confusion matrics
predict <- predict(logit, test_set, type = 'response')
# confusion matrix
table_mat <- table(test_set$income, predict > 0.5)
table_mat
#accuracy
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test
#precision
precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}
recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]# false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}
prec <- precision(table_mat)
prec


rec <- recall(table_mat)
rec
f1 <- 2 * ((prec * rec) / (prec + rec))
f1
library(ROCR)
ROCRpred <- prediction(predict, test_set$income)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2, 1.7))

#r square
nullmod <- glm(income~1, family="binomial" , data = test_set)
1-logLik(mod1)/logLik(nullmod)

