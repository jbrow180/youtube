library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(caret)
setwd("~/youtube")
datax <- read.csv("videos.csv")
glimpse(datax)

#Data Wrangling
dataxx <- datax[c(-1,-4,-5,-6,-7,-12,-13,-14,-15,-16)]
glimpse(dataxx)
nrow(dataxx)

#removing na
dataxx[dataxx == 0] <- NA
dataxx <- na.omit(dataxx)
write.csv(dataxx, file = "dataxx.csv")
#Testing data 20% - Traing Data 80% rando sampels
validation_index <- createDataPartition(dataxx$likes, p=0.80, list=FALSE)
validation <- dataxx[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataxx <- dataxx[validation_index,]

#Exploratoary Data
#dimensbion of data
dim(dataxx)

# list types for each attribute
sapply(dataxx, class)


# take a peek at the first 5 rows of the data
head(dataxx)



# summarize the class distribution
percentage <- prop.table(table(dataxx$likes)) * 100
cbind(freq=table(dataxx$likes), percentage=percentage)


# summarize attribute distributions
summary(dataxx)


#Statistcal Anaylsis
#calculate the diffennces in mean for each trending date for likes
dataxx  %>%
  group_by(trending_date) %>%
  summarise(n_title = n(),
            mean_likes = mean(likes),
            std_error = sd(likes) / sqrt(n_title))
#calculate the diffrence in mean for each trending date for comment count
dataxx  %>%
  group_by(trending_date) %>%
  summarise(n_comments = n(),
            mean_comments = mean(comment_count),
            std_error = sd(comment_count) / sqrt(n_comments))
#Graphs
dta_hist <- ggplot(dataxx, aes(title))
dta_hist + geom_bar(aes(fill=likes), width = 0.6, col="blue") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Likes determined by trending dates")



ggplot(dataxx, aes(x = title, y = likes)) +
  geom_point() + geom_jitter()


ggplot(dataw, aes(x = Rank, y = Total_Shipped)) +
  geom_point()


ggplot(dataw, aes(x = Critic_Score, y = Total_Shipped, color="yellow")) + geom_point()


ggplot(dataw, aes(x=Year)) + 
  geom_line(aes(y=Critic_Score)) + 
  labs(title="Time Series Chart", 
       subtitle="Critic Score Per Year", 
       caption="Source: year", 
       y="Critic Score %")

ggplot(datal, aes(x = Rank, y = Total_Shipped)) +
  geom_point() + xlim(0,600)

dataw %>% count(Platform) %>% arrange(desc(n)) %>% 
  top_n(50) %>% 
  ggplot(aes( x = reorder(Platform,n), y  =  n, fill = Platform))  +
  geom_bar(stat = 'identity')  + theme(legend.position = 'none', axis.text.x = element_text(angle = 90)) +
  coord_flip()




#Machine learniing 
summary(dataw)

#Model9
model9 = lm(Total_Shipped ~ Critic_Score + Year + Rank + Platform + Name, data = dataw)
summary(model9)

model9$residuals

SSE = sum(model9$residuals^2)
SSE
#Model3
model3 = lm(Total_Shipped ~ Year + Platform, data = dataw)
summary(model3)

model3$residuals

SSE = sum(model3$residuals^2)
SSE
#Model4
model4 = lm(Total_Shipped ~ Year + Platform + Critic_Score + Name, data = dataw)
summary(model4)

model4$residuals

SSE = sum(model4$residuals^2)
SSE

#Model5
model5 = lm(Total_Shipped ~ Platform,  data = dataw)
summary(model5)

model5$residuals

SSE = sum(model5$residuals^2)
SSE

results <- kmeans(dataw, "3")

str(Mydata)
summary(Mydata)

head(dataw)
str(dataw)

control <- trainControl(method="cv", number=5)
set.seed(7)
fit <- train(Total_Shipped~Critic_Score + Year + Rank + Platform, data=dataw, method="lm", metric="RMSE", trControl=control)
print(fit)


control <- trainControl(method="cv", number=5)
set.seed(7)
fit <- train(Total_Shipped ~ Critic_Score + Year + Rank + Platform, method="lm", metric="RMSE", trControl=control)
print(fit)

#Model9 for testing data
model9 = lm(Total_Shipped ~ Critic_Score + Year + Rank + Platform, data = validation)
summary(model9)

#29 model for training data
model29 = lm(Total_Shipped ~ Critic_Score + Year + Rank + Platform, data = dataw)
summary(model29)

summary(model9)




