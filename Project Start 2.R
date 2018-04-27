setwd('C:/Users/shope/Desktop/Spring 2017/ITM 883 - Bus Analytics Problem Solving/Project')

library(randomForest)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)

train <- read.csv("train.csv")
train$Alone <- ifelse(train$SibSp >=1 | train$Parch >=1,0,1)
train$Sex <- as.factor(train$Sex)
train$Pclass <- as.factor(train$Pclass)
train$Survived <- as.factor(train$Survived)
train$SibSp <- as.factor(train$SibSp)
train$Parch <- as.factor(train$Parch)


#code to fill Age NA's in training data
trainMale1 <- train %>%
  filter(Sex=="male" & Pclass==1)
trainMale2 <- train %>%
  filter(Sex=="male",Pclass==2)
trainMale3 <- train %>%
  filter(Sex=="male",Pclass==3)
trainFemale1 <- train %>%
  filter(Sex=="female",Pclass==1)
trainFemale2 <- train %>%
  filter(Sex=="female",Pclass==2)
trainFemale3 <- train %>%
  filter(Sex=="female",Pclass==3)
trainMale1 = replace_na(trainMale1, list(Age = mean(trainMale1$Age, na.rm = T)))
trainMale2 = replace_na(trainMale2, list(Age = mean(trainMale2$Age, na.rm = T)))
trainMale3 = replace_na(trainMale3, list(Age = mean(trainMale3$Age, na.rm = T)))
trainFemale1 = replace_na(trainFemale1, list(Age = mean(trainFemale1$Age, na.rm = T)))
trainFemale2 = replace_na(trainFemale2, list(Age = mean(trainFemale2$Age, na.rm = T)))
trainFemale3 = replace_na(trainFemale3, list(Age = mean(trainFemale3$Age, na.rm = T)))
train_new = rbind(trainMale1, trainMale2, trainMale3, trainFemale1, trainFemale2, trainFemale3)

#test variable included for a new glm model without "sex"
# test has three values (1=female, 2 = male, 3 = child under 15yrs)
# improves model accuracy as most children were saved, regardless of sex
# Baylee has glm model for this one, I have a graph for it at the bottom
train_new$test = ifelse(train_new$Age <= 15, 3, train_new$Sex)
train_new$test = as.factor(train_new$test)



#New predictions and glm based on updated table with no null ages
model1 <- glm(Survived ~ Sex + Age + Pclass, data=train_new, family="binomial")
summary(model1)

# still may be able to have a certain survive/die threshold depending on the specific age?? not just a flat 70% or 50%?
numerator_new <- exp(3.735839 -2.570554*ifelse(train_new$Sex=="male",1,0) -.037405*train_new$Age-1.217372*ifelse(train_new$Pclass==2,1,0)-2.500752*ifelse(train_new$Pclass==3,1,0))
denominator_new <- (1 + exp(3.735839 -2.570554*ifelse(train_new$Sex=="male",1,0) -.037405*train_new$Age-1.217372*ifelse(train_new$Pclass==2,1,0)-2.500752*ifelse(train_new$Pclass==3,1,0)))
survival_ratio_new = numerator_new/denominator_new
train_new$Predict <- ifelse(survival_ratio_new  >=.7,1,0) 
ftable(train_new$Survived, train_new$Predict)



#basic graph to show survival counts based on age
#young and old have higher rate of survival
age1<-ggplot(train, aes(Age,fill = Survived)) +
  geom_histogram(bins=50)+
  ggtitle("Survival vs Death Based on Age (With Nulls)") + theme(plot.title = element_text(hjust = 0.5))
age2<-ggplot(train_new, aes(Age,fill = Survived)) +
  geom_histogram(bins=50)+
  ggtitle("Survival vs Death Based on Age (Without Nulls)") + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(age1,age2,ncol=1)




#### density graph of survival based on age as well as line plots of 
####    survival proportions based on age WITH NAs
Q4_a<-ggplot(data=train)+
  geom_area(aes(x=Age,fill=Survived,color=Survived),stat="density",alpha=0.1,position="identity")+
  ggtitle("Density of Survival Based on Age (With Nulls)") + theme(plot.title = element_text(hjust = 0.5))
all <- train %>%
  group_by(Age)%>%
  summarize(all=n())
test_bad<-train %>%
  filter(Survived==0) %>%
  group_by(Age) %>%
  summarize(count1 = n())
test_good<-train %>%
  filter(Survived==1) %>%
  group_by(Age) %>%
  summarize(count2 = n())
join_bad<- right_join(test_bad,all,by=c("Age"))%>%
  mutate_all(funs(replace(., which(is.na(.)), 0))) %>%
  mutate(proportion1=count1/all)
join_good<- right_join(test_good,all,by=c("Age"))%>%
  mutate_all(funs(replace(., which(is.na(.)), 0))) %>%
  mutate(proportion2=count2/all)
Q4_bad<- ggplot(data=join_bad,mapping=aes(x=Age,y=proportion1))+
  geom_point()+
  geom_smooth(method="loess",span=5)+
  ggtitle("Proportion of Death Based on Age (With Nulls)") + theme(plot.title = element_text(hjust = 0.5))
Q4_good<- ggplot(data=join_good,mapping=aes(x=Age,y=proportion2))+
  geom_point()+
  geom_smooth(method="loess",span=2)+
  ggtitle("Proportion of Survival Based on Age (With Nulls)") + theme(plot.title = element_text(hjust = 0.5))
##### WITHOUT NAs
Q4_b<-ggplot(data=train_new)+
  geom_area(aes(x=Age,fill=Survived,color=Survived),stat="density",alpha=0.1,position="identity")+
  ggtitle("Density of Survival Based on Age (Without Nulls)") + theme(plot.title = element_text(hjust = 0.5))
all <- train_new %>%
  group_by(Age)%>%
  summarize(all=n())
test_bad<-train_new %>%
  filter(Survived==0) %>%
  group_by(Age) %>%
  summarize(count1 = n())
test_good<-train_new %>%
  filter(Survived==1) %>%
  group_by(Age) %>%
  summarize(count2 = n())
join_bad<- right_join(test_bad,all,by=c("Age"))%>%
  mutate_all(funs(replace(., which(is.na(.)), 0))) %>%
  mutate(proportion1=count1/all)
join_good<- right_join(test_good,all,by=c("Age"))%>%
  mutate_all(funs(replace(., which(is.na(.)), 0))) %>%
  mutate(proportion2=count2/all)
Q4_b_bad<- ggplot(data=join_bad,mapping=aes(x=Age,y=proportion1))+
  geom_point()+
  geom_smooth(method="loess",span=5)+
  ggtitle("Proportion of Death Based on Age (Without Nulls)") + theme(plot.title = element_text(hjust = 0.5))
Q4_b_good<- ggplot(data=join_good,mapping=aes(x=Age,y=proportion2))+
  geom_point()+
  geom_smooth(method="loess",span=2)+
  ggtitle("Proportion of Survival Based on Age (Without Nulls)") + theme(plot.title = element_text(hjust = 0.5))
#graph
grid.arrange(Q4_a,Q4_bad,Q4_good,Q4_b,Q4_b_bad,Q4_b_good,nrow=2)



#####density of survival based on age per sex
bad<-train%>%
  filter(Survived==0)
good<-train%>%
  filter(Survived==1)
a<-ggplot(data=bad)+
  geom_area(aes(x=Age,fill=Sex,color=Sex),stat="density",alpha=0.1,position="identity")+
  ggtitle("Density of Death: Age and Sex (With Nulls)") + theme(plot.title = element_text(hjust = 0.5))
b<-ggplot(data=good)+
  geom_area(aes(x=Age,fill=Sex,color=Sex),stat="density",alpha=0.1,position="identity")+
  ggtitle("Density of Survival: Age and Sex (With Nulls)") + theme(plot.title = element_text(hjust = 0.5))
bad2<-train_new%>%
  filter(Survived==0)
good2<-train_new%>%
  filter(Survived==1)
a2<-ggplot(data=bad2)+
  geom_area(aes(x=Age,fill=Sex,color=Sex),stat="density",alpha=0.1,position="identity")+
  ggtitle("Density of Death: Age and Sex (Without Nulls)") + theme(plot.title = element_text(hjust = 0.5))
b2<-ggplot(data=good2)+
  geom_area(aes(x=Age,fill=Sex,color=Sex),stat="density",alpha=0.1,position="identity")+
  ggtitle("Density of Survival: Age and Sex (Without Nulls)") + theme(plot.title = element_text(hjust = 0.5))
#graph
grid.arrange(a,b,a2,b2,ncol=2)





#####density of survival based on age per class
a3<-ggplot(data=bad)+
  geom_area(aes(x=Age,fill=Pclass,color=Pclass),stat="density",alpha=0.1,position="identity")+
  ggtitle("Density of Death: Age and Class (With Nulls)") + theme(plot.title = element_text(hjust = 0.5))
b3<-ggplot(data=good)+
  geom_area(aes(x=Age,fill=Pclass,color=Pclass),stat="density",alpha=0.1,position="identity")+
  ggtitle("Density of Survival: Age and Class (With Nulls)") + theme(plot.title = element_text(hjust = 0.5))
a4<-ggplot(data=bad2)+
  geom_area(aes(x=Age,fill=Pclass,color=Pclass),stat="density",alpha=0.1,position="identity")+
  ggtitle("Density of Death: Age and Class (Without Nulls)") + theme(plot.title = element_text(hjust = 0.5))
b4<-ggplot(data=good2)+
  geom_area(aes(x=Age,fill=Pclass,color=Pclass),stat="density",alpha=0.1,position="identity")+
  ggtitle("Density of Survival: Age and Class (Without Nulls)") + theme(plot.title = element_text(hjust = 0.5))
#graph
grid.arrange(a3,b3,a4,b4,ncol=2)




#### Survival and death counts broken down by class
train_new_1<- train_new %>%
  filter(Pclass==1)
train_new_2<- train_new %>%
  filter(Pclass==2)
train_new_3<- train_new %>%
  filter(Pclass==3)
class1<-ggplot(train_new_1, aes(Age,fill = Survived)) +
  geom_histogram(bins=40)+
  ggtitle("Survival/Death Counts: Class 1") + theme(plot.title = element_text(hjust = 0.5))
class2<-ggplot(train_new_2, aes(Age,fill = Survived)) +
  geom_histogram(bins=40)+
  ggtitle("Survival/Death Counts: Class 2") + theme(plot.title = element_text(hjust = 0.5))
class3<-ggplot(train_new_3, aes(Age,fill = Survived)) +
  geom_histogram(bins=40)+
  ggtitle("Survival/Death Counts: Class 3") + theme(plot.title = element_text(hjust = 0.5))
# graph
grid.arrange(class1,class2,class3,ncol=1)



##### density of survival based on sex and age
b5<-ggplot(data=good2)+
  geom_area(aes(x=Age,fill=test,color=test),stat="density",alpha=0.1,position="identity")+
  ggtitle("Density of Survival: Age and Test (Without Nulls)") + theme(plot.title = element_text(hjust = 0.5))
#graph
grid.arrange(b5,ncol=1)
