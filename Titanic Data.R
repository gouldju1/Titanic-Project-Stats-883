library(tidyr)
library(sqldf)
library(multcomp)
library(ggplot2)

train = read.csv("C:\\Users\\gould\\Documents\\MS Business Analytics\\1-Spring 2018\\ITM 883\\Project\\Titanic\\train.csv", header = TRUE, sep = ",")
train$Sex = as.factor(train$Sex)
train$Pclass = as.factor(train$Pclass)
test = read.csv("C:\\Users\\gould\\Documents\\MS Business Analytics\\1-Spring 2018\\ITM 883\\Project\\Titanic\\test.csv", header = TRUE, sep = ",")
test$Sex = as.factor(test$Sex)
test$Pclass = as.factor(test$Pclass)
test$Survived = 0

MyTitanicModel = glm(Survived ~ Pclass + Age + Sex, data = train, family = binomial)

summary(MyTitanicModel)

testMale = sqldf("SELECT * FROM test WHERE Sex = 'male'")
testFemale = sqldf("SELECT * FROM test WHERE Sex = 'female'")
testMale1 = sqldf("SELECT * FROM testMale WHERE pclass = '1'")
testMale2 = sqldf("SELECT * FROM testMale WHERE pclass = '2'")
testMale3 = sqldf("SELECT * FROM testMale WHERE pclass = '3'")
testFemale1 = sqldf("SELECT * FROM testFemale WHERE pclass = '1'")
testFemale2 = sqldf("SELECT * FROM testFemale WHERE pclass = '2'")
testFemale3 = sqldf("SELECT * FROM testFemale WHERE pclass = '3'")

mean(testMale1$Age, na.rm = T)
testMale1 = replace_na(testMale1, list(Age = 40.52))

mean(testMale2$Age, na.rm = T)
testMale2 = replace_na(testMale2, list(Age = 30.94))

mean(testMale3$Age, na.rm = T)
testMale3 = replace_na(testMale3, list(Age = 24.52))

mean(testFemale1$Age, na.rm = T)
testFemale1 = replace_na(testFemale1, list(Age = 41.33))

mean(testFemale2$Age, na.rm = T)
testFemale2 = replace_na(testFemale2, list(Age = 24.38))

mean(testFemale3$Age, na.rm = T)
testFemale3 = replace_na(testFemale3, list(Age = 23.07))


testNEW = rbind(testMale1, testMale2, testMale3, testFemale1, testFemale2, testFemale3)
View(testNEW)

testNEW$Sex = as.factor(testNEW$Sex)
testNEW$Pclass = as.factor(testNEW$Pclass)

Exponents = predict(MyTitanicModel, testNEW)

Prob = exp(Exponents) / (1 + (exp(Exponents)))

testNEW$Survived = ifelse(Prob > 0.7, 1, 0)
testNEW = testNEW[c(1,12,2,3,4,5,6,7,8,9,10,11)]

ComboTrainAndTest = rbind(train, testNEW)

#Missing fare
which(is.na(ComboTrainAndTest$Fare))
Combo3 = sqldf("SELECT * FROM ComboTrainAndTest WHERE PClass = '3'")
mean(Combo3$Fare, na.rm=T)

ComboTrainAndTest = replace_na(ComboTrainAndTest, list(Fare = 13.30289))

comboMale = sqldf("SELECT * FROM ComboTrainAndTest WHERE Sex = 'male'")
comboFemale = sqldf("SELECT * FROM ComboTrainAndTest WHERE Sex = 'female'")
comboMale1 = sqldf("SELECT * FROM comboMale WHERE pclass = '1'")
comboMale2 = sqldf("SELECT * FROM comboMale WHERE pclass = '2'")
comboMale3 = sqldf("SELECT * FROM comboMale WHERE pclass = '3'")
comboFemale1 = sqldf("SELECT * FROM comboFemale WHERE pclass = '1'")
comboFemale2 = sqldf("SELECT * FROM comboFemale WHERE pclass = '2'")
comboFemale3 = sqldf("SELECT * FROM comboFemale WHERE pclass = '3'")

mean(comboMale1$Age, na.rm = T)
comboMale1 = replace_na(comboMale1, list(Age = 41.00671))

mean(comboMale2$Age, na.rm = T)
comboMale2 = replace_na(comboMale2, list(Age = 30.81846))

mean(comboMale3$Age, na.rm = T)
comboMale3 = replace_na(comboMale3, list(Age = 25.78153))

mean(comboFemale1$Age, na.rm = T)
comboFemale1 = replace_na(comboFemale1, list(Age = 37.10119))

mean(comboFemale2$Age, na.rm = T)
comboFemale2 = replace_na(comboFemale2, list(Age = 27.46923))

mean(comboFemale3$Age, na.rm = T)
comboFemale3 = replace_na(comboFemale3, list(Age = 22.29718))


ComboNEW = rbind(comboMale1, comboMale2, comboMale3, comboFemale1, comboFemale2, comboFemale3)
View(ComboNEW)

ComboNEW$Sex = as.factor(ComboNEW$Sex)
ComboNEW$Pclass = as.factor(ComboNEW$Pclass)

summary(ComboNEW)

#T Tests...CHANGE DATA TO "COMBO"!!!! AFter prediction is done
FemaleSurv = sqldf("SELECT * FROM ComboNEW WHERE Sex = 'female' AND Survived = '1'")
FemaleNOTSurv = sqldf("SELECT * FROM ComboNEW WHERE Sex = 'female' AND Survived = '0'")
MaleSurv = sqldf("SELECT * FROM ComboNEW WHERE Sex = 'male' AND Survived = '1'")
MaleNOTSurv = sqldf("SELECT * FROM ComboNEW WHERE Sex = 'male' AND Survived = '0'")
ALLSurv = sqldf("SELECT * FROM ComboNEW WHERE Survived = '1'")
ALLNotSurv = sqldf("SELECT * FROM ComboNEW WHERE Survived = '0'")

#Male and Female mean survive age same?
t.test(MaleSurv$Age, FemaleSurv$Age) #Yes, 0 in conf int

#Male and Female mean NOT survive age same?
t.test(MaleNOTSurv$Age, FemaleNOTSurv$Age) #No, 0 NOT in conf int

#Both genders - survive and not survive mean equal?
t.test(ALLNotSurv$Age, ALLSurv$Age) #Yes, 0 in conf int


#Logistic regression of COMBO!!
MyComboModel = glm(Survived ~ Pclass + Age + Sex, data = ComboNEW, family = binomial)

summary(MyTitanicModel)

#All significant above, so below is the equation of prob
Surv = exp(3.777013 + (-1.309799 * pclass2) + (-2.580625 * pclass3) + (-0.036985 * age) + (-2.522781 * Sexmale)) / (1 +(exp(3.777013 + (-1.309799 * pclass2) + (-2.580625 * pclass3) + (-0.036985 * age) + (-2.522781 * Sexmale))))


JustinGould = exp(3.777013 + (-1.309799 * 1) + (-2.580625 * 0) + (-0.036985 * 22) + (-2.522781 * 1)) / (1 +(exp(3.777013 + (-1.309799 * 1) + (-2.580625 * 0) + (-0.036985 * 22) + (-2.522781 * 1))))
DonaldTrump = exp(3.777013 + (-1.309799 * 0) + (-2.580625 * 0) + (-0.036985 * 71) + (-2.522781 * 1)) / (1 +(exp(3.777013 + (-1.309799 * 0) + (-2.580625 * 0) + (-0.036985 * 71) + (-2.522781 * 1))))
Oprah = exp(3.777013 + (-1.309799 * 0) + (-2.580625 * 0) + (-0.036985 * 64) + (-2.522781 * 0)) / (1 +(exp(3.777013 + (-1.309799 * 0) + (-2.580625 * 0) + (-0.036985 * 64) + (-2.522781 * 0))))
JustinGould
DonaldTrump
Oprah