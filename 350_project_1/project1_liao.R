auto.mpg <- read.csv("auto-mpg.csv", header = TRUE, sep = ",")

#boxplot(electric$FastCharge_KmH~electric$PowerTrain)#need numeric data
horsepower_Num = {}#empty vector
for (i in c(1:length(auto.mpg$horsepower))){
  if (auto.mpg$horsepower[i] == "?"){
    horsepower_Num[i] = 0;
  }else{
    noquote(strtoi(auto.mpg$horsepower[i]))
    horsepower_Num[i] = strtoi(auto.mpg$horsepower[i])
  }
}
# add the numeric horsepower to the data frame and delete the old one
auto.mpg.hpN <- cbind(horsepower_Num, auto.mpg)
auto.mpg.hpN <- subset(auto.mpg.hpN, select = -horsepower)
# delete the ones with invalid data
auto.mpg.hpN <- subset(auto.mpg.hpN, horsepower_Num != 0)

#research of the 3 cylinder and 5 cylinder cars
auto.mpg.hpN[which(auto.mpg.hpN$cylinders == 3),9]
auto.mpg.hpN[which(auto.mpg.hpN$cylinders == 5),9]
#remove the 5 and 3 cylinder cars
auto.mpg.hpN <- subset(auto.mpg.hpN, auto.mpg.hpN$cylinders != 3)
auto.mpg.hpN <- subset(auto.mpg.hpN, auto.mpg.hpN$cylinders != 5)

#histogram analysis and number summary, data distribution
hist(auto.mpg.hpN$horsepower_Num)
hist(auto.mpg.hpN$mpg)
hist(auto.mpg.hpN$cylinders, breaks = c(3,5,7,9))
hist(auto.mpg.hpN$displacement)
hist(auto.mpg.hpN$weight)
hist(auto.mpg.hpN$acceleration)
hist(auto.mpg.hpN$model.year)
hist(auto.mpg.hpN$origin, breaks = c(0.5,1.5,2.5,3.5))

#TODO horsepower - mpg, displacement, [accelerate]
plot(auto.mpg.hpN$mpg~log(auto.mpg.hpN$horsepower_Num))
relation.mpg.horsepower <- lm(auto.mpg.hpN$mpg~log(auto.mpg.hpN$horsepower_Num))
summary(relation.mpg.horsepower)
abline(relation.mpg.horsepower)

plot(auto.mpg.hpN$acceleration ~ auto.mpg.hpN$horsepower_Num)
relation.acc.horsepower <- lm(auto.mpg.hpN$acceleration ~ auto.mpg.hpN$horsepower_Num)
summary(relation.acc.horsepower)
abline(relation.acc.horsepower)
#TODO mpg (response value)

#TODO displacement - mpg
plot(auto.mpg.hpN$mpg~auto.mpg.hpN$displacement)
relation.mpg.displacement <- lm(auto.mpg.hpN$mpg~auto.mpg.hpN$displacement)
summary(relation.mpg.displacement)
abline(relation.mpg.displacement)

plot(auto.mpg.hpN$horsepower_Num~auto.mpg.hpN$displacement)
relation.horsepower.displacement <- lm(auto.mpg.hpN$horsepower_Num~auto.mpg.hpN$displacement)
summary(relation.horsepower.displacement)
abline(relation.horsepower.displacement)

plot(auto.mpg.hpN$acceleration~auto.mpg.hpN$displacement)
relation.acc.placement <- lm(auto.mpg.hpN$acceleration~auto.mpg.hpN$displacement)
summary(relation.acc.placement)
abline(relation.acc.placement)

#TODO weight - 
plot(auto.mpg.hpN$horsepower_Num~auto.mpg.hpN$weight)
relation.hp.weight <- lm(auto.mpg.hpN$horsepower_Num~auto.mpg.hpN$weight)
summary(relation.hp.weight)
abline(relation.hp.weight)

plot(auto.mpg.hpN$mpg~auto.mpg.hpN$weight)
relation.mpg.weight <- lm(auto.mpg.hpN$mpg~auto.mpg.hpN$weight)
summary(relation.mpg.weight)
abline(relation.mpg.weight)

plot(auto.mpg.hpN$acceleration~auto.mpg.hpN$weight)
relation.acc.weight <- lm(auto.mpg.hpN$acceleration~auto.mpg.hpN$weight)
summary(relation.acc.weight)
abline(relation.acc.weight)
#TODO accelerate - mpg
plot(auto.mpg.hpN$mpg~auto.mpg.hpN$acceleration)
relation.mpg.acc <- lm(auto.mpg.hpN$mpg~auto.mpg.hpN$acceleration)
summary(relation.mpg.acc)
abline(relation.mpg.acc)

#TODO cylinders - s-b-s boxplot WITH displacement, accelerate
boxplot(auto.mpg.hpN$horsepower_Num~auto.mpg.hpN$cylinders)
boxplot(auto.mpg.hpN$mpg~auto.mpg.hpN$cylinders)
boxplot(auto.mpg.hpN$displacement~auto.mpg.hpN$cylinders)
boxplot(auto.mpg.hpN$acceleration~auto.mpg.hpN$cylinders)
boxplot(auto.mpg.hpN$weight~auto.mpg.hpN$cylinders)
#TODO origin - s-b-s boxplot WITH horsepower, mpg, weight, 
boxplot(auto.mpg.hpN$horsepower_Num ~ auto.mpg.hpN$origin)
boxplot(auto.mpg.hpN$mpg~auto.mpg.hpN$origin)
boxplot(auto.mpg.hpN$displacement~auto.mpg.hpN$origin)
boxplot(auto.mpg.hpN$acceleration~auto.mpg.hpN$origin)
boxplot(auto.mpg.hpN$weight~auto.mpg.hpN$origin)
#TODO year of make 
boxplot(auto.mpg.hpN$mpg~auto.mpg.hpN$model.year)
