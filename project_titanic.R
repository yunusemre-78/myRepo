library("dplyr")
library(ggplot2)
library(easyGgplot2)
library(forcats)
library(randomForest)

train = read.csv("C:/Users/YunusEmre/Documents/R_Projects/Onur_Boyar/titanic/train.csv")
test = read.csv("C:/Users/YunusEmre/Documents/R_Projects/Onur_Boyar/titanic/test.csv")

test$Survived = NA
allData = rbind(train,test) # 892 ve sonrasý test verisi

# Embarked => S = Southampton C = Cherbourg Q = Queenstown
# not %>% isareti DPLYR kutuphanesi icin GGPLOT kutuphanesinde ki + isareti gibidir
allData = allData %>%
  mutate(Pclass = as.factor(Pclass),
         Cabin = as.character(Cabin),
         Ticket = as.character(Ticket),
         Survived = as.factor(Survived),
         Name = as.character(Name))
str(allData)

plot1 = ggplot(allData[1:891,]) +
  geom_bar(mapping = aes(x = Pclass, fill = Survived), position = "fill")
# plot1'den anlasýlan daha üst seviye bilete sahip olanlarýn kurtulma þansý daha fazla

plot2 = ggplot(allData[1:891,]) +
  geom_freqpoly(mapping = aes(x = Age, color = Survived), bins = 60) +
  theme(legend.position = "none")
# plot2'den anlasýlan daha üst küçük yastakilerin kurtulma þansý daha fazla

plot3 = ggplot(allData[1:891,]) +
  geom_freqpoly(mapping = aes(x = Fare, color = Survived), bins = 10) +
  theme(legend.position = "none")
# plot3'ten anlasýlan daha üst pahalý bileti olanlarýn kurtulma þansý daha fazla

# sibsp = kardes sayýsý parch = cocuksa anne baba sayýsý anne babaysa cocuk sayýsý
plot4 = ggplot(allData[1:891,]) +
  geom_bar(mapping = aes(x = Parch+SibSp, fill = Survived), position = "fill") +
  theme(legend.position = "none")
# plot4'ten anlasýlan 2 veya 3 kisi olanlarýn sansý tek olanlara veya daha kalabalýk olanlara göre daha fazla

ggplot2.multiplot(plot1,plot2,plot3,plot4)

naTable = apply(allData, 2, function(x) sum(is.na(x)))

allData[!is.na(allData$Age),]$Age # bu þekilde na olanlar gelmiyor

titles = sub(".*,.([^.]*)\\..*","\\1",x = allData$Name) # Regular Expression ile unvanlarý ayýklýyoruz
allData$titles = titles
allData = allData %>%
  mutate(titles = as.factor(titles))

allData = allData %>%
  mutate(titles = fct_collapse(titles, "Miss" = c("Mlle","Ms"), "Mrs" = "Mme",
                               "Ranked" = c("Major","Dr","Capt","Rev","Col"),
                               "Royalty" = c("Lady","Don","Dona","the Countess","Sir","Jonkheer")))
levels(allData$titles)

plot5 = ggplot(allData[1:891,]) +
  geom_bar(mapping = aes(x = titles, fill = Survived), position = "fill") +
  theme(legend.position = "none")
# plot5'ten anlasýlan gemi çalýþanlarý ve yalnýz erkekler çok az oranda kurtulmuþ

# yasta bulunan NA'lar yerine ilgili unvana sahip kisilerin median deðerleri atandý
allData = allData %>%
  group_by(titles) %>%
  mutate(Age = ifelse(is.na(Age), round(median(Age,na.rm = T),1),Age))
sum(is.na(allData$Age))

cabinColumn = ifelse(allData$Cabin == "", FALSE, TRUE)
allData$cabinColumn = cabinColumn

plot6 = ggplot(allData[1:891,]) +
  geom_bar(mapping = aes(x = cabinColumn, fill = Survived), position = "fill") +
  theme(legend.position = "none")
# plot6'dan anlasýlan kabini bilinmeyen kisiler daha az oranda kurtulmuþ

allData %>% filter(is.na(Fare))
Fare = ifelse(is.na(allData$Fare), round(median(allData$Fare,na.rm = T),1),allData$Fare)
allData$Fare = Fare
sum(is.na(allData$Fare))

totalColumn = allData$SibSp + allData$Parch
allData$totalColumn = totalColumn
allData$totalColumn = as.factor(allData$totalColumn)
levels(allData$totalColumn)

train = allData[1:891,]
test = allData[892:1309,]

train1 = train[1:800,]
test1 = train[801:891,]

rf = randomForest(Survived ~ Pclass+Sex+SibSp+Parch+Age+Fare+cabinColumn+titles+totalColumn, train1, mtry = 3, ntree = 2000)

prediction = predict(rf, test1[,c(3,5,6,7,8,10,13,14,15)])

resultTable = table(prediction,test1$Survived)
(resultTable[1,1] + resultTable[2,2])/length(test1$Survived)

prediction = predict(rf, test[,c(3,5,6,7,8,10,13,14,15)])


forKaggle = test$PassengerId
forKaggle = as.data.frame(forKaggle)
colnames(forKaggle) = c("PassengerId")
forKaggle$Survived = prediction

write.csv(forKaggle,"result.csv",row.names = F)
