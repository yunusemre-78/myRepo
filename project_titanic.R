library("dplyr")
library(ggplot2)
library(easyGgplot2)
library(forcats)
library(randomForest)

train = read.csv("C:/Users/YunusEmre/Documents/R_Projects/Onur_Boyar/titanic/train.csv")
test = read.csv("C:/Users/YunusEmre/Documents/R_Projects/Onur_Boyar/titanic/test.csv")

test$Survived = NA
allData = rbind(train,test) # 892 ve sonras� test verisi

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
# plot1'den anlas�lan daha �st seviye bilete sahip olanlar�n kurtulma �ans� daha fazla

plot2 = ggplot(allData[1:891,]) +
  geom_freqpoly(mapping = aes(x = Age, color = Survived), bins = 60) +
  theme(legend.position = "none")
# plot2'den anlas�lan daha �st k���k yastakilerin kurtulma �ans� daha fazla

plot3 = ggplot(allData[1:891,]) +
  geom_freqpoly(mapping = aes(x = Fare, color = Survived), bins = 10) +
  theme(legend.position = "none")
# plot3'ten anlas�lan daha �st pahal� bileti olanlar�n kurtulma �ans� daha fazla

# sibsp = kardes say�s� parch = cocuksa anne baba say�s� anne babaysa cocuk say�s�
plot4 = ggplot(allData[1:891,]) +
  geom_bar(mapping = aes(x = Parch+SibSp, fill = Survived), position = "fill") +
  theme(legend.position = "none")
# plot4'ten anlas�lan 2 veya 3 kisi olanlar�n sans� tek olanlara veya daha kalabal�k olanlara g�re daha fazla

ggplot2.multiplot(plot1,plot2,plot3,plot4)

naTable = apply(allData, 2, function(x) sum(is.na(x)))

allData[!is.na(allData$Age),]$Age # bu �ekilde na olanlar gelmiyor

titles = sub(".*,.([^.]*)\\..*","\\1",x = allData$Name) # Regular Expression ile unvanlar� ay�kl�yoruz
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
# plot5'ten anlas�lan gemi �al��anlar� ve yaln�z erkekler �ok az oranda kurtulmu�

# yasta bulunan NA'lar yerine ilgili unvana sahip kisilerin median de�erleri atand�
allData = allData %>%
  group_by(titles) %>%
  mutate(Age = ifelse(is.na(Age), round(median(Age,na.rm = T),1),Age))
sum(is.na(allData$Age))

cabinColumn = ifelse(allData$Cabin == "", FALSE, TRUE)
allData$cabinColumn = cabinColumn

plot6 = ggplot(allData[1:891,]) +
  geom_bar(mapping = aes(x = cabinColumn, fill = Survived), position = "fill") +
  theme(legend.position = "none")
# plot6'dan anlas�lan kabini bilinmeyen kisiler daha az oranda kurtulmu�

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
