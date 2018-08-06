#Read in csv files
Temp <- read.csv("/Users/paotingkung/Desktop/TempUSA.csv", header = TRUE)
str(Temp)
summary(Temp)
install.packages("MASS")
pairs(Temp)
Temp.lm<-lm(NYMin~Date, date=Temp)

#Inbuilt dataset in R
temptrend<-ts(Temp, frequency=12, start =c(2000,1))
is.ts(temptrend)
str(temptrend)
Temp1=Temp$LosAngelesMin
View(Temp1)
str(Temp1)
Temp2<-ts(Temp1, frequency=12, start =c(2000,1) )
str(Temp2)
plot(Temp2)
class(Temp2)
x=stl(Temp2,s.window = "periodic")
plot(x)
#########################
#The command for decomposition is stl, seasonal trend decomposition
#Make a stl object #LAtemmin
TempLATS<-ts(TempLA, frequency=12, start =c(2000,1) )
str(TempLATS)
plot(TempLATS)
class(TempLATS)
x=stl(TempLATS,s.window = "periodic")
plot(x)
##NYMin Inbuilt data in R
TempNY=Temp$NYMin
View(TempNY)
str(TempNY)
#Make a stl object #NYmin
TempNYts<-ts(TempNY,frequency=12,start=c(2000,1))
x1=stl(TempNYts,s.window = "periodic")
plot(x1)
##HoustonMin Inbuilt data in R
TempHous=Temp$HoustonMin
#Make a stl object #Houstmin
TempHousts<-ts(TempHous,frequency=12,start = c(2000,1))
x2=stl(TempHousts,s.window = "periodic")
plot(x2)
#ChicagoMin Inbuilt data in R
Tempchi=Temp$ChicagoMin
#Make a stl object #Chicagomin
Tempchits<-ts(Tempchi,frequency = 12,start = c(2000,1))
x3=stl(Tempchits,s.window = "periodic")
plot(x3)
#Seattle Inbuilt data in R
Tempsea=Temp$SeattleMin
#Make a stl object #Seattle
Tempseats<-ts(Tempsea,frequency = 12,start = c(2000,1))
x4=stl(Tempseats,s.window = "periodic")
plot(x4)

##Density Plot
d<-density(Temp$LosAngelesMin)
plot(d)
plot(d,main="Weight")
polygon(d,col = "red",border="blue")


TNY<-density(Temp$NYMin)
plot(TNY)
polygon(TNY, col = "red",border = "blue")

set.seed(1234)
ind<-sample(2,nrow)

install.packages("rpart")
library(rpart)
#look at data
str(Temp)
#Create the model
Temp_model<-rpart(formula = default~.,data=Temp,method = "class")
