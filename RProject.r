
setwd("C:/Users/Dell/Desktop/R Project 3")
start <- read.csv("dataset.csv")

start$avg <- (start$s_avg + start$f_avg)/2

table(start$salary==0)
table(start$salary==999)


startplaced <- subset(start, salary!=0)
startplaced <- subset(startplaced, salary!=999)
startplaced <- subset(startplaced, salary!=998)


startunplaced <- subset(start, salary==0)


summary(startplaced)

set.seed(2)
library(caTools)
split<-sample.split(startplaced$salary,SplitRatio=0.7)
training_data<-subset(startplaced,split=="TRUe")
testing_data<-subset(startplaced,split=="FALSE")




plot(startplaced$age, startplaced$work_yrs)#linear
plot(startplaced$exam_tot, startplaced$exam_tpc)#linear
plot(startplaced$exam_tot, startplaced$exam_qpc)
plot(startplaced$exam_qpc, startplaced$exam_vpc)
plot(startplaced$exam_tpc, startplaced$exam_qpc)
plot(startplaced$exam_tot, startplaced$s_avg)
plot(startplaced$exam_tot, startplaced$work_yrs)
plot(startplaced$exam_tot, startplaced$avg)
plot(startplaced$salary, startplaced$work_yrs)#linear
plot(startplaced$salary, startplaced$exam_qpc)
plot(startplaced$salary, startplaced$exam_vpc)
plot(startplaced$salary, startplaced$exam_tpc)
plot(startplaced$salary, startplaced$s_avg)
plot(startplaced$salary, startplaced$avg)#linear
plot(startplaced$salary, startplaced$age)#linear
plot(startplaced$salary, startplaced$exam_tot)#linear



library(corrgram)
corrgram(startplaced, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie,text.panel = panel.txt,main="Corrgram of placed student intercorrelation")
cor(startplaced)




model1 <- lm(salary ~ age + work_yrs + avg + exam_tot + exam_vpc + exam_qpc + exam_tpc + s_avg + f_avg, data = startplaced)
summary(model1)

model1A <- lm(salary ~ age + work_yrs + avg + exam_vpc + exam_qpc + exam_tpc + s_avg, data = startplaced)
summary(model1A)
model1B <- lm(salary ~ age + avg + exam_vpc + exam_qpc + exam_tpc + s_avg, data = startplaced)
summary(model1B)
model1C <- lm(salary ~ age + exam_vpc + exam_qpc + exam_tpc + s_avg, data = startplaced)
summary(model1C)
model1D <- lm(salary ~ age + exam_vpc + exam_qpc + exam_tpc, data = startplaced)
summary(model1D)
model1E <- lm(salary ~ age + exam_qpc + exam_tpc, data = startplaced)
summary(model1E)

#regression  function: salary=33744.6+3010.7*age+313*exam_qpc-439.3*exam_tpc


plot(model1E)
predic<-predict(model1E,testing_data)
predic
plot(testing_data$salary,type="l",lty=1.8,col="green")
lines(predic,type="l",col="blue")

#predicting for a given value
predictdata<-read.csv("predict.csv")
print(predictdata)
print(predict(model1E,predictdata))
