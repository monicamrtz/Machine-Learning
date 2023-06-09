
#install.packages('mice')
#install.packages("e1071", dep = TRUE) 
library(tidyverse)
library(readr)
library(readxl)
library(MASS) ## a library of example datasets
library(class) ## a library with lots of classification tools
library(kknn)

ts <- read_excel("MSBA Texas McCombs/2022 Summer/Intro to Machine Learning/Data Sets/survey (1).xlsx")
nrow(ts)
ts<-filter(ts, work_interfere != "NA")
names(ts)



data_new <- ts %>%                          
  mutate(benefits = replace(benefits, benefits == "Yes", 1)) %>%
  mutate(benefits = replace(benefits, benefits =="No", -1)) %>%
  mutate(benefits = replace(benefits, benefits == "Don\'t know", 0)) %>%
  
  mutate(anonymity = replace(anonymity, anonymity == "Yes", 1)) %>%
  mutate(anonymity = replace(anonymity, anonymity =="No", -1)) %>%
  mutate(anonymity = replace(anonymity, anonymity == "Don\'t know", 0)) %>%
  
  mutate(care_options = replace(care_options, care_options == "Yes", 1)) %>%
  mutate(care_options = replace(care_options, care_options =="No", -1)) %>%
  mutate(care_options = replace(care_options, care_options == "Not sure", 0)) %>%
  
  mutate(wellness_program = replace(wellness_program, wellness_program == "Yes", 1)) %>%
  mutate(wellness_program = replace(wellness_program, wellness_program =="No", -1)) %>%
  mutate(wellness_program = replace(wellness_program, wellness_program == "Don't know", 0)) %>%
  
  mutate(seek_help = replace(seek_help, seek_help == "Yes", 1)) %>%
  mutate(seek_help = replace(seek_help, seek_help =="No", -1)) %>%
  mutate(seek_help = replace(seek_help, seek_help == "Don't know", 0)) %>%
  
  mutate(mental_health_consequence = replace(mental_health_consequence, mental_health_consequence == "Yes", 1)) %>%
  mutate(mental_health_consequence = replace(mental_health_consequence, mental_health_consequence =="No", -1)) %>%
  mutate(mental_health_consequence = replace(mental_health_consequence, mental_health_consequence == "Maybe", 0)) %>%
  
  mutate(supervisor = replace(supervisor, supervisor == "Yes", 1)) %>%
  mutate(supervisor = replace(supervisor, supervisor =="No", -1)) %>%
  mutate(supervisor = replace(supervisor, supervisor == "Some of them", 0)) %>%
  
  mutate(coworkers = replace(coworkers, coworkers == "Yes", 1)) %>%
  mutate(coworkers = replace(coworkers, coworkers =="No", -1)) %>%
  mutate(coworkers = replace(coworkers, coworkers == "Some of them", 0)) %>%
  
  mutate(mental_vs_physical = replace(mental_vs_physical, mental_vs_physical == "Yes", 1)) %>%
  mutate(mental_vs_physical = replace(mental_vs_physical, mental_vs_physical =="No", -1)) %>%
  mutate(mental_vs_physical = replace(mental_vs_physical, mental_vs_physical == "Don't know", 0)) %>%
  
  mutate(obs_consequence = replace(obs_consequence, obs_consequence == "Yes", 1)) %>%
  mutate(obs_consequence = replace(obs_consequence, obs_consequence =="No", 0)) %>%
  
  mutate(remote_work = replace(remote_work, remote_work == "Yes", 1)) %>%
  mutate(remote_work = replace(remote_work, remote_work =="No", 0)) %>%
  
  mutate(leave = replace(leave, leave == "Very difficult", -2)) %>%
  mutate(leave = replace(leave, leave =="Somewhat difficult", -1)) %>%
  mutate(leave = replace(leave, leave == "Don\'t know", 0)) %>%
  mutate(leave = replace(leave, leave =="Somewhat easy", 1)) %>%
  mutate(leave = replace(leave, leave =="Very easy", 2)) %>%
  
  mutate(work_interfere = replace(work_interfere, work_interfere =="Never", "NOT")) %>%
  mutate(work_interfere = replace(work_interfere, work_interfere == "Rarely", "YES")) %>%
  mutate(work_interfere = replace(work_interfere, work_interfere =="Sometimes", "YES")) %>%
  mutate(work_interfere = replace(work_interfere, work_interfere =="Often", "YES"))

  data_new[1:8] <- list(NULL) #taking out variables we are not considering for our regression
  data_new[2] <- NULL
  data_new[3] <- NULL
  data_new[10] <- NULL
  data_new[12:13] <- list(NULL)
  data_new[14:15] <- list(NULL)
  names(data_new)

#mean absolute error   
mae=c()

#set some k values
k = c(2, 5,10,15, 20,30,50,100,150,200)
for (i in k){
  set.seed(17)
  #we set the train and test
  train_data <- data_new[-((i*100-99):(i*100)), ]
  test_data <- data_new[((i*100-99):(i*100)), ]
  #linear discriminate analysis 
  m_lda <- lda(work_interfere ~ benefits + leave +care_options, data = train_data)
  #prediction
  m_pred <- predict(m_lda)
  mae <- c(mae, mean(abs( as.integer(m_pred$class == 'YES') - as.integer(train_data$work_interfere == 'YES'))))
  #mean absolute error of mae
  MAE= c()
  MAE=c(MAE, mean(mae))
}

m_lda
length(mae)
length(k)

plot(k, mae,type="b",xlab="k-fold",col="blue",ylab="MAE",lwd=2,cex.lab=1.2)
text(k[1]+15,mae[1]+0.0002,paste("k =",k[1],'| 0.211'),col=2,cex=0.8)
text(k[3]+9,mae[3]-0.0004,paste("k =",k[3]),col=2,cex=0.8)
text(k[4],mae[4]-0.0004,paste("k =",k[4]),col=2,cex=0.8)


