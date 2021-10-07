# 세종시 버스 노선별 정류장 승하차 수 데이터 분석
# 데이터는 노션에 공개

setRepositories(ind=1:7)

path = "C:/Users/ABC-LAB/Desktop/0709아이디어톤"

setwd(path)
getwd()

library(data.table)
library(readxl)
library(dplyr)
library(caret)
library(tidyr)
library(tidyverse)
library(corrplot)
library(stringr)
library(writexl)
library(ggplot2)

data.list <- list.files(path = paste0(path))
data.list

eachbus <- function(data.list){
  user.list <- list()
  name <- c("1004-1","1004","1005-1","1005","203-1","203","204-1","204","222-1","222","52","53","601-1","601")
  
  # 데이터 전처리
  for(i in c(1:length(data.list))){
    user<-read_excel(path = paste0(path,"/",data.list[i]),col_names=TRUE)
    
    user <- user[which(user[['사용자구분']]=="합계"),]
    user <- user[,c(1,2,4,ncol(user))]
    
    user <- user %>%
      group_by(정류장) %>%
      summarise("이용자 수"=sum(승차))
    
    colnames(user) <- c("정류장",name[i])
    
    user.list[[str_sub(data.list[i],1,-5)]] <- user
  }
  
  # 모든 노선별 데이터 합치기 
  result <- Reduce(function(x,y) merge(x,y,by="정류장",all=TRUE),user.list)
  
  # 정류장 별 총 탑승자 수 / 버스 정차 수 비율 구하기
  summ <- c() 
  stop <- c()
  for(i in 1:nrow(result)){
    summ <- append(summ,sum(result[i,2:ncol(result)],na.rm = TRUE))
    stop <- append(stop,length(which(!is.na(result[i,2:ncol(result)])==TRUE)))
  }
  
  ratio<-as.data.frame(summ/stop)
  final<-cbind(result,ratio)
  
  return(final)
}

final <- eachbus(data.list)
View(final)

summary(final$`summ/stop`)

# 이상치 찾기 
Q3 <- summary(final$`summ/stop`)[[5]]
Q1 <- summary(final$`summ/stop`)[[2]]

max <- Q3 + (Q3-Q1)*1.5
min <- Q1 - (Q3-Q1)*1.5 

# 탑승자는 많지만 정차하는 노선의 수가 적은 정류별 추출 
final[which(final$`summ/stop`>max),"정류장"]

# 시각화 
outliers <- c()
for(i in 1:nrow(final)){
  outliers <- append(outliers,ifelse(i %in% which(final$`summ/stop`>max),final[i,"정류장"],""))
}
outliers

windows(8,5)
ggplot(final,aes(x="station",y=`summ/stop`))+
  geom_boxplot(color="red",fill="orange",alpha=0.4,lwd=0.5,outlier.color = "darkorange4",outlier.size = 3,outlier.shape = 22)+
  theme_bw()+labs(x="station name",y="people / stop") + 
  geom_text(aes(label=outliers),position=position_dodge(0.7),vjust=-0.3,hjust=-0.1,size=3,color="navy",check_overlap = TRUE)
