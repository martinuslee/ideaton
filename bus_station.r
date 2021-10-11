# ������ ���� �뼱�� ������ ������ �� ������ �м�
# �����ʹ� ��ǿ� ����

setRepositories(ind=1:7)

path = "C:/Users/ABC-LAB/Desktop/0709���̵����"

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
  
  # ������ ��ó��
  for(i in c(1:length(data.list))){
    user<-read_excel(path = paste0(path,"/",data.list[i]),col_names=TRUE)
    
    user <- user[which(user[['����ڱ���']]=="�հ�"),]
    user <- user[,c(1,2,4,ncol(user))]
    
    user <- user %>%
      group_by(������) %>%
      summarise("�̿��� ��"=sum(����))
    
    colnames(user) <- c("������",name[i])
    
    user.list[[str_sub(data.list[i],1,-5)]] <- user
  }
  
  # ��� �뼱�� ������ ��ġ�� 
  result <- Reduce(function(x,y) merge(x,y,by="������",all=TRUE),user.list)
  
  # ������ �� �� ž���� �� / ���� ���� �� ���� ���ϱ�
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

# �̻�ġ ã�� 
Q3 <- summary(final$`summ/stop`)[[5]]
Q1 <- summary(final$`summ/stop`)[[2]]

max <- Q3 + (Q3-Q1)*1.5
min <- Q1 - (Q3-Q1)*1.5 

# ž���ڴ� ������ �����ϴ� �뼱�� ���� ���� ������ ���� 
final[which(final$`summ/stop`>max),"������"]

# �ð�ȭ 
outliers <- c()
for(i in 1:nrow(final)){
  outliers <- append(outliers,ifelse(i %in% which(final$`summ/stop`>max),final[i,"������"],""))
}
outliers

windows(8,5)
ggplot(final,aes(x="station",y=`summ/stop`))+
  geom_boxplot(color="red",fill="orange",alpha=0.4,lwd=0.5,outlier.color = "darkorange4",outlier.size = 3,outlier.shape = 22)+
  theme_bw()+labs(x="station name",y="people / stop") + 
  geom_text(aes(label=outliers),position=position_dodge(0.7),vjust=-0.3,hjust=-0.1,size=3,color="navy",check_overlap = TRUE)
