# ������ �� �� ��� ���� ���� ������ �м� 
# �����ʹ� ��ǿ� ����

setRepositories(ind=1:7)

path = "C:\\Users\\ABC-LAB\\Desktop\\���̵���� ������ ����"

setwd(path)
getwd()

library(data.table)
library(readxl)
library(dplyr)
library(caret)
library(tidyr)
library(tidyverse)
library(corrplot)
library(writexl)

data.list <- list.files(path = paste0(path))
data.list

# ����� �� ����, ���� ������ column ���ڸ� �����ؾ���. 
autoEX <- function(data,column,dong){
  for(i in 1:nrow(data)){
    name <- dong[which(str_detect(data[i,column[1]],dong))]
    if(identical(character(0),name)){
      data[i,column[1]] <- NA
      if(!is.na(column[2])){
        name2 <- dong[which(str_detect(data[i,column[2]],dong))]
        if(identical(character(0),name2)){
          data[i,column[2]] <- NA
        }else{
          data[i,column[1]] <- name2
        }
      }
    }else{
      data[i,column[1]] <- name
    }
  }
  
  ## NA �� ó��
  naindex <- which(is.na(data[[column[1]]])==TRUE)
  if(identical(integer(0),naindex)){
  }else{
    data <- data[-naindex,]
  }
  
  ## ���� ���ʿ��� ���θ��ּ� column ������ (�־��ٸ�)
  if(!is.na(column[2])){
    data <- data[,-which(colnames(data)==column[2])]
  }
  
  return(data)
}

######################### bus information ###########################
bus<-read_excel(path = paste0(path,"\\",data.list[2]))

bus<-bus%>%
  rename("��"="�� �̸�")

bus <- bus[-c(1,4,8,11,16,25,27,28),] # ������, ������, �ݰ, ��ﵿ,  ������, ������, �հ���, �عе�

dong <- bus$`��`


# �� �� �ŵ��õ鿡 ���ؼ��� ��Ȯ�� �����͸� Ȯ�� �ϱ� ������� ������ �Ϻ� ���� ���� ���Ѿ� �Ѵ�. 


######################### cctv information ###########################
cctv <- read.csv(paste0(path,"\\",data.list[3]))
#View(cctv)
#colnames(cctv)
cctv <- cctv[,c(2,3,5)]

cctv<-autoEX(cctv,c("�����������ּ�","���������θ��ּ�"),dong)

cctv <- cctv %>%
  group_by(�����������ּ�) %>%
  summarise("ī�޶���"=sum(ī�޶���))

cctv<-cctv%>%
  rename("��"="�����������ּ�")

######################### enterprise information ########################### 
enterprise <- read.csv(paste0(path,"\\",data.list[4]))
#View(enterprise)
#colnames(enterprise)
enterprise <- enterprise[,c(2,6)]

enterprise <- autoEX(enterprise,"�����ǥ�ּ�",dong)

enterprise <- enterprise %>%
  group_by(�����ǥ�ּ�) %>%
  summarise("�����������"=sum(��������))

enterprise<-enterprise%>%
  rename("��"="�����ǥ�ּ�")

######################### health institution information ########################### 
health <- read.csv(paste0(path,"\\",data.list[5]))
#View(health)
#colnames(health)
health <- health[,c(3,5)]

health <- autoEX(health,"������",dong)

health <- health %>%
  group_by(������) %>%
  summarise("���ǼҼ�"=n())

health<-health%>%
  rename("��"="������")

######################### restaurant information ########################### 
restaurant <- read.csv(paste0(path,"\\",data.list[7]))
#View(restaurant)
#colnames(restaurant)
restaurant <- restaurant[,c(3,4,2)]

restaurant <- autoEX(restaurant,c("������.����.","������.���θ�."),dong)

restaurant <- restaurant %>%
  group_by(������.����.) %>%
  summarise("�Ϲ���������"=n())

restaurant<-restaurant%>%
  rename("��"="������.����.")

######################### pharmacy information ########################### 
pharmacy <- read.csv(paste0(path,"\\",data.list[9]))
#View(pharmacy)
#colnames(pharmacy)
pharmacy <- pharmacy[,c(2,1)]

pharmacy <- autoEX(pharmacy,"�౹������.���θ��ּ�",dong)

pharmacy <- pharmacy %>%
  group_by(�౹������.���θ��ּ�) %>%
  summarise("�౹��"=n())

pharmacy<-pharmacy%>%
  rename("��"="�౹������.���θ��ּ�")

######################### tumbrel(�̷� �ڰ���+����)  information ###########################
tumbrel <- read.csv(paste0(path,"\\",data.list[11]))
#View(tumbrel)
#colnames(tumbrel)
tumbrel <- tumbrel[,c(1,2,3)]

tumbrel <- autoEX(tumbrel,"��������",dong)

tumbrel <- tumbrel %>%
  group_by(��������) %>%
  summarise("��� �̷�����"=sum(������,�ڰ���))

tumbrel<-tumbrel%>%
  rename("��"="��������")

######################### car registration information ########################### 
car <- read.csv(paste0(path,"\\",data.list[12]))
#View(car)
#colnames(car)
car <- car[,c(1,3,4)]

car <- autoEX(car,"����",dong)

car <- car %>%
  group_by(����) %>%
  summarise("��� �ڵ�����"=sum(�¿���,������))

car<-car%>%
  rename("��"="����")

######################### sport facility information ###########################
sport <- read.csv(paste0(path,"\\",data.list[13]))
#View(sport)
#colnames(sport)
sport <- sport[,c(5,6,1)]

sport <- autoEX(sport,c("�ü��ּ�.����.","�ü��ּ�.���θ�."),dong)

sport <- sport %>%
  group_by(�ü��ּ�.����.) %>%
  summarise("ü���ü���"=n())

sport<-sport%>%
  rename("��"="�ü��ּ�.����.")

######################### real estate information ###########################
estate <- read.csv(paste0(path,"\\",data.list[6]))
#View(estate)
#colnames(estate)
estate <- estate[,c(6,2)]

estate <- autoEX(estate,"�繫���ּ�",dong)

estate <- estate %>%
  group_by(�繫���ּ�) %>%
  summarise("�ε����߰����繫�Ҽ�"=n())

estate<-estate%>%
  rename("��"="�繫���ּ�")

######################### �������� information ###########################
sanitary <- read.csv(paste0(path,"\\",data.list[14]))
#View(sanitary)
#colnames(sanitary)
sanitary <- sanitary[,c(4,5,3)]

sanitary <- autoEX(sanitary,c("������.����.�ּ�","������.���θ�.�ּ�"),dong)

sanitary <- sanitary %>%
  group_by(������.����.�ּ�) %>%
  summarise("�����������Ҽ�"=n())

sanitary<-sanitary%>%
  rename("��"="������.����.�ּ�")


######################### �ҵ� �ǹ��ü� information ###########################
disinfection <- read.csv(paste0(path,"\\",data.list[15]))
#View(disinfection)
#colnames(disinfection)
disinfection <- disinfection[,c(4,3)]

disinfection <- autoEX(disinfection,"������",dong)

disinfection <- disinfection %>%
  group_by(������) %>%
  summarise("�ҵ��ǹ��ü���"=n())

disinfection<-disinfection%>%
  rename("��"="������")



######################### population information ###########################
population <- read_excel(paste0(path,"\\",data.list[1]))
#View(population)
#colnames(population)




############################### Construct final dataframe #####################################

all.data <- list(bus=bus,cctv=cctv,enterprise=enterprise,health=health,restaurant=restaurant,pharmacy=pharmacy,
                 tumbrel=tumbrel,car=car,sport=sport,estate=estate,sanitary=sanitary,disinfection=disinfection,population=population)

final <- Reduce(function(x,y) merge(x,y,by="��",all=TRUE),all.data)

# NA ���� 0���� ��ȯ: ���� ���� ���� �������� ��� ��ü�ϴ� ���� �˸����� �ľ� �ʿ� 
final[is.na(final)] <- 0 

View(final)

################################### Modeling ######################################

# ��� ��� Ȯ�� 
dim(final)
correlation <- cor(final[2:14])
corrplot(correlation)

# final ������������ ������ �ٲ�
rownames(final) <- final$��
final <- final[,-1]
View(final)

# �𵨸� 
fullmodel <-lm(final$`���� ���� ��`~.,data=final)
summary(fullmodel)

step(fullmodel,direction = "both")

# ������ ���� �������� ������ �� 
selected<-lm(formula = final$`���� ���� ��` ~ ī�޶��� + ����������� + 
               ���ǼҼ� + �Ϲ��������� + �౹�� + `��� �̷�����` + `��� �ڵ�����` + 
               ü���ü��� + �ε����߰����繫�Ҽ� + �����������Ҽ� + �ҵ��ǹ��ü��� + 
               �α���, data = final)
summary(selected)

# ���������� ���� �ִ� �� �����ϱ� 
predicted <- as.data.frame(selected$fitted.values)

intersect(rownames(predicted),rownames(final[1]))
dim(final[1])[1] == dim(predicted)[1]

compared <- cbind(predicted,final[1])

colnames(compared) <- c("predicted","Real")

View(compared)


# predicted / Real = �� ���� 1�� ����� ���� ������ ���� ���̶� �Ǵ�, �ּ��� ������ �ִ� ���̶� �Ǵ� 
compared[,1]/compared[,2]
dong==rownames(compared)

plot(xlab="name",ylab="ratio",compared[,1]/compared[,2],pch=15,cex=0.6,col="red")
text(compared[,1]/compared[,2],row.names(compared),pos=3)
abline(a=1,b=0,col="blue",lty=6)