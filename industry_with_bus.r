# 세종시 동 별 산업 발전 수준 데이터 분석 
# 데이터는 노션에 공개

setRepositories(ind=1:7)

path = "C:\\Users\\ABC-LAB\\Desktop\\아이디어톤 데이터 정리"

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

# 사용할 때 지번, 도로 순으로 column 인자를 전달해야함. 
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
  
  ## NA 값 처리
  naindex <- which(is.na(data[[column[1]]])==TRUE)
  if(identical(integer(0),naindex)){
  }else{
    data <- data[-naindex,]
  }
  
  ## 이제 불필요한 도로명주소 column 날리기 (있었다면)
  if(!is.na(column[2])){
    data <- data[,-which(colnames(data)==column[2])]
  }
  
  return(data)
}

######################### bus information ###########################
bus<-read_excel(path = paste0(path,"\\",data.list[2]))

bus<-bus%>%
  rename("동"="동 이름")

bus <- bus[-c(1,4,8,11,16,25,27,28),] # 가람동, 나성동, 반곡동, 산울동,  어진동, 집현동, 합강동, 해밀동

dong <- bus$`동`


# 이 때 신도시들에 대해서는 정확한 데이터를 확보 하기 어려웠기 때문에 일부 동은 제외 시켜야 한다. 


######################### cctv information ###########################
cctv <- read.csv(paste0(path,"\\",data.list[3]))
#View(cctv)
#colnames(cctv)
cctv <- cctv[,c(2,3,5)]

cctv<-autoEX(cctv,c("소재지지번주소","소재지도로명주소"),dong)

cctv <- cctv %>%
  group_by(소재지지번주소) %>%
  summarise("카메라대수"=sum(카메라대수))

cctv<-cctv%>%
  rename("동"="소재지지번주소")

######################### enterprise information ########################### 
enterprise <- read.csv(paste0(path,"\\",data.list[4]))
#View(enterprise)
#colnames(enterprise)
enterprise <- enterprise[,c(2,6)]

enterprise <- autoEX(enterprise,"공장대표주소",dong)

enterprise <- enterprise %>%
  group_by(공장대표주소) %>%
  summarise("기업종업원수"=sum(종업원수))

enterprise<-enterprise%>%
  rename("동"="공장대표주소")

######################### health institution information ########################### 
health <- read.csv(paste0(path,"\\",data.list[5]))
#View(health)
#colnames(health)
health <- health[,c(3,5)]

health <- autoEX(health,"소재지",dong)

health <- health %>%
  group_by(소재지) %>%
  summarise("보건소수"=n())

health<-health%>%
  rename("동"="소재지")

######################### restaurant information ########################### 
restaurant <- read.csv(paste0(path,"\\",data.list[7]))
#View(restaurant)
#colnames(restaurant)
restaurant <- restaurant[,c(3,4,2)]

restaurant <- autoEX(restaurant,c("소재지.지번.","소재지.도로명."),dong)

restaurant <- restaurant %>%
  group_by(소재지.지번.) %>%
  summarise("일반음식점수"=n())

restaurant<-restaurant%>%
  rename("동"="소재지.지번.")

######################### pharmacy information ########################### 
pharmacy <- read.csv(paste0(path,"\\",data.list[9]))
#View(pharmacy)
#colnames(pharmacy)
pharmacy <- pharmacy[,c(2,1)]

pharmacy <- autoEX(pharmacy,"약국소재지.도로명주소",dong)

pharmacy <- pharmacy %>%
  group_by(약국소재지.도로명주소) %>%
  summarise("약국수"=n())

pharmacy<-pharmacy%>%
  rename("동"="약국소재지.도로명주소")

######################### tumbrel(이륜 자가용+관용)  information ###########################
tumbrel <- read.csv(paste0(path,"\\",data.list[11]))
#View(tumbrel)
#colnames(tumbrel)
tumbrel <- tumbrel[,c(1,2,3)]

tumbrel <- autoEX(tumbrel,"행정동명",dong)

tumbrel <- tumbrel %>%
  group_by(행정동명) %>%
  summarise("등록 이륜차수"=sum(관용차,자가용))

tumbrel<-tumbrel%>%
  rename("동"="행정동명")

######################### car registration information ########################### 
car <- read.csv(paste0(path,"\\",data.list[12]))
#View(car)
#colnames(car)
car <- car[,c(1,3,4)]

car <- autoEX(car,"구분",dong)

car <- car %>%
  group_by(구분) %>%
  summarise("등록 자동차수"=sum(승용차,승합차))

car<-car%>%
  rename("동"="구분")

######################### sport facility information ###########################
sport <- read.csv(paste0(path,"\\",data.list[13]))
#View(sport)
#colnames(sport)
sport <- sport[,c(5,6,1)]

sport <- autoEX(sport,c("시설주소.지번.","시설주소.도로명."),dong)

sport <- sport %>%
  group_by(시설주소.지번.) %>%
  summarise("체육시설수"=n())

sport<-sport%>%
  rename("동"="시설주소.지번.")

######################### real estate information ###########################
estate <- read.csv(paste0(path,"\\",data.list[6]))
#View(estate)
#colnames(estate)
estate <- estate[,c(6,2)]

estate <- autoEX(estate,"사무소주소",dong)

estate <- estate %>%
  group_by(사무소주소) %>%
  summarise("부동산중개업사무소수"=n())

estate<-estate%>%
  rename("동"="사무소주소")

######################### 공중위생 information ###########################
sanitary <- read.csv(paste0(path,"\\",data.list[14]))
#View(sanitary)
#colnames(sanitary)
sanitary <- sanitary[,c(4,5,3)]

sanitary <- autoEX(sanitary,c("영업소.지번.주소","영업소.도로명.주소"),dong)

sanitary <- sanitary %>%
  group_by(영업소.지번.주소) %>%
  summarise("공중위생업소수"=n())

sanitary<-sanitary%>%
  rename("동"="영업소.지번.주소")


######################### 소독 의무시설 information ###########################
disinfection <- read.csv(paste0(path,"\\",data.list[15]))
#View(disinfection)
#colnames(disinfection)
disinfection <- disinfection[,c(4,3)]

disinfection <- autoEX(disinfection,"소재지",dong)

disinfection <- disinfection %>%
  group_by(소재지) %>%
  summarise("소독의무시설수"=n())

disinfection<-disinfection%>%
  rename("동"="소재지")



######################### population information ###########################
population <- read_excel(paste0(path,"\\",data.list[1]))
#View(population)
#colnames(population)




############################### Construct final dataframe #####################################

all.data <- list(bus=bus,cctv=cctv,enterprise=enterprise,health=health,restaurant=restaurant,pharmacy=pharmacy,
                 tumbrel=tumbrel,car=car,sport=sport,estate=estate,sanitary=sanitary,disinfection=disinfection,population=population)

final <- Reduce(function(x,y) merge(x,y,by="동",all=TRUE),all.data)

# NA 값을 0으로 전환: 조사 되지 않은 정보들을 어떻게 대체하는 것이 알맞은지 파악 필요 
final[is.na(final)] <- 0 

View(final)

################################### Modeling ######################################

# 상관 계수 확인 
dim(final)
correlation <- cor(final[2:14])
corrplot(correlation)

# final 데이터프레임 구성이 바뀜
rownames(final) <- final$동
final <- final[,-1]
View(final)

# 모델링 
fullmodel <-lm(final$`버스 운행 수`~.,data=final)
summary(fullmodel)

step(fullmodel,direction = "both")

# 최적의 변수 조합으로 구성된 모델 
selected<-lm(formula = final$`버스 운행 수` ~ 카메라대수 + 기업종업원수 + 
               보건소수 + 일반음식점수 + 약국수 + `등록 이륜차수` + `등록 자동차수` + 
               체육시설수 + 부동산중개업사무소수 + 공중위생업소수 + 소독의무시설수 + 
               인구수, data = final)
summary(selected)

# 마지막으로 문제 있는 동 선택하기 
predicted <- as.data.frame(selected$fitted.values)

intersect(rownames(predicted),rownames(final[1]))
dim(final[1])[1] == dim(predicted)[1]

compared <- cbind(predicted,final[1])

colnames(compared) <- c("predicted","Real")

View(compared)


# predicted / Real = 이 값에 1에 가까울 수록 문제가 없는 동이라 판단, 멀수록 문제가 있는 동이라 판단 
compared[,1]/compared[,2]
dong==rownames(compared)

plot(xlab="name",ylab="ratio",compared[,1]/compared[,2],pch=15,cex=0.6,col="red")
text(compared[,1]/compared[,2],row.names(compared),pos=3)
abline(a=1,b=0,col="blue",lty=6)