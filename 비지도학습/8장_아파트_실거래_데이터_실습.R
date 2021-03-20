# 8장 아파트 실거래 데이터 실습 ####

# 1. 문제인식 ####

# 부동산 투자에 앞서 미래 가격 상승이 예상되는 지역 선정 필요 

# 가격 상승을 주도하는 강남아파트 가격과 유사한 패턴으로 상승할 만한 지역 탐색 필요 



# 2. 가설 설정 ####

# 가설 1 : 서울 전체 아파트와 강남 아파트의 가격은 변화하는 양상이 다를 것이다.

# 가설 2 : 강남 아파트의 가격 변화를 따라서 후행적으로 가격이 변화하는 지역이 있을 것이다. 




# 3. 데이터 준비 및 변수 설계####

setwd("비지도학습")
getwd()

df <- read.csv("apt_data_2010_2020.csv") # xlsb 확장자를 csv로 바꿔서 사용 
str(df)

# 거래금액, 건축년도, 년, 법정동, 월, 전용면적 변수들을 사용한다.

library(dplyr)

df$거래금액 <- gsub(',','',df$거래금액) %>% as.integer() #쉼표를 공백으로 치환 

df$법정동 <- gsub(' ','',df$법정동) # 스페이스바를 공백으로 바꿈 


# 변수 추가 

# 월 별로 비교하면 변동성이 너무 심하기 때문에 연/분기 별로 새로운 변수 생성

#분기
df$qrt <- ifelse(df$월 < 4,'Q1',ifelse(df$월<7,'Q2',ifelse(df$월<10,'Q3','Q4')))

# 연/분기
df$yyyyqrt <- paste(df$년,df$qrt)

#평수 = 약 3.3m2

df$평수 <- round(df$전용면적/3.3)

#평단가

df$평단가 <- df$거래금액/df$평수 

str(df)




# 4. 분석 (가설검정) ####

# 4-1.가설 1 : 서울 전체 아파트와 강남 아파트의 가격은 변화하는 양상이 다를 것이다.####

# -> 서울 전체 아파트/ 강남 아파트 가격 변화 추세 비교 필요

#     -> 서울 전체 아파트 매매 가격 추이

#     -> 강남 아파트 매매 가격 추이

#          -> 분기별 평균 거래금액 시각화 !!!



# 1) 서울 전체 아파트 매매가격 추이 

df.1 <- df %>% group_by(yyyyqrt) %>% summarise('평균평단가'=mean(평단가))

library(ggplot2)
theme_set(theme_grey(base_family = 'NanumGothic')) #그래프 내 폰트 설정 

df.1.plot <- ggplot(df.1,aes(x=yyyyqrt,y=평균평단가,group=1))+
  geom_line()+xlab("년도/분기")+ylab("평균 가격(만원")+
  ggtitle("서울 아파트 평당 가격 변화 추이")+
  theme(axis.text.x = element_text(angle=90))+stat_smooth(method='lm')+
  ylim(0,max(df.1$평균평단가))
df.1.plot
# 범주형 변수에서 선형 그래프를 그리기 위해서는 group=1 로 해야 한다
# stat_smooth(method='lm') => 그래프의 추세선 


# 2) 강남 아파트 매매 가격 추이

#    -> 반포동, 서초동, 삼성동, 압구정동 샘플링해서 분기별 평균 거래금액 시각화 


# 반포동

dong <- '반포동'

df.2 <- df %>% filter(법정동==dong)

df.2 <- df.2 %>% group_by(yyyyqrt) %>% summarise('평균평단가'=mean(평단가))

head(df.2)

df.2.plot <- ggplot(df.2,aes(x=yyyyqrt,y=평균평단가,group=1))+
  geom_line()+xlab("년도/분기")+ylab("평균 가격(만원")+
  ggtitle(paste0(dong," 아파트 평당 가격 변화 추이"))+
  theme(axis.text.x = element_text(angle=90))+stat_smooth(method='lm')+
  ylim(0,max(df.2$평균평단가))
df.2.plot


# 서초동

dong <- '서초동'

df.3 <- df %>% filter(법정동==dong)

df.3 <- df.3 %>% group_by(yyyyqrt) %>% summarise('평균평단가'=mean(평단가))

head(df.3)

df.3.plot <- ggplot(df.3,aes(x=yyyyqrt,y=평균평단가,group=1))+
  geom_line()+xlab("년도/분기")+ylab("평균 가격(만원")+
  ggtitle(paste0(dong," 아파트 평당 가격 변화 추이"))+
  theme(axis.text.x = element_text(angle=90))+stat_smooth(method='lm')+
  ylim(0,max(df.3$평균평단가))
df.3.plot

# 삼성동

dong <- '삼성동'

df.4 <- df %>% filter(법정동==dong)

df.4 <- df.4 %>% group_by(yyyyqrt) %>% summarise('평균평단가'=mean(평단가))

head(df.4)

df.4.plot <- ggplot(df.4,aes(x=yyyyqrt,y=평균평단가,group=1))+
  geom_line()+xlab("년도/분기")+ylab("평균 가격(만원")+
  ggtitle(paste0(dong," 아파트 평당 가격 변화 추이"))+
  theme(axis.text.x = element_text(angle=90))+stat_smooth(method='lm')+
  ylim(0,max(df.4$평균평단가))
df.4.plot

# 압구정동

dong <- '압구정동'

df.5 <- df %>% filter(법정동==dong)

df.5 <- df.5 %>% group_by(yyyyqrt) %>% summarise('평균평단가'=mean(평단가))

head(df.5)

df.5.plot <- ggplot(df.5,aes(x=yyyyqrt,y=평균평단가,group=1))+
  geom_line()+xlab("년도/분기")+ylab("평균 가격(만원")+
  ggtitle(paste0(dong," 아파트 평당 가격 변화 추이"))+
  theme(axis.text.x = element_text(angle=90))+stat_smooth(method='lm')+
  ylim(0,max(df.5$평균평단가))
df.5.plot


# 4개의 동 그래프 한번에 보여주기

library(gridExtra)

grid.arrange(df.2.plot,df.3.plot,df.4.plot,df.5.plot,nrow=2,ncol=2)

# 그래프를 보면 반포동과 서초동이 서서히 상승하다 갑자기 오르다가
# 왔다갔다 하는 추세를 보임 
# 삼성동과 압구정동 은 하락하다가 상승하다 빠졌다가 상승하다 빠지고
# 아직 오르지 않음 

# 삼성동이 반포동/서초동 가격을 따라가는 것 처럼 보이기도 함.

# 서울 아파트는 상승했다가 조금 빠지나 조금씩 회복하는 추세


# 즉 반포동과 서초동의 추세에 따라서 가격이 회복하는 지역을 찾으면 
# 그 지역에 어느정도 가격이 형성될지 예측하면 된다.





# 4-2.가설 2 : 강남 아파트의 가격 변화를 따라서 후행적으로 가격이 변화하는 지역이 있을 것이다. ####

# -> 반포동 3개월 이전 까지의 추세와 유사한 지역 탐색

#   -> 계층적 군집분석을 통해 유사한 추세의 지역 확인 

# - 가정 : 2020년 가격 하락세는 코로나 여파로 인한 일시적인 형상이며, 이전의 수준을 회복할 것이다.

#          => 2020년 이전의 데이터로 반포동과 유사한 추세의 지역 탐색

# - 동별 가격 변화 추세로 계층적 군집분석

#   => 각 동별 가격 추세의 상관관계를 유사도(거리) 행렬로 사용

#   => 유사도 행렬로 계층적 군집분석

# 시계열 데이터로 군집분석을 할 때는 추세선의 상관분석을 통해 사용
# => 유사도 행렬의  correlation 필요!!!






# 계층적 군집분석을 위한 데이터셋(유사도행렬) 준비

df.fil <- df %>% filter(!yyyyqrt %in% c('2020 Q1','2020 Q2')) #2020년 데이터 제거  

dong.list <- unique(df$법정동) # 동 리스트 추출(중복되는 행 제거)
dong.list

df.ts <- NULL
install.packages("TTR")
library(TTR)


# 동별/연분기별 이동평균 3 계산

for (i in dong.list) { 
  print(i)
  temp <- merge(i,df.fil%>%filter(법정동==i)%>% #필터링한 데이터를 merge
                  group_by(yyyyqrt)%>%
                  summarise('평균평단가'=mean(평단가))%>%
                  mutate(ma3=runMean(평균평단가,3))) #이동평균 3 계산 
  df.ts <- rbind(df.ts,temp)%>%na.omit() #이상치 제거 
  
}

head(df.ts)

# 2019년까지의 이동평균은 38개가 되어야함. 1년에 4개니까 40이고 2개(이동평균) 빼줌

# 이 값보다 작은 데이터가 많은데 그 이유는 빌라 혹은 아파트가 없는 지역에 있기 때문 

# 이런 지역들은 분석에서 제거



# 아파트 매매가 매 분기 발생하지 않은 지역 제거 

dong.list <- df.ts %>% group_by(x) %>%
  summarise(cnt=n_distinct(yyyyqrt))%>% #tttqrt 개수를 셈
  filter(cnt==38) %>% select(x) #38개인 것들만 선택핵해서 동들만 선택 

df.ts.banpo <- df.ts %>% filter(x=='반포동')

df.ts <- df.ts %>% filter(x %in% dong.list$x, x !='반포동')

str(df.ts)


# 한 열에 한 동의 데이터가 들어가도록 배치

df.trend <- list()
for (i in dong.list$x) {
  print(i)
  temp <- df.ts %>% filter(x==i) %>% select(ma3) %>% as.matrix()
  df.trend[[i]] <- temp[,1] #temp의 첫번쨰 열에 리스트를 넣음 
}

# 리스트를 풀어서 상관행렬을 구할 수 있는 상태로 만듦

df.trend <- as.data.frame(do.call(cbind,df.trend))




# 아까 이동평균을 구하면서 1,2,번쨰 행을 뻇엇기에 다시 붙여넣어야 함

# 반포동 최근 3개월 데이터(마지막행) 제거

# 나머지 동들은 가장 오래된 3개월차 데이터(첫번째 행) 제거

# cbind로 붙임

df.trend.banpo <- df.ts.banpo[-38,] %>% select(ma3) #마지막 행 제거

colnames(df.trend.banpo) <- '반포동'

df.trend <- df.trend[-1,] #전체 동 데이터에서 첫번째 행 뻄 

df.trend <- cbind(df.trend,df.trend.banpo)


head(df.trend)



# 유사도 행렬 (correlation)

df.cor <- cor(df.trend)
df.cor

# 계층적 군집분석

hc <- hclust(as.dist(df.cor),method = "ward.D2")

plot(hc,hang=-1,cex=0.35)

# 반포동과 유사한 패턴인가?

# -> 반포동/나머지동 correlation

cor.banpo <- sort(df.cor[197,1:197],decreasing = TRUE)%>% as.data.frame()
cor.banpo

# 반포동의 3개월 이전과 같은 추세를 보이는 곳들은 양평동5가,양평동3가 등

# 추세를 그래프로 살펴보자

# 이동평균 3 그래프

par(mfrow=c(2,3))
plot(df.trend$반포동,type='l')
plot(df.trend$양평동5가,type='l')
plot(df.trend$사당동,type='l')
plot(df.trend$문래동6가,type='l')
plot(df.trend$가락동,type='l') #추세를 따르지 않으나 비교를 위해 확인 
plot(df.trend$가산동,type='l') #추세를 따르지 않으나 비교를 위해 확

# 그래프의 패턴이 매우 유사하나 유사도행렬에서 값이 작은 지역은 패턴이 다르다


# 5. 결과의 활용방안 도출 ####

# - 반포동과 유사한 패턴의 지역 미래 가격 예측

#    - 2020년 초 거래 가격으로 수준으로 3개원 내 회복할 것으로 예측

#     => 위 예측을 바탕으로 추가 자료를 조사하여 투자 지역 선정



# - 특징적인 서울 아파트 가격의 흐름을 파악 가능

#   - 계층적 군집분석 결과로 어떻게 군집이 묶이는지 확인 가능

#   - 군집별 가격 흐름 파악