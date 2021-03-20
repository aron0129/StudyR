#### 1강 ####

#### chapter 1 ####

library(dplyr)
library(ggplot2)
library(car)
head(mpg) #데이터셋의 윗부분을 보여줌 
dim(mpg) #변수들의 개수 및 데이터 개수 보여줌(크기)
str(mpg)# 변수들의 속성을 보여줌 
summary(mpg) #요약통계량
View(mpg) #표로 보여줌(원자료를 직접 봄 -> 데이터가 많으면 소용X)


#1. 회사별 평균 연비 높은 순 정렬

mpg%>% group_by(manufacturer)%>% #데이터를 종류별로 나눔. 여기서는 제조사별로
     summarise(mean.hwy=mean(hwy))%>% #hwy의 평균을 요약함
     arrange(desc(mean.hwy)) #arrange(정렬), desc내림차순)

#2. 포드 연비 높은순 정렬
mpg%>% filter(manufacturer=='ford')%>%
     group_by(model)%>%
     arrange(desc(why))

# 3. 배기량이 연비에 미치는 영향 회귀분석
lm.mpg <- lm(data=mpg, hwy~displ) # 회귀분석   종속변수 ~ 독립변수 
summary(lm.mpg) 

#Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#    (Intercept)  35.6977     0.7204   49.55   <2e-16 *** 
#    displ        -3.5306     0.1945  -18.15   <2e-16 *** -> 두 변수간의 관계가 통계적으로 유의하다

#Multiple R-squared:  0.5868 -> 배기량이 고속도로 연비의 58.6%정도를 설명한다.


#4. 배기량과 연비 관계 그래프 
qplot(data=mpg, x=displ,y=hwy)

#일반적을 그냥 qplot을 사용하면 빈도그래프가 나온다 

#### chapter 2 ####


# mpg 변수 다루어보기

head(mpg) #먼저 데이터가 어떤 식으로 되어있는지 확인한다. 변수를 확인해본다.

#자동차 고속도로 연비
mean(mpg$hwy) #평균
max(mpg$hwy) # 최대값
min(mpg$hwy) # 최소값
hist(mpg$hwy)# 히스토그램

f<-seq(1,20,by=2) #1~10까지 2 증가


#함수(function)


#문자 처리 함수

e <-c("Help","me")
e
e1 <- paste(e,collapse=" ") #빈칸 구분자로 문자 붙이기기
e1


#함수 파라미터 지정하기

qplot(data=mpg,y=hwy,x=drv,geom="point") #geom = 그레프 형식

qplot(data=mpg,y=hwy,x=drv,geom="boxplot",colour=drv) #박스플롯, 변수별 색을 지정


