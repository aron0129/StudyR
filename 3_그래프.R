
####1. 산점도 ####

# ggplot2은 레이어를 쌓는 형식으로 구성되어있다


library(ggplot2)

# 배경 생성 
ggplot(data=mpg,aes(x=displ,y=hwy))# 배경 생성(x축, y축 지정)


# 그래프 추가하기
ggplot(data=mpg,aes(x=displ,y=hwy))+ geom_point()# geom_point() #배경에 산점도 추가

#x,y축 범위 지정
ggplot(data=mpg,aes(x=displ,y=hwy))+
     geom_point()+
     xlim(3,6)+ #x축 범위 3~6
     ylim(10,30)#y축 범위 10~30


# qplot은 전처리 단계 데이터 확인용 문법 간단, 기능단순
#ggplot : 최종보고용, 색 크기 폰트 등 세부조작 가능



#### 그래프 문제 1 ####


#q1. mpg데이터에서 cty와 hwy 간 산점도
library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg)
ggplot(data=mpg,aes(x=cty,y=hwy))+geom_point()

#q2. midwest 데이터에서 x축 poptotal, y축 popasian으로 산점도 제작. x는 50만 이하, y는 1만 이하
mid <- as.data.frame(ggplot2::midwest)
summary(mid$poptotal)
summary(mid$popasian)
ggplot(data=mid,aes(x=poptotal,y=popasian))+
     geom_point()+xlim(0,500000)+ylim(0,10000)



####2. 막대그래프 ####

####1) 집단별 평균 그래프 ####

# 집단멸 평균 막대그래프를 그리기 위해서는 먼저 집단별 평균표를 만들어야함 

# 집단별 평균표 만들기

library(dplyr)
df_mpg <- mpg %>% 
     group_by(drv) %>% 
     summarise(mean_hwy=mean(hwy))

df_mpg

#2. 그래프 생성
ggplot(data=df_mpg,aes(x=drv,y=mean_hwy))+geom_col() #col에 들어있는 값을 쓰기 떄문에 geom_col

#3. 크기 순으로 정렬
ggplot(data=df_mpg,aes(x=reorder(drv,-mean_hwy),y=mean_hwy))+geom_col()
#reorder : drv를 mean_hwy를 기준으로 내림차순으로 정렬



##### 2) 빈도막대그래프 ####

#y축 입력 X

ggplot(data=mpg,aes(x=drv))+geom_bar()#x축은 범주변수, y축은 빈도

ggplot(data=mpg,aes(x=hwy))+geom_bar()#x축은 연속변수, y축은 빈도-> 히스토그램 같이 나옴




#### 그래프문제 2 ####

#Q1. mpg데이터에서 suv 대상으로 평균 cty가 가장 높은 회사 5곳을 막대그래프로 표현, 연비 높은 순으로
mpg <- as.data.frame(ggplot2::mpg)
mpg1 <- mpg %>% 
     group_by(manufacturer) %>%
     filter(class=="suv") %>% 
     summarise(mean_cty=mean(cty)) %>% 
     arrange(desc(mean_cty)) %>% 
     head(5)
mpg1
ggplot(data=mpg1,aes(x=reorder(manufacturer,-mean_cty),y=mean_cty))+
     geom_col()


#Q2. 어떤 class 자동차가 많은지 알아보기 위해 자동차 종류별 빈도 막대그래프 만들기
ggplot(data=mpg,aes(x=class))+geom_bar()


#### 3. 선그래프 ####

#### 1) 시계열 그래프 만들기 ####

ggplot(data=economics,aes(x=date,y=unemploy))+geom_line()
#실업자수가 70년대부터 등락을 반복하며 증가하다가 2008,9년에 급격히 증가하다가 지금은 떨어지는 경향이 있따

#### 그래프문제 3 ####

# economics데이터에서 psavert(개인 저축율)가 시간에 따라서 어떻게 변해왔는지 알아보기위해 시계열 그래프 그려보기
head(economics)
ggplot(data=economics,aes(x=date,y=psavert))+geom_line()



#### 4. 상자그림 ####

# 집단 간 분포 차이를 표현할 떄 많이 사용 

#원자료를 사용한다. group_by나 summarise 사용할 필요 없다!!!!!

ggplot(data=mpg,aes(x=drv,y=hwy))+geom_boxplot()


#r극단치를 구분하는 기준은 IQR(4분위수 거리)= (3사분위수-1사분위수)*1.5 보다 밖에 있으면 극단치 

# 먼저 중앙값을 본다.

# 두번째로는 상자의 폭을 보면 된다. 면적이 넓을수록 다양성이 크다는 의미
# 좁다면 상대적으로 비슷한 동질의 자동차가 많은 것

# 중앙값이 박스 면적에서 가운데에 봉오리가 있으면 정상분포로 보임. 왼쪽에 치우치면 좌편포 그래프 


#### 그래프문제 4 ####

#Q1. class에서 compact, subcompact, suv의 cty 상자그림 그리기
mpg <- as.data.frame(ggplot2::mpg)
mpg1 <- mpg %>% 
     filter(class =="compact"| class=="subcompact" | class=="suv") %>% 
     select(class,cty) %>% 
     group_by(class) 

mpg1
ggplot(data=mpg1,aes(x=class,y=cty))+geom_boxplot()
#subcompact, compact suv순으로 연비가 높다
#subcompact는 상자 면적이 넓으므로 연비가 높고 낮은 차들이 고르게 분포 
# suv가 일반적으로 연비가 낮으나 극단치가 subcompact의 중앙값보다 높으므로 어떤 차종들은 좋은 연비를 가지고 있따. 
# 일반적으로 compact가 높으나(중앙값 등) 전체를 통틀어서 좋은 차는 subcompact 차량이다 
