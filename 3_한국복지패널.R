#### 한국복지패널 데이터 ####

# 한국인의 삶의질 조사

# 분석1 : 성별에 따른 소득
# 분석2 : 나이와 소득의 관계
# 분석3 : 연령대에 따른 소득
# 분석4 : 연령대 및 성별에 따른 소득


setwd("123Data\\한국복지패널데이터")
getwd()


#spss 파일을 열 떄는 foreign 패키지를 다운받아야함
install.packages("foreign")
library(foreign)

#라이브러리 로드
library(dplyr)
library(ggplot2)

#데이터 불러오기
raw_welfare<-read.spss("data_spss_Koweps2014.sav",to.data.frame = T)#spss데이터를 data.frame으로 변경 


#데이터 카피
welfare <- raw_welfare

#데이터 검토 -> 데이터가 너무 크면 알아보기 힘듦 
dim(welfare)
str(welfare)
head(welfare)
summary(welfare)
View(welfare)

#변수명 변경 
welfare <- rename(welfare,
                  sex=h0901_4, # 성별
                  birth=h0901_5, # 태어난 연도
                  income=h09_din) # 소득

#### 분석1. 성별에 따른 소득 ####

# 성별변수 검토 및 수정

class(welfare$sex) #데이터의 속성을 알려줌
#[1] "numeric" -> 문자로 바꾸어주어야 함.

summary(welfare$sex)    

table(welfare$sex)

# 성별변수 값 변경

welfare$sex <- ifelse(welfare$sex==1,"male","female")
table(welfare$sex)
#female   male 
#2175   4873 
qplot(welfare$sex)

#남성이 많은 이유는 가구주의 성별로 나온거기 때문에 가구주를 남성으로 많이 했기 때문이다 





# 소득변수 검토 및 정제
class(welfare$income)

summary(welfare$income) # 마이너스는 부채로 판단 
#평균 가구소득이 낮다. 그 이유는 1인 가구도 많고 가구주가 미성년자일수도 있고 고령층데이터도 있기 떄문이다 

qplot(welfare$income)+xlim(0,10000) #x축 설정

# 소득변수 이상치 확인
table(is.na(welfare$income))





# 성별소득 평균표 생성
sex_income <- welfare %>% 
     group_by(sex) %>% 
     summarise(mean_income=mean(income))

sex_income

# 그래프 생성
library(ggplot2)
ggplot(data=sex_income,aes(x=sex,y=mean_income))+geom_col()
#여자가 세대주인 경우 1인가구가 몰려잇을 수도 있으므로 추후에 고려해야 함 




#### 분석2 : 나이와 소득의 관계 ####

# 나이라는 변수가 없으므로 파생변수 활용


#태어난 연도 변수 검토
class(welfare$birth)

summary(welfare$birth)

qplot(welfare$birth)


# 이상치 및 결측처리

table(is.na(welfare$birth))


# 나이변수 생성

welfare$age <- 2014-welfare$birth+1
summary(welfare$age)

qplot(welfare$age)


#나이별 소득 평균표
age_income <- welfare %>% 
     group_by(age) %>% 
     summarise(mean_income=mean(income))
age_income


#산점도 만들기

ggplot(data=age_income,aes(x=age,y=mean_income))+geom_point()
#-> 개별 나이의 평균소득을 구하면 대표성이 사라짐 
#-> 연령을 구간으로 나누어 그래프를 만드는 것이 합리적



#### 분석3 : 연령대에 따른 소득 ####

# 연령대 변수 생성
# 초년 : 30 미만, 중년 : 30~59, 노년 : 60 이상

welfare <- welfare %>% 
     mutate(ageg=ifelse(age<30,"young",ifelse(age<=59,"middle","old")))

table(welfare$ageg) #빈도확인 -> young은 65개로 매우 적어 대표성이 적음
qplot(welfare$ageg)
#->young을 제외하는 것이 바람직


#연령대별 소득 평균표 생성
#초년의 빈도는 적으므로 제외

welfare_income <- welfare %>% 
     filter(ageg != "young") %>% # young 제외
     group_by(ageg) %>% 
     summarise(mean_income=mean(income))

welfare_income

#그래프 만들기

ggplot(data=welfare_income,aes(x=ageg,y=mean_income))+geom_col()




#### 분석4 : 연령대 및 성별에 따른 소득 ####

# 연령대 및 성별 소득 평균표 생성

sex_income <- welfare %>% 
     filter(ageg != "young") %>% #young 제외
     group_by(ageg,sex) %>%  #연령대 및 성별 
     summarise(mean_income=mean(income))

sex_income


#그래프 생성

ggplot(data=sex_income,aes(x=ageg,y=mean_income,fill=sex))+geom_col()
#fill=변수명 -> 변수대로 색을 부여하여 그래프를 그려줌 

ggplot(data=sex_income,aes(x=ageg,y=mean_income,fill=sex))+geom_col(position="dodge")#position변경(기본값="stack")




