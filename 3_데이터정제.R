
####1. 결측치 정제 ####

df <- data.frame(sex=c("M","F",NA,"M","F"),
                 score=c(5,4,3,4,NA))
df


#### 결측치 확인 ####

is.na(df)

##### 결측치 빈도 출력 ####

table(is.na(df))



#### 변수별로 결측치 확인 ####

table(is.na(df$sex))
table(is.na(df$score))


#### 결측치 포함한 상태로 분석 ####

mean(df$sex) # -> NA로 나옴


#### 결측치 있는 행 제거하기 ####

library(dplyr)

df %>% filter(is.na(df$score)) # score가 NA인 데이터만 출력

df %>% filter(!is.na(df$score)) # score 결측치 제거

#### 결측치를 제외한 데이터로 분석####

df_nomiss <- df %>% filter(!is.na(df$score)) # score결측치 제거
mean(df_nomiss$score) # 평균 산출


#### 여러변수 동시에 결측치 제외

df_nomiss <- df %>% filter(!is.na(score)&!is.na(sex))
df_nomiss


#### 결측치가 하나라도 있으면 제거!!! ####
# -> na.omit() 함수 사용

df_nomiss2 <- na.omit(df) #모든 변수에 결측치 없는 데이터 추출
df_nomiss2
# 실제로는 잘 안쓴다
# 그 이유는 분석에 사용하지 않는 변수의 결측치 까지 제거하기 떄문
# 사용안하는 변수의 결측치 행이 제거되면 안되기 떄문 
# 그래서 머신러닝 모형 만들 때 주로 사용




#### 함수의 결측치 제외 기능 사용하기 ####
# -> na.rm=T

mean(df$score,na.rm=T) # 결측치 제외하고 평균 산출


#### summarise()에서 na.rm=T 사용하기 ####

setwd("123Data")

exam <- read.csv("csv_exam.csv")

exam[c(3,8,15),"math"] <- NA  # 3, 8, 15행의 math 에 NA 할당

exam %>% summarise(mean_math=mean(math)) # 평균대신 NA로 산출

exam %>% summarise(mean_math=mean(math,na.rm=T)) # 결측치 제외하고 평균 산출




#### 평균값으로 결측치 대체하기 ####

mean(exam$math,na.rm=T) # 결측치 제외하고 math 평균 산출

exam$math <- ifelse(is.na(exam$math),55,exam$math) # matj 가 NA면 55로 대체
table(is.na(exam$math)) #결측치 빈도표

exam
mean(exam$math)




#### mpg데이터 결측치 해결 문제 ####

mpg <- as.data.frame(ggplot2::mpg)
mpg[c(65,124,131,153,212),"hwy"] <- NA # NA할당


#q1. drv변수와 hwy변수에 결측치가 몇개 있나?
table(is.na(mpg$drv)) #0개
table(is.na(mpg$hwy)) #5개

#q2. filter()를 이용해 hwy변수 결측치 제외 후 어떤 drv의 hwy평균이 높은지 알아보기. 하나의 dplyr 구문으로 
mpg %>% 
     group_by(drv) %>% 
     summarise(mean_hwy=mean(hwy,na.rm=T)) %>% 
     arrange(desc(mean_hwy))

mpg %>% 
     filter(!is.na(hwy)) %>% 
     group_by(drv) %>% 
     summarise(mean_hwy=mean(hwy)) %>% 
     arrange(desc(mean_hwy))




#### 2. 이상치 정제 ####


#### 존재할 수 없는 값 정제 ####

outlier <- data.frame(sex=c(1,2,1,3,2,1), #이상치 포함 데이터 생겅
                      score=c(5,4,3,4,2,6))
outlier

table(outlier$sex) #3이 이상치
table(outlier$score)#6이 이상치

### 논리적인 이상치가 있는 경우 NA로 바꾸어 놓는다. 분석시에는 filter러 제외.

#sex가 3이면 NA할당
outlier$sex <- ifelse(outlier$sex==3,NA,outlier$sex)
outlier

#score가 1~5 아니면 NA 할당
outlier$score <- ifelse(outlier$score>5,NA,outlier$score)
outlier

#결측치 제외후 분석
outlier %>% 
     filter(!is.na(sex)&!is.na(score)) %>% 
     group_by(sex) %>% 
     summarise(mean_score=mean(score))



#### 극단적인 값 정제 ####

#보통 표준편차를 이용하여 +-3sd 밖에 있으면 이상치로 판단. 
#상자그림으로도 판단 


#### 상자그림으로 극단치 기준 정해서 제거 ####

mpg <- as.data.frame(ggplot2::mpg)
boxplot(mpg$hwy) # 경계 밖으로 점으로 나온 부분은 이상치

# 상자그림 통계치 출력
boxplot(mpg$hwy)$stats
#[,1]
#[1,]   12 맨 아래 경계
#[2,]   18 1분위수
#[3,]   24 2분위수
#[4,]   27 3분위수
#[5,]   37 맨위쪽 경계



# 12~37 벗어나면 NA할당

mpg$hwy <- ifelse(mpg$hwy<12|mpg$hwy>37,NA,mpg$hwy)
table(is.na(mpg$hwy))

# 결측치 제외하고 분석
mpg %>% 
     group_by(drv) %>% 
     summarise(mean_hwy=mean(hwy,na.rm=T))



####mpg 데이터로 이상치 제거 문제 ####

mpg <- as.data.frame(ggplot2::mpg)
mpg[c(10,14,58,93),"drv"]<-"k" # drv이상치 할당
mpg[c(29,43,129,203),"cty"] <- c(3,4,39,42) #cty이상치 할당


#q1. drv에 이상치 있는지 확인 후 결측처리 하고 사라졌는지 확인. 결측치 처리시 %in% 사용
table(mpg$drv)
mpg$drv <- ifelse(mpg$drv %in% c("4","f","r"),mpg$drv,NA)
table(mpg$drv)

#q2. 상자그림 이용하여 cty에 이상치가 있는지 확인. 통계치를 이용해 정상범위 벗어난 값을 
#    결측처리 후 다시 상자그림을 그려 이상치가 사라졌는지 확인
boxplot(mpg$cty)
boxplot(mpg$cty)$stats
mpg$cty <- ifelse(mpg$cty<9|mpg$cty>26,NA,mpg$cty)
boxplot(mpg$cty)

#q3. 이상치를 제외했으면 drv별로 cty 평균이 어떻게 다른지 하나의 dplyr구문으로 만들어보기
mpg %>% 
     filter(!is.na(drv)&!is.na(cty)) %>%
     group_by(drv) %>% 
     summarise(mean_cty=mean(cty,na.rm=T))

