
#데이터프레임 만들기

history <- c(90,80,60,70) #역사점수
history
math <- c(50,60,100,20) #수학점수
math

df_midterm <- data.frame(history,math) #점수 데이터프레임
df_midterm

class <- c(1,1,2,2) # 반 추가

df_midterm <- data.frame(history,math,class) 

mean(df_midterm$history) #역사점수 평균

mean(df_midterm$math) #수학점수 평균



#엑셀 데이터 불러오기

setwd("123Data")

library(readxl)

df_finalexam <- read_excel("finalexam.xlsx",sheet=1, col_names=TRUE) 
#sheet = 몇번쨰 시트인지  col_names=T : 컬럼의 이름까지 가져올건지 여부
df_finalexam


mean(df_finalexam$math)
mean(df_finalexam$history)
mean(df_finalexam$english)


# csv 파일 불러오기

df_exam <- read.csv("csv_exam.csv",header=T)
df_exam



#### chapter 4 ####

#### exam 데이터 살펴보기 ####


exam <- read.csv("csv_exam.csv")
exam
head(exam) # 앞부분의 6개 행 보여줌
tail(exam) # 뒷부분의 6개 행 보여줌 
tail(exam,10) # 뒷부분의 10개 행 보여줌 

View(exam) # 뷰어탕에서 원데이터 확인

dim(exam) # 행과 열이 몇개의 차원으로 이루어졌는지 확인.  행의갯수   열의 갯수

str(exam) #변수들의 속성 확인

summary(exam) #요약통계량



#### mpg 데이터 파악하기 ####

# ggplot의 mpg 데이터를 데이터프레임 형태로 불러오기

mpg <- as.data.frame(ggplot2::mpg) # ggplot2패키지 안에 있는 특정 데이터를 불러옴 
mpg

head(mpg)
tail(mpg)
dim(mpg)
str(mpg)
summary(mpg)




#### 변수이름 바꾸기 ####

# 보통 데이터 분석 시 변수명을 분석하는 사람 입맛에 맞추어 버꿈

# -> dplyr() 라이브러리 사용

library(dplyr)


#데이터프레임 사용
df_raw <- data.frame(var1=c(1,2,1),var2=c(2,3,2))
df_raw


#데이터의 변수명 등을 수정 시 백업본을 만들어두어야 함

df_new <- df_raw #복사본 생성
df_new



#변수명 바꾸기 -> rename

df_new <- rename(df_new, v2=var2) # var2를 v2로 수정. 새이름=옛날이름
df_new



#### 혼자서 해보기 ####

#Q1. ggplot2의 mpg데이터 불러오고 복사본 만들기

mpg<-as.data.frame(ggplot2::mpg)
mpg

mpg1 <- mpg
mpg

#Q2. cty는 city로, hwy는 highway로 

head(mpg)
mpg1 <- rename(mpg1,city=cty,highway=hwy)
head(mpg1)


#### 파생변수 만들기 ####


df <- data.frame(var1=c(4,3,8),var2=c(2,6,1))
df

df$var_sum <- df$var1+df$var2 #파생변수 생성
df

df$var_mean <- (df$var1+df$var2)/2
df


# mpg 통합 연비 변수 만들기

mpg$total <- (mpg$cty+mpg$hwy)/2
mpg
head(mpg)

mean(mpg$total)


#### 조건문을 활용해 파생변수 만들기 ####

#1. 기준값 정하기

summary(mpg$total)

hist(mpg$total) # 25 이후로 연비가줄어듦


# 조건문으로 합격판정 변수 만들기

mpg$test<-ifelse(mpg$total>=20,"pass","fail")


head(mpg,20) # 합격판정 받은 데이터가 보인

# 3. 빈도표로 합격 판정 자동차수 보기 ->table()

table(mpg$test)

#4. a막대그래프로 빈도 보기

library(ggplot2)
qplot(mpg$test)

#### 중첩 조건문 사용 ####

# 연비등급 변수 만들기

# total 기준으로 A,B,C 등급 부여

mpg$grade <- ifelse(mpg$total>=30,"A",ifelse(mpg$total>=20,"B","C"))
head(mpg,20)
table(mpg$grade)
qplot(mpg$grade)


















#### 정리하기 ####

# 1. 데이터 준비, 패키지 준비
mpg <- as.data.frame(ggplot2::mpg) # 데이터 불러오기
library(dplyr) # dplyr 로드
library(ggplot2) #ggplot2 로드



# 2. 데이터 파악
head(mpg) # raw데이터 앞부분
tail(mpg) # raw데이터 뒷부분
View(mpg) # raw데이터 뷰어창에서 확인
dim(mpg) # 차원
str(mpg) # 속성
summary(mpg) # 요약통계량


# 3. 변수명 수정
mpg <- rename(mpg, company=manufacturer)

# 4. 파생변수 생성
mpg$total <- (mpg$cty+mpg$hwy)/2 # 변수 조합
mpg$test <- ifelse(mpg$total>=20,"Pass","Fail") # 조건문 활용

# 5. 빈도 확인
table(mpg$test) # 빈도표 출력
qplot(mpg$test) # 막대그래프 생성   



#### midwest 데이터 문제풀이 ####

#1. 데이터 불러오기
midwest <- as.data.frame(ggplot2::midwest)
head(midwest)

#2. 변수명 수정
midwest1 <- rename(midwest,total=poptotal, asian=popasian)
head(midwest1)

#3. 전체인구대비 아시아인구 백분율 만들고 빈도표, 막대그래프 만들기

midwest1$per_asian<- (midwest1$asian/midwest1$total)*100
hist(midwest1$per_asian)

#4. 아시아 백분율 평균 초과 large 아니면 small
mean(midwest1$per_asian)

midwest1$per_asian2 <- ifelse(midwest1$per_asian>0.4872462,"large","small")
head(midwest1)

#5. 빈도표, 막대그래프 만들기

table(midwest1$per_asian2)
qplot(midwest1$per_asian2)





