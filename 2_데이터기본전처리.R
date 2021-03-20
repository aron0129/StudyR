#### 데이터 전처리 ####

# -> dplyer 패키지

# filter() : 행 추출
# select() : 열(변수) 추출
# arrange() : 정렬
# mutate() : 변수 추가
# summarise() : 통계치 산출
# group_by() : 집단별로 나누기
# left_join() : 데이터 합치기(열)
# bind_join() : 데이터 합치기(행)


#### 1. 조건에 맞는 데이터만 추출하기, 행 추출 ####

library(dplyr)
exam <- read.csv("csv_exam.csv")
exam

#exam 에서 class 가 1 인 경우만 추출하여 추출
exam %>% filter(class==1) # ctrl+shift+M 단축기

#2반인 경우
exam %>% filter(class==2)

#1반이 아닌 경우
exam %>% filter(class !=1)


#수학점수가 50점을 초과한 경우

exam %>% filter(math>50)

#영어점수 80 이상

exam %>% filter(english>=80)


#1반이면서 수학점수가 50점 이상인경우

exam %>% filter(class==1 & math>=50)

#수학점수가 90점 이상이거나 영어점수가 90점 이상인 경우
exam %>% filter(math>=90 | english>=90)

# 1,3,5반에 해당
exam %>% filter(class==1|class==3|class==5)


# '또는' 이 반복되는 경우 Match operator 사용!   %in%

exam %>% filter(class %in% c(1,3,5))


#1반인 행 추출하고 class1에 할당
class1 <- exam %>% filter(class==1)
class2 <-  exam %>% filter(class==2)

mean(class1$math)
mean(class2$math)


#### mpg 문제 2 ####

# Q1. displ이 4이하인 거랑 5이상인 차 중 hwy이 평균적으로 높은지 확인
library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg)
head(mpg$displ)

mpg1 <- mpg %>% filter(displ<=4)
mpg2 <- mpg %>% filter(displ>=5)
mean(mpg1$hwy) #25.96319
mean(mpg2$hwy) #18.07895

# Q2. audi, toyota중 manufacturer 의cty가 평균적으로 더 높은지
audi <- mpg %>% filter(manufacturer=="audi")
toyota <- mpg %>% filter(manufacturer=="toyota")
mean(audi$cty)#17.61111
mean(toyota$cty)#18.52941

# Q3. chervoret, ford, honda 고속도로 연비 평균 

threecar <- mpg %>% filter(manufacturer %in% c("chevrolet","ford","honda"))
mean(threecar$hwy) #22.50943


#### 데이터 추출(선택)하기, 열 추출 ####

# select 함수 사용

exam %>% select(math) # math 추출

exam %>% select(english) # english 추출

exam %>% select(math,english) # math, englis 추출



#### 2. 제외하고 싶은 변수가 있을 경우 ####

# - 를 변수 앞에 놓는다.

exam %>% select(-math) #math 제외


#여러개일 경우

exam %>% select(-math,-english)


#### filter랑 select 같이 사용 ####

#가독성있께 봄
#1반 영어점수
exam %>% 
     filter(class==1) %>% #class가 1인 행 추출
     select(english) #영어점수




#일부만 보기
exam %>% 
     select(id,math) %>% #id,math 추출
     head   # 앞부분 6행까지 추출







#### mpg데이터 문제 3 ####

#Q1. mpg데이터에서 class, cty변수를 추출해 새로운 데이터를 만든 후 출력

mpg <- as.data.frame(ggplot2::mpg)
head(mpg)
mpg1 <- mpg %>% 
     select(class,cty)
head(mpg1)

#Q2. class가 suv인 자동차와 compact자동차 중 어떤 자동차의 cty가 더 높은지 확인 

mpg2_suv<-mpg1 %>% filter(class=="suv")
mpg2_com<-mpg1 %>% filter(class=="compact")
mean(mpg2_suv$cty) #13.5 
mean(mpg2_com$cty) #20.12766



####3. 데이터 정렬 ####



exam %>% arrange(math) # math 오름차순 정렬

exam %>% arrange(desc(math)) #math 내림차순 정렬



exam %>% arrange(class,math) # class 및 math 오름차순 정렬


#### mpg 문제 4 ####

# audi에서 생산한 차 중 hwy가 1위에서 5위 자동차 출력

mpg <- as.data.frame(ggplot2::mpg)
head(mpg)
mpg_hwy <- mpg %>% 
     filter(manufacturer=="audi") %>% 
     arrange(desc(hwy)) %>% 
     head(5)

mpg_hwy






####4. 파생변수 추가하기 ####
# -> mutate() 사용


exam %>% 
     mutate(total=math+english+science) %>% #총합변수 추가
     head



exam %>% 
     mutate(total=math+english+science,
            mean=(math+english+science)/3) %>%  #총합변수 추가
     head



#mutate() 에서 ifelse 적용하기 %>% 


exam %>% 
     mutate(test=ifelse(science>=60,"pass","fail")) %>% 
     head


#추가한 변수를 dplyr코드에 활용
exam %>% 
     mutate(total=math+english+science) %>% #총합변수 추가
     arrange(total) %>%  #총합변수 기준 정렬 
     head
#여기서는 실제로 assign 하지 않았는데도 된것마냥 사용가능해서 편함
#바로 분석결과 확인가능




#### mpg문제 5 ####

#Q1. mpg데이터 복사본 만들고 cty,hwy더한 합산연비변수 추가
mpg <- as.data.frame(ggplot2::mpg)
head(mpg)

mpg1 <- mpg %>% 
     mutate(total=cty+hwy)  

mpg1

#Q2. 앞에서 구한 합산 연비변수를 2로 나누어 평균연비변수 추가
mpg2 <- mpg1 %>% 
     mutate(average=total/2) 

mpg2

#Q3. 평균연비변수가 가장 높은 자동차 3종의 데이터 출력
mpg3 <- mpg2 %>% 
     arrange(desc(average)) %>% 
     head(3)
mpg3

#Q4. Q1~Q3을 dplyr구문으로 하나의 코드로 출력. 복사본 대신 원본사용
mpg %>% 
     mutate(total=cty+hwy) %>% 
     mutate(average=total/2) %>% 
     arrange(desc(average)) %>% 
     head(3)





######## 5. 집단별로 요약하기 #####
#-> summarise(), group_by() 사용


exam %>% summarise(mean_math=mean(math)) # math 평균 산출
#-> 보통 summarise는 group_by랑 같이 사용  


#class 별로 수학점수의 평균 보여줌
exam %>% 
     group_by(class) %>% #class 별 분리
     summarise(mean_math=mean(math)) # math 평균 산출


#여러 요약통계량
exam %>% 
     group_by(class) %>% # class 별 분리
     summarise(mean_math=mean(math), # math 평균
               sum_math=sum(math), # math 합계
               median_math=median(math), #math 중위수
               n=n()) #학생수 -> 개별 행의 수를 셈. 괄호안에 빈칸



# 각 집단별로 다시 집단 나누기

mpg %>% 
     group_by(manufacturer,drv) %>%  #회사별, 구방방식별 분리
     summarise(mean_cty=mean(cty)) %>% #cty 평균 산출
     head(10)



#### mpg 문제 6 ####

#Q1. class별 cty 평균
mpg <- as.data.frame(ggplot2::mpg)
head(mpg)
mpg %>% 
     group_by(class) %>% 
     summarise(mean_cty=mean(cty))


#Q2. 앞 문제에서 cty평균이 높은 순으로 정렬해서 출력
mpg %>% 
     group_by(class) %>% 
     summarise(mean_cty=mean(cty)) %>% 
     arrange(desc(mean_cty))

#Q3. hwy평균이 가장 높은 회사 세곳 출력
mpg %>% 
     group_by(manufacturer) %>% 
     summarise(mean_hwy=mean(hwy)) %>% 
     arrange(desc(mean_hwy)) %>% 
     head(3)

#Q4. 각 회사별 compact 차종수를 내림차순으로 정렬하여 출력

mpg %>% 
     group_by(manufacturer) %>%
     filter(class=="compact") %>% 
     summarise(compact=n()) %>% 
     arrange(desc(compact))




#### 6. 데이터 합치기 ####

# 중간고사 데이터
test1 <- data.frame(id=c(1,2,3,4,5),midterm=c(60,80,70,90,85))
test1
#기말고사 데이터
test2 <- data.frame(id=c(1,2,3,4,5),final=c(70,83,65,95,80))
test2

#### 가로로 결합하는 작업 -> left_join() 사용

total <- left_join(test1,test2,by="id")#id기준으로 병합하여 total에 할당
total #id가 가장 왼쪽에 있고 나머지들은 순서대로 붙여짐 

name <- data.frame(class=c(1,2,3,4,5),
                   teacher=c("kim","lee","park","choi","jung"))
name

exam_new <- left_join(exam,name,by="class") #class 기준으로 병합 
exam_new



#### 세로로 결합하는 작업 -> bind_rows() 사용

#학생 1~5번 시험데이터 생성
group1 <- data.frame(id=c(1,2,3,4,5),test=c(60,80,70,90,85))
group1

#학생 6~10번 시험데이터 생성
group2 <- data.frame(id=c(6,7,8,9,10),test=c(70,83,65,95,80))
group2


group_all <- bind_rows(group1,group2) #데이터를 합쳐서 group_all에 할당
group_all



#### mpg데이터 문제 7 ####

#연료(fl)별 가격을 입수

fuel <- data.frame(fl=c("c","d","e","p","r"),
                   price_fl=c(2.35,2.38,2.11,2.76,2.22),
                   stringsAsFactors = FALSE) # Factor로 변환하지 않음 
fuel


# Q1. mpg데이터에 price_fl 변수 추가
mpg <- as.data.frame(ggplot2::mpg)

mpg1 <- left_join(mpg,fuel,by="fl")
mpg1

# Q2. model, fl, price_fl 변수 추출해 앞부분 5행 출력

mpg1 %>% 
     select(model,fl,price_fl) %>% 
     head(5)



#### 정리 ####

# 1. 조건에 맞는 데이터만 추출
exam %>% filter(english>=80)

# 여러 조건 동시 충족
exam %>% filter(class==1&math>=50)

# 여러 조건 중 하나 이상 충족
exam %>% filter(math>= 90 | english>=90)
exam %>% filter(class %in% c(1,3,5))

# 2. 필요한 변수만 추출하기
exam %>% select(math)
exam %>% select(class,math,english)

# 3. 함수 조합하기, 일부만 출력하기
exam %>% 
     select(id,math) %>% 
     head(10)


# 4. 순서대로 정렬하기
exam %>% arrange(math) #오름차순 정렬
exam %>% arrange(desc(math)) # 내림차순 정렬
exam %>% arrange(class, math) # 여러 변수 기준 오름차순 정렬


# 5. 파생변수 추가하기
exam %>% mutate(total=math+english+science)

# 여러 파생변수 한 번에 추가하기
exam %>% 
     mutate(total=math+english+science,
            mean=(math+english+science)/3)

# mutate()에 ifelse 적용하기
exam %>% mutate(test=ifelse(science>=60,"pass","fail"))

# 추가한 변수를 dplyr 코드에 바로 활용하기
exam %>% 
     mutate(total=math+english+science) %>% 
     arrange(total)

# 6. 집단별로 요약하기
exam %>% 
     group_by(class) %>% 
     summarise(mean_math=mean(math))

# 각 집단별로 다시 집단 나누기
mpg %>% 
     group_by(manufacturer,drv) %>% 
     summarise(mean_cty=mean(cty))

# 7. 데이터 합치기

# 가로로 합치기
total <- left_join(test1,test2,by="id")

# 세로로 합치기
group_all <- bind_rows(groupa,groupb)




#### midwest 문제 도전 ####

mid <- as.data.frame(ggplot2::midwest)
mid
str(mid)
head(mid)

#q1. 전체인구 대비 미성년 인구 백분율 변수 추가
library(dplyr)
mid1 <- mid %>% 
     mutate(per_teen=(poptotal-popadults)/poptotal*100)
head(mid1)

#q2. 미성년 인구 백분율이 가장 높은 상위 5개 county의 미성년 인구 백분율 출략
mid1 %>% 
     select(county,per_teen) %>% 
     arrange(desc(per_teen)) %>% 
     head(5)


#q3. large 는 40%이상, middle은 30~40%미만, small은 30%미만 으로 등급을 나누고 각 등급에 몇개의 지역이 있는지 추력
mid2 <- mid1 %>% 
     mutate(grade=ifelse(per_teen>=40,"large",
                         ifelse(per_teen>=30,"middle","small"))) %>% 
     group_by(grade) %>% 
     summarise(n=n())

mid2

#Q4. 전체인구대비 아시아인 인구 백분율 변수를 추가하고 하위10개 지역의 state, county, 아시아인 인구 백분율 출력
mid3 <- mid1 %>% 
     mutate(per_asian=(popasian/poptotal)*100) %>% 
     select(state,county,per_asian) %>% 
     head(10)
mid3
