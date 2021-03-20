# 두 집단 간 평균 차이 구하기 ####

# [1] T-test ####

# 집단 샘플 사이즈=3(소표본) ####

setwd("가설검정데이터")

getwd()

rawN3 <- read.csv("htest01.csv",header=T)
rawN3 #소표본

#먼저 해야할 것은 A집단과 B집단을 나누는 것!

groupA <- rawN3[rawN3$group=='A',1:2]
groupB <- rawN3[rawN3$group=='B',1:2]

mean(groupA$height) #170
mean(groupB$height) #181

#그룹 B의 평균키가 더 크다

# 가설 설정 ####

# 귀무가설 : 두 집단간의 평균 키차이는 없다

# 대립가설 : 그룹B의 평균키가 그룹 A의 평균키보다 크다. -> 단측검정


# 데이터 정규성 검정 ####

# 귀무가설 : 데이터셋이 정규분포를 따른다

# 대립가설 : 데이터셋이 정규분포를 따르지 않는다.

# 1. Shapiro-Wilks test
shapiro.test(groupA[,2])

#Shapiro-Wilk normality test
#data:  groupA[, 2]
#W = 1, p-value = 1

#p-value가 0.05보다 크므로 귀무가설 채택 -> 정규분포를 따름 ->t검정 가능



# 2. qqplot
qqnorm(groupA[,2])
qqline(groupA[,2])
#선이 데이터들 사이로 직선 상에 있으면 정규성을 따른다고 판단 

shapiro.test(groupB[,2]) #p-value=0.4173>0.05 -> 귀무가설 채택 
qqnorm(groupB[,2])
qqline(groupB[,2])




# 분산 동질성 검정 ####

# 귀무가설 : 두 집단간 분산이 동일하다(차이가 없다)

# 대립가설 : 두 집단 간 분산이 다르다(차이가 있다)

var.test(groupA[,2],groupB[,2])

# p-value = 0.5385 >0.05 -> 귀무가설 채택->두 집단간 분산 동일


# T-test ####

# 합동분산 사용

# 귀무가설 : 그룹 A, 그룹 B간 평균 키 차이가 없다.

# 대립가설 : 글부 B의 평균 키가 그룹 A의 평균 키보다 크다.

t.test(groupA[,2],groupB[,2],alternative = "less",var.equal = TRUE)
# alternative="less" : 대립가설에서 왼쪽 값이 오른쪽 값보다 작다 
# var.equal=TRUE : 분산이 동일. 만약 다르다면 FALSE

#p-value = 0.1154 >0.05 : 귀무가설 채택 => 두 집단간 평균 동일 


# 결론 ####

# 평균과 분산, 데이터 크기 등을 총체적으로 보았을 때 결론은 평균 키 차이가 없다. 


# 집단 샘플 사이즈=10(소표본) ####

rawN10 <- read.csv("htest02.csv",header=T)
rawN10

groupA <- rawN10[rawN10$group=='A',1:2]
groupB <- rawN10[rawN10$group=='B',1:2]

mean(groupA[,2]) #170
mean(groupB[,2]) #181

# 가설설정 ####

# 귀무가설 : 그룹 A와 그룹 B 간 평균 키 차이가 없다.

# 대립가설 : 그룹 B의 평균 키가 그룹 A의 평균키보다 크다.




# 데이터 정규성 검정 ####

# 귀무가설 : 데이터셋이 정규분포를 따른다.

# 대립가설 : 데이터셋이 정규분포를 따르지 않는다.


shapiro.test(groupA[,2]) # p-value = 0.2826 > 0.05 : 정규성을 따른다
shapiro.test(groupB[,2]) # p-value = 0.9108 > 0.05 : 정규성을 따른다

qqnorm(groupA[,2])
qqline(groupA[,2]) #정규성 만족
qqnorm(groupB[,2])
qqline(groupB[,2])# 정규성 만족 

# 분산 동질성 검정 ####

# 귀무가설 : 두 집단 간 분산이 동일하다

# 대립가설 : 두 집단 간 분산이 다르다.

var.test(groupA[,2],groupB[,2]) #p-value = 0.02602<0.05 => 두 집단 간 분산이 다르다.


# T-test ####

# 귀무가설 : 그룹 A, 그룹 B 간 평균 키 차이가 없다.

# 대립가설 : 그룹 B의 평균 키가 그룹 A의 평균 키보다 크다.

t.test(groupA[,2],groupB[,2],alternative = "less",var.equal = FALSE)
#p-value = 0.01912 < 0.05 => 대립가설 채택 => 그룹B의 평균 키가 그룹 A보다 크다.



# 결론 ####

# 대립가설 채택 : 그룹 B의 평균 키가 그룹 A의 평균 키보다 크다



# 여기서 단순한 평균 비교로는 집단을 비교하여 판단하기는 어렵다는것을 알 수 있다 


# [2] 대응표본 t 검정 ####


# -> 차이의 분포를 고려 !!!!

raw_d <- read.csv("htest02d.csv",header = TRUE)
raw_d # 마케팅에 따른 판매액 차이

groupAd <- raw_d[,1]
groupBd <- raw_d[,2]

mean(groupAd)#[1] 10500
mean(groupBd)#[1] 23800


# 가설 설정 ####

# 귀무가설 : 마케팅을 통한 판매액 변화 없음

# 대립가설 : 마케팅을 통해 판매액이 증가함 



# 데이터 정규성 검정 ####

# 귀무가설 : 데이타셋이 정규분포를 따른다

# 대립가설 : 데이터셋이 정규분포를 따르지 않는다.

d = groupAd - groupBd # difference

shapiro.test(d) #p-value = 0.1621 > 0.05 -> 귀무가설 채택

qqnorm(d)
qqline(d)# 정규분포를 따름 


# 분산 동질성검정 안함 ! ####

# -> 집단이 2개여야 각각의 분산을 비교하는 건데 

#    대응표본은 같은 집단에서 나온 차이 d 하나만 보기 떄문에 분산이 1개 

# T-test ####

# 귀무가설 : 마케팅을 통해 판매액 변화 없음

# 대립가설 : 마케팅을 통해 판매액이 증가함


t.test(groupAd,groupBd,alternative = "less",paired=TRUE)
# paired=TRUE -> 대응표본

# p-value = 0.006745 < 0.05 => 대립가설 채택

# 결론 

# 대립가설 채택 : 마케팅을 통해 판매액이 증가함



# [3] z 검정 ####

#데이터 갯수가 30개 이상일 경우 (대표본)

# 대표본일 경우에는 정규분포를 따르기 떄문에 정규성 검정, 분산 동질성 검정 X


rawN30 <- read.csv("htest03.csv",header=TRUE)
rawN30

groupA3 <- rawN30[rawN30$group=='A',1:2]
groupB3 <- rawN30[rawN30$group=='B',1:2]

mean(groupA3$height) # [1] 179.9
mean(groupB3$height) # [1] 181.65

# 가설 검정 ####

# 귀무가설 : 그룹 A,B간 평균 키 차이가 없다.

# 대립가설 : 그룹 B의 평균키가 그룹 A보다 크다


# Z test ####
z.test <- function(x1, x2){
     n_x1 = length(x1)
     n_x2 = length(x2)
     mean_x1 = mean(x1)
     mean_x2 = mean(x2)
     cat("\n")
     cat("\tTwo Sample z-test\n")
     cat("\n")
     cat("mean of x1:", mean_x1, "\n")
     cat("mean of x2:", mean_x2, "\n")
     var_x1 = var(x1)
     var_x2 = var(x2)
     z = (mean_x1 - mean_x2)/sqrt((var_x1/n_x1)+(var_x2/n_x2))
     abs_z = abs(z)
     cat("z =", abs_z, "\n")
     p_value = 1-pnorm(abs_z)
     cat("p-value =", p_value)
}

z.test(groupA3[,2],groupB3[,2])

#p-value = 0.04866272 <-0.0-5 => 대립가설 채택

# 결론 ####

# 대립가설 채택 : 그룹 B의 평균 키가 그룹 A의 평균키보다 크다



# 만약 z 테스트 사용할때 t검정을 하면??? ####

# 분산 동질설 검정

var.test(groupA3[,2],groupB3[,2]) #p-value = 0.09465 -> 분산 같다

# t-test####

t.test(groupA3[,2],groupB3[,2],var.equal = TRUE,alternative = "less",conf.level = 0.95)
# p-value = 0.05136 > 0.05 -> 귀무가설 채택 -> 평균 키 차이 X

# => 결과에 차이가 존재 


# ANOVA 검정 ####

# 여러 집단 평균 차이 검정

# 집단 내 오차와 집단 간 오차를 비교

# 집단 내 오차 = 집단 내 데이터들의 오차제곱의 합
#              = sum(각 데이터 - 평균)^2

# 집단 간 오차 = 집단 간 데이터 평균 오차제곱의 합

# 집단 간 평균오차  = sum(각 집단 평균 - 전체 평균)

# 집단 간 오차 = sum(n-1(집단별 평균-전체평균))

# if 집단 간 오차 > 집단 내 오차 => 집단 간 평균 차이 존재


# F 통계량 사용


raw_anova <- read.csv("htest04.csv",header=TRUE)

groupA4 <- raw_anova[raw_anova$group=='A',1:2]
groupB4 <- raw_anova[raw_anova$group=='B',1:2]
groupC4 <- raw_anova[raw_anova$group=='C',1:2]

mean(groupA4$height)#74.7778
mean(groupB4$height)#181
mean(groupC4$height)#164.9

# 가설 설정 ####

# 귀무가설 : 세 집단 간 평균 차이가 없다

# 대립가설 : 세 집단 간 평균 차이가 있다.


# 정규성 검정 ####

# 귀무가설 : 데이터셋이 정규분포를 따른다.

# 대립가설 : 데이터셋이 정규분포를 따르지 않는다.

shapiro.test(groupA4$height) # p-value = 0.8978 > 0.05
qqnorm(groupA4$height)
qqline(groupA4$height) # 직선

shapiro.test(groupB4$height) # p-value = 0.9108 > 0.05
qqnorm(groupB4$height)
qqline(groupB4$height) # 직선

shapiro.test(groupC4$height) # p-value = 0.6313 > 0.05
qqnorm(groupC4$height)
qqline(groupC4$height) # 직선

# 모두 정규성 만족


# 등분산성 검정 ####

# H0 : 세집단 간 분산 동일

# H1 : 세 집단 간 분산 동일하지 않음

# levene 테스트

install.packages("lawstat")
library(lawstat)

#그룹별 분산의 차이를 확인 
levene.test(raw_anova$height,raw_anova$group)
# p-value = 0.3298 > 0.05 => 분산이 동일

# bartlett 테스트

#그룹별 분산의 차이를 확인 
bartlett.test(height~group,data=raw_anova)
#p-value = 0.3435 >0.05 => 분산이 동일  


# ANOVA 테스트 ####

# H0 : 세 집단 간 평균 동일

# H1 : 세 집단 간 평균이 다르다.

rawAnova <- aov(height~group,data=raw_anova)
summary(rawAnova)
#            Df Sum Sq Mean Sq F value   Pr(>F)    
#group        2 1316.8   658.4    18.2 1.14e-05 ***
#Residuals   26  940.5    36.2 
# Pr(>F) = 1.14e-05 < 0.05 => 대립가설 채택 => 세 집단 간 평균이 다르다 


# 카이제곱 검정 ####

# H0 : 흡연여부는 폐암유무와 연관성이 없다.

# H1 : 흡연여부와 폐암유무와 연관성이 있다.

raw_chisq <- read.csv("htest05.csv",header=TRUE)
rawTable <- table(raw_chisq)
rawTable

chisq.test(rawTable,correct=FALSE)
#기대도수가 5 보다 크면 correct=FALSE 5보다 작으면 TRUE

# p-value = 0.02686 < 0.05 => 대립가설 채택

# 결론 ####

# 대립가설 채택 : 흡연여부와 폐암유무와 연관성이 있다.