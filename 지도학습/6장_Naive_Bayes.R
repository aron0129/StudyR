# Naive Bayes classification ####

#1. naivebayes 패키지 : method='naive_bayes'

#2. bnclassify 패키지 : method='nbDiscrete'
#                       method='manb' (Model Averaged Naive Bayes) : 모든 가능한 피쳐조합의 조건부확률의 평균
#                       method='awnb' (Attribute Weighting) : 조건부확률에 가중치 부여

#3

library(caret)

ctrl <- trainControl(method="repeatedcv",repeats=5)

nbFit<- train(
    Class~., # 타겟변수 ~ 피쳐
    data=train, #데이터 이름
    method="naive_bayes", # 원하는 알고리즘 설정(머신러닝 방법) Naive Bayes
    trControl=ctrl, # 학습방법 지정
    preProcess=c("center","scale"), # 표준화
    metric="Accuracy" # 모형 평가 방식, Accuracy(정확도) or kappa 사용
)

# usekernel (커널밀도추정 : KDE) : 커널 사용 유무 ####
# Kernel Density Estimation

# 데이터의 히스토그램을 보구 실제 분포를 추정! smoothing 시킨다 


# adjust  : Bandwidth 조절 ####

# 히스토그램의 간격을 조절

# Bandwith 값이 달라지면 추정커널밀도함수 형태가 달라진다



# laplace : 라플라스 스무딩 파라미터 ####

# 세타 = xi + a / N + ad
#  xi=i가 나온 횟수, a=스무딩 파라미터(0이면 no스무딩), N=전체데이터수, d=i:1~d

# ad를 더하는 이유는 데이터 수가 적을 경우, 0 또는 1과 같이 
# 극단적인 값으로 추정하는 것을 방지하기 위함이다.




# 와인 데이터 예제 실습####

library(caret)

# 1) 데이터 불러오기 ####
rawdata <- read.csv("wine.csv", header=TRUE)
rawdata$Class <- as.factor(rawdata$Class)
str(rawdata)

# 2) Train-test 셋 나누기 ####

analdata <- rawdata
set.seed(2021)
datatotal <- sort(sample(nrow(analdata),nrow(analdata)*.7))
train <- rawdata[datatotal,]
test <- rawdata[-datatotal,]

str(train)

train_x <- train[,1:13]
train_y <- train[,14] #target 변수수

test_x <- test[,1:13]
test_y <- test[,14]


# 3) 모형 학습 ####

ctrl <- trainControl(method="repeatedcv",repeats = 5)

nbFit <- train(Class~.,
               data=train,
               method="naive_bayes",
               trControl=ctrl,
               preProcess=c("center","scale"),
               metric="Accuracy")

nbFit
#  usekernel  Accuracy     
#  FALSE      0.9705944   -> 정확도 97퍼

# The final values used for the model were laplace = 0, => 알파=0
# usekernel= TRUE => TRUE 일떄만 유의미. 커널 사용 
#and adjust = 1.


plot(nbFit)


# 4) 예측 ####

pred_test <- predict(nbFit,newdata = test)
confusionMatrix(pred_test,test$Class)

#  Accuracy : 0.9444  정확도 94퍼

# 5) 변수 중요도 ####

importance_nb <- varImp(nbFit,scale=FALSE)
plot(importance_nb) # ROC커브의 면적이 높을수록 중요도가 높음 
#XD280변수가 중요도가 높음