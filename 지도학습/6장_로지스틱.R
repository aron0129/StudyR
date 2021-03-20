# 7장 로지스틱 회귀분석 ####

# caret 패키지 사용 ####

install.packages("caret",dependencies = TRUE)
library(caret)

# LogitBoost ####

ctrl <- trainControl(method="repeatedcv",repeats=5)

logitFit <- train(target~.,
                  data=train,
                  method="LogitBoost", #원하는 로지스틱 모형 설정 
                  trContgrol=ctrl,
                  metric="Accuracy")


#1. Boosted Logistic Regression : method="LogitBoost" ####

#약한 분류기(정확도 떨어짐) 여러개 더하여(정확도 올라감) 부스팅
# -> 가장 간단한 모형으로 시작해 점차 모형 개선
#->피쳐 하나로 로지스틱 모델 제작 가능 


#2. Logistic Model Trees : method='LMT'####
# 로지스틱 회귀와 의사결정나무 합친 모형 

#iter = 반복횟수 


#3. Penalized Model Trees : method='plr'####
# 베타를 제한 ,영역은 타원

#람다값 확인 가능 -> 크기에 따라 베타영역 크기가 달라짐 


#4. Regularized Model Trees : method='regLogistic'####
#베타 영역이 마름모 혹은 타원형
# Accuracy, Kappa

# cost : loss의 합 for 전체데이터셋 

# loss : 손실함수. for 각각의데이터 포인트  
# - primal = 파라미터(베타) 기준 최적화
# - dual = 제약변수(람다) 기준 최적화 

# epsilon : 



# 실습 ####


#1) 심장병 데이터 불러오기 ####

library(caret)

rawdata <- read.csv("heart.csv",header=TRUE)
str(rawdata) # target변수의 데이터 타입을 보면 int인 것을 확인 가능 


#2) 타겟 변수 범주화 ####
#성별, target 변수들 같은 경우는 범주화할 필요가 있다.
rawdata$target<-as.factor(rawdata$target)
unique(rawdata$target) #유일한 값들을 보기. 레벨 확인 

#3) 연속형 독립변수 표준화 ####

rawdata$age <- scale(rawdata$age)
rawdata$trestbps <- scale(rawdata$trestbps)
rawdata$chol <- scale(rawdata$chol)
rawdata$thalach <- scale(rawdata$thalach)
rawdata$oldpeak <- scale(rawdata$oldpeak)
rawdata$slope <- scale(rawdata$slope)


#4) 범주형 독립변수 범주화 ####

newdata <- rawdata

factorVar <- c("sex","cp","fbs","restecg","exang","ca","thal")
newdata[,factorVar] = lapply(newdata[,factorVar],factor) #newdata[,factorVar]가 범주화 되어 리스트로 출력 

#5) train-test 셋 나누기 ####

set.seed(2020)
datatotal <-sort(sample(nrow(newdata),nrow(newdata)*0.7)) #7:3으로 나눔
train <- newdata[datatotal,]
test <- newdata[-datatotal,]

train_x <- train[,1:12]
train_y <- train[,13]

test_x <- train[,1:12]
test_y <- train[,13]


#6) 학습 - LogitBoost 사용####

ctrl <- trainControl(method="repeatedcv",repeats=5)
logitFit <- train(target~.,
                  data=train,
                  method="LogitBoost",
                  trControl=ctrl,
                  metric="Accuracy")
logitFit
# nIter  Accuracy   Kappa    
# 11     0.8151515  0.6227599
# 21     0.8045887  0.6021400
# 31     0.8045455  0.5981815

# The final value used for the model was nIter = 11.

plot(logitFit)


#7) 예측 #### train데이터에서는 약 82%의 정확도가 나왓으니 test데이터에 적용 

pred_test <- predict(logitFit,newdata=test)
confusionMatrix(pred_test,test$target)


#           Reference
#Prediction  0  1
#         0 34  6
#         1 14 37

# Accuracy : 0.7802  


#8) 변수중요도 : 중요한 변수 확인 ####

importance_logit <- varImp(logitFit,scale=FALSE)
plot(importance_logit) #cp 가 가장 중요한 변수로 확인됨