# Support Vector Machine ####

# 선형 서포트 벡터 머신 ####


library(caret)

#caret 패키지는 다른 패키지들의 함수를 가져오는 형식
# kernlab 패키지 함수들 사용한다. 

ctrl <- trainControl(method="repeatedcv", repeats = 5)

svm_linear_Fit <- train(Class~.,
                        data=train,
                        method="svmLinear",
                        trControl=ctrl,
                        preProcess=c("center","scale"),
                        metric="Accuracy")



# 비선형 서포트 벡터 머신 ####

# 피쳐공간 변형 후 서포트벡터 사용 

ctrl <- trainControl(method="repeatedcv", repeats = 5)

svm_poly_Fit <- train(Class~.,
                      data=train,
                      method="poly",
                      trControl=ctrl,
                      preProcess=c("center","scale"),
                      metric="Accuracy")

# Accuracy가 중구난방이다.

# degree : polynominal degree 다항식의 차수. 몇승인지
# 커널의 차수(degree) 설정

# scale : 다항식의 파라미터를 스케일링

# c : cost 모델을 사용시 loss가 얼마나 발생하는지 
# 학습모형의 비용 설정(로지스틱에서의 cost와 동일)
# 경계선의 복잡성을 컨트롤 

# 예제 실습 svm ####

# 1) 데이터 불러오기 ####

rawdata <- read.csv("wine.csv", header=TRUE)
rawdata$Class <- as.factor(rawdata$Class)
str(rawdata)

# 2) Train-Test 데이터 분리 ####

analdata <- rawdata
set.seed(2021)
datatotal <- sort(sample(nrow(analdata),nrow(analdata)*.7))
train <- rawdata[datatotal,]
test <- rawdata[-datatotal,]

str(train)

train_x <- train[,1:13]
train_y <- train[,14] #target 변수

test_x <- test[,1:13]
test_y <- test[,14]

# 3) 선형 서포트 벡터 머신 ####

ctrl <- trainControl(method="repeatedcv",repeats = 5)
svm_linear_fit <- train(Class~.,
                        data=train,
                        method="svmLinear",
                        trControl=ctrl,
                        preProcess=c("center","scale"),
                        metric="Accuracy")
svm_linear_fit # 학습시킨 데이터

#Accuracy 
#0.947028


# 4) 모형 예측(선형) ####

pred_test <- predict(svm_linear_fit,newdata=test)
confusionMatrix(pred_test,test$Class)


#Accuracy : 0.9815   


# 5) 변수 중요도(선형) ####
imp_lin <- varImp(svm_linear_fit,scale=FALSE)
plot(imp_lin)

# 6) 비선형 서포트 벡터 머신 ####

ctrl <- trainControl(method="repeatedcv",repeats = 5)
svm_poly_fit <- train(Class~.,
                      data=train,
                      method="svmPoly",
                      trControl=ctrl,
                      preProcess=c("center","scale"),
                      metric="Accuracy")
svm_poly_fit # 학습시킨 데이터


#degree = 1, scale = 0.1 and C = 0.5.
# 가장 잘 나온 옵션일 때 Accuracy가 높다. 약 99%

plot(svm_poly_fit) #cost가 변함에 따른 그래프 3개 

# 7) 모형 예측(비선형) ####

pred_test <- predict(svm_poly_fit,newdata=test)
confusionMatrix(pred_test,test$Class)

#               Accuracy : 0.9815  


# 8) 변수 중요도(비선형=FALSE)
plot(imp_poly)







