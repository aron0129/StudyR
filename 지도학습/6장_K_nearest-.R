# 6장 K-Nearest Neighbor ####

# caret 패키지 사용 

install.packages("caret", dependencies = TRUE) 
#dependencies = TRUE -> 여러 패키지 중 하나의 패키지만 사용
library(caret)


# trainControl() : Train 과정의 Parameter 설정 ####

trainControl(
    method = "repeatedcv",   # cross-validation 반복
    number = 10, # train 데이터 fold 개수
    repeats = 5  # cross-validation 반복 회수
)

# expand.grid() : 모든 벡터 혹은 인자(factor) 보합인 데이터프레임 생성 ####

expand.grid(k=1:10) # k를 1부터 10까지 고려

# train() : 머신러닝 알고림즘 이용해 데이터학습을 통한 모델 생성 ####

train(
    Class~., # 타겟변수 ~ 피쳐
    data=train, #데이터 이름
    method="knn", # 원하는 알고리즘 설정(머신러닝 방법)
    trControl=trainControl(), # 학습방법 지정
    preProcess=c("center","scale"), # 표준화
    tuneGrid=expand.grid(k=1:10), # 튜닝 파라미터값 목록
    metric="Accuracy" # 모형 평가 방식, Accuracy(정확도) or kappa 사용
)


# 실습 ####




rawdata<-read.csv("wine.csv",header=TRUE)
rawdata$Class <- as.factor(rawdata$Class) #목적변수 범주화
str(rawdata)


#1) Train-Test 데이터 분할 ####

analdata <- rawdata #백업

set.seed(2020) # 시드 설정(랜덤으로 뽑으나 같은 순서로 뽑음.시드번호는 아무번호)
datatotal <- sort(sample(nrow(analdata),nrow(analdata)*0.7)) # 랜덤으로 7:3비율로 나누고 오름차 정렬 
#sample(a,b) : 1부터 a까지 숫자 중에 b개 추출 
train <- rawdata[datatotal,]
test <- rawdata[-datatotal,]#train에서 뽑히지 않은 데이터

train_x <- train[,1:13]
train_y <- train[,14]

test_x <- test[,1:13]
test_y <- test[,14]


#2) 모형 학습 ####

ctrl <- trainControl(method="repeatedcv",number = 10,repeats=5)
customGrid <- expand.grid(k=1:10)

knnFit <- train(Class~.,
                data=train, # 데이터
                method="knn", # k nearest neighbor
                trControl=ctrl, #학습방법
                preProcess=c("center","scale"), # 데이터 전처리는 표준화 
                tuneGrid=customGrid, #k를 1부터 10까지 봄
                metric="Accuracy") #모형평가는 정확도를 봄

knnFit

#k   Accuracy   Kappa    
#1  0.9726041  0.9585595
#2  0.9658725  0.9482692
#3  0.9707859  0.9558296
#4  0.9681901  0.9520425
#5  0.9776956  0.9662325
#6  0.9603280  0.9401272
#7  0.9567100  0.9348356
#8  0.9517965  0.9274056
#9  0.9644023  0.9465077
#10  0.9673876  0.9507825

#The final value used for the model was k = 5.


plot(knnFit) #각 k별로 정확도를 시각화하여 볼 수 있다.k=5일때 가장 높다.


#3) 예측 ####

pred_test <- predict(knnFit, newdata=test)

confusionMatrix(pred_test,test$Class) # 분할표


#            Reference
#Prediction  1  2  3
#         1 14  2  0
#         2  0 24  0
#         3  0  1 13

# Accuracy : 0.9444 

# Kappa : 0.913   


#4) 변수 중요도 : 피쳐 중 중요한 변수 확인####

importance_knn <- varImp(knnFit,scale=FALSE)
importance_knn
plot(importance_knn) #Flavanoid 가 가장 중요한 변수