# [ Decision Tree ]####

install.packages("tree")
library(tree)

#1) 기본 트리 그리기 ####
treeRaw <- tree(Class ~ ., data=train)

#시각화
plot(treeRaw)
text(treeRaw)

#2) cross validation ####

cv_tree <- cv.tree(treeRaw, FUN=prune.misclass)
# FUN : 가지치기 함수 선택
# prune.misclass : 오분류 기준

plot(cv_tree)


# 3) 가지치기 (pruning) ####

prune.tree <- prune.misclass(treeRaw, best=4) # best : cross validation을 통해 구한 사이즈 
plot(prune.tree)
text(prune.tree,pretty=0) # pretty = 0 : 분할 피쳐 이름 바꾸지 않음 


# 4) 예측  ####

pred <- predict(prune.tree, test, type='class')
# prune.tree : 가지치기 이후 트리
# test : 테스트 데이터

confusionMatrix(pred,test$Class)


# [Random Forest] ####

library(caret)

ctrl <- trainControl(method="repeatedcv", repeats = 5)

rfFit <- train(Class~.,
               data=train,
               method="rf",
               trControl=ctrl,
               preProcess=c("center","scale"),
               metric="Accuracy")

# mtry : 각 트리에서 랜덤하게 선택되는 분할 피쳐 후보 갯수 



# 예제 풀이 wine data ####

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


# 3) Decision Tree ####

library(tree)

treeRaw <- tree(Class~.,data=train)
plot(treeRaw)
text(treeRaw)

# 4) cross validation ####

cv_tree <- cv.tree(treeRaw,FUN=prune.misclass)
plot(cv_tree) # 의사결정나무 최적사이즈를 확인 가능.
# 오분류율(misclass)이 낮은 곳이 최적사이즈 -> 정호가도 상승 

# 5) 가지치기(pruning) ####

prune_tree <- prune.tree(treeRaw,best=4) #best : 사이즈
plot(prune_tree)
text(prune_tree,pretty = 0)

# 가지치기전 후를 비교해보자. 트리가 복잡하면 가지ㅣ키 효과를 볼 수 있다. 
# 성능이 같다는 가정하에 정확도가 높으면 그거 사용

# 6) 예측 #### 

pred <- predict(prune_tree,test,type='class')
confusionMatrix(pred,test$Class)
#               Accuracy : 0.9259    
# 의사결정나무의 단점은 오버피팅 


# 7) 랜덤 포레스트 ####


library(caret)

ctrl <- trainControl(method="repeatedcv", repeats = 5)

rfFit <- train(Class~.,
               data=train, #학습시킬 데이터 
               method="rf", # random forest 
               trControl=ctrl,
               preProcess=c("center","scale"),
               metric="Accuracy")

rfFit

#  mtry  Accuracy   Kappa    
#   2    0.9905128

plot(rfFit)


# 8) Random Forest 예측 ####

pred_test <- predict(rfFit,newdata=test)
confusionMatrix(pred_test,test$Class)

#Reference
#Prediction  1  2  3
#         1 23  1  0  오분류가 1개밖에 안됨 오 ㅋㅋ 
#         2  0 18  0
#         3  0  2 10


#Accuracy : 0.9444 -> 더 높음

# 9) Random Forest 변수중요도 확인 ####

importance_rf <- varImp(rfFit,scale=FALSE)
plot(importance_rf)

# 결론 ####

# Decision tree 와 random forest 의 정확도를 비교했을 때
# 정확도는 Random forest가 더 높았다