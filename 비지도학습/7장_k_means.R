# K-means Clustering 기본 ####

setwd("비지도학습")

getwd()

df <- read.csv("Wholesale customers data.csv",stringsAsFactors = FALSE,header=TRUE)

head(df)

str(df)

library(dplyr)

df$Channel <- df$Channel %>% as.factor() #범주형 데이터 팩터로 변경
df$Region <- df$Region %>% as.factor() #범주형 데이터 팩터로 변경 
str(df)


colSums(is.na(df)) # 결측치 확인

summary(df) # 기술통계

options(scipen = 100) # 지수표기법. 아라비아 숫자로 보임 
boxplot(df[,3:ncol(df)]) # 범주형 변수는 제외 

# 이상치 제거하는 함수 ####

temp <- NULL

for (i in 3:ncol(df)) {
    temp <- rbind(temp,df[order(df[,i],decreasing = TRUE),] %>% slice(1:5))
}#오름차순으로 1~4행까지 추출하여 temp와 rbind 함. 중복추출

temp %>% arrange(Fresh) %>% head() # 중복이 있다. 

temp <- distinct(temp) #중복 제거 

dr.rm.outlier <- anti_join(df,temp) #df에서 temp제거 
#같은 것 들을 찾아서 제거해주는 함수



# 이상치 제거 후 박스플롯 확인 ####

par(mfrow=c(1,2))

boxplot(df[,3:ncol(df)])
boxplot(dr.rm.outlier[,3:ncol(df)]) #어느정도 제거된 이상치 확인 가능 

#결과에 영향을 줄만한 outlier만 제거!! ####




# k-means 군집 분석 예제 ####

# 1. k 군집개수 설정(elbow method) ####

library(factoextra)

set.seed(2021) # 난수를 고정해주는 기능 

fviz_nbclust(dr.rm.outlier[,3:ncol(dr.rm.outlier)],kmeans,method="wss",k.max=15)+
    theme_minimal()+
    ggtitle("Elbow Method")
# 5지점에서 기울기가 완만해짐 -> k=5


# 2. k 군집개수 설정(Silhouette method) ####

fviz_nbclust(dr.rm.outlier[,3:ncol(dr.rm.outlier)],kmeans,method="silhouette",k.max=15)+
    theme_minimal()+
    ggtitle("Silhouette Method")
#k=3일때 실루엣 스코어가 가장 높다

# 3. K means 모델 생성####

df.kmeans <- kmeans(dr.rm.outlier[,3:ncol(dr.rm.outlier)],centers = 5,iter.max =1000)
#iter.max = 반복최대횟수 
df.kmeans

#K-means clustering with 5 clusters of sizes 179, 42, 72, 110, 18


# 4. 군집별 평균치 시각화 ####

barplot(t(df.kmeans$centers), beside=TRUE, col=1:6)
legend("topleft",colnames(df[,3:8]),fill=1:6, cex=0.5)


# 5. raw data에 cluster 할당

dr.rm.outlier$cluster <- df.kmeans$cluster
head(dr.rm.outlier)



# 이미지 데이터 k means 적용 ####

# 1.데이터 불러오기 ####

library(jpeg)
img <- readJPEG('cat.jpeg')
class(img) # array

dim(img)
#[1]  855 1280    3 -> RGB


# 2. 3차원 데이터를 2차원으로 펼침

imgdim <- as.vector(dim(img)) #855   1280   3  각각 넣어놓음

imgRGB <- data.frame( #데이터프레임 만들기
    x=rep(1:imgdim[2],each=imgdim[1]), #1~1280 반복하는데 855번
    y=rep(imgdim[1]:1,imgdim[2]), #855~1까지 1280번 반복 
    R = as.vector(img[,,1]),
    G = as.vector(img[,,2]),
    B = as.vector(img[,,3])
)

head(imgRGB)

tail(imgRGB)

# 3. 색상 개수 축소 ####

kCluster <- c(3,5,10,15,30,50) #축소할 색상 클러스터 개수

set.seed(2021)

for (i in kCluster) {
    img.kmeans <- kmeans(imgRGB[,c("R","G","B")],center=i)#3,5,순서대로 kmeans를 돌린다 
    img.result <- img.kmeans$centers[img.kmeans$cluster,]#각각 데이터의 클러스터에 해당되는 평균값들을 저장 : 2차원
    img.array <- array(img.result,dim=imgdim) #다시 array로 돌림. dim=원래 가지고있던 디멘션 값 
    writeJPEG(img.array,paste('kmeans_',i,'clusters.jpeg',sep=''))
}




