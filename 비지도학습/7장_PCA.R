# PCA 실습 ####

# 1. 데이터 확인 ####

head(iris)

# 2. 결측치 확인 ####

colSums(is.na(iris))

# 3. 변수별 기술계 및 분포 확인 ####

summary(iris)

boxplot(iris[,1:4])

# 4. pca함수 적용 및 요약결과 확인 ####

iris.pca <- prcomp(iris[1:4],center=TRUE,scale.=TRUE) #pca 함수 

summary(iris.pca) #pca 요약정보
# standad deviation의 제곱 = eigenvalue

iris.pca$rotation # 각 주성분의 eigenvector

head(iris.pca$x, 10) #각 주성분의 값

# 5. Scree plot 확인 ####

plot(iris.pca, type="l", main='Scree plot') #PC의 분산을 Y축으로 scree plot 생성 

# 6. 차원축소 ####

head(iris.pca$x[,1:2],10) # 2개의 차원으로 축소

# 7. 2차원 시각화 ####

install.packages("ggfortify")
library(ggfortify)

autoplot(iris.pca, data=iris, colour='Species') #2차원으로 축소된 데이터 시각화 
#초록과 파란은 구분이 되지만 큰 차이가 없다.


# 고양이사진 PCA 실습 ####
setwd("비지도학습")
getwd()
install.packages("jpeg")

# 1. 데이터 확인 ####

library(jpeg)
cat <- readJPEG('cat.jpeg')
class(cat) # array

dim(cat)
#[1]  855 1280    3 -> RGB

# 2. RGB 데이터 분할 및 주성분 분석 ####

r <- cat[,,1] #array에서 r에 해당되는 데이터
g <- cat[,,2] #array에서 g에 해당되는 데이터
b <- cat[,,3] #array에서 b에 해당되는 데이터

cat.r.pca <- prcomp(r,center=F) # r데이터 주성분분석 
cat.g.pca <- prcomp(g,center=F) # r데이터 주성분분석 
cat.b.pca <- prcomp(b,center=F) # r데이터 주성분분석 
# center=F => 표준화를 안하는 이유는 차원축소 후 사진을 다시 만들어야하기 때문 

rgb.pca <- list(cat.r.pca,cat.g.pca,cat.b.pca) #분석결과 rgb로 합침 


# 3.차원을 축소하여 jpg로 저장 ####

pc <- c(2,10,50,100,300) #축소할 차원 수

for(i in pc){
     pca.img <- sapply(rgb.pca,function(j){ #각각의 리스트에 function(j) 적용 
          compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i]) #주성분의 값[1~i번쨰 열] 곱하기 고유벡터[1~i번째열] 곱합 -> rgb의 array형태 
     },simplify = 'array') # 적용 후 array로 만들어주기 
 ,sep=''))
}


