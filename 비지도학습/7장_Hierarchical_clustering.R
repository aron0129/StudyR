# Hierarchical clustering 실습 ####

# 1. 데이터 확인 ####

df <- USArrests
head(df)

# 2. 결측치 확인 ####

colSums(is.na(df))


# 3. 변수들 기술통계 및 분포 확인####

summary(df)

boxplot(df)

#Assault 같은 경우 최대값이랑 최소값이 차이가 크다.
# 영향력이 너무 커지기 때문이다.
# 표준화 필요

# 4.표준화 ####

library(dplyr)
df <- scale(df) %>% as.data.frame()
boxplot(df) #비슷한 분포로 변경됨 

# 5. 이상치 제거 ####

library(tibble)

# dplyr에서 arrange함수 쓰면 행이름이 사라진다.

df.rm.outlier <- df %>% rownames_to_column('rname') %>% # 행이름을 살려주는 함수
     arrange(desc('Rape')) %>% # Rape의 내림차순으로 정렬
     slice(-1:-2) %>% #첫번째 두번째 행 없애줌
     column_to_rownames('rname') # 변수를 다시 행 이름으로 바꾸어줌 

boxplot(df.rm.outlier)

# 5. 유사도 행렬 생성(유클리드 거리) ####

df.dist <- dist(df.rm.outlier, method="euclidian")
df.dist

# 6. 군집 구성 방식 선택 ####

df.hclust.sing <- hclust(df.dist,method="single")
df.hclust.cplt <- hclust(df.dist, method = "complete" )
df.hclust.avg <- hclust(df.dist, method = "average" )
df.hclust.cent <- hclust(df.dist, method = "centroid" )
df.hclust.ward <- hclust(df.dist, method = "ward.D2" )




# 7. Dendrogram 생성 & 군집 시각화####

# 7-1. single method ####

plot(df.hclust.sing,cex=0.6,hang = -1) #hang = 줄기가 바닥에서 부터 나옴 -1

rect.hclust(df.hclust.sing,k=4,border = 2:5) #k=군집수
#군집간 경계 표시

# 7-2. complete method ####
plot(df.hclust.cplt, cex = 0.6, hang = -1)
rect.hclust(df.hclust.cplt, k = 4, border = 2:5)

# 7-3. average method ####
plot(df.hclust.avg, cex = 0.6, hang = -1)
rect.hclust(df.hclust.avg, k = 4, border = 2:5)


# 7-4. centroid method ####
plot(df.hclust.cent, cex = 0.6, hang = -1)
rect.hclust(df.hclust.cent, k = 4, border = 2:5)


# 7-5. Ward method ####
plot(df.hclust.ward, cex = 0.6, hang = -1)
rect.hclust(df.hclust.ward, k = 4, border = 2:5) # 덩어리가 크게 군집이 나뉨 





# 8. raw data에 cluster 할당 ####

df.cluster <- cutree(df.hclust.ward, k=4)
table(df.cluster)
#df.cluster
#1  2  3  4 
#11 19  6 12 

df.rm.outlier$cluster <- df.cluster #데이터에 군집 변수 추가
head(df.rm.outlier)


# 9. 2차원 시각화 ####
install.packages("factoextra")
library(factoextra)
fviz_cluster(list(data=df.rm.outlier[,1:ncol(df.rm.outlier)-1],#cluser변수명을 제외 
                  cluster=df.cluster))


# 10.군집 별 평균치 확인 및 시각화 ####
install.packages("reshape2")
library(reshape2)

temp <- df.rm.outlier %>% melt(id='cluster') #cluster기준으로 데이터 정렬(펼침)
head(temp)

df.means <- dcast(temp,cluster~variable, mean) #그룹별 평균 구하기 
#cluster를 행, variable을 열로 함 
df.means

#  cluster     Murder    Assault   UrbanPop        Rape
#1       1  0.7499801  1.1199128  0.9361748  1.21564322
#2       2 -0.3621789 -0.3444705  0.3953887 -0.21863180
#3       3  1.6367009  0.9968282 -0.8202782  0.05708357
#4       4 -1.0782511 -1.1370610 -0.9296640 -1.00344660



barplot(t(df.means[,-1]), beside = TRUE,col=1:4,names.arg = c(1:4))
# beside = TRUE : 가로로 펼쳐 놓아라
# col = 1~4번색 까지 사용
# names.arg x축을 1번부터 4번까지 번호로 매겨라
legend("topright", colnames(df.rm.outlier[1:4]), fill = 1:4, cex = 0.5)

# 1번 군집과 4번 군집이 대비. 1번은 다 높은데 4번은 다 낮음
# 2번 군집은 인구는 높은데 범죄율은 낮고
# 3번 군집은 인구는 낮은데 범죄율은 높다