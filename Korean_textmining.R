#volcano plot

library(datasets)
volcano 

# A  display of the volcano
x <- 10*(1:nrow(volcano))
y <- 10*(1:ncol(volcano))
image(x, y, volcano, col = terrain.colors(100), axes = FALSE) # volcano plot 그리기
contour(x, y, volcano, levels = seq(90, 200, by = 5),# 등고선 추가
        add = TRUE, col = "peru")
axis(1, at = seq(100, 800, by = 100)) # 축 레벨링
axis(2, at = seq(100, 600, by = 100))
box()
title(main = "Big Island Volcano", font.main = 4)


##################################################################
#example 4
#한글 인식 textmining

#installing a Java version
#https://www.java.com/en/download/
Sys.setenv(JAVA_HOME='Java\\jre1.8.0_131') # for 64-bit version
#Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre7') # for 32-bit version
#컴퓨터>속성에서 시스템 종류 확인
#JAVE 관련 readme file 있는 곳으로 지정


#change the system environment

install.packages('rJava')
install.packages('withr')

library(rJava)



install.packages("tidyverse")

install.packages("\\KoreanMining\\NLP4kec_1.4.0.zip",repos=NULL)



#repos=NULL:
#Can be NULL to install from local files


#install.packages("RColorBrewer")

# import
library(tidyverse) # 데이터 전처리(stringr)와 그래프(ggplot2)
library(stringr)
library(rJava) # NLP4kec 사용 목적
library(NLP4kec) # 자연어 처리
library(wordcloud) # 워드클라우드

library(rjson)
library(plyr)
library(tm)
library(ggplot2)
library(RColorBrewer)


library(readxl) #read excel file


data <- read_excel(path = "\\KoreanMining\\KoreanMining\\sample.xlsx",
                   sheet = "Sheet1",
                   col_names = FALSE)

data
nrow(data)
data[1,1]

# 문자열 합치기
sentence <- ""
for(i in 1:nrow(data)){
     sentence <- paste(sentence, data[i,1]) # sentence 뒤에 문장을 한줄로 더 붙임 
}
sentence


# 한글 제외 문자 삭제
sentence <- sentence %>% stringr::str_remove_all(pattern = "[^가-힣]")



# 단어 구분
data2 <- NLP4kec::r_parser_r(contentVector = sentence, language = "ko")


# 리스트 형태로 나누기
data3 <- strsplit(data2, split=" ")

# 테이블화


data3<-ifelse(data3=='현진','서현진',data3)


wordcount  <- table(data3)


head(sort(wordcount,decreasing=T),40)



names.freq<-names(wordcount)
new.names<-!names.freq%in%c('하다','있다',
                            '것','보다','되다'
                            ,'주다','않다','좋다','수','때','중',
                            '많다','없다','받다','같다','지다','오다')
show.id<-which(new.names)

nw_wordFreq.trim<-(wordcount)[show.id]


head(sort(nw_wordFreq.trim,decreasing=T),40)



names.freq<-names(nw_wordFreq.trim)
new.names<-!names.freq%in%c('알다','대하다',
                            '때문','보이다','맞추다','가다','김','합',
                            '씨')
show.id<-which(new.names)

nw_wordFreq.trim<-(nw_wordFreq.trim)[show.id]




pal <- brewer.pal(8, "Accent")


wordcloud(names(nw_wordFreq.trim), freq=nw_wordFreq.trim,
          min.freq=20, random.order=F, rot.per=0.1, colors=pal)


##########################################


####
#example 5
##
library(pdftools)

setwd('')


(file.names <- list.files(pattern = "pdf$"))
#create a vector of PDF file names
#pdf_text: 문장 선택

#list 로 변환하여 저장됨
(pages <- lapply(file.names, pdf_text))

pp<-length(pages[[1]])
pp

sentence <- "" 
for(jj in 1:pp){
     sentence <- paste(sentence, pages[[1]][jj]) # 문자열 하나로 합침
}

# 한글 제외 문자 삭제
sentence <- sentence %>% stringr::str_remove_all(pattern = "[^가-힣]")



# 단어 구분
data2 <- NLP4kec::r_parser_r(contentVector = sentence, language = "ko")


# 리스트 형태로 나누기
data3 <- strsplit(data2, split=" ")





wordcount  <- table(data3)
wordcount

head(sort(wordcount,decreasing=T),40)


#의미없는 단어들 trimming
names.freq<-names(wordcount)
new.names<-!names.freq%in%c('있다','수','후','않다')
show.id<-which(new.names)

nw_wordFreq.trim<-(wordcount)[show.id]


head(sort(nw_wordFreq.trim,decreasing=T),40)




pal <- brewer.pal(8, "Accent")


wordcloud(names(nw_wordFreq.trim), freq=nw_wordFreq.trim,
          min.freq=20, random.order=F, rot.per=0.1, colors=pal)


install.packages('wordcloud2')


library(wordcloud2)

#create html file
wordcloud2(nw_wordFreq.trim,
           color = "random-light",
           fontFamily = "NanumGothic")




#####################################






