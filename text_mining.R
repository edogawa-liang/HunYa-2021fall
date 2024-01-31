#getwd()
#rm(list=ls())

# 匯入資料
## 輿情
library(readxl)
Qnews<- read_xlsx("輿情分析資料/輿情/QSearch_新聞媒體網站資料.xlsx")
Qforum<- read_xlsx("輿情分析資料/輿情/QSearch_論壇網站資料.xlsx")
social_media<- read_xlsx("輿情分析資料/輿情/社群媒體輿情資料.xlsx")
forum_08<- read.csv("輿情分析資料/輿情/forum_202108.csv", header=T)
ig_07<- read.csv("輿情分析資料/輿情/instagram_202107.csv")
ig_08<- read.csv("輿情分析資料/輿情/instagram_202108.csv")
yt_07<- read.csv("輿情分析資料/輿情/youtube_kol_202107.csv")
yt_08<- read.csv("輿情分析資料/輿情/youtube_kol_202108.csv")
  

## 發票
bill1_all<- read.table("輿情分析資料/發票/bill1_all.txt")
bill1_female<- read.table("輿情分析資料/發票/bill1_female.txt")
bill1_male<- read.table("輿情分析資料/發票/bill1_male.txt", header=T)
bill2_all<- read.table("輿情分析資料/發票/bill2_all.txt", header=T)
bill2_female<- read.table("輿情分析資料/發票/bill2_female.txt", header=T)
bill2_male<- read.table("輿情分析資料/發票/bill2_male.txt", header=T)
bill3_all<- read.table("輿情分析資料/發票/bill3_all.txt", header=T)
bill3_female<- read.table("輿情分析資料/發票/bill3_female.txt", header=T)
bill3_male<- read.table("輿情分析資料/發票/bill3_male.txt", header=T)
bill4_all<- read.table("輿情分析資料/發票/bill4_all.txt", header=T)
bill4_female<- read.table("輿情分析資料/發票/bill4_female.txt", header=T)
bill4_male<- read.table("輿情分析資料/發票/bill4_male.txt", header=T)




# trim
Qnews<- Qnews[colnames(Qnews)=="content"]
Qforum<- Qforum[colnames(Qforum)=="content"]

social_media_trim<- social_media[colnames(social_media)=="content"]
naa<- which(is.na(social_media_trim$content==""))
social_media_trim[naa, ]<-social_media[naa, colnames(social_media)=="push_content"]
social_media<- social_media_trim

forum_08<- forum_08[colnames(forum_08)=="content"]
ig_07<- ig_07[colnames(ig_07)=="content"]
ig_08<- ig_08[colnames(ig_08)=="content"]
yt_07<- yt_07[colnames(yt_07)=="content"]
yt_08<- yt_08[colnames(yt_08)=="content"]


# Qnews_trim、Qforum_trim標頭拿掉
nohead<- function(data, type){
  clean<- matrix(0, nrow=nrow(data), ncol=1)
  if(type=="新聞"){
    for(i in 1:nrow(data)){
      no<- regexpr(".報導",data[i,])[1]
      clean[i,]<- substr(data[i,], no+4, nchar(data[i,]))
    }
  }else{
    for(i in 1:nrow(data)){
      no<- regexpr(".〕",data[i,])[1]
      clean[i,]<- substr(data[i,], no+2, nchar(data[i,]))
    }
  }
  return(clean)
}

clean_Qnews<- nohead(Qnews, "新聞")
clean_Qforum<- nohead(Qforum, "論壇")


#clean_Qnews、clean_Qforum、social_media_trim、forum_08_trim
#ig_07_trim、ig_08_trim、yt_07_trim、yt_08_trim

# 將發票第二欄拿掉
bill1_female<- bill1_female[, 1]
bill1_male<- bill1_male[, 1]
bill2_female<- bill2_female[, 1]
bill2_male<- bill2_male[, 1]
bill3_female<- bill3_female[, 1]
bill3_male<- bill3_male[, 1]
bill4_female<- bill4_female[, 1]
bill4_male<- bill4_male[, 1]

# 移除數字英文鬼符號
## 輿情
nonoise<- function(data){
  new<- matrix(0, nrow = nrow(data), 1)
  for(i in 1:nrow(data)){
    new[i, ]<- gsub("[a-z]|=|&|\\.|\\d|\\[|\\]|#|＃|:|\\?|↓|[A-Z]|\\(|\\)|\\/|@|-|【|】|←|→|＿|—|「|」|\\*|『|』|《|》|－|%|※|:|：|+" , 
                    "", data[i,])
  }
  return(new)
}

## 發票
nonoise<- function(data){
  data<- data[1:nrow(data), ]
  new<- matrix(0, nrow = length(data), 1)
  for(i in 1:length(data)){
    new[i, ]<- gsub("[a-z]|=|&|\\.|\\d|\\[|\\]|#|＃|:|\\?|↓|[A-Z]|\\(|\\)|\\/|@|-|【|】|←|→|＿|—|「|」|\\*|『|』|《|》|－|%|※|:|：|+" , 
                    "", data[i])
  }
  return(new)
}

View(bill1_all_2)

bill1_all_2<- nonoise(bill1_all)
#bill1_female<- nonoise(bill1_female)
#bill1_male<- nonoise(bill1_male)
bill2_all_2<- nonoise(bill2_all)
#bill2_female<- nonoise(bill2_female)
#bill2_male<- nonoise(bill2_male)
bill3_all_2<- nonoise(bill3_all)
#bill3_female<- nonoise(bill3_female)
#bill3_male<- nonoise(bill3_male)
bill4_all_2<- nonoise(bill4_all)
#bill4_female<- nonoise(bill4_female)
#bill4_male<- nonoise(bill4_male)


write.table(bill1_all_2, file = "輿情分析資料/發票/bill1_all.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
#write.table(bill1_female, file = "輿情分析資料/發票/bill1_female.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
#write.table(bill1_male, file = "輿情分析資料/發票/bill1_male.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
write.table(bill2_all_2, file = "輿情分析資料/發票/bill2_all.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
#write.table(bill2_female, file = "輿情分析資料/發票/bill2_female.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
#write.table(bill2_male, file = "輿情分析資料/發票/bill2_male.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
write.table(bill3_all_2, file = "輿情分析資料/發票/bill3_all.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
#write.table(bill3_female, file = "輿情分析資料/發票/bill3_female.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
#write.table(bill3_male, file = "輿情分析資料/發票/bill3_male.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
write.table(bill4_all_2, file = "輿情分析資料/發票/bill4_all.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
#write.table(bill4_female, file = "輿情分析資料/發票/bill4_female.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
#write.table(bill4_male, file = "輿情分析資料/發票/bill4_male.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")



# 輿情
clean_Qnews<- nonoise(clean_Qnews)
clean_Qforum<- nonoise(clean_Qforum)
clean_social_media<- nonoise(social_media)
clean_forum_08<- nonoise(forum_08)
clean_ig_07<- nonoise(ig_07)
clean_ig_08<- nonoise(ig_08)
clean_yt_07<- nonoise(yt_07)
clean_yt_08<- nonoise(yt_08)




# 輿情
write.table(clean_Qnews, file = "輿情分析資料/輿情/Qnews.txt", sep = " ", row.names=FALSE)
write.table(clean_Qforum, file = "輿情分析資料/輿情/Qforum.txt", sep = " ", row.names=FALSE)
write.table(clean_social_media, file = "輿情分析資料/輿情/social_media.csv", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
write.table(clean_forum_08, file = "輿情分析資料/輿情/forum_08.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
write.table(clean_ig_07, file = "輿情分析資料/輿情/ig_07.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
write.table(clean_ig_08, file = "輿情分析資料/輿情/ig_08.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
write.table(clean_yt_07, file = "輿情分析資料/輿情/yt_07.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
write.table(clean_yt_08, file = "輿情分析資料/輿情/yt_08.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")

# 發票
write.table(bill1_all, file = "輿情分析資料/發票/bill1_all.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
write.table(bill1_female, file = "輿情分析資料/發票/bill1_female.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
write.table(bill1_male, file = "輿情分析資料/發票/bill1_male.csv", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
write.table(bill2_all, file = "輿情分析資料/發票/bill2_all.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
write.table(bill2_female, file = "輿情分析資料/發票/bill2_female.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
write.table(bill2_male, file = "輿情分析資料/發票/bill2_male.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
write.table(bill3_all, file = "輿情分析資料/發票/bill3_all.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
write.table(bill3_female, file = "輿情分析資料/發票/bill3_female.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
write.table(bill3_male, file = "輿情分析資料/發票/bill3_male.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
write.table(bill4_all, file = "輿情分析資料/發票/bill4_all.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
write.table(bill4_female, file = "輿情分析資料/發票/bill4_female.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")
write.table(bill4_male, file = "輿情分析資料/發票/bill4_male.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")



# 新Q
library(jsonlite)
library(data.table)
library(stringr)
forum_202005 <- fromJSON("輿情分析資料/輿情/forum.202005.json")


forum_202005 <- lapply(readLines("輿情分析資料/raw_data/forum.202005.json"), fromJSON)
forum_202005_trim<- matrix(0, length(forum_202005), 1)
for (i in 1: length(forum_202005)){
  forum_202005_trim[i, ]<- forum_202005[[i]]$content
}
nonoise<- function(data){
  new<- matrix(0, nrow = nrow(data), 1)
  for(i in 1:nrow(data)){
    new[i, ]<- gsub("[a-z]|=|&|\\.|\\d|\\[|\\]|#|＃|:|\\?|↓|[A-Z]|\\(|\\)|\\/|@|-|【|】|←|→|＿|—|「|」|\\*|『|』|《|》|－|%|※|:|：|+" , 
                    "", data[i,])
  }
  return(new)
}
forum_202005<- nonoise(forum_202005_trim)
View(forum_202005)
write.table(forum_202005, file = "輿情分析資料/輿情/forum_202005.txt", sep = " ", row.names=FALSE, fileEncoding="UTF-8")


