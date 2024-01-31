freq_sex<- read.csv("發票資料/freq_sex.csv")
fdata<- read.csv("發票資料/fdata.csv")

# freq_sex圖
x11()
library(ggplot2)
freq_sex[, 1]<- fdata[freq_sex[,1], 2]
attach(freq_sex)
merge<- freq_sex[which(口味=="醇黑"), 3]+freq_sex[which(口味=="黑巧"), 3]
freq_sex[which(口味=="黑巧"), 3]<- merge
freq_sex<- freq_sex[-which(口味=="醇黑"), ]

level=c("乳製", "巧克力", "牛奶", "檸檬", "可可", "花生", "豆奶", "杏仁", "藍莓", "黑巧")
ggplot(freq_sex, aes(x = factor(口味, levels=level), y= 人數, fill = 性別)) +   
  geom_bar(stat = "identity", position = "dodge", color = "white") +  
  theme_bw() +   # 設定背景
  theme(text = element_text(size=20)) +
  labs(x="口味", y = "人數", fill = "性別", title="2020年發票資料 -- 性別與偏好口味")    # 設定標籤




# freq_age圖

library(gg.gap)
freq_age<- read.csv("發票資料/freq_age.csv")
freq_age<- read.csv("發票資料/freq_age改.csv")

freq_age[, 1]<- fdata[freq_age[,1], 2]
age<- c("童年: 0~6歲", "少年: 7~17歲", "青年: 18~40歲", "中年: 41~65歲", "老年: 66歲以上")
freq_age<- cbind(freq_age, age= age[freq_age[, 2]])



x11()

#原來
p1<- ggplot(freq_age, aes(x = factor(口味, levels=level), y= 人數, fill = factor(age, levels = c("童年: 0~6歲", "少年: 7~17歲", "青年: 18~40歲", "中年: 41~65歲", "老年: 66歲以上")))) +   
    geom_bar(stat = "identity", position = "dodge", color = "white") +  
    theme_bw() +   # 設定背景
    theme(text = element_text(size=20))+
    labs(x="口味", y = "人數", fill = "", title="2020年發票資料 -- 年齡層與偏好口味")  

#改
x11()
freq_age
freq_age$age <- factor(freq_age$age, levels=c("童年: 0~6歲", "少年: 7~17歲", "青年: 18~40歲", "中年: 41~65歲", "老年: 66歲以上"))
p1<- ggplot(freq_age, aes(x= freq_age$age, y= 人數, fill = factor(口味, levels=level))) +   
  geom_bar(stat = "identity", position = "dodge", color = "white") +  
  theme_bw() +   # 設定背景
  theme(text = element_text(size=20))+
  #axis.text.x = element_text(size=20), 
  #axis.text.y = element_text(size=20)) +
  labs(x="年齡層", y = "人數", fill = "口味", title="2020年發票資料 -- 年齡層與偏好口味")

gg.gap(plot = p1, 
       segments = list(c(28, 100),c(800, 1200)), 
       tick_width = c(4, 100, 200),
       ylim=c(0, 2500), 
       ylab=c(28, 30))

#add.legend(plot = p1,
#     margin = c(top=1,right=1,bottom=300,left= 430))

#改
add.legend(plot = p1,
           margin = c(top=1,right=1,bottom=150,left= 550))


#併 
attach(freq_age)
merge<- freq_age[which(年齡層==1), 3]+freq_age[which(年齡層==5), 3]
freq_age[which(年齡層==5), 3]<- merge
freq_age<- freq_age[-which(年齡層==1), ]
freq_age[which(freq_age[, 2]==5), 4]<- "6歲以下|66歲以上"
freq_age


freq_age$age <- factor(freq_age$age, levels=c("少年: 7~17歲", "青年: 18~40歲", "中年: 41~65歲", "6歲以下|66歲以上"))
p1<- ggplot(freq_age, aes(x= freq_age$age, y= 人數, fill = factor(口味, levels=level))) +   
  geom_bar(stat = "identity", position = "dodge", color = "white") +  
  theme_bw() +   # 設定背景
  theme(text = element_text(size=20))+
  labs(x="年齡層", y = "人數", fill = "口味", title="2020年發票資料 -- 年齡層與偏好口味")

gg.gap(plot = p1, 
       segments = list(c(28, 100),c(800, 1200)), 
       tick_width = c(4, 100, 200),
       ylim=c(0, 2500), 
       ylab=c(28, 30))
add.legend(plot = p1,
           margin = c(top=1,right=1,bottom=110,left= 480))





#合併地區
freq_area<- read.csv("發票資料/freq_area_合併.csv")
freq_area[, 1]<- fdata[freq_area[,1], 2]

attach(freq_area)
freq_area$地區 <- factor(freq_area$地區, levels=c("北部", "中部", "南部", "東部", "外島"))
ggplot(freq_area, aes(x = freq_area$地區, y= 人數, fill = factor(口味, levels=level))) +   
  geom_bar(stat = "identity", position = "dodge", color = "white") +  
  theme_bw() +   # 設定背景
  theme(text = element_text(size=20)) +
  labs(x="地區", y = "人數", fill = "口味", title="2020年發票資料 -- 地區與偏好口味")   


freq_area[which(freq_area$地區=="南部"), ]
levels(口味)

