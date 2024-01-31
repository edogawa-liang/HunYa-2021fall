#rm(list=ls())
getwd()
library(jsonlite)
library(data.table)
library(stringr)
choco <- fromJSON("chocolate.json")
View(choco)
# 品名、原料、葷素類別、淨重、原產地
choco_table<- choco[, 1:5]

# 營養標示: 每單位重、單位總數
choco_table<- cbind(choco_table, choco[,6][1], choco[,6][2])

# 營養標示: 成分(熱量~鈉)
anal_type<- nrow(choco[1,6][3]$成分[[1]])
table<- matrix(0, nrow(choco_table), anal_type)

## 將錯誤data放進wrong
wrong<- c()
for(number in 1: nrow(choco_table)){
  filter<- which(duplicated(choco[number,6][3]$成分[[1]][, 1])==F)
  element<- choco[number,6][3]$成分[[1]][filter, ]
  
  if(length(which(element[, 1]==c("熱量", "蛋白質", "脂肪", "飽和脂肪", "反式脂肪", "碳水化合物", "糖", "鈉")))==nrow(element)){
    all_element<-c()
    for(i in 1:nrow(element)){
      all_element<- cbind(all_element, element[i, 2])
    }
    table[number, ]<- all_element
    wrong[number]<- 1
    
  }else{
    table[number, ]<-rep(NA, anal_type)
    wrong[number]<- 0 
  }
  
}
wrong<- which(wrong==0)
#wrong    #67、125列資料要另外整理



# 合併
choco_table2<- cbind(choco_table, table)
colnames(choco_table2)<- c(colnames(choco_table), 
                           paste(element[, 1], rep("(每單位含量)", length(element[, 1]))))


# 轉數值
for(i in c(4, 6:ncol(choco_table2))){
  choco_table2[,i]<- gsub("公克| |大卡|份|毫克|%", "", choco_table2[,i])
  choco_table2[grep("\\D$", choco_table2[,i]), i]<- NA
  choco_table2[,i]<- as.numeric(choco_table2[,i])
} 


# 刪除重複項目(相同品項但資料不同的還留著)
choco_table2<- unique(choco_table2)



# 轉數值fun
changenum<- function(column){
  col.f<- levels(factor(choco_table2[, column]))
  for(i in 1: nrow(choco_table2)){
    for(j in 1:length(col.f)){
      if (choco_table2[i, column]==col.f[j]){
        choco_table2[i, column]<- j
      }
    }
  }
  choco_table2[, column]<- as.numeric(choco_table2[,  column])
  return(list(choco_table2[, column], col.f))
}

## 葷素轉數值
#changenum(3)[[2]]  #check種類有沒有一樣但不同名字
choco_table2[, 3]<- changenum(3)[[1]]
choco_table2[which(choco_table2[, 3]==8), 3]<- 6  #因為葷跟葷食一樣

## 原產地轉數值
#changenum(5)[[2]]  #check種類有沒有一樣但不同名字
choco_table2[, 5]<- changenum(5)[[1]]


#View(choco_table2) 


basic_table<- choco_table2[, c(1, 3:ncol(choco_table2))]
#View(basic_table)




# 原料 ingredient_table
ingredient<- matrix(0, nrow=nrow(choco_table2), ncol=25)
for(i in 1:nrow(choco_table2)){
  for(j in 1:length(choco_table2[i, 2][[1]]))
  ingredient[i, j]<- choco_table2[i, 2][[1]][j]
}
ingredient[which(ingredient==0)]<- NA
ingredient<- as.character(ingredient)
ingredient<- levels(factor(ingredient))
ingredient<- ingredient[which(ingredient!="")]

## 將原料名中有括號的去掉
for(i in 1: length(ingredient)){
  no<- regexpr("\\(|\\)|【", ingredient[i])[1]
  if(no!=-1){
    ingredient[i]<- substr(ingredient[i], start= 1, stop= no-1) 
  }
}
ingredient<- levels(factor(ingredient))


ingredient_table<- matrix(0, nrow=nrow(choco_table2), ncol=length(ingredient))

for(i in 1:nrow(choco_table2)){
  product<- choco_table2[i, 2][[1]]
  for(j in 1: length(product)){
    ingre<- regexpr("\\(|\\)|【", product[j])[1]
    if(ingre!=-1){
      product[j]<- substr(product[j], start= 1, stop= ingre-1) 
    }
    ingredient_table[i, which(product[j]==ingredient)]<- 1
  }
}
colnames(ingredient_table)<- ingredient
rownames(ingredient_table)<- choco_table2[,1]
#View(ingredient_table)


## 檢查有這項原料的產品數量
numberof1<- c()
for(i in 1:ncol(ingredient_table)){
  numberof1[i]<- length(which(ingredient_table[, i]==1))
}
#numberof1



# 統一單位 unit_table
## 100g下之營養素 (營養素每單位含量*(100g/每單位重))
## 總營養素 (營養素每單位含量*單位總數)

eight<- length(element[,1])
unitg<- 100/choco_table2[, which(colnames(choco_table2)=="每單位重")]
unit<- choco_table2[, which(colnames(choco_table2)=="單位總數")]

down<- which(colnames(choco_table2)=="熱量 (每單位含量)")
unit_table<- matrix(0, nrow=nrow(choco_table2), ncol=eight*2)

for(i in 1:eight){
  ## 100g下之營養素
  unit_table[, i]<- round(choco_table2[, i+down-1]*unitg,2)
  
  ## 總營養素
  unit_table[, i+eight]<- round(choco_table2[, i+down-1]*unit,2)
}

colnames(unit_table)<- paste(rep(element[, 1], 2), rep(c("/100g", " 總和"), each=eight), sep="") 
rownames(unit_table)<- choco_table2[,1]
#View(unit_table)  



# 口味
flavor<- c("咖啡", "檸檬", "焦糖", "榛果", "地瓜", "擂茶", "洋蔥", "起司", "葡萄", "紅豆",
           "百香果", "覆盆子", "堅果", "芝麻", "水果", "紅茶", "原味", "奶油", "洛神", "鳳梨","黑森林",
           "可可", "黑糖", "珍奶", "珍珠", "芋頭", "牛奶", "益生菌", "牛奶", "鮮乳","蛋黃", "醇黑",
           "乳酸", "藍莓", "綠茶", "芝麻", "豆奶", "草莓", "穀麥", "薄荷", "焦糖", "燕麥", "唐辛子", "柚子", "胡椒", 
           "椒香", "辣雞翅", "起司", "摩卡", "花生", "巧克力", "抹茶", "黑巧", "杏仁", "乳", "楓糖", "莓果", 
           "脆可", "穀麥")

# 口味代號 flavor   
flavor<- levels(factor(flavor))
#flavor

product_flavor<- list()
blank<- c()
for(i in 1:nrow(choco_table2)){
  product_flavor[[i]]<- which(str_detect(choco_table2[i,1], flavor))
  if(identical(product_flavor[[i]], integer(0))){
    product_flavor[[i]]<- 0   #沒被分到口味的給0 (可能是不須特別寫的原味)
    blank<- append(blank,i)
  }
}
#choco_table2[blank,1]   #看沒有被分到口味的
names(product_flavor)<- choco_table2[,1]

#View(product_flavor)




# 蛋糕餅乾總類
type<- c("小西餅", "酥", "悌納餅", "鬆餅", "達克瓦滋", "蛋白餅", "圈圈餅", 
         "曲奇餅", "蛋糕", "三明治", "球", "醬", "乳加", "派", "bar", "脆可", "千層", 
         "塔", "玉米四層胚", "雪茄捲",  "巧克力", "威化")

# 種類代號
type<- levels(factor(type))
#type


product_type<- list()
blank<- c()
for(i in 1:nrow(choco_table2)){
  product_type[[i]]<- which(str_detect(choco_table2[i,1], type))
  if(identical(product_type[[i]], integer(0))){
    product_type[[i]]<- 0   #沒被分到種類的給0  (可能是普通餅乾)
    blank<- append(blank,i)
  }
}
#choco_table2[blank,1]   #看沒有被分到種類的
names(product_type)<- choco_table2[,1]

#View(product_type)


wantdata<- c(28, 42, 44, 55, 60, 64, 69, 81, 96, 97, 105, 110, 115, 116, 
             132, 148, 155, 162, 164, 188, 189)
basic_table[wantdata-1, 1]

# 匯出
getwd()
basic_table_2<- basic_table[wantdata-1, ]
ingredient_table_2<- ingredient_table[wantdata-1, ]
unit_table_2<- unit_table[wantdata-1, ]

write.csv(basic_table_2, file = "產品成分整理/basic_table_2.csv", sep=",", col.names=TRUE, row.names=F)
write.csv(ingredient_table_2, file = "產品成分整理/ingredient_table_2.csv", sep=",", col.names=TRUE)
write.csv(unit_table_2, file = "產品成分整理/unit_table_2.csv", sep=",", col.names=TRUE)


#---------------------------------table---------------------------------------#
# View(choco_table1)  最原始json資料直翻，程式碼在try.r裡

#粗略一覽(原料是list，不能匯出)
View(choco_table2)      

# 基本資料表
View(basic_table)
basic_table[c()]
# 原料表
View(ingredient_table)

# 營養素表
View(unit_table) 


# 口味 (list，不能匯出)
View(product_flavor)

# 種類 (list，不能匯出)
View(product_type)

