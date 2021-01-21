###Loading data and converting it to a data frame
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)

folder = "C:/Users/MACHENIKE/Desktop/MDS/Principle of Data Science/Project/Mobile and Gadgets"
file_list = list.files(folder, pattern = "*.csv")

final_df <- data.frame()

for (i in 1:5){
  name = file_list[i]
  path = paste(folder, name, sep = "/")
  keyword = substr(name, 8, nchar(file_list[1])-5)
  
  mydata <- read.csv(path)
  dfdata = data.frame(mydata)
  
  ###Discard unuseful columns and add a keyword column
  dfdata <- subset(dfdata , select = -c(Index,Review_Time ))
  dfdata<- cbind(keyword = keyword, dfdata) # Edit your keyword
  
  ###Inserting 'Nil' for missing values
  dfdata$Buyer_Review[is.na(dfdata$Buyer_Review)] <- "Nil"
  dfdata$Buyer_Tags[is.na(dfdata$Buyer_Tags)] <- "Nil"
  
  ###Tidy-ing data
  ##separating buyer_tags column
  
  dfdata <- separate(dfdata,Buyer_Tags, sep = "}",into = c("T1", "T2", "T3","T4","T5"), extra = "merge", fill = "right")
  
  ##remove tag_id and syntax
  dfdata$T1<- str_remove(dfdata$T1, "'tag_id.*") #remove tag_id
  dfdata$T2<- str_remove(dfdata$T2, "'tag_id.*")
  dfdata$T3<- str_remove(dfdata$T3, "'tag_id.*")
  dfdata$T4<- str_remove(dfdata$T4, "'tag_id.*")
  dfdata$T5<- str_remove(dfdata$T5, "'tag_id.*")
  
  dfdata$T1<-str_replace_all(dfdata$T1, "[[:punct:]]", "") #remove syntax
  dfdata$T2<-str_replace_all(dfdata$T2, "[[:punct:]]", "")
  dfdata$T3<-str_replace_all(dfdata$T3, "[[:punct:]]", "")
  dfdata$T4<-str_replace_all(dfdata$T4, "[[:punct:]]", "")
  dfdata$T5<-str_replace_all(dfdata$T5, "[[:punct:]]", "")
  
  dfdata$T1<- str_remove(dfdata$T1, "tagdescription") #remove tag_desc header
  dfdata$T2<- str_remove(dfdata$T2, "tagdescription")
  dfdata$T3<- str_remove(dfdata$T3, "tagdescription")
  dfdata$T4<- str_remove(dfdata$T4, "tagdescription")
  dfdata$T5<- str_remove(dfdata$T5, "tagdescription")
  
  dfdata$T1<- trimws(dfdata$T1) #remove whitespace before and after
  dfdata$T2<- trimws(dfdata$T2)
  dfdata$T3<- trimws(dfdata$T3)
  dfdata$T4<- trimws(dfdata$T4)
  dfdata$T5<- trimws(dfdata$T5)
  
  dfdata<-mutate_all(dfdata, list(~na_if(.,""))) #remove empty strings ("")
  
  fill_blank <- function(dataframe, x){
    new_df <- replace(dataframe, is.na.data.frame(dataframe), x)
  }
  
  dfdata <- fill_blank(dfdata,"Nil")
  
  #breakdown the review tag and aggregate into structure columns
  for (x in 1:nrow(dfdata)) {
    GPQ = 0
    GVM = 0
    FD = 0
    ESS = 0
    others = 0
    for (y in 1:5){
      if (y == 1){
        col_name = 'T1'
      }else if (y == 2){
        col_name = 'T2'
      }else if (y == 3){
        col_name = 'T3'
      }else if (y == 4){
        col_name = 'T4'
      }else {
        col_name = 'T5'
      }
      
      z = dfdata[x, col_name]
      if (z == "Good product quality"){
        GPQ = 1
      } else if (z == "Good value for money"){
        GVM = 1
      } else if (z == "Fast delivery"){
        FD = 1
      } else if (z == "Excellent service by seller"){
        ESS = 1
      } else if (z == "Nil"){
        print("nothing")
      } else{
        others = 1
      }
    }
    dfdata[x,"Good product quality"] <- GPQ
    dfdata[x,"Good value for money"] <- GVM
    dfdata[x,"Fast delivery"] <- FD
    dfdata[x,"Excellent service by seller"] <- ESS
    dfdata[x,"OtherTags"] <- others
    
  }
  
  dfdata <- dfdata[-c(11:15)]
  
  final_df <- rbind.data.frame(final_df, dfdata, make.row.names = TRUE)
  
}

# Save the clean data as csv
write.csv(final_df, "C:/Users/MACHENIKE/Desktop/MDS/Principle of Data Science/Project/CLeaned/Mobile&Gadgets.csv", row.names = TRUE)
