#library checker 
if(!require(methods)){install.packages("methods")}

libs <- c("rio","readxl","jiebaR","dplyr","parallel","text2vec","taRifx","tidytext")

for (i in libs){
  if( !is.element(i, .packages(all.available = TRUE)) ) {
    install.packages(i)
  }
  library(i,character.only = TRUE)
}
lapply(libs, require, character.only = TRUE)

#Set working directory to the folder where I saved my code 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


cat("===== Excluded luckydraw =====\r\n")

#Read files
location <- file.choose( )
location <- sub("clickme",replacement = "",location)
input_location <- paste0(location,"1_Input_Data")
output_location <- paste0(location,"2_Outcome")
setting_location <- paste0(location,"3_Setting")

#Input the adjusting parameters
setwd(setting_location)
index <- read_excel("luckydraw_Index.xlsx")
threshold_1 <- index[1,2] %>% as.numeric()
threshold_n <- index[2,2] %>% as.numeric()
threshold_n2 <- index[3,2] %>% as.numeric()

cat("Correlation Index =",threshold_1,"\r\n")
cat("If the number of Docs with same title is more than",threshold_n,"\r\n")
cat("( Correlation Index =",threshold_n2,")\r\n")

setwd(input_location)
file_list <- cbind(list.files(all.files = TRUE))#read name of files
file_list <- file_list[3:length(file_list),]

raw_data_rbind <- as.character()

for ( p in 1:length(file_list))
{
  setwd(input_location)
  raw_data <- read_excel(file_list[p])
  time_begin <- Sys.time() %>% as.character()
  
  #Keep the previous column name
  colname_raw <- colnames(raw_data)
  colname_analysis <- colname_raw
  
  #Change the col name into Eng
  setwd(setting_location)
  colname_sub <- read_excel("luckydraw_Colname_Sub.xlsx")[,1]
  colname_sub_title <- colname_sub[2,1] %>% as.character()
  colname_sub_content <- colname_sub[3,1] %>% as.character()
  colname_sub_content_type <- colname_sub[6,1] %>% as.character()
  colname_sub_author <- colname_sub[17,1] %>% as.character()
  colname_sub_channel <- colname_sub[5,1] %>% as.character()
  
  colname_analysis <- sub(colname_sub_title,replacement = "title",colname_raw)
  colname_analysis <- sub(colname_sub_content,replacement = "content",colname_analysis)
  colname_analysis <- sub(colname_sub_content_type,replacement = "content_type",colname_analysis)
  colname_analysis <- sub(colname_sub_author,replacement = "author",colname_analysis)
  colname_analysis <- sub(colname_sub_channel,replacement = "channel",colname_analysis)
  colnames(raw_data) <- colname_analysis
  
  main_sub <- read_excel("luckydraw_Colname_Sub.xlsx")[1,2] %>% as.character()
  raw_data[raw_data$content_type == main_sub,]$content_type <- "main"
  
  Encoding(raw_data$title)<-'UTF-8'
  Encoding(raw_data$content)<-'UTF-8'
  
  #Clean the dataset
  data_sum <- cbind(raw_data$title,raw_data$content,raw_data$content_type) %>% as.data.frame()
  colnames(data_sum) <- c("title","content","content_type")
  
  #Remove the duplication
  title_unique <- unique(raw_data$title) %>% as.character() %>% as.data.frame() %>% remove.factors()
  
  #jieba applied
  cutter <- worker(bylines = TRUE)
  
  #Open multi-core processor
  cl_num <- detectCores()
  cl <- makeCluster(cl_num - 1)
  
  #Create a blank list with the titles of documents, which have lucky-draw incentive
  luckydraw_list <- as.character()
  unluckydraw_list <- as.character()
  
  fun_gsub1 <- function(x){gsub("(http|https|HTTP|HTTPS|url=http:).{0,5}[a-zA-Z0-9.?/&=:\\+\\-_%]*|lineID.{0,2}[a-zA-Z0-9.?/&=:\\-_%]*|[a-zA-Z0-9]*@.{0,10}(com|com.tw)",replacement = "",x)}
  fun_gsub2 <- function(x){gsub("[[:punct:]]",replacement = "",x)}
  fun_gsub3 <- function(x){gsub("[A-Za-z0-9]+",replacement = "A",x)}
  fun_gsub4 <- function(x){gsub(" ",replacement = "",x)}
  fun_count <- function(x){nchar(x)}
  
  pb<-txtProgressBar(min = 0, max = nrow(title_unique), style = 3)
  #1
  for ( i in 1:nrow(title_unique))
  {
    article_words_collect <- as.character()
    dimension_tag <- as.character()
    cos_sim_min <- NA
    
    #Filter the content from each title for word segmentation
    data_sum_select <- data_sum[data_sum$title==title_unique[i,],] %>% remove.factors()
    
    #Fill NA
    data_sum_select_na <- data_sum_select[is.na(data_sum_select$content),]
    if (nrow(data_sum_select_na)>0){data_sum_select_na$content <- "A"}
    data_sum_select_nonna <- data_sum_select[!is.na(data_sum_select$content),]
    data_sum_select <- rbind(data_sum_select_na,data_sum_select_nonna)
    rm(data_sum_select_na,data_sum_select_nonna)
    
    #If there are a lot of same content of the main-content, remain the one for analysis
    if (nrow(data_sum_select[data_sum_select$content_type== "main",]) == 1)
    {
      data_sum_select <- data_sum_select[data_sum_select$content_type!= "main",]
    }
    
    #2 If > 5, then calcuate the possibility of lucky-draw posts 
    if (nrow(data_sum_select) > 4 )
    {
      #Exclude the web links & Eng name
      data_sum_select_list <- t(data_sum_select$content) %>% as.list()
      data_sum_select_gsub1 <- parLapply(cl,data_sum_select_list,fun_gsub1)
      data_sum_select_gsub2 <- parLapply(cl,data_sum_select_gsub1,fun_gsub2)
      data_sum_select_gsub3 <- parLapply(cl,data_sum_select_gsub2,fun_gsub3)
      data_sum_select_gsub4 <- parLapply(cl,data_sum_select_gsub3,fun_gsub4)
      data_sum_select_count <- parLapply(cl,data_sum_select_gsub4,fun_count)
      data_sum_select_gsub4 <- data_sum_select_gsub4 %>% unlist() %>% tibble()
      data_sum_select_count <- data_sum_select_count %>% unlist() %>% tibble() 
      data_sum_select <- cbind(data_sum_select_gsub4,data_sum_select_count) 
      colnames(data_sum_select) <- c("content","char_n")
      rm(data_sum_select_list,data_sum_select_gsub1,data_sum_select_gsub2,data_sum_select_gsub3,data_sum_select_gsub4,data_sum_select_count)
      
      # If > 5, then sample
      if (nrow(data_sum_select) > 100 )
      {
        random <- sample(1:nrow(data_sum_select),100)
        data_sum_select <- data_sum_select[random,]
      }
      data_sum_select$num <- 1
      data_sum_select_content_agg <- aggregate(num ~ content, data=data_sum_select, sum )
      data_sum_select_content_agg <- data_sum_select_content_agg[order(data_sum_select_content_agg$num,decreasing = TRUE),]
      #3
      #If the replies are 30% the same, then the post is labeled as the document with the lucky-draw incentive.
      if ( data_sum_select_content_agg[1,]$num > nrow(data_sum_select)*0.3 )
      {
        luckydraw_temp <- title_unique[i,] %>% as.data.frame()
        luckydraw_temp <- cbind(luckydraw_temp,round(cos_sim_min,2))
        luckydraw_list <- rbind(luckydraw_list,luckydraw_temp)
        rm(luckydraw_temp,data_sum_select_content_agg)
      }
      else
      {
        #4
        #Calculate the mean number & SD of the characters to determine to analyze or not 
        if ( mean(data_sum_select$char_n) > 9 || sd(data_sum_select$char_n) > 9 )
        {
          cutter_content <- data_sum_select$content %>% as.character()
          article_words <- cutter[cutter_content]
          rm(cutter_content)
          
          #Create a list of segment-keyword 
          article_words_collect <- as.character()
          for ( j in 1:length(article_words))
          {
            article_words_temp <- article_words[j] %>% as.data.frame()
            colnames(article_words_temp) <- c("keyword_list")
            article_words_collect <- rbind(article_words_collect,article_words_temp)
            article_words_collect <- unique(article_words_collect) %>% remove.factors
          }
          article_words_collect_count <- parLapply(cl,article_words_collect,fun_count) %>% unlist() %>% tibble()
          article_words_collect <- cbind(article_words_collect,article_words_collect_count)
          article_words_collect <- article_words_collect %>% filter(.>1) #Keep the length >1
          article_words_collect <- article_words_collect[,-2] %>% tibble()
          rm(article_words_temp,article_words_collect_count)
          
          #Dimention labeled
          cutter_content_list <- data_sum_select$content %>% as.list()  
          dimension_count <- as.character()
          dimension_tag <- 1:nrow(data_sum_select) %>% tibble()
          for ( k in 1:nrow(article_words_collect))
          {
            fun_dim <- function(x){grepl(article_words_collect[k,],x)}
            clusterExport(cl,"k")
            clusterExport(cl,"article_words_collect")
            clusterExport(cl,"fun_dim")
            dim <- parLapply(cl,cutter_content_list,fun_dim)
            dim <- sub("TRUE",replacement = 1,dim)
            dim <- sub("FALSE",replacement = 0,dim)
            
            dim_tag <- dim %>% as.numeric() %>% tibble()
            colnames(dim_tag) <- article_words_collect[k,]
            dimension_tag <- cbind(dimension_tag,dim_tag)
            
            dim_count <- dim %>% as.numeric() %>% sum()
            dim_count <- cbind(article_words_collect[k,],dim_count)
            colnames(dim_count) <- c("keyword","count")
            dimension_count <- rbind(dimension_count,dim_count)
          }
          dimension_tag <- dimension_tag[,-1]
          dimension_count <- dimension_count %>% arrange(desc(count))
          rm(cutter_content_list,fun_dim,dim_tag,dim_count,dim)
          
          #5
          #If terms with top 5 TF occur in 90% documents, then the post will be labeled as lucky-draw incentive
          if ( (nrow(dimension_count) >  4 &&  mean(dimension_count[1:5,]$count)) > nrow(data_sum_select)*0.9 )
          {
            luckydraw_temp <- title_unique[i,] %>% as.data.frame()
            luckydraw_temp <- cbind(luckydraw_temp,round(cos_sim_min,2))
            luckydraw_list <- rbind(luckydraw_list,luckydraw_temp)
          } 
          else
          {
            #Take first thirty words into calculation
            if ( nrow(dimension_count) > 29)
            {tag_sum_order_word <- dimension_count[1:30,1]}
            else
            {tag_sum_order_word <- dimension_count[1:nrow(dimension_count),1]}
            dimension_tag_order <- dimension_tag[,tag_sum_order_word] %>% as.data.frame()
            
            #Cosine theorem applied
            cos_sim_min <- 1
            if (nrow(data_sum_select) < 50 )
            {
              sample_temp <- sample(1:nrow(data_sum_select),5)
              for (o in 1:length(sample_temp))
              {
                cos_sim_temp <- sim2(x = dimension_tag_order[sample_temp[o],] %>% as.matrix(),
                                     y = dimension_tag_order %>% as.matrix(), method = "cosine", norm = "l2")
                cos_sim_min <- min(mean(cos_sim_min),mean(cos_sim_temp))
              }
            }
            else
            {
              random <- sample(1:nrow(data_sum_select),50)
              dimension_tag_order <- dimension_tag_order[random,]
              sample_temp <- sample(1:50,5)
              for (o in 1:length(sample_temp))
              {
                cos_sim_temp <- sim2(x = dimension_tag_order[sample_temp[o],] %>% as.matrix(),
                                     y = dimension_tag_order %>% as.matrix(), method = "cosine", norm = "l2")
                cos_sim_min <- min(mean(cos_sim_min),mean(cos_sim_temp))
              }
            }
            rm(sample_temp,cos_sim_temp)
            
            #If the content is too similar, then it will be labeled as lucky-draw incentive as well
            if(cos_sim_min > threshold_1 || (nrow(data_sum_select) > threshold_n && cos_sim_min > threshold_n2))
            {
              luckydraw_temp <- title_unique[i,] %>% as.data.frame()
              luckydraw_temp <- cbind(luckydraw_temp,round(cos_sim_min,2))
              luckydraw_list <- rbind(luckydraw_list,luckydraw_temp)
            }
            else
            {
              unluckydraw_temp <- title_unique[i,] %>% as.data.frame()
              unluckydraw_temp <- cbind(unluckydraw_temp,round(cos_sim_min,2))
              unluckydraw_list <- rbind(unluckydraw_list,unluckydraw_temp)
            }
            #5
          }
          #4
        }
        else
        {
          luckydraw_temp <- title_unique[i,] %>% as.data.frame()
          luckydraw_temp <- cbind(luckydraw_temp,round(cos_sim_min,2))
          luckydraw_list <- rbind(luckydraw_list,luckydraw_temp)
        }
        #3
      }
      #2
    }
    setTxtProgressBar(pb, i)
    #1
  }
  close(pb)
  
  
  luckydraw <- 1:nrow(raw_data)
  raw_data <- cbind(luckydraw,luckydraw,raw_data)
  raw_data[,1] <- 0
  raw_data[,2] <- NA
  

  for(l in 1:nrow(luckydraw_list))
  {
    raw_data[raw_data$title==luckydraw_list[l,1],][,1] <- 1
    raw_data[raw_data$title==luckydraw_list[l,1],][,2] <- luckydraw_list[l,2]
  }
  for(l in 1:nrow(unluckydraw_list))
  {
    raw_data[raw_data$title==unluckydraw_list[l,1],][,1] <- 0
    raw_data[raw_data$title==unluckydraw_list[l,1],][,2] <- unluckydraw_list[l,2]
  }
  
  ##remove the redundant symbols
  fun_gsub5 <- function(x){gsub(" > ",replacement = " of " , x)}
  fun_gsub6 <- function(x){gsub("[[:punct:]]|[[:cntrl:]]|\u00a0",replacement = " " , x)}
  fun_gsub7 <- function(x){gsub(",|\r\n|\"|\\\\|^\\=|^\\-|^\\+",replacement = " " , x)}
  fun_space <- function(x){gsub("\\s{3,100}|#",replacement = "",x)}
  fun_substr <- function(x){substr(x,1,20000)}
  
  for(each in 1:ncol(raw_data))
  {
    if(raw_data[,each] %>% is.numeric() == FALSE) 
    {
      current_data <- raw_data[,each] %>% tibble()
      if(colnames(raw_data)[each] %in% c("channel"))
      {
        current_data <- parLapply(cl,current_data,fun_gsub5)
      }
      if(colnames(raw_data)[each] %in% c("title","content","author","channel"))
      {
        current_data <- parLapply(cl,current_data,fun_gsub6)
      }
      current_data <- parLapply(cl,current_data,fun_gsub7)
      current_data <- parLapply(cl,current_data,fun_space)
      current_data <- parLapply(cl,current_data,fun_substr)
      raw_data[,each] <- current_data %>% unlist()
      Encoding(raw_data[,each]) <- 'UTF-8'
    }
  }
  
  #Close multi-core processor
  stopCluster(cl)
  
  #Paste the original col back
  raw_data[raw_data$content_type == "main",]$content_type <- main_sub
  colname_luckydraw <- read_excel("luckydraw_colname_sub.xlsx")[1,3] %>% as.character()
  colnames(raw_data) <- c(colname_luckydraw,"cosine",colname_raw)
  luckydraw_y <- sum(raw_data[,1])
  luckydraw_n <- nrow(raw_data)-luckydraw_y
  
  #Outcome
  cat("Done!\r\n")
  cat("No.",p,"\r\n")
  cat("Start time:",time_begin,"\r\n")
  cat("Finish time:",Sys.time() %>% as.character(),"\r\n")
  cat("luckydraw labeled \"1\" =",luckydraw_y," (",round((luckydraw_y/nrow(raw_data))*100,2),"% )\r\n")
  cat("luckydraw labeled \"0\" =",luckydraw_n," (",round((luckydraw_n/nrow(raw_data))*100,2),"% )\r\n")
  
  setwd(output_location)
  output_temp <- gsub(".xlsx",".csv",file_list[p])
  write.table(raw_data, file = output_temp, sep = ",",row.names = FALSE) #csv
  
  #Bind the files and export
  raw_data_rbind <- rbind(raw_data_rbind,raw_data)
}
setwd(output_location)
output_xlsx <- paste0(Sys.Date(),"_luckydraw_labeled",".xlsx")
output_csv <- paste0(Sys.Date(),"_luckydraw_labeled",".csv")
export(raw_data_rbind,output_xlsx)
write.table(raw_data_rbind, file = output_csv, sep = ",",row.names = FALSE) #csv
cat("\nTimJ\n")

