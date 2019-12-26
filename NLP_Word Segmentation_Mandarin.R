#library checker 
if(!require(methods)){install.packages("methods")}

libs <- c("rio","readxl","jiebaR","ngram","dplyr","parallel","stringr","taRifx","tidytext")

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


cat("===== Word_Segmentation =====\r\n")

time1 <- Sys.time()

#Read files
location <- file.choose( )
location <- sub("clickme",replacement = "",location)
input_location <- paste0(location,"1_Input_Data")
output_location <- paste0(location,"2_Outcome")
setting_location <- paste0(location,"3_Setting")

#Filter
setwd(setting_location)
index <- read_xlsx("Segmentation_Index.xlsx")
repeat_num <- index[1,2] %>% as.numeric()
ngram_num <- index[2,2] %>% as.numeric()
#freq_ngrams_num <- index[3,2] %>% as.numeric() #biger than
wordlen_num <- index[5,2] %>% as.numeric() 
freq_standard <- index[3,2] %>% as.numeric() 
doc_standard <- index[4,2] %>% as.numeric() 

#Jieba Applied
cutter <- worker(bylines = TRUE)

#White list reading
setwd(setting_location)
white_list <- read_excel("White_List.xlsx")
for(i in 1:nrow(white_list))
{
  vocab_power_chr <- as.character(white_list[i,1])
  new_user_word(cutter,vocab_power_chr,"n")
}

#Multi-core processor
cl_num <- detectCores()
cl <- makeCluster(cl_num-1)

#Read data
cat("\nInput data:\n")
setwd(input_location)
file_name <- cbind(list.files(all.files = TRUE))
file_name <- file_name[3:length(file_name),]

nrow_num <- 10000
skip_num <- nrow_num+1

#Iteration
input_initial <- read.table(file_name,header = T,stringsAsFactors = F,sep=",",nrows = nrow_num,skip = 0)
input_name <- colnames(input_initial)


if (nrow(input_initial)>(10000-1))
{
  repeat
  {
    input_temp <- read.table(file_name,header = F,stringsAsFactors = F,sep=",",nrows = nrow_num,skip = skip_num)
    colnames(input_temp) <- input_name
    input_initial <- rbind(input_initial,input_temp)
    skip_num <- nrow_num + skip_num
    cat("Number of data to load:",nrow(input_initial),"\n")
    if (nrow(input_temp) < nrow_num ){break}
  }
  rm(nrow_num,skip_num,file_name,input_name,input_temp)
}else{ 
  cat("Number of data to load:",nrow(input_initial),"\n")
  rm(nrow_num,skip_num,file_name,input_name) }

#Keep the original col name
setwd(setting_location)
colname_raw <- colnames(input_initial)
colname_analysis <- colname_raw

colname_sub <- read_excel("Segmentation_Colname_Sub.xlsx")[,1]
colname_sub_content <- colname_sub[3,1] %>% as.character()
colname_analysis <- sub(colname_sub_content,replacement = "content",colname_analysis)
colnames(input_initial) <- colname_analysis

#The data waited for being labeled
analysis_content <- content_labeled <- input_initial$content
analysis_content <- gsub("(http|https|HTTP|HTTPS|url=http:).{0,5}[a-zA-Z0-9.?/&=:\\+\\-_%]*|lineID.{0,2}[a-zA-Z0-9.?/&=:\\-_%]*|[a-zA-Z0-9]*@.{0,10}(com|com.tw)",replacement = "",analysis_content)

#Calculation
cat("\nWord segmentation:\n")
select_num <- 1
select_num1 <- repeat_num
ng_table_sum1 <- as.character()
repeat
{
  #Data Preparation
  content_select <- analysis_content[select_num:select_num1]
  content_select <- content_select[is.na(content_select) == FALSE ]
  
  freq_ngrams_num <- length(content_select)/100
  if(freq_ngrams_num < 10){freq_ngrams_num <- 10}
  
  #Word Segmentation
  content_select <- cutter[content_select]
  article_con <- concatenate(content_select)
  #Remove the redundant symbols
  article_con <- gsub("\u00a0",replacement = "",article_con)
  article_con <- gsub("c\\(",replacement = "",article_con)
  article_con <- gsub("[[:punct:]]|[[:cntrl:]]",replacement = " ",article_con) # replace punctuation with space
  article_con <- gsub(" BR ",replacement = " ",article_con)
  article_con <- gsub("\\s{1,100}",replacement = " ",article_con)

  
  #N-gram applied
  ng_table_sum <- as.character()
  pb_j <- txtProgressBar(min = 1, max = ngram_num, style = 3)
  for (j in 1:ngram_num)
  {
    ng_words <- ngram(article_con,n=j)
    ng_table <- get.phrasetable(ng_words)
    Encoding(ng_table[,1]) <- 'UTF-8'
    if(nrow(ng_table)>0)
    {
      ng_table[,1] <- gsub(" ",replacement = "",ng_table[,1]) 
      ng_table$ng_n <- j
      ng_table$wordlen <- nchar(ng_table$ngrams)
      ng_table1 <- ng_table %>% 
        filter(freq > freq_ngrams_num) %>% 
        filter(wordlen > 1) %>% 
        filter(wordlen < wordlen_num)
      ng_table_sum <- rbind(ng_table_sum,ng_table1)
    }
    setTxtProgressBar(pb_j, j)
  }
  #Remove the duplication
  ng_table_sum  <- ng_table_sum %>% group_by(ngrams) %>% top_n(1,freq) %>% ungroup()
  ng_table_sum1 <- rbind(ng_table_sum1,ng_table_sum)
  
  if(length(content_select) < repeat_num){
    cat("\nData processing:",length(analysis_content),"( 100.00 %)\n")
  }else{
    cat("\nData processing:",select_num1,"(",round(select_num1/length(analysis_content)*100,2),"%)\n")
  }
  if(length(content_select) < repeat_num){break}
  select_num <- select_num + repeat_num
  select_num1 <- select_num1 + repeat_num
}
rm(content_select,article_con,ng_table_sum,ng_words,ng_table,ng_table1,select_num,select_num1,pb_j)

#Summarize the freq
ng_table_sum2 <- aggregate(freq~ngrams,ng_table_sum1,sum)
ng_table_unique <- ng_table_sum2$ngrams %>% tibble()
colnames(ng_table_unique) <- "ngrams"

#Dealing with the articles with only Eng & numbers
ng_table_dim_notc_label <- as.character()
fun_dim_nonc <- function(x){grepl("^[A-Za-z0-9]+$",x)}
clusterExport(cl,"fun_dim_nonc")
ng_table_dim_notc_label <- parLapply(cl,ng_table_unique[,1],fun_dim_nonc) %>% unlist()
ng_table_dim_notc_label <- sub("TRUE",replacement = 1, ng_table_dim_notc_label)
ng_table_dim_notc_label <- sub("FALSE",replacement = 0, ng_table_dim_notc_label)
ng_table_dim_notc_label <- ng_table_dim_notc_label %>% as.integer() %>% as.data.frame()
colnames(ng_table_dim_notc_label) <- "notchinese"
ng_table_sum2 <- cbind(ng_table_unique,ng_table_dim_notc_label)
rm(ng_table_dim_notc_label,fun_dim_nonc)

ng_table_dim_notc <- ng_table_sum2[ng_table_sum2$notchinese == 1,] %>% remove.factors()
ng_table_dim_isc <- ng_table_sum2[ng_table_sum2$notchinese == 0,] %>% remove.factors()

label_notc <- "[\u4e00-\u9fa5| ]subname[\u4e00-\u9fa5| ]|^subname[\u4e00-\u9fa5| ]|[\u4e00-\u9fa5| ]subname$|^subname$"
fun_notc_sub <- function(x){gsub("subname",x,label_notc)}
clusterExport(cl,"label_notc")
data_notc_sub <- parLapply(cl,ng_table_dim_notc[,1],fun_notc_sub) %>% unlist()  %>% tibble()
colnames(data_notc_sub) <- "keyword"
ng_table_dim_notc <- cbind(ng_table_dim_notc,data_notc_sub)
rm(label_notc,fun_notc_sub,data_notc_sub)

ng_table_dim_isc_sub <- ng_table_dim_isc[,1] %>% unlist() %>% as.data.frame()
colnames(ng_table_dim_isc_sub) <- "keyword"
ng_table_dim_isc <- cbind(ng_table_dim_isc,ng_table_dim_isc_sub)
rm(ng_table_dim_isc_sub)

ng_table_dimension <- rbind(ng_table_dim_notc,ng_table_dim_isc)
rm(ng_table_dim_notc,ng_table_dim_isc)

#List of TF and DF
dimension_data_freq <- 1:length(content_labeled) %>% tibble()
dimension_data_doc <- 1:length(content_labeled) %>% tibble()

#Label
cat("\nFreq counts and Doc counts: \n")
ng_count <- as.character()
pb_l <- txtProgressBar(min = 1, max = nrow(ng_table_dimension), style = 3)
for(l in 1:nrow(ng_table_dimension))
{
  fun_freq <- function(x){str_count(x,ng_table_dimension[l,3])}
  clusterExport(cl,"l")
  clusterExport(cl,"str_count")
  clusterExport(cl,"ng_table_dimension")
  clusterExport(cl,"fun_freq")
  
  freq_num <- parLapply(cl,content_labeled,fun_freq) %>% unlist() %>% as.data.frame()
  freq_num[is.na(freq_num)==TRUE,] <- 0
  freq_sum <- sum(freq_num) %>% tibble()
  dimension_data_freq <- cbind(dimension_data_freq,freq_num)
  
  doc_num <- freq_num
  doc_num[doc_num > 1,] <-1
  doc_sum <- sum(doc_num) %>% tibble()
  dimension_data_doc <- cbind(dimension_data_doc,doc_num)
  
  ng_freq_doc <- cbind(ng_table_dimension[l,1],freq_sum,doc_sum) %>% as.data.frame()
  ng_count <- rbind(ng_count,ng_freq_doc)
  setTxtProgressBar(pb_l, l)
}
stopCluster(cl)
rm(fun_freq,freq_num,freq_sum,doc_num,doc_sum,ng_freq_doc,pb_l)


ng_count[,2:3] <- ng_count[,2:3] %>% unlist() %>% as.integer()
colnames(ng_count) <- c("ngram","freq","doc")

#TF-IDF Calculation
ng_count_filter <- ng_count %>% filter(freq > freq_standard & doc > doc_standard) %>% arrange(desc(doc)) %>% remove.factors()
ng_count_filter$filter <- 0


t_dimension<-as.character(t(ng_table_dimension[ ,1]))
colnames(dimension_data_freq)<-c("NO.",t_dimension)
colnames(dimension_data_doc)<-c("NO.",t_dimension)

filter_token <- ng_count_filter[,1] %>% t() %>% as.list
sav <- colnames(dimension_data_freq) %in% filter_token
dimension_data_freq <- cbind(dimension_data_freq[,1],dimension_data_freq[,sav])
dimension_data_doc <- cbind(dimension_data_doc[,1],dimension_data_doc[,sav])

#Export
cat("\nResult output:\n")
setwd(output_location)
export(ng_count_filter,"words_stat.xlsx")
write.csv(dimension_data_freq,file="dimension_freq.csv",row.names = FALSE)
write.csv(dimension_data_doc,file="dimension_doc.csv",row.names = FALSE)

time2 <- Sys.time()
cat("Done!\n")
print(time2-time1)
cat("\nTimJ\n")
