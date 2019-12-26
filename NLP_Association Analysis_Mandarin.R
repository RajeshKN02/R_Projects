#library checker 
if(!require(methods)){install.packages("methods")}

libs <- c("rio","readxl","dplyr","taRifx","tidytext")

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

cat("===== Association_Analysis =====\r\n")

#Read files
location <- file.choose( )
location <- sub("clickme",replacement = "",location)
input_location <- paste0(location,"1_Input_Data")
output_location <- paste0(location,"2_Outcome")
setting_location <- paste0(location,"3_Setting")

#PMI applied
setwd(input_location)
words_stat <- read_excel("words_stat.xlsx")
raw_data <- import("dimension_doc.csv") #raw_data = doc x term matrix

words_stat_select <- words_stat %>% filter(filter == 1) #filter docs
sav <- colnames(raw_data) %in% (words_stat_select$ngram)
raw_data <- raw_data[,sav]

#Conduct the calculation
occurrence_data <- as.character()
for ( i in 1:(ncol(raw_data)-1))
{
  #Data matching
  raw_data1 <- raw_data[,i] %>% as.data.frame() %>% remove.factors()
  colnames(raw_data1) <- colnames(raw_data[i])
  
  if(i == ncol(raw_data)-1)
  {
    raw_data2 <- raw_data[,-1:-i] %>% as.data.frame() %>% remove.factors()
    colnames(raw_data2) <- colnames(raw_data[ncol(raw_data)])
  }
  else{raw_data2 <- raw_data[,-1:-i]}
  
  for ( j in 1:ncol(raw_data2))
  {
    
    occurrence_beg <- raw_data2[,j] %>% as.data.frame() %>% remove.factors()
    colnames(occurrence_beg) <- colnames(raw_data2[j])
    
    occurrence <- cbind(raw_data1,occurrence_beg)
    var1 <- sum(raw_data1) 
    var2 <- sum(occurrence_beg) 
    #Create the col
    occurrence_name <- cbind(colnames(raw_data1[1]),colnames(occurrence_beg[1])) %>% 
      as.data.frame() %>% remove.factors()
    #Calculate the co-occurrence
    occurrence_sum <- rowSums(occurrence) %>% as.data.frame() %>% remove.factors()
    occurrence_count <- occurrence_sum[occurrence_sum[1]>1,]
    
    occurrence_temp <- cbind(occurrence_name,length(occurrence_count),var1,var2)
    occurrence_data <- rbind(occurrence_data,occurrence_temp)
  }
}
colnames(occurrence_data) <- c("Variable_1","Variable_2","Co-occurrence","variable_1 (Doc)","variable_2 (Doc)")

#PMI（Pointwise Mutual Information）
occurrence_data$PMI <- log2((occurrence_data[,3]*(sum(occurrence_data[,3])))/(occurrence_data[,4]*occurrence_data[,5]))
occurrence_data$PMI <- round(occurrence_data$PMI,2)
occurrence_data_PPMI <- occurrence_data %>% filter( PMI > 0) %>% arrange(desc(PMI))

setwd(output_location)
export(occurrence_data_PPMI,"Co-occurrence.xlsx")

#TF-IDF 
setwd(input_location)
words_stat <- read_excel("words_stat.xlsx")
raw_data <- import("dimension_freq.csv") #raw_data = freq x term matrix
col_num <- raw_data[,1] %>% as.data.frame()
colnames(col_num) <- "No."

words_stat_select <- words_stat %>% filter(filter == 1) 
sav <- colnames(raw_data) %in% (words_stat_select$ngram)
raw_data <- raw_data[,sav]

#IDF 
tfidf_list <- as.list(1)
idf_list <- log2(nrow(raw_data) / words_stat_select$doc ) %>% as.list()
tf_list <- lapply(seq_len(ncol(raw_data)),function(x){raw_data[,x]})

for (i in 1:length(idf_list))
{
  tfidf_list[i] <- lapply(tf_list[i],function(x){x*idf_list[[i]]})
}

tfidf <- data.frame(matrix(unlist(tfidf_list), nrow=nrow(raw_data), byrow=F),stringsAsFactors=FALSE)
tfidf <- round(tfidf,2)
colnames(tfidf) <- colnames(raw_data)

tfidf <- cbind(col_num,tfidf)

setwd(output_location)
export(tfidf,"TFIDF.xlsx")
write.table(tfidf, file = "TFIDF.csv", sep = ",",row.names = FALSE) 
cat("\nTimJ\n")
