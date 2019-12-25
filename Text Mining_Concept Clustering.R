###Concept Clustering###
#library checker 
libs = c("tidyverse", "data.table","dplyr", "Rtsne", "readxl", "ggplot2", "factoextra")

for (i in libs){
  if( !is.element(i, .packages(all.available = TRUE)) ) {
    install.packages(i)
  }
  library(i,character.only = TRUE)
}
lapply(libs, require, character.only = TRUE)

getwd()
{
setwd("")
Key <- read_xlsx(path="Concept Clustering.xlsx", sheet = 'name&key') %>% as.data.frame() 
data <- read_xlsx(path="Concept Clustering.xlsx", sheet = 'data') 
data <- data[,-1]  %>% as.data.frame() #data need to be tagged(Document)
Key <- Key$key %>% as.data.frame() #Key words
}
#Create dimention-Document Term Matrix
colbase = NULL
for (i in 1:nrow(Key))
{
  x <- ifelse(grepl(Key[i,],data[,1])==TRUE,1,0)
  colbase <- cbind(colbase,x)
}

colbase <- colbase %>% as.data.frame
name2 <- as.character(Key$name)
colnames(colbase) <- c(name2)

#final <- cbind(data,colbase)

#DTM format confirmed
{
colbase$sum <- apply(colbase,1,sum)
colbase <- colbase %>% filter(!sum==0)
colbase <- colbase %>% select(-sum)
newdata <- colbase %>% t() %>% as.data.frame()
}
#tSNE-dimention reduction

newdata <- newdata %>% as.matrix()

tsne <- Rtsne(newdata, dims = 2, perplexity=50, verbose=TRUE, max_iter = 1200, pca = FALSE)

T <- tsne$Y %>% as.data.frame()

rownames(T)=row.names(newdata)
#sp = ggplot(T, aes(T[,1], T[,2],label = rownames(T)))
#sp + geom_point()
#sp + geom_text()
#Check the optimized number of clusters
fviz_nbclust(T, 
             FUNcluster = kmeans,
             method = "silhouette",     
             k.max = 15)+labs(title="最佳分群數")
#+geom_vline(xintercept = 7,linetype = 2) 


Cluster=kmeans(T,7)
sp = ggplot(T, aes(T[,1], T[,2],label = rownames(T),color =Cluster$cluster))
sp +geom_point() +scale_colour_gradientn(colours=rainbow(5))
sp +geom_text(size=4.5) +scale_colour_gradientn(colours=rainbow(3))

#Interpretation
Cluster=kmeans(T,3)

w=Cluster$cluster%>% as.data.frame()

table(w$.)


#PCA for dimention reduction
#pca <- prcomp(newdata, scale=TRUE)
#plot(pca)

#rd <- pca$x %>% as.data.frame() %>% select(PC1,PC2,PC3,PC4,PC5,PC6)

#library(factoextra)
#fviz_nbclust(rd, 
#             FUNcluster = kmeans,
#             method = "wss",     
#             k.max = 12)+labs(title="最佳分群數")+geom_vline(xintercept = 7,linetype = 2) 

#Cluster<-kmeans(rd,8)
#w<-Cluster$cluster%>% as.data.frame()
#table(w$.)

#sp <- ggplot(rd, aes(rd[,1], rd[,2],label = rownames(rd)))
#sp + geom_point()
#sp + geom_text()
