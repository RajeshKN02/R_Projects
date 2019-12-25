#library checker 
libs = c("dplyr", "ggplot2","cluster", "Rtsne", "readxl", "tidyverse", "dendextend", "factoextra")

for (i in libs){
  if( !is.element(i, .packages(all.available = TRUE)) ) {
    install.packages(i)
  }
  library(i,character.only = TRUE)
}
lapply(libs, require, character.only = TRUE)

#Set working directory 
setwd("/Users/Tim/Desktop/Consumer Insights & Analytics")
getwd()
#read file
Chipo <- read_xlsx(path="Chipotle.xlsx", sheet = 'Sheet1') %>% as.data.frame() 
#Chipo <- select(Chipo, importanthealthy, female, age, income, plan, spending, buylocal, healthyimportanttome)
Chipo <- na.omit(Chipo)
glimpse(Chipo)

df <- scale(Chipo[-1]) %>% as.data.frame()
df <- df[-18] 
glimpse(df)


#glimpse(Chipo)
gower_dist <- daisy(df[,c(15,23)],
                    metric = "gower",)


# Check attributes to ensure the correct methods are being used
# (I = interval, N = nominal)
# Note that despite logratio being called, 
# the type remains coded as "I"

#summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

# Output most similar pair
#Chipo[
#  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
#        arr.ind = TRUE)[1, ], ]

# Output most dissimilar pair
#Chipo[
#  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
#        arr.ind = TRUE)[1, ], ]
# Calculate silhouette width for many k using PAM

#sil_width <-  c(NA)

#for(i in 2:10){
  
#  pam_fit <- pam(gower_dist,
#                 diss = TRUE,
#                 k = i)
#  
#  sil_width[i] <- pam_fit$silinfo$avg.width
  
#}


#kmeans
#set.seed(123)
#kmeans_fit<-kmeans(gower_dist, centers=5)
#table(kmeans_fit$cluster)

# Plot sihouette width (higher is better)

#plot(1:10, sil_width,
#     xlab = "Number of clusters",
#     ylab = "Silhouette Width")
#lines(1:10, sil_width)

# Another way
library(factoextra)
fviz_nbclust(gower_mat, 
             FUNcluster = pam,
             method = "silhouette",     
             k.max = 20)+labs(title="Best Number of clusters")
#Create Model
pam_fit <- pam(gower_dist, diss = TRUE, k = 3)
#Descriptive Statistics
pam_results <- Chipo %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary
#Another advantage of the PAM algorithm is that the center point of each cluster is the actual sample point
Chipo[pam_fit$medoids, ]

#One way to visualize many variables in a lower dimensional space is with t-SNE.
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE, perplexity=30, max_iter = 1000, pca = FALSE)
#tsne <- Rtsne(gower_dist, dims = 2, perplexity=5, verbose=TRUE, max_iter = 10, pca = FALSE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering)
  )

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


##Check the outliers
#tsne_data %>%
#filter(X > -20 & X < -10,
#       Y > -15 & Y < -10) %>%
#  left_join(Chipo, by = "name") %>%
#  collect %>%
#  .[["name"]]

Chipo$cluster <- pam_fit$clustering
Mean.Target <- aggregate(Chipo, list(Chipo$cluster), mean)
write.csv(Mean.Target,"Mean.Target.csv")


