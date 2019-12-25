#library checker 
libs = c("dplyr", "dendextend","cluster", "readxl", "tidyverse", "dendextend", "factoextra")

for (i in libs){
  if( !is.element(i, .packages(all.available = TRUE)) ) {
    install.packages(i)
  }
  library(i,character.only = TRUE)
}
lapply(libs, require, character.only = TRUE)


df <- Chipo
df <- na.omit(df)
head(df)
df.scaled <- scale(df[-1])
head(df.scaled)

# Dissimilarity matrix
d <- dist(df.scaled, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

# Compute with agnes
hc2 <- agnes(df.scaled, method = "complete")

# Agglomerative coefficient
hc2$ac

# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(df.scaled, method = x)$ac
}

map_dbl(m, ac)

hc3 <- agnes(df.scaled, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

# compute divisive hierarchical clustering
hc4 <- diana(df.scaled)

# Divise coefficient; amount of clustering structure found
hc4$dc

# plot dendrogram
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")

# Ward's method
hc5 <- hclust(d, method = "ward.D2" )

# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 4)

# Number of members in each cluster
table(sub_grp)

Chipo %>%
  mutate(cluster = sub_grp) %>%
  head

plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 4, border = 2:5)


fviz_cluster(list(data = df.scaled, cluster = sub_grp))

# Cut agnes() tree into 4 groups
hc_a <- agnes(df.scaled, method = "ward")
cutree(as.hclust(hc_a), k = 4)

# Cut diana() tree into 4 groups
hc_d <- diana(df.scaled)
cutree(as.hclust(hc_d), k = 4)

# Compute distance matrix
res.dist <- dist(df.scaled, method = "euclidean")

# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "complete")
hc2 <- hclust(res.dist, method = "ward.D2")

# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

tanglegram(dend1, dend2)

dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)
#Determining Optimal Clusters
fviz_nbclust(df.scaled, FUN = hcut, method = "wss")
fviz_nbclust(df.scaled, FUN = hcut, method = "silhouette")
gap_stat <- clusGap(df.scaled, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
