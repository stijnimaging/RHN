# Health Regions Peer Groups - Overview of the methodology
# Stijn Michielse - GGD Zuid-Limburg The Netherlands
#
#install.packages("klaR","openxlsx","tidyverse","factoextra","cluster","labelled","ggplot2","corrr","ggcorplot","factominer","ggarrange")
#
# Load required libraries - ignore warnings
library(openxlsx) 
library(tidyverse) 
library(factoextra)
library(cluster)
library(labelled)
library(ggplot2)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(klaR)
library(ggrepel)
library(flextable)
rm(list = ls())
# Set you LOCAL working directory - the folder where the Excel workbook 20231130_RHN Peer_Regions_indicators.xlsx is located
setwd('//GZLFS001/home$/stijn.michielse/Documents/RHN_WHO')

# Step 0 - Load data ----
NL_data <- openxlsx::read.xlsx("20231130_RHN Peer_Regions_indicators.xlsx",'Netherlands')
# Drop ABOPER and MEDSHR
NL_data <- NL_data[ , -which(names(NL_data) %in% c("ABOPER"))]
NL_data <- NL_data[ , -which(names(NL_data) %in% c("MEDSHR"))]

# Rename variable Region
NL_data <- NL_data %>% 
  rename(Region = Original.indicators.from.Canada)
NL_data <- NL_data[-1,]
NL_data <- NL_data %>% remove_rownames %>% column_to_rownames(var="Region")

# Step 1 - Scale variables ----
# Make numeric and standardize variables Mean is 0 with standard deviation 1.
NL_data[1:25] <- as.numeric(unlist(NL_data[1:25]))
NL_data[1:25] <- scale(NL_data[1:25])
NL_data <- NL_data %>% 
  dplyr::select(-c(NGROWTH,GREEN,GREY))

# Step 2 - K-means clustering ----
# Clustering itself; K-means clustering (non-hierarchical algorithm)
# Based on FASTCLUS in SAS - Four iterations using the standardized data

# Step 2a - Identify outliers ----
# Number of clusters depends on number of regions (Canada started with 130, now 105). 
# Number of regions divided by seven (will give around 15 regions per cluster). 
cluster_K <- ceiling(nrow(NL_data)/7)

png(file="Plot_clustersSOS.png",
    width=800, height=800)
fviz_nbclust(NL_data, kmeans, method = "wss")
dev.off()
# Based on SoS use five clusters
cluster_K <- 5
# Region which appears in only one cluster is defined as an outlier.
# RMS should be as low as possible. Radius is the distance between regions and should be as low as possible.
kmeans_result <- kmeans(NL_data, centers = cluster_K, iter.max = 1, algorithm = c("Hartigan-Wong"))

# No unique outliers were identified.

# SAS PROC FASTCLUS was directly inspired by the Hartigan (1975) leader algorithm and the MacQueen (1967) 
# k-means algorithm. PROC FASTCLUS uses a method that Anderberg (1973) calls nearest centroid sorting.
# A set of points called cluster seeds is selected as a first guess of the means of the clusters. 
# Each observation is assigned to the nearest seed to form temporary clusters. 
# The seeds are then replaced by the means of the temporary clusters, and the process is repeated until no further changes occur in the clusters. 
# 2b Reduce effects of outliers
# Perform this several times and compare - select smallest sum of squares (total sum of squares).
kmeans_result <- kmeans(NL_data, centers = cluster_K, iter.max = 100000 , algorithm = c("Hartigan-Wong"))
# See for the best cluster solution 10x
list_k <- list()                                          # an empty list
for (i in 1:10){                                          # fill with the kmeans solutions
  list_k[[i]] <- kmeans(NL_data, centers = cluster_K, iter.max = 10000 , algorithm = c("Hartigan-Wong"))
}
cluster_results <- data.frame(list_k[[1]]$cluster)
cluster_results <- cluster_results %>% 
  rename(solution_1 = list_k..1...cluster)
for (i in 1:10){
  cluster_i <- list_k[[i]]$cluster
  rownames(cluster_i)<-NULL
  cluster_results[[i]] <- as.numeric(unlist((cluster_i)))
}

names(cluster_results)[1:ncol(cluster_results)] <- paste0("solution", "_",1:10)
cluster_results = data.frame(row.names(NL_data), cluster_results)
names(cluster_results)[1] <- "Region"
openxlsx::write.xlsx(cluster_results, "Cluster_solutions10x.xlsx")

# Add cluster assignments to the original dataset
NL_data$cluster <- kmeans_result$cluster

# 2c Final run
# 2d Assigning outliers to their nearest cluster

# Step 3 - Visualize the clusters ----
png(file="Plot_clusters1.png",
    width=800, height=800)
fviz_cluster(kmeans_result, data = NL_data, labelsize = 8, repel=T)
dev.off()

pca_x = princomp(NL_data)
x_cluster = data.frame(pca_x$scores,kmeans_result$cluster, row.names(NL_data))
# Sort by first component
pca_regions <- x_cluster[order(x_cluster$Comp.1, decreasing = T),]
openxlsx::write.xlsx(pca_regions, "Loadings_PCA_regions.xlsx")


# Use ellipse to cluster the regions
ggplot(x_cluster, aes(x = Comp.1, y = Comp.2, color = as.factor(kmeans_result.cluster), 
                      fill = as.factor(kmeans_result.cluster)),label = rownames(x_cluster)) + 
  geom_point() + 
  stat_ellipse(type = "t",geom = "polygon",alpha = 0.4) +
  geom_text_repel(label = rownames(x_cluster)) +
  theme_minimal() 


# Analysis 1 - Strongest predictors ----
aggregate(NL_data[,1:22], by=list(cluster=kmeans_result$cluster), mean)
NL_data[,1:22] %>%
  mutate(Cluster = kmeans_result$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

# Perform step-wise discriminant analysis
step_clas <- stepclass(cluster ~., NL_data, method = "lda", criterion = "AS")
plot(step_clas)
model_lda <- lda(cluster ~.,NL_data)
model_lda_means <- data.frame(t(model_lda$means))
model_lda_means$Indicator <- rownames(model_lda_means)
model_lda_means <- model_lda_means[order(model_lda_means$X1, decreasing = T),]
model_lda_means <- model_lda_means %>%
  relocate(Indicator)

save_as_image(autofit(flextable(model_lda_means)), "Plot_LDA1.png", expand = 10, res = 200)


# Step 4 PCA ----
## Step 4A Check correlations ----
corr_matrix <- cor(print(NL_data[,1:22], row.names = FALSE))
ggcorrplot(corr_matrix)
png(file="Plot_correlation_matrix1.png",
    width=800, height=800)
ggcorrplot(corr_matrix)
dev.off()

# Change code and use prcomp
data_pca2 <- prcomp(corr_matrix)

data_pca <- princomp(corr_matrix)
summary(data_pca)

fviz_nbclust(NL_data, kmeans, method = "wss")

## According to the scree plot there are five components
loading_pca <- data.frame(data_pca$loadings[, 1:5])
loading_pca$Indicator <- rownames(loading_pca)
# Sort by first component
data_pca <- loading_pca[order(loading_pca$Comp.1, decreasing = T),]
openxlsx::write.xlsx(loading_pca, "Loadings_PCA1.xlsx")

# Make PCA results visible
data_pca_viz <- princomp(corr_matrix)
fviz_eig(data_pca_viz, addlabels = TRUE)
png(file="Plot_scree1.png",
    width=800, height=800)
fviz_eig(data_pca_viz, addlabels = TRUE)
dev.off()

fviz_pca_var(data_pca_viz, col.var = "black")
fviz_cos2(data_pca_viz, choice = "var", axes = 1:2)
fviz_pca_var(data_pca_viz, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)



