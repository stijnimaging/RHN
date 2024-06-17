# Health Regions Peer Groups - Overview of the methodology
# Stijn Michielse - GGD Zuid-Limburg The Netherlands
#
# install.packages("klaR","openxlsx","tidyverse","factoextra","cluster","labelled","ggplot2","corrr","ggcorplot","factominer","ggarrange")
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

# Set you LOCAL working directory - the folder where the Excel workbook 20231130_RHN Peer_Regions_indicators.xlsx is located
setwd('//Set/Your/Own/Working/Directory/RHN_WHO')

# Step 0 - Load data ----
NL_data <- read.xlsx("20231130_RHN Peer_Regions_indicators.xlsx",'Netherlands')
# Drop ABOPER
NL_data <- NL_data[ , -which(names(NL_data) %in% c("ABOPER"))]
# Rename variable Region
NL_data <- NL_data %>% 
  rename(Region = Original.indicators.from.Canada)
NL_data <- NL_data[-1,]
NL_data <- NL_data %>% remove_rownames %>% column_to_rownames(var="Region")

# Step 1 - Scale variables ----
# Make numeric and standardize variables Mean is 0 with standard deviation 1.
NL_data[1:26] <- as.numeric(unlist(NL_data[1:26]))
NL_data[1:26] <- scale(NL_data[1:26])
NL_data <- NL_data %>% 
  dplyr::select(-c(GINI,NGROWTH,GREEN,GREY))

# Step 2 - K-means clustering ----
# Clustering itself; K-means clustering (non-hierarchical algorithm)
# Based on FASTCLUS in SAS - Four iterations using the standardized data

# Step 2a - Identify outliers ----
# Number of clusters depends on number of regions (Canada started with 130, now 105). 
# Number of regions divided by seven (will give around 15 regions per cluster). 
cluster_K <- ceiling(length(NL_data)/7)
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
kmeans_result <- kmeans(NL_data, centers = cluster_K, iter.max = 10000 , algorithm = c("Hartigan-Wong"))
NL_data$cluster <- kmeans_result$cluster
fviz_nbclust(NL_data, kmeans, method = "wss")

# Add cluster assignments to the original dataset
NL_data$cluster <- kmeans_result$cluster

# 2c Final run
# 2d Assigning outliers to their nearest cluster

# Step 3 - Visualize the clusters ----
png(file="Plot_clusters.png",
    width=800, height=800)
fviz_cluster(kmeans_result, data = NL_data, labelsize = 8, repel=T)
dev.off()

pca_x = princomp(NL_data)
x_cluster = data.frame(pca_x$scores,kmeans_result$cluster, row.names(NL_data))

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

save_as_image(autofit(flextable(model_lda_means)), "Plot_LDA.png", expand = 10, res = 200)


# Step 4 PCA ----
## Step 4A Check correlations ----
corr_matrix <- cor(print(NL_data[,1:22], row.names = FALSE))
ggcorrplot(corr_matrix)
png(file="Plot_correlation_matrix.png",
    width=800, height=800)
ggcorrplot(corr_matrix)
dev.off()

data_pca <- princomp(corr_matrix)
summary(data_pca)
data_pca$loadings[, 1:2]

fviz_eig(data_pca, addlabels = TRUE)
png(file="Plot_scree.png",
    width=800, height=800)
fviz_eig(data_pca, addlabels = TRUE)
dev.off()

fviz_pca_var(data_pca, col.var = "black")
fviz_cos2(data_pca, choice = "var", axes = 1:2)
fviz_pca_var(data_pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)



