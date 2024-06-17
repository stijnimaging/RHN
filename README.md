# Regions for Health - WHO

Regions for Health collaboration methods
# Step 0 - Load data

# Step 1 - Scale variables
## Make numeric and standardize variables Mean is 0 with standard deviation 1.

# Step 2 - K-means clustering 
Clustering itself; K-means clustering (non-hierarchical algorithm)
Based on FASTCLUS in SAS - Four iterations using the standardized data

# Step 2a - Identify outliers 
Number of clusters depends on number of regions (Canada started with 130, now 105). 
Number of regions divided by seven (will give around 15 regions per cluster). 
Region which appears in only one cluster is defined as an outlier.
RMS should be as low as possible. Radius is the distance between regions and should be as low as possible.

# Step 2b Reduce effects of outliers
# Step 2c Final run
# Step 2d Assigning outliers to their nearest cluster

# Step 3 - Visualize the clusters

Analysis 1 - Strongest predictors
