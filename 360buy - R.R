# ================================================================
# BEM2041 Marketing Analytics – Coursework 2
# 360buy Customer Segmentation: INSEAD 9-Step Process
# Candidate: 740047551

# install.packages(c("readxl","cluster","factoextra","ggplot2","dplyr","fmsb"))
library(readxl)
library(cluster)
library(factoextra)
library(ggplot2)
library(dplyr)

# ----------------------------------------------------------------
# USER INPUTS – change these to experiment
# ----------------------------------------------------------------
datafile_name         <- "360buy_SurveyData_1.xlsx"
segmentation_attributes_used <- c(5, 6, 7, 8, 10)   # CusChoice, ConstUp, ReplacReminder, ProInsuCov, ProdReturn
profiling_attributes_used    <- c(1, 2, 3, 4, 9)    # CusAgeYr, CusGen, LevEdn, LevIncome, CusAcct
numb_clusters_used    <- 3       # chosen after Step 6
distance_used         <- "euclidean"
hclust_method         <- "ward.D"
kmeans_method         <- "Lloyd"
# ----------------------------------------------------------------

ProjectData <- ProjectData <- data.matrix(as.data.frame(read_excel( "360buy_SurveyData.xlsx")))
cat("Data loaded:", nrow(ProjectData), "observations,", ncol(ProjectData), "variables\n")
cat("Column names:", colnames(ProjectData), "\n\n")


# STEP 1: Check data is metric

cat("Step 1 – Summary statistics (check data is metric/numeric):\n")
print(round(apply(ProjectData, 2, function(x) c(mean=mean(x), sd=sd(x), min=min(x), max=max(x))), 3))


# STEP 2: Scale the data (z-score standardisation)

ProjectData_scaled <- apply(ProjectData, 2, function(r) {
  if (sd(r) != 0) (r - mean(r)) / sd(r) else 0 * r
})


# STEP 3: Select segmentation attributes vs profiling attributes

ProjectData_segment  <- ProjectData_scaled[, segmentation_attributes_used]
ProjectData_profile  <- ProjectData[,         profiling_attributes_used]   # raw values for profiling

seg_names     <- colnames(ProjectData)[segmentation_attributes_used]
profile_names <- colnames(ProjectData)[profiling_attributes_used]

cat("\nSegmentation attributes:", seg_names, "\n")
cat("Profiling attributes:   ", profile_names, "\n\n")


# STEP 4: Define distance metric

Pairwise_Distances <- dist(ProjectData_segment, method = distance_used)


# STEP 5: Visualise the data


# 5a: Histogram of each segmentation variable (raw)
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
for (v in seg_names) {
  hist(ProjectData[, v],
       main  = paste("Distribution of", v),
       xlab  = v,
       col   = "#c7ddf5",
       border = "white",
       breaks = 7)
}
par(mfrow = c(1, 1))

# 5b: Histogram of all pairwise Euclidean distances
hist(Pairwise_Distances,
     main   = "Histogram of Pairwise Euclidean Distances\n(Segmentation Variables)",
     xlab   = "Euclidean Distance",
     col    = "#aed6f1",
     border = "white",
     breaks = 30)


# STEP 6: Hierarchical clustering → dendrogram + heights plot

Hierarchical_Cluster_distances <- dist(ProjectData_segment, method = distance_used)
Hierarchical_Cluster           <- hclust(Hierarchical_Cluster_distances, method = hclust_method)

# 6a: Dendrogram
plot(Hierarchical_Cluster,
     main   = paste("Dendrogram (distance:", distance_used, "| method:", hclust_method, ")"),
     xlab   = "Observations",
     ylab   = "Distance",
     labels = FALSE,
     hang   = -1)

# 6b: Heights plot ("distances traveled before merging clusters")
# Shows the n-1 merge heights — look for a big jump to decide k
num_heights_to_show <- 20
plot(sort(Hierarchical_Cluster$height, decreasing = TRUE)[1:num_heights_to_show],
     type   = "b",
     col    = "#2471a3",
     pch    = 19,
     main   = paste("Hierarchical Clustering: Top", num_heights_to_show, "Merge Heights"),
     xlab   = "Merge step (from top of tree)",
     ylab   = "Distance (height)")
abline(v = numb_clusters_used, col = "red", lty = 2)
legend("topright",
       legend = paste("Cut at k =", numb_clusters_used),
       col    = "red", lty = 2, bty = "n")


# STEP 7: Decide number of clusters → cut tree

cluster_memberships_hclust <- cutree(Hierarchical_Cluster, k = numb_clusters_used)

cat("\n--- Hierarchical Clustering: Cluster sizes (k =", numb_clusters_used, ") ---\n")
print(table(cluster_memberships_hclust))

# Show first 10 observations' cluster assignments
ProjectData_with_hclust_membership <- cbind(
  ObsNumber        = 1:nrow(ProjectData),
  Cluster_hclust   = cluster_memberships_hclust
)
cat("\nFirst 10 observations (hclust membership):\n")
print(head(ProjectData_with_hclust_membership, 10))


# STEP 8: K-means with chosen k

set.seed(42)
kmeans_clusters <- kmeans(ProjectData_segment,
                           centers   = numb_clusters_used,
                           iter.max  = 2000,
                           algorithm = kmeans_method)

cluster_memberships_kmeans <- kmeans_clusters$cluster

cat("\n--- K-means: Cluster sizes (k =", numb_clusters_used, ") ---\n")
print(table(cluster_memberships_kmeans))

ProjectData_with_kmeans_membership <- cbind(
  ObsNumber      = 1:nrow(ProjectData),
  Cluster_kmeans = cluster_memberships_kmeans
)
cat("\nFirst 10 observations (kmeans membership):\n")
print(head(ProjectData_with_kmeans_membership, 10))


# STEP 9a: Profile the segments


# Use hclust membership for profiling (as per INSEAD guide)
ProjectData_all <- cbind(ProjectData, Cluster = cluster_memberships_hclust)

# Mean of segmentation attributes by cluster
cat("\n=== Segment Profiles: Mean Segmentation Attributes (hclust) ===\n")
seg_profile <- aggregate(ProjectData[, segmentation_attributes_used],
                          by  = list(Cluster = cluster_memberships_hclust),
                          FUN = mean)
print(round(seg_profile, 3))

# Mean of profiling attributes by cluster
cat("\n=== Segment Profiles: Mean Profiling Attributes (hclust) ===\n")
prof_profile <- aggregate(ProjectData[, profiling_attributes_used],
                           by  = list(Cluster = cluster_memberships_hclust),
                           FUN = mean)
print(round(prof_profile, 3))

# Median of segmentation attributes by cluster
cat("\n=== Segment Profiles: Median Segmentation Attributes (hclust) ===\n")
seg_median <- aggregate(ProjectData[, segmentation_attributes_used],
                         by  = list(Cluster = cluster_memberships_hclust),
                         FUN = median)
print(round(seg_median, 2))


# STEP 9b: Snake plot (standardised segmentation attributes by cluster)

# Calculate mean of STANDARDISED segmentation variables per cluster
ProjectData_scaled_seg <- ProjectData_scaled[, segmentation_attributes_used]
cluster_means_scaled   <- aggregate(ProjectData_scaled_seg,
                                     by  = list(Cluster = cluster_memberships_hclust),
                                     FUN = mean)

# Reshape for ggplot
snake_data <- reshape(cluster_means_scaled,
                       idvar     = "Cluster",
                       varying   = seg_names,
                       v.names   = "Mean_Scaled",
                       timevar   = "Variable",
                       times     = seg_names,
                       direction = "long")
snake_data$Cluster  <- factor(snake_data$Cluster,
                               labels = paste("Cluster", 1:numb_clusters_used))
snake_data$Variable <- factor(snake_data$Variable, levels = seg_names)

ggplot(snake_data, aes(x = Variable, y = Mean_Scaled,
                        group = Cluster, colour = Cluster)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = c("#2196F3", "#FF5722", "#4CAF50")) +
  labs(title    = "Snake Plot: Standardised Mean Segmentation Attributes by Cluster",
       subtitle = paste("Hierarchical Clustering (k =", numb_clusters_used, "| method:", hclust_method, ")"),
       x        = "Segmentation Variable",
       y        = "Standardised Mean (z-score)",
       colour   = "Segment") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        plot.title  = element_text(face = "bold"))


# STEP 9c: Robustness check — compare hclust vs kmeans

# % of observations in the SAME cluster across both methods
overlap <- sum(cluster_memberships_hclust == cluster_memberships_kmeans) / nrow(ProjectData)
cat("\n=== Robustness Check ===\n")
cat("% observations in same cluster (hclust vs kmeans):", round(overlap * 100, 1), "%\n")

# Cross-tabulation
cat("\nCross-tabulation hclust (rows) vs kmeans (cols):\n")
print(table(hclust = cluster_memberships_hclust,
            kmeans = cluster_memberships_kmeans))

# Profile comparison: how similar are the segment means?
kmeans_seg_profile <- aggregate(ProjectData[, segmentation_attributes_used],
                                 by  = list(Cluster = cluster_memberships_kmeans),
                                 FUN = mean)
cat("\n=== K-means Segment Means (for comparison) ===\n")
print(round(kmeans_seg_profile, 3))

cat("\n=== DONE ===\n")
cat("Cluster sizes (hclust):\n"); print(table(cluster_memberships_hclust))
cat("Cluster sizes (kmeans):\n"); print(table(cluster_memberships_kmeans))
