################################################################################
# PROJECT: Nonparametric framework for the definition, adaptive detection and 
#          probabilistic interpretation of outliers
# AUTHORS: 
# LOCATION: MAIN, Figure 3
# DESCRIPTION: ODK-means at alpha=0.05 (top-left); Trimmed K-means at p=0.018 
#              (top-right); LOF=1.7 + K-means (bottom-left); LOF=1.38 + K-means 
#              (bottom-right). Crossed units represent detected outliers (Note: 
#              Trimmed K-means places outliers in an additional cluster).
# NOTE: The rendered image in the manuscript is manually annotated with circles 
#       around the three key observations discussed in Subsection 7.2.
################################################################################

# ==============================================================================
# IMPORT ALL THE NEEDED PACKAGES
# ==============================================================================
library(ggplot2)
library(gridExtra)
library(trimcluster)
library(Rlof)
library(tclust)
library(FNN)

# ==============================================================================
# IMPORT ODKmeans
# ==============================================================================
source("ODKm_pub.R")

# ==============================================================================
# INITIAL SETUP
# ==============================================================================
data(faithful)
X <- scale(as.matrix(faithful))
set.seed(123)

color_cluster1 <- "#440154" 
color_cluster2 <- "#21908C" 
color_outlier  <- "red"

panel_theme <- theme_minimal() + 
  theme(plot.margin = margin(5, 5, 5, 5), legend.position = "right")

# Ensures "Cluster" is always the top legend
legend_order <- guides(
  color = guide_legend(order = 1),
  shape = guide_legend(order = 2)
)

set.seed(42)

# ==============================================================================
# 1. ODKM (STANDARD - ALPHA = 0.05) [POSITION: TOP-LEFT]
# ==============================================================================
res_alpha05 <- odkm(data = X, K = 2, restart = 20, alpha = 0.05)
cluster_assignments <- apply(res_alpha05$U, 1, which.max)
df_odkm <- as.data.frame(X)
df_odkm$Cluster <- as.factor(cluster_assignments)
df_odkm$ID <- 1:nrow(X)

p_odkm <- ggplot(df_odkm, aes(x = eruptions, y = waiting)) +
  geom_point(aes(color = Cluster), size = 2, alpha = 0.6) +
  geom_point(data = df_odkm[1:nrow(X) %in% res_alpha05$out, ], 
             aes(shape = "Outlier"), color = color_outlier, size = 4, stroke = 1.5) +
  geom_text(data = df_odkm[1:nrow(X) %in% res_alpha05$out, ], 
            aes(label = ID), vjust = 2, color = color_outlier, size = 3, fontface = "bold") +
  coord_fixed(ratio = 1) +
  scale_color_manual(values = c("1" = color_cluster1, "2" = color_cluster2)) +
  scale_shape_manual(values = c("Outlier" = 4)) +
  labs(title = expression(paste("ODK-means (", alpha, " = 0.05)")), 
       color = "Cluster", shape = "Reference") + panel_theme + legend_order

# ==============================================================================
# 2. TKM (P = 0.018) [POSITION: TOP-RIGHT]
# ==============================================================================
p_val <- 0.018
t_km <- trimkmeans(X, k = 2, trim = p_val)
df_tkm <- as.data.frame(X)
df_tkm$Cluster <- as.factor(t_km$classification)
df_tkm$ID <- 1:nrow(X)

p_tkm <- ggplot(df_tkm, aes(x = eruptions, y = waiting)) +
  geom_point(aes(color = Cluster), size = 2, alpha = 0.5) +
  geom_point(data = subset(df_tkm, Cluster == 3), aes(shape = "Outlier"), color = color_outlier, size = 4, stroke = 1.5) +
  geom_text(data = subset(df_tkm, Cluster == 3), aes(label = ID), vjust = 2, color = color_outlier, size = 2.5, fontface = "bold") +
  coord_fixed(ratio = 1) +
  scale_color_manual(values = c("1" = color_cluster1, "2" = color_cluster2, "3" = "grey80")) +
  scale_shape_manual(values = c("Outlier" = 4)) +
  labs(title = paste0("Trimmed K-means (p = ", p_val, ")"), color = "Cluster", shape = "Reference") + panel_theme + legend_order

# ==============================================================================
# 3. LOF CALCULATIONS & PLOTS [POSITIONS: BOTTOM-LEFT & BOTTOM-RIGHT]
# ==============================================================================
scores <- lof(X, k = 5)
sorted_scores <- sort(scores, decreasing = TRUE)
lof_top5_val <- sorted_scores[5]

km_std <- kmeans(X, centers = 2, nstart = 20)
df_lof <- as.data.frame(X)
df_lof$Cluster <- as.factor(km_std$cluster)

# LOF Top 5 [POSITION: BOTTOM-LEFT]
p_lof_top5 <- ggplot(df_lof, aes(x = eruptions, y = waiting)) +
  geom_point(aes(color = Cluster), size = 2, alpha = 0.5) +
  geom_point(data = df_lof[scores >= lof_top5_val, ], aes(shape = "Outlier"), color = color_outlier, size = 4, stroke = 1.5) +
  geom_text(data = df_lof[scores >= lof_top5_val, ], aes(label = which(scores >= lof_top5_val)), vjust = 2, color = color_outlier, size = 2.5, fontface = "bold") +
  coord_fixed(ratio = 1) +
  scale_color_manual(values = c("1" = color_cluster1, "2" = color_cluster2)) +
  scale_shape_manual(values = c("Outlier" = 4)) +
  labs(title = "LOF (Top 5 Isolated)", color = "Cluster", shape = "Reference") + panel_theme + legend_order

# LOF Threshold 1.38 [POSITION: BOTTOM-RIGHT]
p_lof_138 <- ggplot(df_lof, aes(x = eruptions, y = waiting)) +
  geom_point(aes(color = Cluster), size = 2, alpha = 0.5) +
  geom_point(data = df_lof[scores >= 1.38, ], aes(shape = "Outlier"), color = color_outlier, size = 4, stroke = 1.5) +
  geom_text(data = df_lof[scores >= 1.38, ], aes(label = which(scores >= 1.38)), vjust = 2, color = color_outlier, size = 2.5, fontface = "bold") +
  coord_fixed(ratio = 1) +
  scale_color_manual(values = c("1" = color_cluster1, "2" = color_cluster2)) +
  scale_shape_manual(values = c("Outlier" = 4)) +
  labs(title = "LOF (Threshold 1.38)", color = "Cluster", shape = "Reference") + panel_theme + legend_order

# ==============================================================================
# 4. COMBINED 2x2 QUAD PLOT
# Top-Left: ODK-means | Top-Right: Trimmed K-means
# Bottom-Left: LOF (Top 5) | Bottom-Right: LOF (1.38)
# ==============================================================================
comp_all <- grid.arrange(p_odkm, p_tkm, p_lof_top5, p_lof_138, ncol = 2, nrow = 2)
ggsave("comp_all.jpg", comp_all, width = 12, height = 12, dpi = 300)
