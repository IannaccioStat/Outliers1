################################################################################
# PROJECT: Nonparametric framework for the definition, adaptive detection and 
#          probabilistic interpretation of outliers
# AUTHORS: 
# LOCATION: SUPPLEMENTARY, Figure 3
# DESCRIPTION: Scree Plot of Observation Rank (increasing order) v. LOF Score.
################################################################################

library(Rlof)
library(ggplot2)

# 1. Load and prepare dataset
data(faithful)
X <- scale(as.matrix(faithful))

# 2. Compute LOF scores and prepare rank data frame
scores <- lof(X, k = 5)
sorted_scores <- sort(scores, decreasing = TRUE)
lof_top5_val <- sorted_scores[5]
prop_top5 <- 5 / nrow(X)

df_scree <- data.frame(
  rank = 1:nrow(X),
  score = sorted_scores
)

# 3. Build diagnostic scree plot
fig_s3 <- ggplot(df_scree, aes(x = rank, y = score)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "steelblue", size = 1.5, alpha = 0.6) +
  
  # Top 5 line and annotation
  geom_hline(yintercept = lof_top5_val, color = "darkgreen", linetype = "dashed") +
  annotate("text", x = 25, y = lof_top5_val, 
           label = paste0("p = ", round(prop_top5, 3), ", LOF = ", round(lof_top5_val, 2)),
           vjust = -1, color = "darkgreen", fontface = "bold") +
  
  # LOF = 1.38 elbow line and annotation
  geom_hline(yintercept = 1.38, color = "red", linetype = "dotted") +
  annotate("text", x = 25, y = 1.38, label = "LOF = 1.38",
           vjust = -1, color = "red", fontface = "bold") +
  
  labs(title = "LOF Score Scree Plot", x = "Observation Rank", y = "LOF Score") +
  coord_cartesian(xlim = c(1, 50)) + 
  theme_minimal()

# 4. Save high-resolution plot
ggsave(filename = "lof_scree_plot.jpg", plot = fig_s3, width = 1920, height = 1080, units = "px", dpi = 300, scale = 1.75)