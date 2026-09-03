################################################################################
# PROJECT: Nonparametric framework for the definition, adaptive detection and 
#          probabilistic interpretation of outliers
# AUTHORS: 
# LOCATION: MAIN, Figure 1
# DESCRIPTION: Illustration of the geometric taxonomy of multivariate outliers. 
#              An external outlier (red diamond) falls outside conv(S^star); 
#              a non-cluster-specific internal outlier (yellow diamond) resides 
#              inside conv(S^star) but outside any local hull; a cluster-specific 
#              outlier (orange diamond) falls within its corresponding local 
#              hull conv(S^star_k).
################################################################################

library(ggplot2)
library(dplyr)

# 1. Setup Parameters
n_points_per_cluster <- 100
radius <- 1
side_length <- 7

h <- (sqrt(3)/2) * side_length
centers <- data.frame(
  cx = c(0, side_length/2, -side_length/2),
  cy = c(h * 2/3, -h * 1/3, -h * 1/3)
)

# 2. Generate Hollow Circular Clusters
set.seed(42)
data_list <- lapply(1:3, function(i) {
  angle <- runif(n_points_per_cluster, 0, 2*pi)
  r_noise <- rnorm(n_points_per_cluster, mean = radius, sd = 0.05)
  
  data.frame(
    x = centers$cx[i] + r_noise * cos(angle),
    y = centers$cy[i] + r_noise * sin(angle),
    cluster = as.factor(i)
  )
})

df <- do.call(rbind, data_list)

# 3. Calculate Global Convex Hull
global_hull_indices <- chull(df$x, df$y)
global_hull_data <- df[global_hull_indices, ]

# 4. Calculate Individual Convex Hulls for Each Cluster
individual_hulls <- df %>%
  group_by(cluster) %>%
  reframe(data.frame(x = x[chull(x, y)], y = y[chull(x, y)]))

# 5. Define Manual Outlier Taxonomy Points
manual_points <- data.frame(
  x = c(0, 2, 0, -3.2, 7, -5),
  y = c(0, 0.2, 3.8, -1.8, 0, 5),
  PointType = c("Internal", "Internal", 
                "Cluster-specific", "Cluster-specific", 
                "External", "External")
)

# 6. Build Plot
p1 <- ggplot(df, aes(x = x, y = y)) +
  # Individual Cluster Hulls
  geom_polygon(data = individual_hulls, aes(group = cluster, fill = "Convex Hulls"), 
               color = "black", alpha = 0.15, linewidth = 0.8) +
  
  # Global Convex Hull
  geom_polygon(data = global_hull_data, aes(fill = "Convex Hulls"), 
               color = "black", alpha = 0.05, linewidth = 0.8) +
  
  # Original Cluster Data
  geom_point(aes(color = "Non-outliers"), alpha = 0.4) +
  
  # Outlier Points
  geom_point(data = manual_points, 
             aes(x = x, y = y, color = PointType), 
             size = 4, shape = 18) + 
  
  # Manual Scales
  scale_color_manual(name = "Outliers Legend", 
                     values = c("Non-outliers" = "blue", 
                                "Internal" = "gold", 
                                "Cluster-specific" = "orange", 
                                "External" = "red")) +
  scale_fill_manual(name = "", values = c("Convex Hulls" = "black")) +
  
  # Tight aspect ratio limits matching the data bounds
  coord_fixed(xlim = c(-5.5, 7.5), ylim = c(-3.2, 5.2), expand = FALSE) +
  theme_minimal() +
  theme(plot.margin = margin(5, 5, 5, 5, "pt")) +
  labs(x = expression(X[1]), y = expression(X[2]))

# 7. Save High-Resolution Plot with 8:5 proportion (prevents vertical blank space)
ggsave("taxonomy.png", plot = p1, width = 8, height = 5, dpi = 300)