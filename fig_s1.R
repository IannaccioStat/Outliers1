################################################################################
# PROJECT: Nonparametric framework for the definition, adaptive detection and 
#          probabilistic interpretation of outliers
# AUTHORS: 
# LOCATION: SUPPLEMENTARY, Figure 1
# DESCRIPTION: Illustration of cell-wise and case-wise outliers in 2 dimensions.
################################################################################

# ==============================================================================
# 1. SETUP AND PACKAGE LOAD
# ==============================================================================
library(ggplot2)
library(dplyr)
library(mvtnorm) 

# ==============================================================================
# 2. GENERATE AND CONSOLIDATE THE DATASET
# ==============================================================================
set.seed(42)

# --- Core Data (Central Mass) ---
n_regular <- 500
tight_sigma <- matrix(c(0.2, 0.05, 0.05, 0.2), 2)

df_core <- as.data.frame(rmvt(n_regular, delta = c(0, 0), sigma = tight_sigma, df = 5))
colnames(df_core) <- c("Feature_1", "Feature_2")
df_core$Anomalous <- "Regular"

# --- Outlier 1 (Univariate / Cell-wise) ---
outlier_1 <- data.frame(
  Feature_1 = runif(1, min = -0.2, max = 0.2),
  Feature_2 = 6,
  Anomalous = "Cell-wise Outlier"
)

# --- Outlier 2 (Multivariate / Row-wise) ---
outlier_2 <- data.frame(
  Feature_1 = 6,
  Feature_2 = 5.8,
  Anomalous = "Row-wise Outlier"
)

df_combined <- bind_rows(df_core, outlier_1, outlier_2)

# ==============================================================================
# 3. CREATE THE DATA VISUALIZATION (GGPLOT2)
# ==============================================================================
p_anomalies <- ggplot(df_combined, aes(x = Feature_1, y = Feature_2, color = Anomalous, shape = Anomalous)) +
  # Regular points
  geom_point(data = subset(df_combined, Anomalous == "Regular"), alpha = 0.5, size = 1.8) +
  # Outliers: bold stroke for thickness
  geom_point(data = subset(df_combined, Anomalous != "Regular"), size = 6, stroke = 1.8) +
  
  scale_color_manual(values = c("Regular" = "deepskyblue4", 
                                "Cell-wise Outlier" = "#D55E00", 
                                "Row-wise Outlier" = "#CC79A7")) +
  
  scale_shape_manual(values = c("Regular" = 16, "Cell-wise Outlier" = 4, "Row-wise Outlier" = 8)) +
  
  labs(
    title = "Conceptual Map: Cell-wise vs. Row-wise Outliers",
    x = "Feature 1",
    y = "Feature 2",
    color = "Observation Type",
    shape = "Observation Type"
  ) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "right") +
  
  guides(
    color = guide_legend(override.aes = list(alpha = 1, size = 5, stroke = 1.5)),
    shape = guide_legend(override.aes = list(size = 5, stroke = 1.5))
  ) +
  expand_limits(x = 0, y = 0)

print(p_anomalies)

# ==============================================================================
# 4. SAVE HIGH-RESOLUTION PLOT
# ==============================================================================
ggsave("case_v_cell.jpg", p_anomalies, width = 1920, height = 1080, units = "px", dpi = 300)
