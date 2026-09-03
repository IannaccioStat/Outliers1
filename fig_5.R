################################################################################
# PROJECT: Nonparametric framework for the definition, adaptive detection and 
#          probabilistic interpretation of outliers
# AUTHORS: 
# LOCATION: MAIN, Figure 5
# DESCRIPTION: ODK-means applied to Setosa and Virginica species from the Iris 
#              dataset (alpha = {0.05, 0.075, 0.10}). All observations remain 
#              categorized as regular.
################################################################################

library(ggplot2)
library(gridExtra)

# ==============================================================================
# DATA PREPARATION & PSEUDO-ISOLATION CALCULATION
# ==============================================================================
items <- iris[, 3:4] # Petal.Length & Petal.Width

setosa_idx    <- which(iris$Species == "setosa")
virginica_idx <- which(iris$Species == "virginica")

i_setosa    <- items[setosa_idx, ]
i_virginica <- items[virginica_idx, ]

# Calibration constants
N_sub <- 50
K <- ceiling(log(N_sub))
h <- ceiling(K / 10)

alphas <- c(0.05, 0.075, 0.10)
k_factors <- sqrt((1 / alphas) - 1)

# --- 1. Virginica Calculations ---
d_virginica  <- as.matrix(dist(i_virginica))
pi_virginica <- numeric(N_sub)

for (i in 1:N_sub) {
  s_vir <- sort(d_virginica[i, ])
  pi_virginica[i] <- sum(s_vir[h:K])
}

m_vir <- mean(pi_virginica)
v_vir <- sd(pi_virginica)
t_vir_vals <- m_vir + k_factors * v_vir

df_vir <- data.frame(Y = pi_virginica)

# --- 2. Setosa Calculations ---
d_setosa  <- as.matrix(dist(i_setosa))
pi_setosa <- numeric(N_sub)

for (i in 1:N_sub) {
  s_set <- sort(d_setosa[i, ])
  pi_setosa[i] <- sum(s_set[h:K])
}

m_set <- mean(pi_setosa)
v_set <- sd(pi_setosa)
t_set_vals <- m_set + k_factors * v_set

df_set <- data.frame(Y = pi_setosa)

# ==============================================================================
# GGPLOT GENERATION
# ==============================================================================
panel_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "right",
    legend.title = element_blank(),
    panel.grid.minor = element_blank()
  )

# Color mapping & legend labels for thresholds
thresh_colors <- c("red", "orange", "yellow")

labels_vir <- c(
  sprintf("T[0.05] == %.3f", t_vir_vals[1]),
  sprintf("T[0.075] == %.3f", t_vir_vals[2]),
  sprintf("T[0.1] == %.3f", t_vir_vals[3])
)

labels_set <- c(
  sprintf("T[0.05] == %.3f", t_set_vals[1]),
  sprintf("T[0.075] == %.3f", t_set_vals[2]),
  sprintf("T[0.1] == %.3f", t_set_vals[3])
)

# --- Plot A: Virginica ---
df_lines_vir <- data.frame(
  x = t_vir_vals,
  label = factor(labels_vir, levels = labels_vir)
)

p_vir <- ggplot(df_vir, aes(x = Y)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, 
                 fill = "mediumorchid4", color = "black", alpha = 0.85) +
  geom_vline(data = df_lines_vir, aes(xintercept = x, color = label), 
             linewidth = 1.2, linetype = "solid") +
  scale_color_manual(values = thresh_colors, labels = parse(text = labels_vir)) +
  coord_cartesian(xlim = c(0, 1.5)) +
  labs(title = "Virginica", x = "Y", y = "Density") +
  panel_theme

# --- Plot B: Setosa ---
df_lines_set <- data.frame(
  x = t_set_vals,
  label = factor(labels_set, levels = labels_set)
)

p_set <- ggplot(df_set, aes(x = Y)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, 
                 fill = "mediumpurple3", color = "black", alpha = 0.85) +
  geom_vline(data = df_lines_set, aes(xintercept = x, color = label), 
             linewidth = 1.2, linetype = "solid") +
  scale_color_manual(values = thresh_colors, labels = parse(text = labels_set)) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(title = "Setosa", x = "Y", y = "Density") +
  panel_theme

# ==============================================================================
# COMBINE & SAVE
# ==============================================================================
plot_iris_no_outliers <- grid.arrange(p_vir, p_set, ncol = 2)

ggsave("iris.jpg", plot_iris_no_outliers, width = 12, height = 6, dpi = 300)

