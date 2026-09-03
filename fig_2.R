################################################################################
# PROJECT: Nonparametric framework for the definition, adaptive detection and 
#          probabilistic interpretation of outliers
# AUTHORS: 
# LOCATION: MAIN, Figure 2
# DESCRIPTION: Illustration of a synthetic dataset realization in a three-dimensional 
#              feature space (N_k=200, r=1, s=4.5, sigma^2=0.02, p=0.05). Regular 
#              observations and anomalies are shown in blue and red, respectively. 
#              The inner cubes around each cluster and the outer bounding box 
#              define the spatial boundaries used to control the generation of 
#              internal versus external outliers according to the definitions 
#              in Subsection 2.2.
################################################################################

library(scatterplot3d)

# 1. Reproducibility & Data Generation
set.seed(42) 
source("ODKm_pub.R")

data  <- tethraset(200, 0.05, 0.02, 4.5, 1)
cubes <- data$cubes

# 2. Geometric Plotting Utilities
draw_cube_wireframe <- function(bounds, s3d, color = "black", lwd = 2, lty = 1) {
  x_min <- bounds[1, 1]; x_max <- bounds[1, 2]
  y_min <- bounds[2, 1]; y_max <- bounds[2, 2]
  z_min <- bounds[3, 1]; z_max <- bounds[3, 2]
  
  vertices <- matrix(c(
    x_min, y_min, z_min,  x_max, y_min, z_min,  x_max, y_max, z_min,  x_min, y_max, z_min,
    x_min, y_min, z_max,  x_max, y_min, z_max,  x_max, y_max, z_max,  x_min, y_max, z_max 
  ), ncol = 3, byrow = TRUE)
  
  edges <- list(
    c(1, 2), c(2, 3), c(3, 4), c(4, 1), # Base
    c(5, 6), c(6, 7), c(7, 8), c(8, 5), # Top
    c(1, 5), c(2, 6), c(3, 7), c(4, 8)  # Vertical Pillars
  )
  
  for (edge in edges) {
    s3d$points3d(vertices[edge, ], type = "l", col = color, lwd = lwd, lty = lty)
  }
}

# 3. High-Resolution Output Initialization
png("gen.png", width = 2300, height = 2000, res = 300)

# Configure plot boundaries
radius <- 1
side   <- 4.5

x_min <- -3 * radius; x_max <- side + 3 * radius
y_min <- -3 * radius; y_max <- side + 3 * radius
z_min <- -3 * radius; z_max <- side + 3 * radius

dummy_points <- matrix(c(x_min, y_min, z_min, x_max, y_max, z_max), ncol = 3, byrow = TRUE)

s3d <- scatterplot3d(
  dummy_points[,1], dummy_points[,2], dummy_points[,3],
  xlim = c(x_min, x_max), ylim = c(y_min, y_max), zlim = c(z_min, z_max),
  scale.y = 0.9, angle = 45,
  color = "white", pch = NA,
  xlab = expression(x[1]), ylab = expression(x[2]), zlab = expression(x[3]),
  main = ""
)

# Layer 1: Global Bounding Domain (Big Cube)
big_cube_bounds <- matrix(c(x_min, x_max, y_min, y_max, z_min, z_max), nrow = 3, byrow = TRUE)
draw_cube_wireframe(big_cube_bounds, s3d, color = "gray60", lwd = 1.5, lty = 2)

# Layer 2: Regular Points vs. Outliers
items <- data$data
col <- rep("blue", nrow(items))
col[data$outliers] <- "red"
s3d$points3d(items[,1], items[,2], items[,3], col = col, pch = 16, cex = 0.6)

# Layer 3: Cluster Hyper-Rectangles (Small Cubes)
for (k in 1:dim(cubes)[3]) {
  draw_cube_wireframe(cubes[,,k], s3d, color = "black", lwd = 2, lty = 1)
}

# Layer 4: Legend
legend(
  "topright",
  legend = c("Regular", "Outliers", "Small cubes", "Big cube"),
  col    = c("blue", "red", "black", "gray60"),
  pch    = c(16, 16, NA, NA),
  lty    = c(NA, NA, 1, 2),
  lwd    = c(NA, NA, 2, 1.5),
  pt.cex = 1,
  cex    = 0.85,
  bg     = "white",
  box.col = "black",
  inset  = c(0.02, 0.02)
)

dev.off()