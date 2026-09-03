################################################################################
# PROJECT: Nonparametric framework for the definition, adaptive detection and 
#          probabilistic interpretation of outliers
# AUTHORS: 
# LOCATION: SUPPLEMENTARY, Figure 2
# DESCRIPTION: Visualization of the selection tool for Trimmed K-means (ctlcurves 
#              routine in R's package TClust). The restriction factor bounds 
#              the ratio of maximum to minimum eigenvalues of the covariance 
#              matrix of the data, preventing degenerate solutions (set to 50 
#              as standard).
################################################################################

library(tclust)

# 1. Load and prepare dataset
data(faithful)
X <- scale(as.matrix(faithful))

# 2. Open graphics device and generate diagnostic plot
jpeg("trimmed_parameter.jpg", width = 1920, height = 1080, res = 300)

ctrl <- ctlcurves(x = X, k = 1:3, alpha = seq(0, 0.5, by = 0.01))
plot(ctrl, xlab = "p (Trimming Proportion)", main = "TClust Diagnostic Curves")

dev.off()