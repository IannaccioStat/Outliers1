################################################################################
# PROJECT: Nonparametric framework for the definition, adaptive detection and 
#          probabilistic interpretation of outliers
# AUTHORS: 
# DESCRIPTION: Full script with all the routines contained in the paper.
################################################################################

library(FNN)

# ------------------------------------------------------------------------------
# FUNCTION: penalty
# DESCRIPTION: Assigns observation weights and enforces soft-trimming bounds 
# as in Subsection 2.3 of the manuscript.
# ------------------------------------------------------------------------------
penalty <- function(sums, threshold, mult = 0) {
  if (length(sums) > 0 && any(sums >= threshold)) {
    max_ratio <- min(sums[sums >= threshold] / threshold)
    if (mult > max_ratio) {
      stop(sprintf("Parameter 'mult' (q = %.4f) exceeds the soft-trimming upper bound (max allowed: %.4f).", 
                   mult, max_ratio))
    }
  }
  
  weights <- ifelse(sums < threshold, 1, mult * (threshold / sums))
  return(weights / sum(weights))
}

# ------------------------------------------------------------------------------
# FUNCTION: outdet
# DESCRIPTION: Computes pseudo-isolation sums and applies Cantelli thresholding 
# for outlier identification as in Subsections 2.1-2.2 of the manuscript.
# ------------------------------------------------------------------------------
outdet <- function(labels, distance, margin = 0, mult = 0, alpha = 0.05) {
  dist_sub <- distance[labels, labels, drop = FALSE]
  l <- length(labels)
  if (l == 1) return(list("k" = 1, "sums" = numeric(0), "out" = numeric(0), "w" = 1))
  if (l == 2) return(list("k" = 1, "sums" = numeric(0), "out" = numeric(0), "w" = c(0.5, 0.5)))
  
  k <- ifelse(l > 2, floor(log(l)), 1)
  h <- ceiling(k / 10)
  
  col_max <- if (l < (2 * k + 1)) l else (k + 1)
  dist_sorted <- t(apply(dist_sub, 1, function(x) sort(x, partial = 1:col_max)[1:col_max]))
  
  raw_sums <- if (l < (2 * k + 1)) {
    rowSums(dist_sorted)
  } else {
    rowSums(dist_sorted[, h:(k + 1), drop = FALSE])
  }
  
  names(raw_sums) <- labels
  sums_k <- sort(raw_sums)
  
  tvec <- numeric(0); told <- Inf; deltat <- Inf; epsilon <- .00001
  K_val <- sqrt((1 / alpha) - 1)
  
  while (deltat > 0 + epsilon) {
    negatives <- sums_k[sums_k <= told]
    m <- mean(negatives); s <- sd(negatives)
    tnew <- m + K_val * s
    tvec <- c(tvec, tnew); deltat <- told - tnew; told <- tnew
  }
  
  outl <- which(sums_k > told)
  
  w_sorted <- penalty(sums_k, threshold = told, mult = mult)
  w_original_order <- w_sorted[match(as.character(labels), names(sums_k))]
  
  return(list(
    "k" = k, 
    "sums" = sums_k, 
    "out" = as.numeric(names(sums_k[outl])), 
    "w" = as.numeric(w_original_order), 
    "tvec" = tvec
  ))
}

# ------------------------------------------------------------------------------
# FUNCTION: assign
# DESCRIPTION: Assigns observations to the nearest cluster centroid using 
# Euclidean distance.
# ------------------------------------------------------------------------------
assign <- function(data, c) {
  k <- nrow(c)
  n <- nrow(data)
  dif <- matrix(0, k, n)
  for (idx in 1:k) {
    dif[idx, ] <- colSums((t(data) - c[idx, ])^2)
  }
  return(max.col(-t(dif)))
}

# ------------------------------------------------------------------------------
# FUNCTION: odkm
# DESCRIPTION: Runs the simultaneous clustering and outlier detection 
# optimization procedure as in Subsection 5.1 of the manuscript.
# ------------------------------------------------------------------------------
odkm <- function(data, K, margin = 0, restart = 10, mult = 0, alpha = 0.05) {
  data <- scale(data) 
  nr <- nrow(data) 
  epsilon <- .0000001 
  c_final <- NA; f_final <- Inf; lab_final <- NA
  di <- as.matrix(dist(data))
  
  for (s in 1:restart) {
    idx <- sample(1:nr, K)
    c <- data[idx, , drop = FALSE]
    convergence <- FALSE
    f0 <- Inf
    W <- numeric(nr)
    restart_flag <- FALSE
    
    while (!convergence) {
      lab <- assign(data, c) 
      if (length(unique(lab)) != K) {
        restart_flag <- TRUE
        break
      }
      for (k in 1:K) {
        u_k <- which(lab == k)
        out_res <- outdet(u_k, di, margin, mult, alpha)
        W[u_k] <- out_res$w
        
        if (length(u_k) == 1) {
          c[k, ] <- data[u_k, ]
        } else {
          c[k, ] <- colSums(W[u_k] * data[u_k, , drop = FALSE])
        }
      }
      
      f <- sum((data - c[lab, , drop = FALSE])^2)
      convergence <- ifelse(f0 - f < epsilon, TRUE, FALSE)
      f0 <- f
    }
    
    if (restart_flag) { next }
    
    if (f0 < f_final) {
      f_final <- f0; c_final <- c; lab_final <- lab
    }
  }
  
  U <- matrix(0, nr, K)
  for (i in 1:nr) { U[i, lab_final[i]] <- 1 }
  outliers <- numeric(0)
  for (k in 1:K) {
    lab <- which(U[, k] == 1)
    outliers <- c(outliers, outdet(lab, di, margin, mult, alpha)$out)
  }
  if (length(outliers) == 0) { outliers <- NULL } else { outliers <- sort(unique(outliers)) }
  
  return(list("U" = U, "c" = c_final, "f" = f_final, "out" = outliers))
}

# ------------------------------------------------------------------------------
# FUNCTION: evaluate_alpha
# DESCRIPTION: Estimates optimal alpha tuning bounds via cumulative isolation 
# analysis as in Section 5.3 of the manuscript.
# ------------------------------------------------------------------------------
evaluate_alpha <- function(data, number, U, pos = TRUE, h = NULL, l = NULL, epsilon = 0.001) {
  n_total <- nrow(data)
  
  if (!pos) {
    target_unit <- number
    cluster_idx <- which.max(U[target_unit, ])
    cluster_members <- which(U[, cluster_idx] == 1)
    
    cluster_data <- data[cluster_members, , drop = FALSE]
    n_c <- nrow(cluster_data)
    
    k_log <- floor(log(n_c))
    lk <- if (is.null(l)) k_log else l
    hk <- if (is.null(h)) ceiling(k_log / 10) else h
    lk <- min(lk, n_c - 1)
    
    knn_dist <- get.knn(cluster_data, k = lk)$nn.dist
    y_local <- if (hk == lk) knn_dist[, hk] else rowSums(knn_dist[, hk:lk, drop = FALSE])
    
    internal_idx <- which(cluster_members == target_unit)
    y_target <- y_local[internal_idx]
    sorted_y <- sort(y_local)
    m_rank <- which(sorted_y == y_target)[1]
    
  } else {
    global_y <- numeric(n_total)
    K_clusters <- ncol(U)
    
    for (k in 1:K_clusters) {
      members <- which(U[, k] == 1)
      n_c <- length(members)
      if (n_c < 2) { global_y[members] <- 0; next }
      
      k_log <- floor(log(n_c))
      lk <- if (is.null(l)) k_log else l
      hk <- if (is.null(h)) ceiling(k_log / 10) else h
      lk <- min(lk, n_c - 1)
      
      knn_dist <- get.knn(data[members, , drop = FALSE], k = lk)$nn.dist
      global_y[members] <- if (hk == lk) knn_dist[, hk] else rowSums(knn_dist[, hk:lk, drop = FALSE])
    }
    
    sorted_global <- sort(global_y)
    y_target <- sorted_global[number]
    
    target_unit_idx <- which(global_y == y_target)[1]
    cluster_idx <- which.max(U[target_unit_idx, ])
    cluster_members <- which(U[, cluster_idx] == 1)
    
    sorted_y <- sort(global_y[cluster_members])
    m_rank <- which(sorted_y == y_target)[1]
  }
  
  n_c <- length(sorted_y)
  if (m_rank < 2) return(1)
  
  idx_seq <- 1:n_c
  cum_sums <- cumsum(sorted_y)
  cum_sq_sums <- cumsum(sorted_y^2)
  
  means <- cum_sums / idx_seq
  vars <- (cum_sq_sums - (cum_sums^2) / idx_seq) / (idx_seq - 1)
  vars[vars < 0] <- 0
  sds <- sqrt(vars)
  
  eval_range <- m_rank:n_c
  valid_mask <- !is.na(sds[eval_range]) & sds[eval_range] > 0
  
  alphas_to_check <- numeric(length(eval_range))
  alphas_to_check[valid_mask] <- 1 / (((sorted_y[eval_range][valid_mask] - means[eval_range][valid_mask]) / sds[eval_range][valid_mask])^2 + 1)
  
  return(max(alphas_to_check))
}

# ------------------------------------------------------------------------------
# FUNCTION: smallcube
# DESCRIPTION: Helper function defining bounding boxes for central clusters 
# as in Section S3 of the manuscript's supplementary material.
# ------------------------------------------------------------------------------
smallcube <- function(dev, coord) {
  mat <- matrix(NA, 3, 2)
  for (i in 1:3) { mat[i, 1] <- coord[i] - dev; mat[i, 2] <- coord[i] + dev }
  return(mat)
}

# ------------------------------------------------------------------------------
# FUNCTION: bigcube
# DESCRIPTION: Helper function generating uniformly distributed noise points 
# outside cluster bounds as in Section S3 of the manuscript's supplementary 
# material.
# ------------------------------------------------------------------------------
bigcube <- function(ag, tbg, s, r, cube) {
  over <- 3; total <- tbg * over
  ng <- matrix(runif(3 * total, -3 * r, s + 3 * r), total, 3)
  in_any <- rep(FALSE, total)
  for (c in 1:4) {
    in_any <- in_any | (ng[,1] > cube[1,1,c] & ng[,1] < cube[1,2,c] &
                          ng[,2] > cube[2,1,c] & ng[,2] < cube[2,2,c] &
                          ng[,3] > cube[3,1,c] & ng[,3] < cube[3,2,c])
  }
  valid <- ng[!in_any, ]
  if (nrow(valid) >= tbg) return(valid[1:tbg, ])
  return(rbind(valid, bigcube(matrix(0, 0, 3), tbg - nrow(valid), s, r, cube)))
}

# ------------------------------------------------------------------------------
# FUNCTION: tethraset
# DESCRIPTION: Synthesizes four-cluster 3D tetrahedral benchmarking datasets 
# with optional outliers as in Section S3 of the manuscript's supplementary 
# material.
# ------------------------------------------------------------------------------
tethraset <- function(nk, perc_out, var_fluc, side, r) {
  if (perc_out > 0.05) perc_out <- 0.05
  Z <- matrix(rnorm(nk * 3), nk, 3)
  X <- t(apply(Z, 1, function(x) { x / (sqrt(sum(x * x))) })) * r
  
  centers <- matrix(c(0,0,0, side,0,0, side/2,side,0, 
                      side/2,side/2,sqrt(side^2-2*(side/2)^2)), 4, 3, TRUE)
  
  data <- rbind(X + matrix(rnorm(nk*3, 0, var_fluc), nk, 3),
                X + matrix(rnorm(nk*3, 0, var_fluc), nk, 3) + matrix(centers[2,], nk, 3, TRUE),
                X + matrix(rnorm(nk*3, 0, var_fluc), nk, 3) + matrix(centers[3,], nk, 3, TRUE),
                X + matrix(rnorm(nk*3, 0, var_fluc), nk, 3) + matrix(centers[4,], nk, 3, TRUE))
  
  cubes <- array(NA, c(3, 2, 4))
  for (c in 1:4) { cubes[, , c] <- smallcube(var_fluc + 1.5 * r, centers[c, ]) }
  
  if (perc_out > 0) {
    toberemoved <- floor(perc_out * (4 * nk))
    data <- data[-sample(1:nrow(data), toberemoved), ]
    m <- floor(toberemoved * 0.3)
    inl <- centers[sample(1:4, m, TRUE), ] + matrix(rnorm(3 * m, 0, var_fluc), m, 3)
    outl <- bigcube(matrix(0, 0, 3), toberemoved - m, side, r, cubes)
    data <- rbind(data, inl, outl)
    return(list("data" = data, "outliers" = (nrow(data) - toberemoved + 1):nrow(data), "cubes" = cubes))
  }
  return(list("data" = data, "outliers" = NULL, "cubes" = cubes))
}

# ------------------------------------------------------------------------------
# FUNCTION: metrics
# DESCRIPTION: Computes confusion matrices, precision, recall, and F1-scores 
# for outlier classification as in Section 6 of the manuscript.
# ------------------------------------------------------------------------------
metrics <- function(n, true, est) {
  true_m <- est_m <- matrix(c(0, 1), n, 2, byrow = TRUE)
  if (length(est) > 0) est_m[est, ] <- matrix(c(1, 0), length(est), 2, byrow = TRUE)
  if (length(true) > 0) true_m[true, ] <- matrix(c(1, 0), length(true), 2, byrow = TRUE)
  
  conf_mat <- t(est_m) %*% true_m
  precision <- ifelse((conf_mat[1, 1] + conf_mat[1, 2]) == 0, 0, conf_mat[1, 1] / (conf_mat[1, 1] + conf_mat[1, 2]))
  sensitivity <- ifelse((conf_mat[1, 1] + conf_mat[2, 1]) == 0, 0, conf_mat[1, 1] / (conf_mat[1, 1] + conf_mat[2, 1]))
  F1 <- ifelse((precision + sensitivity) == 0, 0, (2 * precision * sensitivity) / (precision + sensitivity))
  
  return(list("measure" = F1, "conf" = conf_mat))
}