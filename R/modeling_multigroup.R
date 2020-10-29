#' Rename R function which sorts character strings containing
#' embedded numbers so that the numbers are numerically sorted 
#' rather than sorted by character value e.g. pic1, pic2, pic10
#' instead of pic1, pic10, pic2
#' @param .....

library(gtools)
sort_nat <- mixedsort 


#' LoadFeatureMaps
#' @param vidwidth stimulus width in pixels
#' @param vidheight stimulus height in pixels
#' @param path_all path to the folder containing folders from static and dynamic saliency maps

library(mvtnorm)

LoadFeatureMaps <- function(vidwidth, vidheight, path_all) { 
  
  # static saliency sorted
  path_stat_sal <- paste(path_all, "static_saliency", sep = "/")
  list_stat_sal <- list.files(path = path_stat_sal)
  sorted_static_saliency_maps <- sort_nat(list_stat_sal)
  
  # dynamic saliency sorted
  path_dyn_sal <- paste(path_all, "dynamic_saliency", sep = "/")
  list_dyn_sal <- list.files(path = path_dyn_sal)
  sorted_dynamic_saliency_maps <- sort_nat(list_dyn_sal)
  
  # center bias map
  mu <- c(vidwidth / 2, vidheight / 2)
  wy <- vidheight / 12 
  wx <- vidwidth / 12
  Sigma <-  matrix(c(wx * wx, 0, 0, wy * wy), nrow = 2, ncol = 2, byrow = TRUE)
  x1 <-  1:vidwidth
  x2 <-  1:vidheight
  
  pts <- expand.grid(x = x1, y = x2)
  pts <- pts[, c(2, 1)]
  F. <- dmvnorm(pts, mu, Sigma) 
  F. <- matrix(F., nrow = vidheight, ncol = vidwidth, byrow = TRUE)
 
  center_bias_map <- F. / sum(F.)
  
  # uniform distribution map
  uniform_map <- matrix(1, vidheight, vidwidth) / (vidheight * vidwidth)
  
  
  res <- list(sorted_static_saliency_maps, sorted_dynamic_saliency_maps,
              center_bias_map, uniform_map)
  names(res) <- c('sorted_static_saliency_maps', 'sorted_dynamic_saliency_maps',
                  'center_bias_map', 'uniform_map')
  return(res)
}




#' ReadCurrentMaps
#' @param vidheight stimulus height in pixels
#' @param vidwidth stimulus width in pixels
#' @param vidheight_monitor stimulus height on monitor in pixels
#' @param vidwidth_monitor stimulus width on monitor in pixels
#' @param Nmap number feature maps in model
#' @param iframe frame number
#' @param StatSal static saliency map as PNG file
#' @param DynSal dynamic saliency map as PNG file
#' @param AOI_dyn dynamic AoI matrix
#' @param AOI_stat static AoI matrix
#' @param center_bias_map center bias matrix
#' @param uniform_map uniform matrix
#' @param EyePos_example_Treat list with gaze coordinates treatment group
#' @param EyePos_example_Cont list with gaze coordinates treatment control group


library(ks)

ReadCurrentMaps <- function(vidheight, vidwidth, vidheight_monitor,
                            vidwidth_monitor, Nmap, iframe, StatSal, DynSal,
                            AOI_dyn, AOI_stat,
                            center_bias_map, uniform_map, EyePos_example_Treat,
                            EyePos_example_Cont) {
  
  X <- matrix(nrow = vidheight * vidwidth * 2, ncol = Nmap + Nmap)
  G <- c(rep(1, vidheight * vidwidth), rep(0, vidheight * vidwidth))
  
  # Normalization to probability density functions, which are organized columnwise in a matrix X:
  X[, 1] <- rep((as.vector(StatSal) / sum(StatSal)), 2)
  X[, 2] <- rep((as.vector(DynSal) / sum(DynSal)), 2)
  X[, 3] <- rep((as.vector(center_bias_map) / sum(center_bias_map)), 2)
  X[, 4] <- rep((as.vector(uniform_map) / sum(uniform_map)), 2)
  X[, 5] <- rep(as.vector(AOI_dyn) / sum(AOI_dyn), 2)
  X[, 6] <- rep(as.vector(AOI_stat) / sum(AOI_stat), 2)
  # Interactions:
  X[, 7] <- X[, 1] * G
  X[, 8] <- X[, 2] * G
  X[, 9] <- X[, 3] * G
  X[, 10] <- X[, 4] * G
  X[, 11] <- X[, 5] * G
  X[, 12] <- X[, 6] * G
  
  
  # Read current frame eye position density map (extract current frame):
  
  
  # EyePos_example$EyePos_example[,, 19] 19th participant
  # EyePos_example$EyePos_example[, 2,] 2nd frame
  
  EyePos_i1 <- t(EyePos_example_Treat[, iframe,])
  EyePos_i2 <- t(EyePos_example_Cont[, iframe,])
  # remove outliers: 
  EyePos_i1[which(EyePos_i1[, 1] > vidwidth_monitor), 1] <- NA
  EyePos_i1[which(EyePos_i1[, 2] > vidheight_monitor), 2] <- NA
  EyePos_i1[which(EyePos_i1[, 1] < 0), 1] <- NA 
  EyePos_i1[which(EyePos_i1[, 2] < 0), 2] <- NA
  
  EyePos_i2[which(EyePos_i2[, 1] > vidwidth_monitor), 1] <- NA
  EyePos_i2[which(EyePos_i2[, 2] > vidheight_monitor), 2] <- NA
  EyePos_i2[which(EyePos_i2[, 1] < 0), 1] <- NA 
  EyePos_i2[which(EyePos_i2[, 2] < 0), 2] <- NA
  
  # Delete coordinate (0,0):
  EyePos_i1[which(EyePos_i1[, 1] == -258 & EyePos_i1[, 2] == 0), ] <- NA
  EyePos_i2[which(EyePos_i2[, 1] == -258 & EyePos_i2[, 2] == 0), ] <- NA
  
  
  # if x or y coordinate is NA, delete whole eye position:
  EyePos_i1 <- EyePos_i1[!is.na(EyePos_i1[, 1]), ] 
  EyePos_i1 <- EyePos_i1[!is.na(EyePos_i1[, 2]), ]
  
  EyePos_i2 <- EyePos_i2[!is.na(EyePos_i2[, 1]), ] 
  EyePos_i2 <- EyePos_i2[!is.na(EyePos_i2[, 2]), ]
  
  rotate_back <- function(x) apply(t(x), 2, rev)
  
  EyePos_it1 <- EyePos_i1
  EyePos_it1[, 2] <- abs(EyePos_i1[, 2] - vidheight_monitor)
  Hlscv1 <- Hlscv(EyePos_it1)
  kde1 <-  kde(EyePos_it1, H = Hlscv1, gridsize = c(vidwidth, vidheight),
               bgridsize = c(vidwidth, vidheight), xmin = c(0, 0), 
               xmax = c(vidwidth_monitor, vidheight_monitor))$estimate
  Eye_Position_Map1 <- rotate_back(kde1)
  
  EyePos_it2 <- EyePos_i2
  EyePos_it2[, 2] <- abs(EyePos_i2[, 2] - vidheight_monitor)
  Hlscv2 <- Hlscv(EyePos_it2)
  kde2 <-  kde(EyePos_it2, H = Hlscv2, gridsize = c(vidwidth, vidheight),
               bgridsize = c(vidwidth, vidheight), xmin = c(0, 0), 
               xmax = c(vidwidth_monitor, vidheight_monitor))$estimate
  Eye_Position_Map2 <- rotate_back(kde2)
  
  
  
  res <- list(Eye_Position_Map1, Eye_Position_Map2, X)
  names(res) <- c("Eye_Position_Map1", "Eye_Position_Map2", "X")
  return(res)
}



library('glmnet')
library('png') # readPNG()
library('HDeconometrics') # ic.glmnet()

#' Final_Lasso
#' @param EyePos_example_Treat gaze coordinates treatments as 3 dimensional list
#' @param EyePos_example_Cont gaze coordinates controls as 3 dimensional list
#' @param vidheight stimulus height in pixels
#' @param vidwidth stimulus width in pixels
#' @param vidheight_monitor stimulus height on monitor in pixels
#' @param vidwidth_monitor stimulus width on monitor in pixels
#' @param Nmap number feature maps in model
#' @param nframes number frames
#' @param path_all path to the folder containing folders from static and dynamic saliency maps
#' @param listname_AOI_dym name of list with dynamic saliency maps
#' @param listname_AOI_stat name of static saliency map
#' @param least_squares if TRUE, calculates least square model 
#' @param lasso if TRUE, calculates lasso solution (if least_squares = TRUE and lasso = TRUE, calculates comparative metric D)
#' @param Permutation if TRUE a permutation test for 10 frames is executed, if TRUE the function sets automatically least_squares = TRUE and lasso = FALSE
#' @return List of beta coefficients from ic.glmnet for each frame, coefficients of determination, 
#'        lambda from the regularization, if desired comparative metric D
#' @export

Final_Lasso <- function(EyePos_example_Treat, EyePos_example_Cont, vidheight, 
                        vidwidth, vidheight_monitor, vidwidth_monitor, Nmap,
                        nframes, path_all,
                        listname_AOI_dyn, 
                        listname_AOI_stat, least_squares = TRUE,
                        lasso = FALSE, Permutation = FALSE) {
  
  # either lasso or least squares or both
  stopifnot(least_squares == T|lasso == T)
  
  # if Permutation == TRUE only compute least squares solution
  if(Permutation){ 
    lasso = FALSE
    least_squares = TRUE}
  
  
  LFM_res <- LoadFeatureMaps(vidwidth, vidheight, path_all)
  static_saliency_maps <- LFM_res$sorted_static_saliency_maps
  dynamic_saliency_maps <- LFM_res$sorted_dynamic_saliency_maps
  center_bias_map <- LFM_res$center_bias_map
  uniform_map <- LFM_res$uniform_map
  
  
  path_stat_sal <- paste(path_all, "static_saliency", sep = "/")
  list_stat_sal <- list.files(path = path_stat_sal)
  list_stat_sal <- sort_nat(list_stat_sal)
  
  path_Dyn_sal <- paste(path_all, "dynamic_saliency", sep = "/")
  list_Dyn_sal <- list.files(path = path_Dyn_sal)
  list_Dyn_sal <- sort_nat(list_Dyn_sal)
  
  # empty vectors for R-squared solution, comparative metric D and selected
  # penalty parameter lambda in the lasso by using BIC
  rsq <- c()
  rsq_adj <- c()
  comp <- c()
  lambda <- c()
  
  if(Permutation){
    # selecting a equidistant sequence of 10 frames (starting with frame number 7)
    # for which permutation is conducted 
    frames <- round(seq(7, nframes, len = 10))
    # Empty matrix for feature map weights
    beta_weights_kq <- matrix(nrow = length(frames), ncol = Nmap + Nmap)
    pval_kq <- matrix(nrow = length(frames), ncol = Nmap + Nmap)
  }
  
 else{
      frames <- 1:nframes
      # Empty matrix for feature map weights:
      chosen_beta_weights <- matrix(nrow = nframes, ncol = Nmap + Nmap)
      beta_weights_kq <- matrix(nrow = nframes, ncol = Nmap + Nmap)
      pval_kq <- matrix(nrow = nframes, ncol = Nmap + Nmap)
 }
  
  n_permut <- 0
  # read and preprocess feature maps, run model for each frame
  for(iframe in frames){
    n_permut <- n_permut + 1
    StatSal <- readPNG(paste(path_stat_sal, list_stat_sal[iframe],
                                           sep = "/")) * 255
    DynSal <- readPNG(paste(path_Dyn_sal, list_Dyn_sal[iframe],
                                          sep = "/")) * 255
    
    AOI_dyn <- listname_AOI_dyn[[iframe]]
    
    AOI_stat <- listname_AOI_stat
    
    RCM_res <- ReadCurrentMaps(vidheight, vidwidth, vidheight_monitor,
                               vidwidth_monitor, Nmap, iframe, StatSal,
                               DynSal, AOI_dyn, AOI_stat, 
                               center_bias_map, uniform_map, 
                               EyePos_example_Treat, EyePos_example_Cont)
    
    Feature_Maps <- RCM_res$X
    Eye_Position_Map1 <- RCM_res$Eye_Position_Map1
    Eye_Position_Map2 <- RCM_res$Eye_Position_Map2
    
   
    # if(!is.null(Feature_Maps[is.na(Feature_Maps)])||
    #    !is.null(Eye_Position_Map[is.na(Eye_Position_Map1)])||
    #    !is.null(Eye_Position_Map[is.na(Eye_Position_Map2)])){
    #   if(lasso){
    #   chosen_beta_weights[iframe, ] <- rep(NA, Nmap + Nmap)}
    #   if(least_squares){
    #     beta_weights_kq[iframe, ] <- rep(NA, Nmap + Nmap)
    #   }
    # }
    
    # else{
    # LASSO 
    Eye_Position_Map1 <- scale(Eye_Position_Map1, center = TRUE, scale = FALSE)
    Eye_Position_Map2 <- scale(Eye_Position_Map2, center = TRUE, scale = FALSE)
    
    Feature_Maps1 <- scale(Feature_Maps[1:(nrow(Feature_Maps)/2), ], 
                           center = TRUE, scale = FALSE)
    Feature_Maps2 <- scale(Feature_Maps[-(1:(nrow(Feature_Maps)/2)), ], 
                           center = TRUE, scale = FALSE)
    
    scale1 <- function(X){
      d <- sqrt(rowSums(t(X)^2))
      d[which(d == 0)] <- 1
      return(t(t(X)/d))}
    
    Feature_Maps1 <- scale1(Feature_Maps1) 
    Feature_Maps2 <- scale1(Feature_Maps2)
    
    Feature_Maps <- rbind(Feature_Maps1, Feature_Maps2)
    
    if(lasso){
    fit.lasso <- ic.glmnet(Feature_Maps, c(as.vector(Eye_Position_Map1), 
                                       as.vector(Eye_Position_Map2)), 
                                       crit = "bic", 
                       intercept = F, standardize = F, standardize.response = T)
    
   
    lambda[iframe] <- fit.lasso$lambda
    
    y_predicted <- predict(fit.lasso, s = fit.lasso$lambda, newx = Feature_Maps)
    
    y <- c(as.vector(Eye_Position_Map1), as.vector(Eye_Position_Map2))
    
    # coefficient of determination
    sst <- sum((y - mean(y))^2)
    sse <- sum((y_predicted - y)^2)
    n <- length(y)
    p <- Nmap + Nmap
    
    rsq[iframe] <- 1 - sse / sst
    rsq_adj[iframe] <- 1 - (1 - rsq[iframe]) * (n - 1) / (n - p)
    
    chosen_beta_weights[iframe, ] <- fit.lasso$coefficients[-1]
    }
     # comparison
    if(least_squares){
      
      lmfit <- lm(c(as.vector(Eye_Position_Map1), 
                    as.vector(Eye_Position_Map2)) ~ Feature_Maps)
      kq <- lmfit$coefficients
      colnames(beta_weights_kq) <- names(kq[-1])
      colnames(pval_kq) <- names(kq[-1])
      
      # fill weights into an empty matrix (columns: Feature maps, rows: Frame number) and keep NA estimates:
      j <- 0
      for(i in colnames(beta_weights_kq)){
        j <- j + 1
        beta_weights_kq[n_permut, j] <- lmfit$coefficients[i]
        pval_kq[n_permut, j] <- summary(lmfit)$coefficients[,4][i]
        
      }
     
      if(least_squares & lasso)
      comp[iframe] <- sum(abs(fit.lasso$coefficients[-1])) / sum(abs(kq[-1]),
                                                             na.rm = T)
      
    }
    
    
    
  # }
  }
  if(least_squares & lasso){
    return(list(beta = chosen_beta_weights, r2_adj = rsq_adj, lambda = lambda,
                betakq = beta_weights_kq, pvaluekq = pval_kq#, D = comp
                ))
  }
  if(least_squares & !lasso){
    
    return(list(betakq = beta_weights_kq, pvaluekq = pval_kq))

  }

  if(lasso & !least_squares){
    
    return(list(beta = chosen_beta_weights, r2_adj = rsq_adj, lambda = lambda))
    
  }  
  
}
