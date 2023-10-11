# functions_customized_dist_left.R
#
# statistical matching with fine-balance
# adopted from code repo of https://arxiv.org/abs/2211.15849
# original author by Chang Chen

## Functions
library(tableone)
library(xtable)
library(cpt)
library(ggplot2)

# match submission id for `emb_vec`
match_embID <- function(x)
{
  out <- ifelse((x %in% spec_emb$id), spec_emb$encoded_vec[which(spec_emb$id == x)], NA)
  out
}
# match submission id for `cluster`
match_clustID <- function(x)
{
  out <- ifelse((x %in% sub_clust10$submission_id), sub_clust10$cluster[which(sub_clust10$submission_id == x)], NA)
  out <- as.character(out)
  out
}

# in duplicate list, chose index element in numeric with descending order
convert_toIndex <- function(x,index)
{
  out <- sort(as.numeric(unlist(strsplit(as.character(x),split = "\\[|\\,|\\]"))[-1]),decreasing = T)[index]
  out
}

# adjust `n_review`: x must be `rating_int` row
convert_to_n_review <- function(x)
{
  out <- length(as.numeric(unlist(strsplit(as.character(x),split = "\\[|\\,|\\]"))[-1]))
  out
}  

# in duplicate list, chose index element in character
convert_toIndex_char <- function(x,index)
{
  out <- unlist(strsplit(as.character(x),split = "\\[|\\,|\\]"))[-1][index]
  out <- ifelse(out == 'nan',"'not report'", out)
  return(out)
}

# convert boolean to 0/1
convert_toBinary <- function(x)
{
  out <- mean(as.logical(unlist(strsplit(as.character(x),split = "\\[|\\,|\\]"))[-1][1]))
  return(out)
}

convert_toMean <- function(x)
{
  out <- mean(as.numeric(unlist(strsplit(as.character(x),split = "\\[|\\,|\\]"))[-1]))
  return(out)
}

# convert list to numeric vector
convert_toVec <- function(x)
{
  out <- as.numeric(unlist(strsplit(as.character(x),split = "\\[|\\,|\\]"))[-1])
  out
}


# construct cosine similarity
cos.sim <- function(A, B) 
{
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
} 

# construct dist_list_left with soft exact constraints plus soft caliper  
create_list_from_scratch_overall <- function(Z, X, exact = NULL, soft_exact = FALSE,
                                             p = NULL, caliper_low = 1, caliper_high = NULL, k = NULL,
                                             penalty = Inf){
  if (is.null(k)) k = length(Z) - sum(Z)
  
  if (is.null(caliper_high)) caliper_high = caliper_low
  
  n_t = sum(Z) # n_t is number of treated
  n_c = length(Z) - n_t
  # Cast X into matrix if it is a vector
  if (is.vector(X)) X = matrix(X, ncol=1)
  
  X_treated = X[Z == 1,]
  X_control = X[Z == 0,]
  
  if (is.vector(X_treated)) X_treated = matrix(X_treated, ncol=1)
  if (is.vector(X_control)) X_control = matrix(X_control, ncol=1)
  
  start_n = rep(seq(1,n_t,1), each = n_c)
  end_n = rep(seq(n_t+1, n_t+n_c, 1), n_t)
  d = numeric(n_t*n_c)
  point_start = 1
  point_end = 1
  for (i in 1:n_t){
#     temp_d = (1 - cos_mtx[i,])
    temp_d = rep(0,n_c)
    
    # find ind_exact
    X_control_exact_cols = X_control[,exact]
    if (is.vector(X_control_exact_cols)) X_control_exact_cols = data.frame(X_control_exact_cols)
    ind_control_exact = which(apply(X_control_exact_cols, 1,
                                    function(x) identical(unname(x), unname(X_treated[i, exact]))))
    ind_control_not_exact = setdiff(seq(1, n_c, 1), ind_control_exact)
    temp_d[ind_control_not_exact] = temp_d[ind_control_not_exact] + 1000
    
    if (!is.null(p)){ # only do sof caliper for now
      # find within/outside the caliper
      p_treated = p[which(Z == 1)]
      p_control = p[which(Z == 0)]
      control_outside_caliper = ((p_control < p_treated[i] - caliper_low) | (p_control > p_treated[i] + caliper_high)) + 0
      temp_d = temp_d + control_outside_caliper*penalty
    }
    
    
    point_end = point_start + length(temp_d) - 1
    d[point_start:point_end] = temp_d
    point_start = point_end + 1
  }
  
  if (any(d < 0)){
    offset_d = -min(d)
    d = d + offset_d
  }    
  return(list(start_n = unname(start_n),
              end_n = unname(end_n),
              d = unname(d)))
}

## taken from `tableone` smd-module

## Proportions to variance-covariance matrix
## https://stackoverflow.com/questions/19960605/r-multinomial-distribution-variance
## diagonal p_k(1-p_k); off-diagonal -1 * p_k * p_l
MultinomialVar <- function(multinomialMeans) {
  ## Mean for each
  p <- multinomialMeans
  ## Diagonal elements are p_i(1-p_i)
  vars <- p * (1 - p)
  ## Off-diagonal elements are - p_i p_j where i != j
  covs <- - outer(p, p)
  ## Put diagonal elements
  diag(covs) <- vars
  ## Reduce to a scalar if 1x1
  drop(covs)
}

## here can provide predefined vcov matrix S
StdDiffFromLstMeans <- function(lstMeans, S = NULL) {
  
  ## list of variance-covariance matrices
  ## https://stackoverflow.com/questions/19960605/r-multinomial-distribution-variance
  ## diagonal p_k(1-p_k); off-diagonal -1 * p_k * p_l
  lstCovs <- lapply(lstMeans, MultinomialVar)
  
  ## All possible mean vector differences
  lstMeanDiffs <- lapply(lstMeans, function(x) {
    lapply(lstMeans, function(y) {
      x - y
    })
  })
  
  ## All possible covariance matrix means
  lstCovMeans <- lapply(lstCovs, function(x) {
    lapply(lstCovs, function(y) {
      (x + y) / 2
    })
  })
  
  ## Initialize a numeric vector object for capturing values
  sqSmds <- vector(mode = "numeric")
  
  ## Add upper triangle elements (i < j) to output list
  for (i in seq_along(lstMeans)) {
    for (j in seq_along(lstMeans)) {
      if (i < j) {
        ## For upper triangle elements only
        ## Squared Mahalanobis distance
        ## meanDiffs^T (pooled vcov)^-1 meanDiffs
        ## Generalized inverse for protection against singularity
        ## Reduces to true inverse if non-singular
        
        ## Column mean difference vector
        T_C <- t(t(lstMeanDiffs[[i]][[j]]))
        
        ## Pooled vcov
        if(is.null(S)){
          S   <- lstCovMeans[[i]][[j]]
        }
        
        if (all(S[!is.na(S)] %in% 0)) {
          ## If S is a zero matrix, ginv is a zero matrix
          ## which gives a zero SMD regardless of mean
          ## difference. Such a case should be NaN.
          ## NOTE: NA's are dropped first. Non-NA elements
          ## are assessed for 0's. If all remaining
          ## are zeros or no element remained (all NA),
          ## all() returns TRUE, and sqMD is forced to NaN.
          
          if (all(!is.na(T_C) & (T_C == 0))) {
            ## If the mean difference vector is a zero vector,
            ## the distance can be defined 0.
            sqMD <- 0
          } else {
            sqMD <- NaN
          }
          
        } else {
          ## Squared Mahalanobis distance
          sqMD <- t(T_C) %*% MASS::ginv(S) %*% T_C
        }
        
        ## Add sqrt of MD to output
        ## Not efficient; room for improvement
        sqSmds <- c(sqSmds, sqMD)
      }
    }
  }
  ## We want it on the original scale
  return(list(smd = sqrt(sqSmds), vcov = S))
}

check_balance_adj <- function(Z, match_object, cov_list, plot_propens, propens){
  
  dt <- match_object$data_with_matched_set_ind
  dt_matched = dt
  Z_matched = Z[!is.na(dt_matched$matched_set)]
  
  # if categorical
  cate_cov_list = cov_list[which(sapply(dt[,cov_list],is.numeric) == F)]
  if(length(cate_cov_list) != 0){
    
    smd_before_bin <- numeric(length(cate_cov_list))
    smd_after_bin <- numeric(length(cate_cov_list))
    pooled_vcov_lst <- list()
    
    # Compute before-matching balance statistics
    for(i in seq_along(cate_cov_list)){
      lstMeans <- list(colMeans(model.matrix(~., data = data.frame(dt[Z == 1, cate_cov_list[i]]))[,-1]),
                       colMeans(model.matrix(~., data = data.frame(dt[Z == 0, cate_cov_list[i]]))[,-1]))
      
      smd_res <- StdDiffFromLstMeans(lstMeans)
      smd_before_bin[i] <- smd_res$smd
      pooled_vcov_lst[[i]] <- smd_res$vcov
    }
    
    # Compute after-matching balance statistics
    dt_matched_cate = dt_matched[!is.na(dt_matched$matched_set), cate_cov_list]
    
    for(j in seq_along(cate_cov_list)){
      lstMeans <- list(colMeans(model.matrix(~., data = data.frame(dt_matched_cate[Z_matched == 1, cate_cov_list[j]]))[,-1]),
                       colMeans(model.matrix(~., data = data.frame(dt_matched_cate[Z_matched == 0, cate_cov_list[j]]))[,-1]))
      
      smd_res <- StdDiffFromLstMeans(lstMeans, S = pooled_vcov_lst[[j]])
      smd_after_bin[j] <- smd_res$smd
    }
    
    # dt_cate_cov_list <- lapply(as.data.frame(dt[,cate_cov_list]),as.factor)
    # names(dt_cate_cov_list) <- cate_cov_list
  }
  
  num_cov_list = cov_list[!cov_list %in% cate_cov_list]
  # Compute before-matching balance statistics
  dt_treated_before_num <- dt[Z==1, num_cov_list]
  dt_control_before_num <- dt[Z==0, num_cov_list]
  
  mean_treated_before_num = sapply(dt_treated_before_num, mean) # `sapply()`instead of `apply`
  mean_control_before_num = sapply(dt_control_before_num, mean)
  
  mean_diff_before_num = abs(mean_treated_before_num - mean_control_before_num)
  
  sd_treated_before_num = sapply(dt_treated_before_num, stats::sd)
  sd_control_before_num = sapply(dt_control_before_num, stats::sd)
  
  pooled_sd_num = sqrt((sd_treated_before_num^2 + sd_control_before_num^2)/2) # corrected math
  
  std_before_num = mean_diff_before_num/pooled_sd_num
  
  # Compute after-matching balance statistics
  dt_matched_num = dt_matched[!is.na(dt_matched$matched_set), num_cov_list]
  dt_treated_after_num <- dt_matched_num[Z_matched==1, ]
  dt_control_after_num <- dt_matched_num[Z_matched==0, ]
  
  mean_treated_after_num = sapply(dt_treated_after_num, mean)
  mean_control_after_num = sapply(dt_control_after_num, mean)
  
  mean_diff_after_num = abs(mean_treated_after_num - mean_control_after_num)
  
  std_after_num = mean_diff_after_num/pooled_sd_num
  
  # combine cate and num
  mean_treated_before = c(mean_treated_before_num, rep(NA, length(cate_cov_list)))
  mean_control_before = c(mean_control_before_num, rep(NA, length(cate_cov_list)))
  std_before = c(std_before_num, smd_before_bin)
  mean_control_after = c(mean_control_after_num, rep(NA, length(cate_cov_list)))
  std_after = c(std_after_num, smd_after_bin)
  
  # Tabulate all results
  balance_table = data.frame(mean_treated_before, mean_control_before, std_before,
                             mean_control_after, std_after)
  rownames(balance_table) <- c(num_cov_list, cate_cov_list)
  colnames(balance_table) <- c('Z = 1', 'Z = 0 (Bef)',
                               'Std. Diff (Bef)',
                               'Z = 0 (Aft)',
                               'Std. Diff (Aft)')
  
  # Plot the propensity score distribution if plot_propens == TRUE
  if (plot_propens) {
    propens_treated = propens[Z == 1]
    propens_all_control = propens[Z == 0]
    propens_matched_control = propens[Z == 0 & (!is.na(match_object$data_with_matched_set_ind$matched_set))]
    
    
    group = c(rep('Treated', length(propens_treated)),
              rep('All controls', length(propens_all_control)),
              rep('Matched controls', length(propens_matched_control)))
    
    
    propensity = c(propens_treated, propens_all_control, propens_matched_control)
    
    propens_df = data.frame(group, propensity)
    pt = ggplot2::ggplot(data = propens_df, ggplot2::aes(x = propensity, color = group)) +
      ggplot2::geom_density(size = 1.5) +
      ggplot2::theme_bw(base_size = 20) +
      ggplot2::theme(legend.position = 'top')
    
    pt_2 = ggplot2::ggplot(data = propens_df, ggplot2::aes(x = propensity, color = group)) +
      ggplot2::geom_freqpoly(size = 1.5) +
      ggplot2::theme_bw(base_size = 20) +
      ggplot2::theme(legend.position = 'top')
  }
  
  return(list(balance_table = balance_table, pt = pt, pt_2 = pt_2))
}

## extract percentage
extract_perc <- function(x){
  out <- as.numeric(unlist(strsplit(x,split = "[()]"))[2])/100
  out
}

## calculate smd for each level in cate covars
smd_level <- function(x){
  
  p_0_bef <- extract_perc(out_tb[x, "0(bef)"])
  p_0_aft <- extract_perc(out_tb[x, "0(aft)"])
  p_1 <- extract_perc(out_tb[x, "1"])
  
  pooled_sd <- sqrt(sum(p_1*(1-p_1),p_0_bef*(1-p_0_bef))/2)
  
  smd_bef <- round(abs(p_1 - p_0_bef)/pooled_sd, 3)
  smd_aft <- round(abs(p_1 - p_0_aft)/pooled_sd, 3)
  
  return(c(smd_bef, smd_aft))
}

## smd_aft for cts covars
smd_aft <- function(x){
  cov_name <- unlist(strsplit(rownames(out_tb)[x], split = " "))[1]
  
  sd_t <- tb_bef$ContTable$`1`[cov_name,'sd']
  sd_c <- tb_bef$ContTable$`0`[cov_name,'sd']
  
  pooled_sd <- sqrt(sum(sd_t^2, sd_c^2)/2)
  
  mean_c <- tb_aft$ContTable$`0`[cov_name,'mean']
  mean_t <- tb_bef$ContTable$`1`[cov_name,'mean']
  
  smd <- round(abs(mean_t - mean_c)/pooled_sd, 3)
  
  return(smd)
}


compute_keywords_dist <- function(X_control, X_treated_i){
  
  ## only comparing primary keyword
  compute_keywords_dist_pairwise <- function(x, X_treated_i){
    d = 0
    if (sum(c(X_treated_i[1], x[1]) %in% "'not report'") == 1) d = 1
    else if (sum(c(X_treated_i[1], x[1]) %in% "'not report'") == 2) d = 0
    else if (X_treated_i[1] == x[1]) d = 0
    else if (sum(X_treated_i[c(1,2)] %in% x[c(1,2)]) == 2) d = 1
    else if (sum(X_treated_i[c(1,2)] %in% x[c(1,2)]) == 1) d = 2
    else d = 3
    return(d)
  }
  return(apply(X_control, 1 , compute_keywords_dist_pairwise, X_treated_i = X_treated_i))
}
