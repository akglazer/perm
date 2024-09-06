#' Fisher combining function
#'
#' This function takes an array of p-values and returns a combined p-value using fisher's combining function:
#' \eqn{-2 \sum_i \log(p_i)}
#'
#' @param pvalues Array of p-values
#' @return Combined p-value using fisher's method
#' @export
fisher <- function(pvalues){
  # compute fisher combination
  value <- -2 * log(prod(pvalues))

  return(value)
}


#' Tippett combining function
#'
#' This function takes an array of p-values and returns a combined p-value using Tippett's combining function:
#' \eqn{\max_i \{1-p_i\}}
#'
#' @param pvalues Array of p-values
#' @return Combined p-value using Tippett's method
#' @export
tippett <- function(pvalues){
  # compute tippett
  value <- max(1-pvalues)

  return(value)
}


#' Liptak combining function
#'
#' This function takes an array of p-values and returns a combined p-value using Liptak's combining function:
#' \eqn{\sum_i \Phi^{-1}(1-p_i)} where \eqn{\Phi} is the CDF of the Normal distribution
#'
#' @importFrom stats qnorm
#' @param pvalues Array of p-values
#' @return Combined p-value using Liptak's method
#' @export
liptak <- function(pvalues){

  value <- sum(qnorm(1-pvalues))

  return(value)
}


#' Run NPC
#'
#' This function takes a data frame and group and outcome column names as input
#' and returns the NPC omnibus p-value
#'
#' @param df A data frame
#' @param group_col The name of the column in df that corresponds to the group label
#' @param outcome_cols The names of the columns in df that corresponds to the outcome variable
#' @param strata_col The name of the column in df that corresponds to the strata
#' @param test_stat Test statistic function
#' @param perm_func Function to permute group
#' @param combn Combining function method to use
#' @param shift Value of shift to apply in one- or two-sample problem
#' @param reps Number of iterations to use when calculating permutation p-value
#' @param perm_set Matrix of permutations to use instead of reps iterations of perm_func
#' @param complete_enum Boolean, whether to calculate P-value under complete enumeration of permutations
#' @param seed An integer seed value
#' @return A list containing the permutation test p-value, and the test statistic distribution if applicable
#' @export
npc <- function(df, group_col, outcome_cols, strata_col = NULL,
                test_stat = "diff_in_means",
                perm_func = permute_group,
                combn = "fisher",
                shift = 0,
                reps = 10000,
                perm_set = NULL,
                complete_enum = F,
                seed = NULL){

  # Check the combination function
  if(is.character(combn) && combn == "fisher"){
    combn_func <- fisher
  } else if(is.character(combn) && combn == "tippett"){
    combn_func <- tippett
  } else if(is.character(combn) && combn == "liptak"){
    combn_func <- liptak
  } else {
    combn_func <- combn
  }

  # Get matrix of p-values
  n_out <- length(outcome_cols)
  obs_p_value <- rep(NA, n_out)
  p_value_mat <- matrix(NA, nrow = reps, ncol = n_out)

  for(i in 1:n_out){
      output <- permutation_test(df = df, group_col = group_col,
                                 outcome_col = outcome_cols[i], strata_col = strata_col,
                                 test_stat = test_stat, perm_func = perm_func,
                                 shift = shift, reps = reps, perm_set = perm_set,
                                 complete_enum = complete_enum, alternative = "greater",
                                 return_test_dist = T, return_perm_dist = T,
                                 seed = seed)

      obs_p_value[i] <- output$p_value

    if(i == 1){
      perm_set <- output$perm_indices_mat
      reps <- nrow(perm_set)
    }
      #### VERIFY ####
      p_value_mat[,i] <- (reps - rank(output$test_stat_dist, ties.method = "min") + 1)/(reps+1)
  }
  # Get combined p-values
  combn_pvalues <- rep(NA, reps)
  obs_combn_pvalue <- combn_func(obs_p_value)

  for(j in 1:reps){
    combn_pvalues[j] <- combn_func(p_value_mat[j, ])
  }
  # Get omnibus p-values
  omnibus_p <- (sum(combn_pvalues >= obs_combn_pvalue)+1) / (reps+1)
  # Return omnibus p-value
  return(omnibus_p)
}

