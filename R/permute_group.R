#' Unstratified group permutation
#'
#' This function takes a data frame and group and outcome column name as input
#' and returns the dataframe with the group column randomly permuted
#'
#' @param df A data frame
#' @param group_col The name of the column in df that corresponds to the group label
#' @param strat_col The name of the column in df that corresponds to the strata
#' @param seed An integer seed value
#' @param return_perm Boolean, return the permuted indices if TRUE
#' @return The inputted data frame with the group column randomly shuffled
#' @export
permute_group <- function(df, group_col, strata_col=NULL,
                          seed=NULL, return_perm=FALSE){
  if(is.null(seed) == FALSE){
    set.seed(seed)
  }

  if(is.null(strata_col) == FALSE){
    warning("For stratified permutation, use function strat_permute_group")
  }

  perm_df <- df
  # Sample the indices of the group column
  permuted_indices <- sample(seq_along(perm_df[[group_col]]), length(perm_df[[group_col]]), replace = FALSE)

  # Reorder the group column based on the permuted indices
  perm_df[[group_col]] <- df[[group_col]][permuted_indices]

  if(return_perm){
    return_perm_value <- permuted_indices
  }else{
    return_perm_value <- NULL
  }

  return(list(perm_df=perm_df, return_perm_value=return_perm_value))
}

#' Stratified group permutation
#'
#' This function takes a data frame and group and outcome column name as input
#' and returns the dataframe with the group column randomly permuted
#'
#' @param df A data frame
#' @param group_col The name of the column in df that corresponds to the group label
#' @param strat_col The name of the column in df that corresponds to the strata
#' @param seed An integer seed value
#' @param return_perm Boolean, return the permuted indices if TRUE

#' @return The inputted data frame with the group column randomly shuffled
#' @export
strat_permute_group <- function(df, group_col, strata_col,
                                seed=NULL, return_perm=FALSE){
  if(is.null(seed) == FALSE){
    set.seed(seed)
  }

  strata_values <- unique(df[[strata_col]])
  perm_df <- df
  full_permuted_indices <- 1:length(df$group)

  for (s in strata_values) {
    # Get the indices of rows in the current stratum
    strata_indices <- which(df[[strata_col]] == s)

    # Sample the indices within this stratum
    permuted_indices <- sample(strata_indices, length(strata_indices), replace = FALSE)

    # Reorder the group column based on the permuted indices
    perm_df[[group_col]][strata_indices] <- df[[group_col]][permuted_indices]

    # Store the permuted indices
    full_permuted_indices[strata_indices] <- permuted_indices
  }

  if(return_perm){
    return_perm_value <- full_permuted_indices
  }else{
    return_perm_value <- NULL
  }

  return(list(perm_df=perm_df, return_perm_value=return_perm_value))
}

#' Sign permutation
#'
#' This function takes a data frame and group and outcome column name as input
#' and returns the dataframe with the group column replaced with randomly assigned signs
#'
#' @param df A data frame
#' @param group_col The name of the column in df that corresponds to the group label
#' @param strat_col The name of the column in df that corresponds to the strata
#' @param seed An integer seed value
#' @param return_perm Boolean, return the permuted indices if TRUE
#' @return The inputted data frame with the group column replaced with randomly assigned signs
#' @export
permute_sign <- function(df, group_col, strata_col=NULL,
                          seed=NULL, return_perm=FALSE){
  # Set seed if not NULL
  if(is.null(seed) == FALSE){
    set.seed(seed)
  }

  # Issue warning if strata inputted
  if(is.null(strata_col) == FALSE){
    warning("Stratified permutation not supported for this function")
  }

  # Permute sign
  perm_df <- df
  perm_sign <- sample(c(1, -1), size = length(df[[group_col]]), replace = T)
  perm_df[[group_col]] <- perm_sign

  if(return_perm){
    return_perm_value <- perm_sign
  }else{
    return_perm_value <- NULL
  }

  return(list(perm_df=perm_df, return_perm_value=return_perm_value))

}

