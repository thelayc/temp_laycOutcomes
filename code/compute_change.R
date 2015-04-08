#' compute_change()
#'
#' This is a helper function that computes change in score between pre and post test. The data must be pre-processed by the followng functions: sum_weights() and id_prepost()
#' @param df dataframe: a dataframe returned by id_prepost()
#' @return dataframe
#' @export
#' @import dplyr
#' @examples
#' tp <- laycUtils::load_txt('./my_data_folder/touchpoints.txt')
#' tp <- laycUtils::format_data(tp)
#' tp <- tp[tp$tp_name == "jrt pre-/post test", ]
#' tp <- sum_weights(tp)
#' tp <- id_prepost(tp)
#' 
#' compute_change(df = tp)

compute_change <- function(df) {
  
  ## Compute change: post-score minus pre-score
  # Select only relevant columns
  out <- df[ , c('subject_id', 'prepost', 'score')]
  # Remove duplicates
  out <- unique(out)
  # Remove NAs
  out <- out[!is.na(out$prepost), ]
  # Sort rows
  out <- dplyr::arrange_(out, ~subject_id, ~desc(prepost)) # Add unit test to check that the ordering is correct
  # Group by subject_id in order to compute change for each participant
  out <- dplyr::group_by_(out, ~subject_id)
  # Remove participants without matching pre / post  
  out <-  dplyr::mutate_(out, n = ~length(subject_id))
  out <- dplyr::filter_(out, ~n > 1)
  # Compute change
  out <-  dplyr::mutate_(out, change = ~diff(score))
  # Keep only relevant information
  out <-  dplyr::ungroup(out)
  out <-  dplyr::select_(out, ~subject_id, ~change)
  out <-  dplyr::distinct_(out)
  
  # Add classification variable: positive, no change, negative
  out$change_ord[out$change > 0] <- 'positive'
  out$change_ord[out$change == 0] <- 'no change'
  out$change_ord[out$change < 0] <- 'negative'
  
  # Merge out with riginal dataset
  df <- left_join(df, out, by = 'subject_id')
  
  # Return dataframe
  return(df)
}