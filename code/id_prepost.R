#' id_prepost()
#'
#' This is a helper function that identifies 'pre' and 'post' test based on the date the test was taken. 
#' @param df dataframe: a dataframe containing longitudinal data.
#' @return dataframe
#' @export
#' @import dplyr
#' @examples
#' tp <- laycUtils::load_txt('./my_data_folder/touchpoints.txt')
#' tp <- laycUtils::format_data(tp)
#' tp <- tp[tp$tp_name == "jrt pre-/post test", ]
#' 
#' id_prepost(df = tp)

id_prepost <- function(df){
  
  # Identify when the test was taken for the first & last time
  df %>%
    dplyr::group_by_(~subject_id) %>%
    mutate_(first = ~min(date),
            last = ~max(date)) %>%
    ungroup() ->
    df
  
  # Assign pre / post values to first / last date taken 
  
  df$prepost[df$date == df$first] <- 'pre'
  df$prepost[df$date == df$last] <- 'post'
  
  # Return dataframe
  return(as.data.frame(df))
}







