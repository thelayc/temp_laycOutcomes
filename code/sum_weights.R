#' sum_weights()
#'
#' This is a helper function that sums question weights from surveys to compute a total score. The data comes from the following ETO Results report: "[Admin] raw_touchpoint_report_detailed".
#' @param tp_data dataframe: a dataframe containing touchpoint data."[Admin] raw_touchpoint_report_detailed"
#' @param var character vector: column names to keep for calculations
#' @param weight_id character: pattern that identifies the question to be included in the sum of weights. Can be  a regular expression.
#' @return dataframe
#' @export
#' @import dplyr
#' @examples
#' tp <- laycUtils::load_txt('./my_data_folder/touchpoints.txt')
#' tp <- laycUtils::format_data(tp)
#' tp <- tp[tp$tp_name == "jrt pre-/post test", ]
#' 
#' sum_weights(tp_data = tp)

sum_weights <- function(tp_data, 
                        var = c('subject_id', 'date', 'answer_id', 'question_id', 
                                'question_short', 'answer_weight'),
                        weight_var = 'answer_weight',
                        weight_id = '^q',
                        group_var = c('subject_id', 'answer_id')) {
  
  # Keep only relevant columns
  tp_data <- tp_data[ , colnames(tp_data) %in% var]
  
  # Identify weigthed question to be summed & create a subset of tp_data
  keep <- stringr::str_detect(tp_data$question_short, '^q')
  to_sum <- tp_data[keep, ]
  to_sum[ , weight_var] <- as.numeric(to_sum[ , weight_var])
  
  # Compute score for each unique combination of subject_id & answer_id
  # Code from http://stackoverflow.com/questions/21208801/group-by-multiple-columns-in-dplyr-using-string-vector-input
  dots <- lapply(group_var, as.symbol) 
  to_sum %>%
    group_by_(.dots=dots) %>%
    mutate_(score = ~sum(answer_weight, na.rm = TRUE)) %>%
    ungroup() %>%
    select_(.dots = list(quote(subject_id), quote(answer_id), quote(score))) %>%
    distinct ->
    to_sum
  
  # Merge scores with original dataset
  tp_data %>%
    left_join(to_sum, by = group_var) ->
    tp_data
    
  # Return dataframe
  return(tp_data)
}