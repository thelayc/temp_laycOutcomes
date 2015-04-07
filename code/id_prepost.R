#' id_prepost()
#'
#' This is a helper function that identifies 'pre' and 'post' test based on the date the test was taken. The data comes from the following ETO Results report: "[Admin] raw_touchpoint_report_detailed".
#' @param tp_data dataframe: a dataframe containing touchpoint data."[Admin] raw_touchpoint_report_detailed"
#' @return dataframe
#' @export
#' @import dplyr
#' @examples
#' tp <- laycUtils::load_txt('./my_data_folder/touchpoints.txt')
#' tp <- laycUtils::format_data(tp)
#' tp <- tp[tp$tp_name == "jrt pre-/post test", ]
#' 
#' id_prepost(tp_data = tp)

id_prepost <- function(tp_data){
  
  # Identify when the test was taken for the first & last time
  tp_data %>%
    dplyr::group_by_(~subject_id) %>%
    mutate_(first = ~min(date),
            last = ~max(date)) ->
    tp_data
  
  # Assign pre / post values to first / last date taken 
  
  tp_data$prepost[tp_data$date == tp_data$first] <- 'pre'
  tp_data$prepost[tp_data$date == tp_data$last] <- 'post'
  
  # Return dataframe
  return(tp_data)
}







