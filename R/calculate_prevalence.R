#' Calculate Prevalence
#'
#' Calculate the prevalence of a categorical variable related to another categorical variable in a dataset.
#'
#' @param data A data frame containing the dataset.
#' @param group_var The name of the variable used for grouping.
#' @param count_var The name of the variable used for counting.
#'
#' @return A data frame containing the prevalence calculation, including the group variable, count and proportion.
#'
#' @examples
#' library(dplyr)
#' data <- data.frame(group_var = c("A", "B", "A", "B"),
#'                    count_var = c("F1", "F1", "G1", "G1"))
#' filtered_data <- data %>% pRevalence::calculate_prevalence(group_var = "group_var", count_var = "count_var")
#'
#' @importFrom dplyr group_by summarise mutate n 
#' @importFrom magrittr %>%
#' @export
calculate_prevalence <- function(data, group_var, count_var) {
  # Check if group_var and count_var are valid columns in the dataset
  if (!(group_var %in% names(data)) || !(count_var %in% names(data))) {
    stop("group_var or count_var is not a valid column in the dataset.")
  }
  
# Check if group_var and count_var are categorical variables
if (!is.character(data[[group_var]]) || !is.character(data[[count_var]])) {
  stop("group_var and count_var must be categorical variables.")
}

unique_count <- length(unique(data$group_var))

  
prevalence <- data %>%
  dplyr::group_by(group_var, count_var) %>%
  dplyr::summarize(count = n(), .groups = "drop") %>%
  dplyr::group_by(count_var) %>%
  dplyr::summarize(count = n(), proportion = n() / unique_count)
  
  
  return(as.data.frame(prevalence))
}