#' Filter Dataset by Prevalence
#'
#' Filter a dataset based on the prevalence of a categorical variable related to another categorical variable, considering a specified limit and direction.
#'
#' @param dataset A data frame containing the dataset to be filtered.
#' @param group_var The name of the categorical variable used for grouping.
#' @param count_var The name of the categorical variable used for counting.
#' @param limit The limit value for the prevalence. Must be a valid number lower than 1.
#' @param at_least Logical value indicating whether to filter for values at least greater than or equal to the limit (TRUE) or values less than the limit (FALSE). Default is TRUE.
#'
#' @return A data frame containing the filtered dataset based on the prevalence filtering.
#'
#' @examples
#' library(dplyr)
#' data <- data.frame(group_var = c("A", "B", "A", "B"),
#'                    count_var = c("F1", "F1", "G1", "G1"))
#' filtered_data <- data %>% pRevalence::calculate_prevalence(group_var = "group_var", count_var = "count_var")
#' filter_by_prevalence(data, group_var = "group_var", count_var = "count_var", limit = 0.4, at_least = TRUE)
#'
#' @importFrom dplyr group_by summarise filter
#' @importFrom magrittr %>%
#' @export
filter_by_prevalence <- function(dataset, group_var, count_var, limit, at_least = TRUE) {
  # Check if group_var and count_var are valid columns in the dataset
  if (!(group_var %in% names(dataset)) || !(count_var %in% names(dataset))) {
    stop("group_var or count_var is not a valid column in the dataset.")
  }
  
  # Check if group_var and count_var are categorical variables
  if (!is.character(dataset[[group_var]]) || !is.character(dataset[[count_var]])) {
    stop("group_var and count_var must be categorical variables.")
    }
    
  # Check if limit is a valid number lower than 1
  if (!is.numeric(limit) || is.na(limit) || limit <= 0 || limit >= 1) {
    stop("limit must be a valid number lower than 1.")
  }

  # Check if at_least is boolean
  if (!is.logical(at_least)) {
    stop("at_least must be a boolean.")
  }
  
  prevalence <- dataset %>% pRevalence::calculate_prevalence(group_var = group_var, count_var = count_var)
  
  # Filter the dataset based on the limit direction
  if (at_least) {
    filtered_dataset <- dataset %>%
      dplyr::filter({{group_var}} %in% prevalence[dataset$proportion >= limit, {{group_var}}])
  } else {
    filtered_dataset <- dataset %>%
      dplyr::filter({{group_var}} %in% prevalence[dataset$proportion < limit, {{group_var}}])
  }
  
  return(as.data.frame(filtered_dataset))
}
