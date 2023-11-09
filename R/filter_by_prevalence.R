#' Filter data by Prevalence
#'
#' Filter data based on the prevalence of a categorical variable related to another categorical variable, considering a specified limit and direction.
#'
#' @param data A data frame containing the data to be filtered.
#' @param group_var The name of the categorical variable used for grouping.
#' @param count_var The name of the categorical variable whose prevalence is to be calculated related to group_var.
#' @param limit The limit value for the prevalence. Must be a valid number lower than 1.
#' @param at_least Logical value indicating whether to filter for values of prevalence at least greater than or equal to the limit (TRUE) or values less than the limit (FALSE). Default is TRUE.
#'
#' @return A data frame containing the filtered data based on prevalence.
#'
#' @examples
#' library(dplyr)
#' data <- data.frame(group_var = c("A", "B", "A", "B", "C"),
#'                     count_var = c("G1", "G2", "G2", "G2", "G2"))
#'  result <- data %>% filter_by_prevalence(group_var, count_var, limit = 0.5, at_least = FALSE)
#'
#' @importFrom dplyr group_by summarise filter
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @export
filter_by_prevalence <- function(data, group_var, count_var, limit, at_least = TRUE) {
  gv <- dplyr::enquo(group_var)
  cv <- dplyr::enquo(count_var)

  gv_name <- dplyr::quo_name(gv)
  cv_name <- dplyr::quo_name(cv)

   # Check if group_var and count_var are valid columns in the data
  if (!(gv_name %in% names(data)) || !(cv_name %in% names(data))) {
    stop("group_var or count_var is not a valid column in the data.")
  }
  
  # Check if group_var and count_var are categorical variables
  if (!is.character(data[[gv_name]]) || !is.character(data[[cv_name]])) {
    stop("group_var and count_var must be categorical variables of type string.")
    }
    
  # Check if limit is a valid number lower than 1
  if (!is.numeric(limit) || is.na(limit) || limit <= 0 || limit >= 1) {
    stop("limit must be a valid number lower than 1.")
  }

  # Check if at_least is boolean
  if (!is.logical(at_least)) {
    stop("at_least must be a boolean.")
  }

  data <- na.omit(data)
  attr(data, "na.action") <- NULL

  # check if dataset contains rows
  if (nrow(data) == 0) {
    stop("Dataset contains no rows (after removing rows with missing values).")
  }
  
  prev <- data %>% calculate_prevalence(!!gv, !!cv)

  if (at_least){
    filtered <- dplyr::filter(prev, proportion >= limit) %>% dplyr::pull(!!cv)
  }
  else {
    filtered <- dplyr::filter(prev, proportion < limit) %>% dplyr::pull(!!cv)
  }
 
  filtered_dataset <- data %>% dplyr::filter(!!cv %in% filtered)

  return(as.data.frame(filtered_dataset))
}
