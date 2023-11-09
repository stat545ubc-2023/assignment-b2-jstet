#' Calculate Prevalence
#'
#' Calculate the prevalence of a categorical variable related to another categorical variable in a dataset.
#'
#' @param data A data frame containing the dataset.
#' @param group_var The column of thecategorical variable used for grouping.
#' @param count_var The column of the categorical variable whose prevalence is to be calculated related to group_var.
#' @return A data frame containing the prevalence calculation, including the count var, the count (the number of group_var count_var is represented in) and proportion.
#'
#' @examples
#' library(magrittr)
#' data <- data.frame(group_var = c("A", "B", "A", "B"),
#'                    count_var = c("F1", "G1", "G1", "G1"))
#' prevalence <- data %>% calculate_prevalence(group_var, count_var )
#'
#' @importFrom dplyr group_by summarise mutate n enquo quo_name distinct
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @export
calculate_prevalence <- function(data, group_var, count_var) {
  gv <- dplyr::enquo(group_var)
  cv <- dplyr::enquo(count_var)

 gv_name <- dplyr::quo_name(gv)
 cv_name <- dplyr::quo_name(cv)

   # Check if group_var and count_var are valid columns in the dataset
  if (!(gv_name %in% names(data)) || !(cv_name %in% names(data))) {
    stop("group_var or count_var is not a valid column in the dataset.")
  }
  
  # Check if group_var and count_var are categorical variables
  if (!is.character(data[[gv_name]]) || !is.character(data[[cv_name]])) {
    stop("group_var and count_var must be categorical variables of type string.")
    }

  data <- na.omit(data)

  # check if dataset contains rows
  if (nrow(data) == 0) {
    stop("Dataset contains no rows (after removing rows with missing values).")
  }

unique_count <- data %>% dplyr::distinct(!!gv) %>% nrow()
  
prevalence <- data %>%
  dplyr::group_by(!!gv, !!cv) %>%
  dplyr::summarize(count = dplyr::n(), .groups = "drop") 

prevalence <- prevalence %>%
  dplyr::group_by(!!cv) %>%
  dplyr::summarize(count = dplyr::n(), proportion = dplyr::n() / unique_count)
  
  return(as.data.frame(prevalence))
}