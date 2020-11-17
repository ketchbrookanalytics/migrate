#' Title
#'
#' @param data
#' @param rating
#' @param metric
#' @param method
#'
#' @return
#' @export
#'
#' @examples
build_matrix <- function(data, method = "console") {

  rating_start_sym <- data %>%
    dplyr::select(where(is.factor) & contains("start")) %>%
    colnames() %>%
    dplyr::sym()

  rating_end_sym <- data %>%
    dplyr::select(where(is.factor) & contains("end")) %>%
    colnames() %>%
    dplyr::sym()

  metric_sym <- data %>%
    dplyr::select(where(is.numeric)) %>%
    colnames() %>%
    dplyr::sym()

  row_names <- data %>%
    dplyr::pull(!! rating_start_sym) %>%
    unique() %>%
    sort()

  col_names <- data %>%
    dplyr::pull(!! rating_end_sym) %>%
    unique() %>%
    sort()

  vals <- data %>%
    dplyr::pull(!! metric_sym)

  vals <- ifelse(is.infinite(vals), NA, vals)

  matrix(
    vals,
    nrow = length(row_names),
    ncol = length(col_names),
    byrow = TRUE,
    dimnames = list(
      row_names,
      col_names
    )
  )

}
