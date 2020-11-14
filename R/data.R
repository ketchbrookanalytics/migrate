#' Mock dataset containing credit statistics by customer during two time intervals
#'
#' @source Developed by Ketchbrook Analytics
#' @format A data frame with columns:
#' \describe{
#'  \item{customer_id}{Customer ID for 500 unique customers.}
#'  \item{date}{Date of the observation; there are two unique dates in the dataset.}
#'  \item{risk_rating}{Factor representing risk level on a 1-14 scale.}
#'  \item{principal_balance}{Principal balance outstanding on the loan.}
#' }
#' @examples
#' \dontrun{
#'  mock_credit
#' }
"mock_credit"
