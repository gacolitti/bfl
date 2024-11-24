#' Get Result from BFL API
#'
#' This function sends a GET request to the BFL API to retrieve the result associated with a specific ID.
#'
#' @param id A character string representing the unique identifier for the result to be retrieved.
#' @param x_key API key. By default, this is retrieved from the environment variable `BFL_API_KEY`.
#'
#' @return The response object from the API request.
#'
#' @examples
#' \dontrun{
#' # Example usage of the function
#' result <- get_result(id = "12345")
#' }
#'
get_result <- function(id, x_key = Sys.getenv("BFL_API_KEY")) {
  req <- httr2::request(
    "https://api.bfl.ml/v1/get_result"
  ) |>
    httr2::req_method("GET") |>
    httr2::req_headers(
      `X-Key` = x_key
    ) |>
    httr2::req_url_query(id = id)

  httr2::req_perform(req)
}
