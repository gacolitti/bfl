
#' Repeat a function until it returns expected value
#' @param func Function to run.
#' @param expected_result Expected result of `func` that ends loop of repeated evaluation.
#' @param sleep_time Optional amount of seconds to sleep between retries.
#' @param ... Other arguments passed to func.
#' @export
repeat_until <- function(func, expected_result, sleep_time = 1, ...) {
  result <- func(...)  # Initial function call
  while (!identical(result, expected_result)) {  # Check if result matches expected
    Sys.sleep(sleep_time)  # Optional: wait for a moment before retrying
    result <- func(...)  # Call the function again
  }
  return(result)  # Return the expected result
}


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
#' @export
get_result <- function(id, x_key = Sys.getenv("BFL_API_KEY"), output = c("json", "response", "request")) {

  output <- rlang::arg_match(output)

  req <- httr2::request(
    "https://api.bfl.ml/v1/get_result"
  ) |>
    httr2::req_method("GET") |>
    httr2::req_headers(
      `X-Key` = x_key
    ) |>
    httr2::req_url_query(id = id)

  if (output == "request") return(req)

  resp <- httr2::req_perform(req)

  if (output == "response") return(resp)

  httr2::resp_body_json(resp)
}

#' Get Result Status
#' @inheritParams get_result
#' @export
get_result_status <- function(id, x_key = Sys.getenv("BFL_API_KEY")) {
  get_result(id, x_key = x_key, output = "json")$status
}
