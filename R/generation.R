#' Generate an Image with FLUX 1.1 Pro
#'
#' Submits a request to generate an image using the FLUX 1.1 [Pro] API endpoint.
#' The function allows customization of the image generation through several parameters such as prompt,
#' image dimensions, and other options like upsampling, seed, and output format.
#'
#' @param prompt A character string representing the text prompt for image generation. This should
#'   describe the desired image in detail.
#' @param image_prompt An optional base64 encoded image that can be used with Flux Redux. Default is
#'   NULL.
#' @param width An integer specifying the width of the generated image in pixels. Must be between
#'   256 and 1440, and a multiple of 32. Default is 1024.
#' @param height An integer specifying the height of the generated image in pixels. Must be between
#'   256 and 1440, and a multiple of 32. Default is 768.
#' @param prompt_upsampling A boolean indicating whether to perform upsampling on the prompt.
#'   Default is FALSE.
#' @param seed An optional seed for reproducibility of the generated image. Default is NULL.
#' @param safety_tolerance An integer between 0 and 6 specifying the tolerance level for input and
#'   output moderation. 0 is most strict, and 6 is least strict. Default is NULL.
#' @param output_format A character string specifying the output format of the generated image. Can
#'   be 'jpeg' or 'png'. Default is NULL.
#' @param output Determines whether the function returns the request, response, or result. If
#' @param follow_url Logical. If `TRUE` and output = 'result', then the generated image will in your
#'  browser.
#' @param download_path If `TRUE` and output = 'result', then passing a path to this argument will
#'   download the file to that specified path.
#'
#' @return Either an `httr2` response or request object, or the JSON body of the finished result.
#' @export
#'
#' @examples
#' \dontrun{
#' gen_flux_pro1.1(
#'   prompt = "A serene landscape with mountains in the background and a calm lake in the foreground.",
#'   seed = 1
#'  )
#' }
gen_flux_pro1.1 <- function(
    prompt,
    image_prompt = NULL,
    width = NULL,
    height = NULL,
    prompt_upsampling = NULL,
    seed = NULL,
    saftey_tolerance = NULL,
    output_format = NULL,
    output = c("request", "response", "result"),
    api_key = Sys.getenv("BFL_API_KEY"),
    follow_url = TRUE,
    download_path = NULL
) {

  output <- rlang::arg_match(output)

  url_path <- "https://api.bfl.ml/v1/flux-pro-1.1"

  params <- list(
    prompt = prompt,
    image_prompt = image_prompt,
    width = width,
    height = height,
    prompt_upsampling = prompt_upsampling,
    seed = seed,
    safety_tolerance = saftey_tolerance,
    output_format = output_format
  ) |>
    purrr::compact()

  req <- httr2::request(
    url_path
  ) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      `X-Key` = api_key
    ) |>
    httr2::req_body_json(
      params
    )

  if (output == "request") return(req)

  resp <- httr2::req_perform(req)

  if (output == "response") return(resp)

  id <- httr2::resp_body_json(resp)$id

  # Poll the 'get-result' endpoint until image is ready
  repeat_until(\() get_result_status(id), "Ready")

  res <- get_result(id, output = "json")

  if (follow_url) {
    sample_url <- res$result$sample
    browseURL(sample_url)
  }

  if (!is.null(download_path)) {
    download.file(url = sample_url, destfile = download_path)
  }

  res

}


#' Generate an Image with FLUX Pro
#'
#' Submits a request to generate an image using the FLUX 1.1 [Pro] API endpoint.
#' The function allows customization of the image generation through several parameters such as prompt,
#' image dimensions, and other options like upsampling, seed, and output format.
#'
#' @param prompt A character string representing the text prompt for image generation. This should
#'   describe the desired image in detail.
#' @param image_prompt An optional base64 encoded image that can be used with Flux Redux. Default is
#'   NULL.
#' @param width An integer specifying the width of the generated image in pixels. Must be between
#'   256 and 1440, and a multiple of 32. Default is 1024.
#' @param height An integer specifying the height of the generated image in pixels. Must be between
#'   256 and 1440, and a multiple of 32. Default is 768.
#' @param steps Integer. Number of steps for the image generation process between 1 and 50.
#' @param prompt_upsampling A boolean indicating whether to perform upsampling on the prompt.
#'   Default is FALSE.
#' @param seed An optional seed for reproducibility of the generated image. Default is NULL.
#' @param guidance Guidance scale for image generation. High guidance scales improve prompt
#'   adherence at the cost of reduced realism. Number between 1.5 and 5.
#' @param safety_tolerance An integer between 0 and 6 specifying the tolerance level for input and
#'   output moderation. 0 is most strict, and 6 is least strict. Default is NULL.
#' @param interval Interval parameter for guidance control. Number between 1 and 4.
#' @param output_format A character string specifying the output format of the generated image. Can
#'   be 'jpeg' or 'png'. Default is NULL.
#' @param output Determines whether the function returns the request, response, or result. If
#' @param follow_url Logical. If `TRUE` and output = 'result', then the generated image will in your
#'  browser.
#' @param download_path If `TRUE` and output = 'result', then passing a path to this argument will
#'   download the file to that specified path.
#'
#' @return Either an `httr2` response or request object, or the JSON body of the finished result.
#' @export
#'
#' @examples
#' \dontrun{
#' gen_flux_pro(
#'   prompt = "A serene landscape with mountains in the background and a calm lake in the foreground.",
#'   seed = 1
#'  )
#' }
gen_flux_pro <- function(
    prompt,
    image_prompt = NULL,
    width = NULL,
    height = NULL,
    steps = NULL,
    prompt_upsampling = NULL,
    seed = NULL,
    guidance = NULL,
    saftey_tolerance = NULL,
    interval = NULL,
    output_format = NULL,
    output = c("request", "response", "result"),
    api_key = Sys.getenv("BFL_API_KEY"),
    follow_url = TRUE,
    download_path = NULL
) {

  output <- rlang::arg_match(output)

  url_path <- "https://api.bfl.ml/v1/flux-pro"

  params <- list(
    prompt = prompt,
    image_prompt = image_prompt,
    width = width,
    height = height,
    prompt_upsampling = prompt_upsampling,
    seed = seed,
    safety_tolerance = saftey_tolerance,
    output_format = output_format
  ) |>
    purrr::compact()

  req <- httr2::request(
    url_path
  ) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      `X-Key` = api_key
    ) |>
    httr2::req_body_json(
      params
    )

  if (output == "request") return(req)

  resp <- httr2::req_perform(req)

  if (output == "response") return(resp)

  id <- httr2::resp_body_json(resp)$id

  # Poll the 'get-result' endpoint until image is ready
  repeat_until(\() get_result_status(id), "Ready")

  res <- get_result(id, output = "json")

  if (follow_url) {
    sample_url <- res$result$sample
    browseURL(sample_url)
  }

  if (!is.null(download_path)) {
    download.file(url = sample_url, destfile = download_path)
  }

  res

}
