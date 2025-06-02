# Environment to store the API key
.kma_api_env <- new.env(parent = emptyenv())

#' Set KMA API Authentication Key
#'
#' Stores the KMA API authentication key for the current session.
#' It is recommended to set this key once per session or store it
#' in your .Renviron file as KMA_API_KEY.
#'
#' @param auth_key Your KMA API authentication key (character string).
#' @return Invisibly returns the authentication key.
#' @export
#' @examples
#' \dontrun{
#' set_kma_auth_key("YOUR_ACTUAL_API_KEY")
#' }
set_kma_auth_key <- function(auth_key) {
  if (!is.character(auth_key) || length(auth_key) != 1 || nchar(auth_key) == 0) {
    stop("auth_key must be a non-empty character string.", call. = FALSE)
  }
  .kma_api_env$auth_key <- auth_key
  invisible(auth_key)
}

#' Get KMA API Authentication Key
#'
#' Retrieves the KMA API authentication key stored for the current session.
#' It first checks an environment variable KMA_API_KEY, then the session-specific storage.
#'
#' @return The API authentication key as a character string.
#' @keywords internal
get_kma_auth_key <- function() {
  auth_key <- Sys.getenv("KMA_API_KEY")
  if (nchar(auth_key) > 0) {
    return(auth_key)
  }
  if (exists("auth_key", envir = .kma_api_env)) {
    return(.kma_api_env$auth_key)
  }
  stop("KMA API authentication key not found. \n",
       "Please set it using set_kma_auth_key() or ",
       "by setting the KMA_API_KEY environment variable.", call. = FALSE)
}

#' Make a Request to KMA API
#'
#' Internal function to construct and execute GET requests to the KMA API.
#'
#' @param base_url The base URL for the API endpoint (character string).
#' @param params A list of query parameters for the API request.
#'        The authKey will be added automatically.
#' @param timeout_seconds Timeout for the request in seconds. Default is 30.
#' @return The content of the response, typically as text.
#'         Specific parsing (XML, JSON, binary) should be handled by calling functions.
#' @importFrom httr GET add_headers content stop_for_status user_agent timeout
#' @importFrom utils packageVersion
#' @keywords internal
make_kma_request <- function(base_url, params = list(), timeout_seconds = 30) {
  if (!is.character(base_url) || length(base_url) != 1 || nchar(base_url) == 0) {
    stop("base_url must be a non-empty character string.", call. = FALSE)
  }
  if (!is.list(params)) {
    stop("params must be a list.", call. = FALSE)
  }

  # Add authentication key to parameters
  params$authKey <- get_kma_auth_key()

  # Filter out NULL parameters
  params <- Filter(Negate(is.null), params)

  # Ensure all params are character or coercible to character
  # KMA API typically expects string parameters
  params <- lapply(params, as.character)

  # Construct User-Agent string
  pkg_version <- "0.0.0.9000" # Default if packageVersion fails (e.g. during dev)
  tryCatch({
    pkg_version <- utils::packageVersion("KMAapiR")
  }, error = function(e) {
    # Keep default version if package is not installed (common during dev)
  })
  ua_string <- paste0("KMAapiR/", pkg_version,
                      " (R ", getRversion(), "; https://github.com/your-username/KMAapiR)")
                      # Replace with actual repo URL later

  response <- httr::GET(
    url = base_url,
    query = params,
    httr::user_agent(ua_string),
    httr::timeout(timeout_seconds)
  )

  # Check for HTTP errors
  httr::stop_for_status(response)

  return(response) # Return the full response object
}
