#' Get Upper-air (TEMP) Data
#'
#' Retrieves upper-air observation data (Radiosonde/Rawinsonde - TEMP).
#' Corresponds to KMA API '레윈존데관측' (upp_temp.php).
#' Response is typically CSV-like text. Time is in UTC.
#'
#' @param tm Observation time in 'YYYYMMDDHHMM' or 'YYYYMMDDHH' format (UTC).
#' @param stn Station ID. "0" for all. Can be partial for list (e.g., "47").
#' @param pa Pressure level (hPa). "0" or missing for all levels.
#' @param help_text If "1", adds help text. Default "0".
#' @return Character string containing the raw API response.
#' @export
get_upper_air_temp_data <- function(tm, stn = "0", pa = "0", help_text = "0") {
  if (!grepl("^\d{10,12}$", tm)) { # YYYYMMDDHH or YYYYMMDDHHMM
    stop("Parameter 'tm' must be in YYYYMMDDHH(MM) format (UTC).", call. = FALSE)
  }
  base_url <- "https://apihub.kma.go.kr/api/typ01/url/upp_temp.php"
  params <- list(tm = tm, stn = stn, pa = pa, help = help_text)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "text", encoding = "UTF-8"))
}

#' Get Sea K-Ship Upper-air (TEMP) Data
#'
#' Retrieves upper-air observation data from K-Ship (Radiosonde/Rawinsonde - TEMP).
#' Corresponds to KMA API '기상1호(레윈존데)' (sea_kship_temp.php).
#' Response is typically CSV-like text. Time is in UTC.
#'
#' @inheritParams get_upper_air_temp_data
#' @export
get_upper_air_sea_kship_temp_data <- function(tm, stn = "0", pa = "0", help_text = "0") {
  if (!grepl("^\d{10,12}$", tm)) {
    stop("Parameter 'tm' must be in YYYYMMDDHH(MM) format (UTC).", call. = FALSE)
  }
  base_url <- "https://apihub.kma.go.kr/api/typ01/url/sea_kship_temp.php"
  params <- list(tm = tm, stn = stn, pa = pa, help = help_text)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "text", encoding = "UTF-8"))
}

#' Get Mobile Upper-air (TEMP) Data
#'
#' Retrieves mobile upper-air observation data (Radiosonde/Rawinsonde - TEMP).
#' Corresponds to KMA API '이동기상관측(레윈존데)' (upp_mbl_temp.php).
#' Response is typically CSV-like text. Time is in UTC.
#'
#' @inheritParams get_upper_air_temp_data
#' @export
get_upper_air_mobile_temp_data <- function(tm, stn = "0", pa = "0", help_text = "0") {
  if (!grepl("^\d{10,12}$", tm)) {
    stop("Parameter 'tm' must be in YYYYMMDDHH(MM) format (UTC).", call. = FALSE)
  }
  base_url <- "https://apihub.kma.go.kr/api/typ01/url/upp_mbl_temp.php"
  params <- list(tm = tm, stn = stn, pa = pa, help = help_text)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "text", encoding = "UTF-8"))
}

#' Get Rawinsonde Maximum Altitude Data
#'
#' Retrieves Rawinsonde maximum altitude data for a specified period.
#' Corresponds to KMA API '국내 레윈존데 최대고도 자료 조회' (upp_raw_max.php).
#' Response is typically CSV-like text.
#'
#' @param tm1 Start date in 'YYYYMMDD' format.
#' @param tm2 End date in 'YYYYMMDD' format.
#' @param stn Station ID. "0" for all (excluding Osan, Gwangju).
#' @param help_text If "1", adds help text. Default "0".
#' @return Character string containing the raw API response.
#' @export
get_upper_air_raw_max_altitude <- function(tm1, tm2, stn = "0", help_text = "0") {
  if (!grepl("^\d{8}$", tm1)) stop("Parameter 'tm1' must be in YYYYMMDD format.", call. = FALSE)
  if (!grepl("^\d{8}$", tm2)) stop("Parameter 'tm2' must be in YYYYMMDD format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ01/url/upp_raw_max.php"
  params <- list(tm1 = tm1, tm2 = tm2, stn = stn, help = help_text)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "text", encoding = "UTF-8"))
}

#' Get Rawinsonde Analysis Data (Stability, etc.)
#'
#' Retrieves Rawinsonde analysis data (stability indices, etc.) for a period.
#' Corresponds to KMA API '국내 레윈존데 분석자료(안정도 등) 조회' (upp_idx.php).
#' Response is typically CSV-like text. Time is in UTC.
#'
#' @param tm1 Start time in 'YYYYMMDDHH' or 'YYYYMMDDHHMM' format (UTC).
#' @param tm2 End time in 'YYYYMMDDHH' or 'YYYYMMDDHHMM' format (UTC).
#' @param stn Station ID. "0" for all.
#' @param help_text If "1", adds help text. Default "0".
#' @return Character string containing the raw API response.
#' @export
get_upper_air_analysis_data <- function(tm1, tm2, stn = "0", help_text = "0") {
  if (!grepl("^\d{10,12}$", tm1)) stop("Parameter 'tm1' must be in YYYYMMDDHH(MM) format (UTC).", call. = FALSE)
  if (!grepl("^\d{10,12}$", tm2)) stop("Parameter 'tm2' must be in YYYYMMDDHH(MM) format (UTC).", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ01/url/upp_idx.php"
  params <- list(tm1 = tm1, tm2 = tm2, stn = stn, help = help_text)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "text", encoding = "UTF-8"))
}

# --- UppMtlyInfoService Functions ---

#' Get Monthly Upper-air Report Notes
#'
#' Retrieves notes for monthly upper-air reports from UppMtlyInfoService.
#' Supports XML or JSON dataType.
#'
#' @param page_no Page number.
#' @param num_of_rows Number of rows per page.
#' @param data_type "XML" or "JSON".
#' @param year Year (YYYY).
#' @param month Month (MM, e.g., "09").
#' @return Character string containing the raw API response (XML or JSON).
#' @export
get_upper_air_monthly_report_notes <- function(page_no, num_of_rows, data_type, year, month) {
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/UppMtlyInfoService/getNote"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "text", encoding = "UTF-8"))
}

#' Get Upper-air Station List Table for Monthly Report
#' @inheritParams get_upper_air_monthly_report_notes
#' @export
get_upper_air_station_list_table <- function(page_no, num_of_rows, data_type, year, month) {
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/UppMtlyInfoService/getUppLstTbl"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Standard Isobaric Surface Values for Monthly Upper-air Report
#' @inheritParams get_upper_air_monthly_report_notes
#' @param station Station ID (e.g., "47102").
#' @export
get_upper_air_std_isobaric_values <- function(page_no, num_of_rows, data_type, year, month, station) {
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/UppMtlyInfoService/getStdIsbrsfValue"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month, station=station)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Tropopause and Maximum Wind Data for Monthly Upper-air Report
#' @inheritParams get_upper_air_monthly_report_notes
#' @param station Station ID. (Note: KMA docs are a bit ambiguous, but output implies station-specific data)
#' @export
get_upper_air_max_wind_data <- function(page_no, num_of_rows, data_type, year, month, station) {
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/UppMtlyInfoService/getMaxWind"
  # KMA docs show station in example for getStdIsbrsfValue, and stnId in output for getMaxWind.
  # For consistency and to allow potential filtering, including station parameter.
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month, station=station)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Temperature and Humidity Significant Levels for Monthly Upper-air Report
#' @inheritParams get_upper_air_std_isobaric_values
#' @export
get_upper_air_temp_humidity_levels <- function(page_no, num_of_rows, data_type, year, month, station) {
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/UppMtlyInfoService/getTaHmLevel"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month, station=station)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Wind Significant Levels for Monthly Upper-air Report
#' @inheritParams get_upper_air_std_isobaric_values
#' @export
get_upper_air_wind_levels <- function(page_no, num_of_rows, data_type, year, month, station) {
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/UppMtlyInfoService/getWindLevel"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month, station=station)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}
