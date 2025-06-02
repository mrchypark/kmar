#' Get Hourly Surface Observation Data
#'
#' Retrieves hourly surface observation data from KMA for a specific time and station(s).
#' Corresponds to KMA API '시간자료' (kma_sfctm2.php).
#' The response is typically CSV-like text.
#'
#' @param tm Observation time in 'YYYYMMDDHHMM' format (KST).
#' @param stn Station ID(s). Use "0" for all stations. Multiple stations can be separated by ":" (e.g., "108:112").
#' @param help_text If "1", adds help text to the output. Default "0".
#' @return A character string containing the raw API response (likely CSV-like text).
#'         Further parsing may be needed.
#' @export
#' @examples
#' \dontrun{
#'   set_kma_auth_key("YOUR_API_KEY")
#'   hourly_data <- get_sfc_hourly_data(tm = "202310271000", stn = "108")
#'   print(hourly_data)
#' }
get_sfc_hourly_data <- function(tm, stn = "0", help_text = "0") {
  if (!grepl("^\d{12}$", tm)) {
    stop("Parameter 'tm' must be in YYYYMMDDHHMM format.", call. = FALSE)
  }
  base_url <- "https://apihub.kma.go.kr/api/typ01/url/kma_sfctm2.php"
  params <- list(
    tm = tm,
    stn = stn,
    help = help_text
  )
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "text", encoding = "UTF-8"))
}

#' Get Hourly Surface Observation Data for a Period
#'
#' Retrieves hourly surface observation data from KMA for a specified period and station(s).
#' Corresponds to KMA API '시간자료(기간 조회)' (kma_sfctm3.php).
#' The response is typically CSV-like text. Max 31 days.
#'
#' @param tm1 Start time for the period in 'YYYYMMDDHHMM' format (KST).
#' @param tm2 End time for the period in 'YYYYMMDDHHMM' format (KST).
#' @param stn Station ID(s). Use "0" for all stations. Multiple stations can be separated by ":"
#' @param help_text If "1", adds help text to the output. Default "0".
#' @return A character string containing the raw API response.
#' @export
#' @examples
#' \dontrun{
#'   set_kma_auth_key("YOUR_API_KEY")
#'   period_data <- get_sfc_hourly_data_period(tm1 = "202310260000", tm2 = "202310260500", stn = "108")
#'   print(period_data)
#' }
get_sfc_hourly_data_period <- function(tm1, tm2, stn = "0", help_text = "0") {
  if (!grepl("^\d{12}$", tm1)) {
    stop("Parameter 'tm1' must be in YYYYMMDDHHMM format.", call. = FALSE)
  }
  if (!grepl("^\d{12}$", tm2)) {
    stop("Parameter 'tm2' must be in YYYYMMDDHHMM format.", call. = FALSE)
  }
  base_url <- "https://apihub.kma.go.kr/api/typ01/url/kma_sfctm3.php"
  params <- list(
    tm1 = tm1,
    tm2 = tm2,
    stn = stn,
    help = help_text
  )
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "text", encoding = "UTF-8"))
}

#' Get Daily Surface Observation Data
#'
#' Retrieves daily surface observation data from KMA for a specific date and station(s).
#' Corresponds to KMA API '일자료' (kma_sfcdd.php).
#' The response is typically CSV-like text.
#'
#' @param tm Observation date in 'YYYYMMDD' format (KST).
#' @param stn Station ID(s). Use "0" for all stations.
#' @param disp Display option. "0" for CSV without spaces, "1" for fixed width. Default "0".
#' @param help_text If "1", adds help text to the output. Default "0".
#' @return A character string containing the raw API response.
#' @export
#' @examples
#' \dontrun{
#'   set_kma_auth_key("YOUR_API_KEY")
#'   daily_data <- get_sfc_daily_data(tm = "20231026", stn = "108")
#'   print(daily_data)
#' }
get_sfc_daily_data <- function(tm, stn = "0", disp = "0", help_text = "0") {
  if (!grepl("^\d{8}$", tm)) {
    stop("Parameter 'tm' must be in YYYYMMDD format.", call. = FALSE)
  }
  base_url <- "https://apihub.kma.go.kr/api/typ01/url/kma_sfcdd.php"
  params <- list(
    tm = tm,
    stn = stn,
    disp = disp,
    help = help_text
  )
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "text", encoding = "UTF-8"))
}

#' Get Daily Surface Observation Data for a Period
#'
#' Retrieves daily surface observation data for a specified period, observation type, and station(s).
#' Corresponds to KMA API '일자료(기간 조회)' (kma_sfcdd3.php).
#' The response is typically CSV-like text.
#'
#' @param tm1 Start date for the period in 'YYYYMMDD' format (KST).
#' @param tm2 End date for the period in 'YYYYMMDD' format (KST).
#' @param obs Observation type (e.g., "TA" for temperature, "RN" for rainfall). Refer to KMA docs.
#' @param stn Station ID(s). Use "0" for all stations.
#' @param help_text If "1", adds help text. Default "0".
#' @param mode Mode option (e.g., "0" for decoded results only). Default "0". Refer to KMA docs.
#' @return A character string containing the raw API response.
#' @export
get_sfc_daily_data_period <- function(tm1, tm2, obs, stn = "0", help_text = "0", mode = "0") {
  if (!grepl("^\d{8}$", tm1)) {
    stop("Parameter 'tm1' must be in YYYYMMDD format.", call. = FALSE)
  }
  if (!grepl("^\d{8}$", tm2)) {
    stop("Parameter 'tm2' must be in YYYYMMDD format.", call. = FALSE)
  }
  # obs parameter validation can be added if a fixed set of values is known
  base_url <- "https://apihub.kma.go.kr/api/typ01/url/kma_sfcdd3.php"
  params <- list(
    tm1 = tm1,
    tm2 = tm2,
    obs = obs,
    stn = stn,
    help = help_text,
    mode = mode
  )
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "text", encoding = "UTF-8"))
}

#' Get Elemental Surface Observation Data
#'
#' Retrieves surface observation data for a specific element and time.
#' Corresponds to KMA API '요소별 조회' (kma_sfctm5.php).
#' The response is typically CSV-like text.
#'
#' @param tm2 End time/date in 'YYYYMMDDHHMM' or 'YYYYMMDD' format (KST).
#'        The API documentation mentions 'tm2' as the primary time parameter.
#' @param obs Observation type (e.g., "TA", "HM"). Refer to KMA docs.
#' @param stn Station ID(s). Use "0" for all stations.
#' @param disp Display option. Default "0".
#' @param help_text If "1", adds help text. Default "0".
#' @param tm1 Optional start time/date for a period. If provided, 'tm2' is the end of the period.
#'        Must be in the same format as 'tm2'.
#' @return A character string containing the raw API response.
#' @export
get_sfc_elemental_data <- function(tm2, obs, stn = "0", disp = "0", help_text = "0", tm1 = NULL) {
  if (!grepl("^\d{8}(\d{4})?$", tm2)) { # YYYYMMDD or YYYYMMDDHHMM
    stop("Parameter 'tm2' must be in YYYYMMDD or YYYYMMDDHHMM format.", call. = FALSE)
  }
  if (!is.null(tm1) && !grepl("^\d{8}(\d{4})?$", tm1)) {
      stop("Parameter 'tm1' must be in YYYYMMDD or YYYYMMDDHHMM format if provided.", call. = FALSE)
  }
  base_url <- "https://apihub.kma.go.kr/api/typ01/url/kma_sfctm5.php"
  params <- list(
    tm1 = tm1, # Will be NULL if not provided, filtered by make_kma_request
    tm2 = tm2,
    obs = obs,
    stn = stn,
    disp = disp,
    help = help_text
  )
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "text", encoding = "UTF-8"))
}

#' Get Normal (Climatological) Surface Data
#'
#' Retrieves normal (climatological average) surface data.
#' Corresponds to KMA API '지상 평년값 조회' (sfc_norm1.php).
#' The response is typically CSV-like text.
#'
#' @param norm Type of normal: "D" (daily), "S" (ten-day), "M" (monthly), "Y" (yearly).
#' @param tmst Period for normal calculation (e.g., "2021" for 1991-2020 normals).
#' @param mm1 Start month (e.g., "5" for May).
#' @param dd1 Start day or special code for ten-day normals (100 for early, 200 for mid, 300 for late).
#' @param mm2 End month.
#' @param dd2 End day or special code.
#' @param stn Station ID(s). Use "0" for all stations.
#' @return A character string containing the raw API response.
#' @export
get_sfc_normal_data <- function(norm, tmst, mm1, dd1, mm2, dd2, stn = "0") {
  # Basic validation, can be expanded
  if (!norm %in% c("D", "S", "M", "Y")) {
    stop("Parameter 'norm' must be one of 'D', 'S', 'M', 'Y'.", call. = FALSE)
  }
  base_url <- "https://apihub.kma.go.kr/api/typ01/url/sfc_norm1.php"
  params <- list(
    norm = norm,
    tmst = tmst,
    MM1 = mm1, # API uses MM1, DD1, etc.
    DD1 = dd1,
    MM2 = mm2,
    DD2 = dd2,
    stn = stn
  )
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "text", encoding = "UTF-8"))
}

# --- SfcYearlyInfoService Functions ---

#' Get Yearly Summary Information (Type 1)
#'
#' Retrieves yearly summary information (type 1) from SfcYearlyInfoService.
#' Supports XML or JSON dataType.
#'
#' @param page_no Page number.
#' @param num_of_rows Number of rows per page.
#' @param data_type "XML" or "JSON".
#' @param year Year (YYYY).
#' @return Character string containing the raw API response (XML or JSON).
#' @export
get_sfc_yearly_summary_info1 <- function(page_no, num_of_rows, data_type, year) {
  # Input validation
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)

  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SfcYearlyInfoService/getYearSumry"
  params <- list(
    pageNo = page_no,
    numOfRows = num_of_rows,
    dataType = data_type,
    year = year
  )
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "text", encoding = "UTF-8"))
}

#' Get Yearly Summary Information (Type 2)
#'
#' Retrieves yearly summary information (type 2) from SfcYearlyInfoService.
#'
#' @inheritParams get_sfc_yearly_summary_info1
#' @return Character string containing the raw API response (XML or JSON).
#' @export
get_sfc_yearly_summary_info2 <- function(page_no, num_of_rows, data_type, year) {
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)

  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SfcYearlyInfoService/getYearSumry2"
  params <- list(
    pageNo = page_no,
    numOfRows = num_of_rows,
    dataType = data_type,
    year = year
  )
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "text", encoding = "UTF-8"))
}

#' Get Average Temperature Anomaly
#' @inheritParams get_sfc_yearly_summary_info1
#' @export
get_sfc_avg_temp_anomaly <- function(page_no, num_of_rows, data_type, year) {
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SfcYearlyInfoService/getAvgTaAnamaly"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Precipitation Anomaly
#' @inheritParams get_sfc_yearly_summary_info1
#' @export
get_sfc_precip_anomaly <- function(page_no, num_of_rows, data_type, year) {
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SfcYearlyInfoService/getRnAnamaly"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Station Phenomenon Data (Type 1)
#' @inheritParams get_sfc_yearly_summary_info1
#' @param station Station ID.
#' @export
get_sfc_stn_phenom_data1 <- function(page_no, num_of_rows, data_type, year, station) {
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SfcYearlyInfoService/getStnPhnmnData"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, station=station)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Station Phenomenon Data (Type 2)
#' @inheritParams get_sfc_stn_phenom_data1
#' @export
get_sfc_stn_phenom_data2 <- function(page_no, num_of_rows, data_type, year, station) {
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SfcYearlyInfoService/getStnPhnmnData2"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, station=station)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Station Phenomenon Data (Type 3)
#' @inheritParams get_sfc_stn_phenom_data1
#' @export
get_sfc_stn_phenom_data3 <- function(page_no, num_of_rows, data_type, year, station) {
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SfcYearlyInfoService/getStnPhnmnData3"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, station=station)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

# --- SfcMtlyInfoService Functions ---

#' Get Monthly Report Notes
#'
#' Retrieves notes for monthly reports from SfcMtlyInfoService.
#'
#' @inheritParams get_sfc_yearly_summary_info1
#' @param month Month (MM, e.g., "09").
#' @return Character string containing the raw API response (XML or JSON).
#' @export
get_sfc_monthly_report_notes <- function(page_no, num_of_rows, data_type, year, month) {
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)

  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SfcMtlyInfoService/getNote"
  params <- list(
    pageNo = page_no,
    numOfRows = num_of_rows,
    dataType = data_type,
    year = year,
    month = month
  )
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "text", encoding = "UTF-8"))
}

#' Get Surface Station List Table
#' @inheritParams get_sfc_monthly_report_notes
#' @export
get_sfc_station_list_table <- function(page_no, num_of_rows, data_type, year, month) {
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SfcMtlyInfoService/getSfcStnLstTbl"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Monthly Summary (Type 1)
#' @inheritParams get_sfc_monthly_report_notes
#' @export
get_sfc_monthly_summary1 <- function(page_no, num_of_rows, data_type, year, month) {
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SfcMtlyInfoService/getMmSumry"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Monthly Summary (Type 2)
#' @inheritParams get_sfc_monthly_report_notes
#' @export
get_sfc_monthly_summary2 <- function(page_no, num_of_rows, data_type, year, month) {
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SfcMtlyInfoService/getMmSumry2"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Daily Weather Data for a Specific Month
#' @inheritParams get_sfc_monthly_report_notes
#' @param station Station ID.
#' @export
get_sfc_daily_weather_data_for_month <- function(page_no, num_of_rows, data_type, year, month, station) {
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SfcMtlyInfoService/getDailyWthrData"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month, station=station)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Surface Weather Phenomenon Chart
#'
#' Retrieves a graphical chart of surface weather phenomenon.
#' Corresponds to KMA API '(그래픽) 지상기상현상(관서) 조회' (sfc_ww_pnt.php).
#' This function returns raw binary image content.
#'
#' @param obs_type Observation type (maps to 'obs' API parameter).
#' @param tm Time in 'YYYYMMDDHHMM' format.
#' @param val Value (specific to 'obs_type').
#' @param stn Station information flag.
#' @param obj Object type.
#' @param map_type Map type code (maps to 'map' API parameter).
#' @param grid Grid type.
#' @param legend Legend flag ("0" or "1").
#' @param size Image size (pixels, e.g., "600").
#' @param itv Interval.
#' @param zoom_level Zoom level.
#' @param zoom_x Zoom X coordinate.
#' @param zoom_y Zoom Y coordinate.
#' @param gov Optional government agency code. Default "".
#' @return Raw vector containing the binary image data.
#' @export
get_sfc_weather_phenomenon_chart <- function(obs_type, tm, val, stn, obj, map_type,
                                             grid, legend, size, itv,
                                             zoom_level, zoom_x, zoom_y, gov = "") {
  # Basic validation for key parameters
  if (!grepl("^\d{12}$", tm)) { # Corrected from YYYYMMDDHHMM to YYYYMMDDHH as per KMA docs for this endpoint
    stop("Parameter 'tm' must be in YYYYMMDDHHMM format.", call. = FALSE) # Doc says YYYYMMDDHH for this specific API
  }

  base_url <- "https://apihub.kma.go.kr/api/typ03/php/alw/sfc/sfc_ww_pnt.php"
  params <- list(
    obs = obs_type, # API uses 'obs'
    tm = tm,
    val = val,
    stn = stn,
    obj = obj,
    map = map_type, # API uses 'map'
    grid = grid,
    legend = legend,
    size = size,
    itv = itv,
    zoom_level = zoom_level,
    zoom_x = zoom_x,
    zoom_y = zoom_y,
    gov = gov
  )
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "raw")) # Return raw binary content
}
