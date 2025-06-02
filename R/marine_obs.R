#' Get Comprehensive Marine Observation Data
#'
#' Retrieves comprehensive marine observation data (buoy, wave buoy, drifter buoy etc.)
#' Corresponds to KMA API '해양기상종합관측' (sea_obs.php).
#' Response is typically CSV-like text.
#'
#' @param tm Observation time in 'YYYYMMDDHHMM' format (KST). Data for the closest production time is returned.
#' @param stn Station ID. "0" for all stations.
#' @param help_text If "1", adds help text. Default "0".
#' @return Character string containing the raw API response.
#' @export
get_marine_comprehensive_obs <- function(tm, stn = "0", help_text = "0") {
  if (!grepl("^\d{12}$", tm)) {
    stop("Parameter 'tm' must be in YYYYMMDDHHMM format.", call. = FALSE)
  }
  base_url <- "https://apihub.kma.go.kr/api/typ01/url/sea_obs.php"
  params <- list(tm = tm, stn = stn, help = help_text)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "text", encoding = "UTF-8"))
}

#' Get Marine Buoy Data for a Period
#'
#' Retrieves marine buoy data for a specified period and station(s).
#' Corresponds to KMA API '해양기상부이(기간조회)' (kma_buoy2.php).
#' Response is typically CSV-like text.
#'
#' @param tm1 Start time for the period in 'YYYYMMDDHHMM' format (KST).
#' @param tm2 End time for the period in 'YYYYMMDDHHMM' format (KST).
#' @param stn Station ID. "0" for all stations.
#' @param help_text If "1", adds help text. Default "0".
#' @return Character string containing the raw API response.
#' @export
get_marine_buoy_data_period <- function(tm1, tm2, stn = "0", help_text = "0") {
  if (!grepl("^\d{12}$", tm1)) {
    stop("Parameter 'tm1' must be in YYYYMMDDHHMM format.", call. = FALSE)
  }
  if (!grepl("^\d{12}$", tm2)) {
    stop("Parameter 'tm2' must be in YYYYMMDDHHMM format.", call. = FALSE)
  }
  base_url <- "https://apihub.kma.go.kr/api/typ01/url/kma_buoy2.php"
  params <- list(tm1 = tm1, tm2 = tm2, stn = stn, help = help_text)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "text", encoding = "UTF-8"))
}

#' Get KMA Buoy Data
#'
#' Retrieves KMA marine buoy data for a specific time and station.
#' Corresponds to KMA API '해양기상부이' (kma_buoy.php).
#' Response is typically CSV-like text.
#'
#' @param tm Observation time in 'YYYYMMDDHHMM' format (KST).
#' @param stn Station ID. "0" for all stations.
#' @param help_text If "1", adds help text. Default "0".
#' @return Character string containing the raw API response.
#' @export
get_marine_kma_buoy_data <- function(tm, stn = "0", help_text = "0") {
  if (!grepl("^\d{12}$", tm)) {
    stop("Parameter 'tm' must be in YYYYMMDDHHMM format.", call. = FALSE)
  }
  base_url <- "https://apihub.kma.go.kr/api/typ01/url/kma_buoy.php"
  params <- list(tm = tm, stn = stn, help = help_text)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "text", encoding = "UTF-8"))
}

# --- SeaMtlyInfoService Functions ---

#' Get Monthly Marine Report Notes
#'
#' Retrieves notes for monthly marine reports from SeaMtlyInfoService.
#' Supports XML or JSON dataType.
#'
#' @param page_no Page number.
#' @param num_of_rows Number of rows per page.
#' @param data_type "XML" or "JSON".
#' @param year Year (YYYY).
#' @param month Month (MM, e.g., "09").
#' @return Character string containing the raw API response (XML or JSON).
#' @export
get_marine_monthly_report_notes <- function(page_no, num_of_rows, data_type, year, month) {
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)

  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getNote"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "text", encoding = "UTF-8"))
}

#' Get Buoy List Table for Monthly Marine Report
#' @inheritParams get_marine_monthly_report_notes
#' @export
get_marine_buoy_list_table <- function(page_no, num_of_rows, data_type, year, month) {
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getBuoyLstTbl"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Lighthouse AWS List Table for Monthly Marine Report
#' @inheritParams get_marine_monthly_report_notes
#' @export
get_marine_lhaws_list_table <- function(page_no, num_of_rows, data_type, year, month) {
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getLhawsLstTbl"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Wave Buoy List Table for Monthly Marine Report
#' @inheritParams get_marine_monthly_report_notes
#' @export
get_marine_wave_buoy_list_table <- function(page_no, num_of_rows, data_type, year, month) {
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getWaveBuoyLstTbl"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Observation Open Years for Marine Stations
#' @inheritParams get_marine_monthly_report_notes
#' @export
get_marine_obs_open_years <- function(page_no, num_of_rows, data_type, year, month) {
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getObsOpenYear"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

# Buoy Monthly Summaries
#' Get Marine Buoy Monthly Summary (Type 1)
#' @inheritParams get_marine_monthly_report_notes
#' @export
get_marine_buoy_monthly_summary1 <- function(page_no, num_of_rows, data_type, year, month){
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getBuoyMmSumry"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Marine Buoy Monthly Summary (Type 2)
#' @inheritParams get_marine_monthly_report_notes
#' @export
get_marine_buoy_monthly_summary2 <- function(page_no, num_of_rows, data_type, year, month){
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getBuoyMmSumry2"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Daily Buoy Data for a Specific Month
#' @inheritParams get_marine_monthly_report_notes
#' @param station Station ID.
#' @export
get_marine_daily_buoy_data_for_month <- function(page_no, num_of_rows, data_type, year, month, station){
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getDailyBuoy"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month, station=station)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

# Lighthouse AWS Monthly Summaries
#' Get Lighthouse AWS Monthly Summary (Type 1)
#' @inheritParams get_marine_monthly_report_notes
#' @export
get_marine_lhaws_monthly_summary1 <- function(page_no, num_of_rows, data_type, year, month){
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getLhawsMmSumry"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Lighthouse AWS Monthly Summary (Type 2)
#' @inheritParams get_marine_monthly_report_notes
#' @export
get_marine_lhaws_monthly_summary2 <- function(page_no, num_of_rows, data_type, year, month){
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getLhawsMmSumry2"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Daily Lighthouse AWS Data for a Specific Month
#' @inheritParams get_marine_daily_buoy_data_for_month
#' @export
get_marine_daily_lhaws_data_for_month <- function(page_no, num_of_rows, data_type, year, month, station){
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getDailyLhaws"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month, station=station)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

# Wave Buoy Monthly Summaries
#' Get Wave Buoy Monthly Summary (Type 1)
#' @inheritParams get_marine_monthly_report_notes
#' @export
get_marine_wave_buoy_monthly_summary1 <- function(page_no, num_of_rows, data_type, year, month){
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getWaveBuoyMmSumry"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Wave Buoy Monthly Summary (Type 2)
#' @inheritParams get_marine_monthly_report_notes
#' @export
get_marine_wave_buoy_monthly_summary2 <- function(page_no, num_of_rows, data_type, year, month){
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getWaveBuoyMmSumry2"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Daily Wave Buoy Data for a Specific Month
#' @inheritParams get_marine_daily_buoy_data_for_month
#' @export
get_marine_daily_wave_buoy_data_for_month <- function(page_no, num_of_rows, data_type, year, month, station){
  if (!data_type %in% c("XML", "JSON")) stop("'data_type' must be 'XML' or 'JSON'.", call. = FALSE)
  if (!grepl("^\d{4}$", year)) stop("'year' must be in YYYY format.", call. = FALSE)
  if (!grepl("^\d{2}$", month)) stop("'month' must be in MM format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getDailyWaveBuoy"
  params <- list(pageNo=page_no, numOfRows=num_of_rows, dataType=data_type, year=year, month=month, station=station)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Marine Observation Spatial Distribution Chart
#'
#' Retrieves a graphical chart of marine observation spatial distribution.
#' Corresponds to KMA API '(그래픽) 해양관측 공간분포 조회(배경지도 없음)' (nph-sea_obs_imgp1).
#' This function returns raw binary image content.
#'
#' @param proj 경량화 PROJECT ID (e.g., "SEA_CMP_WAVEH").
#' @param start_x 시작 X 좌표.
#' @param start_y 시작 Y 좌표.
#' @param end_x 종료 X 좌표.
#' @param end_y 종료 Y 좌표.
#' @param zoom_lvl 현재 줌 Level.
#' @param tm 현재시각 'YYYYMMDDHHMM'.
#' @param tm_st 시작 시각 'YYYYMMDDHHMM'.
#' @param map_type 지도종류 (maps to 'map' API parameter).
#' @param grid 격자크기 (km).
#' @param itv 분석간격 (분).
#' @param data_dtl_cd 경량화 Layer Name (maps to 'dataDtlCd' API parameter, e.g., "htsg").
#' @param obs 변수 구분 (e.g., "wh").
#' @param stn 지점정보 ("0":최대최소값만, "1":영역내 지점값 포함, "2":지점값만 객관분석없음).
#' @param size 이미지크기 (픽셀, e.g., "600").
#' @param sel_ws 풍속단위 ("kh": km/h, "ms": m/s, maps to 'selWs' API parameter).
#' @return Raw vector containing the binary image data.
#' @export
get_marine_spatial_chart <- function(proj, start_x, start_y, end_x, end_y,
                                     zoom_lvl, tm, tm_st, map_type, grid,
                                     itv, data_dtl_cd, obs, stn, size, sel_ws) {
  # Basic validation for key parameters
  if (!grepl("^\d{12}$", tm)) {
    stop("Parameter 'tm' must be in YYYYMMDDHHMM format.", call. = FALSE)
  }
  if (!grepl("^\d{12}$", tm_st)) {
    stop("Parameter 'tm_st' must be in YYYYMMDDHHMM format.", call. = FALSE)
  }

  base_url <- "https://apihub.kma.go.kr/api/typ03/cgi/aws3/nph-sea_obs_imgp1"
  params <- list(
    PROJ = proj,         # API uses PROJ
    STARTX = start_x,    # API uses STARTX
    STARTY = start_y,    # API uses STARTY
    ENDX = end_x,        # API uses ENDX
    ENDY = end_y,        # API uses ENDY
    ZOOMLVL = zoom_lvl,  # API uses ZOOMLVL
    tm = tm,
    tm_st = tm_st,
    map = map_type,      # API uses map
    grid = grid,
    itv = itv,
    dataDtlCd = data_dtl_cd, # API uses dataDtlCd
    obs = obs,
    stn = stn,
    size = size,
    selWs = sel_ws       # API uses selWs
  )
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "raw")) # Return raw binary content
}
