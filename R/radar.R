#' Get Station Radar File List
#'
#' Retrieves a list of radar files for a specific KMA radar station, date, and data type.
#' Corresponds to KMA API '레이더 지점별 파일 목록' (rdr_stn_file_list.php).
#' Response is typically text listing filenames.
#'
#' @param tm Observation date in 'YYYYMMDD' format (KST).
#' @param stn Radar station code (e.g., "KWK").
#' @param rdr Data type (e.g., "HSR", "RAW"). Refer to KMA docs.
#' @param size If "Y", includes file sizes (can be slower). Default "N".
#' @param filter_str Optional string to filter filenames. API param: filter.
#' @return Character string containing the raw API response (list of files).
#' @export
get_radar_station_file_list <- function(tm, stn, rdr, size = "N", filter_str = NULL) {
  if (!grepl("^\d{8}$", tm)) stop("Parameter 'tm' must be in YYYYMMDD format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ01/url/rdr_stn_file_list.php"
  params <- list(tm = tm, stn = stn, rdr = rdr, size = size, filter = filter_str)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "text", encoding = "UTF-8"))
}

#' Get Composite Radar File List
#'
#' Retrieves a list of composite radar files for a specific date and composite type.
#' Corresponds to KMA API '레이더 합성파일 목록' (rdr_cmp_file_list.php).
#' Response is typically text listing filenames.
#'
#' @param tm Observation date in 'YYYYMMDD' format (KST).
#' @param cmp Composite type (e.g., "HSR", "PPI"). Refer to KMA docs.
#' @param ext Include related agency data? "Y" (default) or "K" (KMA only).
#' @param size If "Y", includes file sizes. Default "N".
#' @param filter_str Optional string to filter filenames. API param: filter.
#' @return Character string containing the raw API response.
#' @export
get_radar_composite_file_list <- function(tm, cmp, ext = "Y", size = "N", filter_str = NULL) {
  if (!grepl("^\d{8}$", tm)) stop("Parameter 'tm' must be in YYYYMMDD format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ01/url/rdr_cmp_file_list.php"
  params <- list(tm = tm, cmp = cmp, ext = ext, size = size, filter = filter_str)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as = "text", encoding = "UTF-8"))
}

#' Get Composite Radar File Information
#'
#' Retrieves information about a composite radar file.
#' Corresponds to KMA API 'HSR 합성파일 정보' (nph-rdr_cmp_inf).
#' Describes data format (ASCII or Binary).
#'
#' @param tm Time of composite data in 'YYYYMMDDHHMM' or 'YYYYMMDDHH' format (KST).
#' @param cmp Composite type (e.g., "HSR", "PPI").
#' @param qcd Quality control designator (e.g., "MSK", "KMA").
#' @param obs Optional. Element (e.g., "ECHO").
#' @param acc Optional. Accumulation period (minutes, multiple of 60 for PCP*).
#' @param map_area Map area code (e.g., "HB"). Default "HB". (API param 'map')
#' @param disp Data provision format: "A" (ASCII) or "B" (Binary). Default "A".
#' @return Character string containing the raw API response (data format description).
#' @export
get_radar_composite_info <- function(tm, cmp, qcd, obs = NULL, acc = NULL, map_area = "HB", disp = "A") {
  if (!grepl("^\d{10,12}$", tm)) stop("Parameter 'tm' must be in YYYYMMDDHH(MM) format.", call. = FALSE)
  base_url <- "https://apihub.kma.go.kr/api/typ01/cgi-bin/url/nph-rdr_cmp_inf"
  params <- list(tm=tm, cmp=cmp, qcd=qcd, obs=obs, acc=acc, map=map_area, disp=disp)
  response <- make_kma_request(base_url, params)
  return(httr::content(response, as="text", encoding="UTF-8"))
}

#' Get Composite Radar Data
#'
#' Retrieves actual composite radar data (e.g., HSR, masked HSR, accumulated).
#' Corresponds to KMA API (e.g., 'HSR에 마스킹처리', nph-rdr_cmp1_api).
#' Returns ASCII text or raw binary data based on 'disp' parameter.
#'
#' @inheritParams get_radar_composite_info
#' @param obs Element (e.g., "ECHO"). This is not optional for data retrieval.
#' @return If disp="A", character string (ASCII grid). If disp="B", raw vector (binary grid).
#' @export
get_radar_composite_data <- function(tm, cmp, qcd, obs, map_area = "HB", disp = "A", acc = NULL) {
  if (!grepl("^\d{10,12}$", tm)) stop("Parameter 'tm' must be in YYYYMMDDHH(MM) format.", call. = FALSE)
  if (missing(obs) || is.null(obs)) stop("Parameter 'obs' is required for data retrieval.", call. = FALSE)

  base_url <- "https://apihub.kma.go.kr/api/typ01/cgi-bin/url/nph-rdr_cmp1_api"
  params <- list(tm=tm, cmp=cmp, qcd=qcd, obs=obs, acc=acc, map=map_area, disp=disp)
  response <- make_kma_request(base_url, params)

  if (toupper(disp) == "B") {
    return(httr::content(response, as = "raw"))
  } else {
    return(httr::content(response, as = "text", encoding = "UTF-8"))
  }
}
