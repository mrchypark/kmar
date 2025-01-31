
get_cache_path <- function(timestamp) {
  temp_dir <- "./cache" # 시스템 임시 폴더
  filename <- paste0("cache_", timestamp, ".csv") # "cache_202411151000.csv" 형식
  return(file.path(temp_dir, filename))
}

get_stn <- function(lat, lon) {
  path <- get_stn_cache_path()
  if (!file.exists(path)) {
    dat <- get_stn_web()
    readr::write_csv(dat, path)
  }
  res <- readr::read_csv(path, show_col_types = FALSE)

  if (missing(lat)) return(res)
  if (missing(lon)) return(res)
  
    res %>%
    dplyr::mutate(
      distance = sqrt((LAT - lat)^2 + (LON - lon)^2)
    ) %>%
      dplyr::arrange(distance) %>% # 거리 기준 정렬
        dplyr::slice(1) %>%
    return()
}

get_stn_cache_path <- function() {
  temp_dir <- "./cache" # 시스템 임시 폴더
  filename <- "stn.csv"
  return(file.path(temp_dir, filename))
}

get_stn_web <- function() {
  httr::GET(
    "https://apihub.kma.go.kr/api/typ01/url/stn_inf.php?inf=SFC&stn=&tm=202211300900&help=0&authKey=sv6kSMtCTI6-pEjLQjyOtA"
  ) %>%
    httr::content() -> res
  # 불필요한 줄 제거
  res_cleaned <- res %>%
    stringr::str_remove_all("#START7777") %>%
    stringr::str_remove_all("#7777END") %>%
    stringr::str_replace_all(" Gun", "gun") %>%
    stringr::str_split("\n") %>%
    unlist()

  # 헤더 및 데이터 구분
  header_index <- which(stringr::str_detect(res_cleaned, "STN")) # 헤더 찾기
  data_start_index <- header_index + 2 # 데이터 시작 인덱스
  data_lines <- res_cleaned[data_start_index:length(res_cleaned)] # 데이터 부분만 추출
  data_lines <- data_lines[data_lines != ""]

  # 데이터 프레임 변환
  weather_tibble <- readr::read_table(
    paste(data_lines, collapse = "\n"),

    col_names = c(
      "STN_ID",
      "LON",
      "LAT",
      "STN_SP",
      "HT",
      "HT_PA",
      "HT_TA",
      "HT_WD",
      "HT_RN",
      "STN_AD",
      "STN_KO",
      "STN_EN",
      "FCT_ID",
      "LAW_ID",
      "BASIN"
    )
  ) %>%
    dplyr::as_tibble() %>%
      dplyr::select(-BASIN)

  return(weather_tibble)
}
