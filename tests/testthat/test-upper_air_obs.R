library(testthat)
# Assuming KMAapiR is loaded and with_mock_auth is available 
# (e.g., from a helper file or defined locally if needed for isolated runs)

if (!exists("with_mock_auth", mode="function")) {
    with_mock_auth <- function(expr) {
      # This simplified version primarily relies on Sys.setenv for get_kma_auth_key behavior.
      old_key_env_var <- Sys.getenv("KMA_API_KEY", unset = NA)
      Sys.setenv(KMA_API_KEY = "testmockkey_upper_air_fallback") 
      
      original_session_key <- NULL
      session_key_was_present <- FALSE
      if (exists("set_kma_auth_key", where = "package:KMAapiR", mode="function") &&
          exists(".kma_api_env", where = asNamespace("KMAapiR"))) {
            kma_env <- KMAapiR:::.kma_api_env
            if(exists("auth_key", envir = kma_env)) {
                original_session_key <- kma_env$auth_key
                session_key_was_present <- TRUE
            }
            kma_env$auth_key <- "testmockkey_upper_air_fallback"
      }

      on.exit({
        if (is.na(old_key_env_var)) {
          Sys.unsetenv("KMA_API_KEY")
        } else {
          Sys.setenv(KMA_API_KEY = old_key_env_var)
        }
        if (exists("set_kma_auth_key", where = "package:KMAapiR", mode="function") &&
            exists(".kma_api_env", where = asNamespace("KMAapiR"))) {
            kma_env <- KMAapiR:::.kma_api_env
            if(session_key_was_present) {
                kma_env$auth_key <- original_session_key
            } else {
                if(exists("auth_key", envir = kma_env)) {
                    rm("auth_key", envir = kma_env)
                }
            }
        }
      }, add = TRUE)
      
      eval.parent(substitute(expr))
    }
}

test_that("get_upper_air_temp_data input validation and structure", {
  expect_error(KMAapiR::get_upper_air_temp_data(tm = "20231027"), "Parameter 'tm' must be in YYYYMMDDHH\\(MM\\) format \\(UTC\\).")
  with_mock_auth({
    mockery::stub(KMAapiR::get_upper_air_temp_data, 'make_kma_request', 
                  function(base_url, params) {
                    expect_equal(base_url, "https://apihub.kma.go.kr/api/typ01/url/upp_temp.php")
                    expect_equal(params$tm, "202301010000")
                    expect_equal(params$authKey, "testmockkey_upper_air_fallback")
                    return(structure(list(status_code = 200, content = charToRaw("Mocked TEMP CSV")), class = "response"))
                  })
    expect_equal(KMAapiR::get_upper_air_temp_data(tm = "202301010000"), "Mocked TEMP CSV")
  })
})

test_that("get_upper_air_sea_kship_temp_data works", {
  expect_error(KMAapiR::get_upper_air_sea_kship_temp_data(tm = "2023"), "Parameter 'tm' must be in YYYYMMDDHH\\(MM\\) format \\(UTC\\).")
  with_mock_auth({
    mockery::stub(KMAapiR::get_upper_air_sea_kship_temp_data, 'make_kma_request', 
                  function(base_url, params) {
                    expect_equal(base_url, "https://apihub.kma.go.kr/api/typ01/url/sea_kship_temp.php")
                    expect_equal(params$authKey, "testmockkey_upper_air_fallback")
                    return(structure(list(status_code = 200, content = charToRaw("Mocked KSHIP TEMP CSV")), class = "response"))
                  })
    expect_equal(KMAapiR::get_upper_air_sea_kship_temp_data(tm = "202301011200"), "Mocked KSHIP TEMP CSV")
  })
})

test_that("get_upper_air_mobile_temp_data works", {
  expect_error(KMAapiR::get_upper_air_mobile_temp_data(tm = "2023010112000"), "Parameter 'tm' must be in YYYYMMDDHH\\(MM\\) format \\(UTC\\).")
  with_mock_auth({
    mockery::stub(KMAapiR::get_upper_air_mobile_temp_data, 'make_kma_request', 
                  function(base_url, params) {
                    expect_equal(base_url, "https://apihub.kma.go.kr/api/typ01/url/upp_mbl_temp.php")
                    expect_equal(params$authKey, "testmockkey_upper_air_fallback")
                    return(structure(list(status_code = 200, content = charToRaw("Mocked MOBILE TEMP CSV")), class = "response"))
                  })
    expect_equal(KMAapiR::get_upper_air_mobile_temp_data(tm = "2023010100"), "Mocked MOBILE TEMP CSV")
  })
})

test_that("get_upper_air_raw_max_altitude works", {
  expect_error(KMAapiR::get_upper_air_raw_max_altitude(tm1 = "2023", tm2 = "20230102"), "Parameter 'tm1' must be in YYYYMMDD format.")
  expect_error(KMAapiR::get_upper_air_raw_max_altitude(tm1 = "20230101", tm2 = "2023010200"), "Parameter 'tm2' must be in YYYYMMDD format.")
  with_mock_auth({
    mockery::stub(KMAapiR::get_upper_air_raw_max_altitude, 'make_kma_request', 
                  function(base_url, params) {
                    expect_equal(base_url, "https://apihub.kma.go.kr/api/typ01/url/upp_raw_max.php")
                    expect_equal(params$authKey, "testmockkey_upper_air_fallback")
                    return(structure(list(status_code = 200, content = charToRaw("Mocked MAX ALT CSV")), class = "response"))
                  })
    expect_equal(KMAapiR::get_upper_air_raw_max_altitude(tm1 = "20230101", tm2 = "20230102"), "Mocked MAX ALT CSV")
  })
})

test_that("get_upper_air_analysis_data works", {
  expect_error(KMAapiR::get_upper_air_analysis_data(tm1 = "20230101", tm2 = "202301020000"), "Parameter 'tm1' must be in YYYYMMDDHH\\(MM\\) format \\(UTC\\).")
  expect_error(KMAapiR::get_upper_air_analysis_data(tm1 = "2023010100", tm2 = "2023010200001"), "Parameter 'tm2' must be in YYYYMMDDHH\\(MM\\) format \\(UTC\\).")
  with_mock_auth({
    mockery::stub(KMAapiR::get_upper_air_analysis_data, 'make_kma_request', 
                  function(base_url, params) {
                    expect_equal(base_url, "https://apihub.kma.go.kr/api/typ01/url/upp_idx.php")
                    expect_equal(params$authKey, "testmockkey_upper_air_fallback")
                    return(structure(list(status_code = 200, content = charToRaw("Mocked ANALYSIS CSV")), class = "response"))
                  })
    expect_equal(KMAapiR::get_upper_air_analysis_data(tm1 = "202301010000", tm2 = "202301020000"), "Mocked ANALYSIS CSV")
  })
})

# Helper for UppMtlyInfoService tests
test_upper_air_xml_json_service <- function(func_name_str, base_url_expected, required_params, optional_params = list()) {
  # Ensure func_name_str is a character string for the function name
  qualified_func_name <- paste0("KMAapiR::", func_name_str)
  target_func <- eval(parse(text = qualified_func_name))

  # Test with XML
  with_mock_auth({ 
    call_args_xml <- c(list(page_no=1, num_of_rows=10, data_type="XML"), required_params, optional_params)
    mockery::stub(where = qualified_func_name, what = 'make_kma_request',
                  how = function(base_url, params) {
                    expect_equal(base_url, base_url_expected)
                    expect_equal(params$dataType, "XML")
                    for(p_name in names(required_params)) expect_equal(params[[p_name]], required_params[[p_name]])
                    if (length(optional_params) > 0 && "station" %in% names(optional_params)) {
                        expect_equal(params$station, optional_params$station)
                    }
                    expect_equal(params$authKey, "testmockkey_upper_air_fallback")
                    return(structure(list(status_code = 200, content = charToRaw("<xml>Mocked UpperAir XML</xml>")), class = "response"))
                  })
    expect_equal(do.call(target_func, call_args_xml), "<xml>Mocked UpperAir XML</xml>")
  })

  # Test with JSON
  with_mock_auth({
    call_args_json <- c(list(page_no=1, num_of_rows=10, data_type="JSON"), required_params, optional_params)
    mockery::stub(where = qualified_func_name, what = 'make_kma_request',
                  how = function(base_url, params) {
                    expect_equal(base_url, base_url_expected)
                    expect_equal(params$dataType, "JSON")
                    expect_equal(params$authKey, "testmockkey_upper_air_fallback")
                    return(structure(list(status_code = 200, content = charToRaw("{\"json\":\"Mocked UpperAir JSON\"}")), class = "response"))
                  })
    expect_equal(do.call(target_func, call_args_json), "{\"json\":\"Mocked UpperAir JSON\"}")
  })
  
  call_args_invalid_dt <- c(list(page_no=1, num_of_rows=10, data_type="TXT"), required_params, optional_params)
  expect_error(do.call(target_func, call_args_invalid_dt), "'data_type' must be 'XML' or 'JSON'.")
  
  base_test_args <- c(list(page_no = 1, num_of_rows = 10, data_type = "XML"), optional_params)

  if("year" %in% names(required_params)) {
    invalid_year_args <- c(base_test_args, required_params)
    invalid_year_args$year <- "23" 
    expect_error(do.call(target_func, invalid_year_args), "'year' must be in YYYY format.")
  }
  if("month" %in% names(required_params)) {
    invalid_month_args <- c(base_test_args, required_params) 
    invalid_month_args$month <- "1" 
    if (!("year" %in% names(invalid_month_args) && invalid_month_args$year == "23")) {
        if (!("year" %in% names(invalid_month_args))) invalid_month_args$year <- "2023" 
    }
    expect_error(do.call(target_func, invalid_month_args), "'month' must be in MM format.")
  }
}

# --- UppMtlyInfoService Tests ---
test_that("get_upper_air_monthly_report_notes works", {
  test_upper_air_xml_json_service("get_upper_air_monthly_report_notes", 
                            "https://apihub.kma.go.kr/api/typ02/openApi/UppMtlyInfoService/getNote", 
                            list(year="2023", month="01"))
})
test_that("get_upper_air_station_list_table works", {
  test_upper_air_xml_json_service("get_upper_air_station_list_table", 
                            "https://apihub.kma.go.kr/api/typ02/openApi/UppMtlyInfoService/getUppLstTbl", 
                            list(year="2023", month="01"))
})
test_that("get_upper_air_std_isobaric_values works", {
  test_upper_air_xml_json_service("get_upper_air_std_isobaric_values", 
                            "https://apihub.kma.go.kr/api/typ02/openApi/UppMtlyInfoService/getStdIsbrsfValue", 
                            list(year="2023", month="01"), list(station="47102"))
})
test_that("get_upper_air_max_wind_data works", {
  test_upper_air_xml_json_service("get_upper_air_max_wind_data", 
                            "https://apihub.kma.go.kr/api/typ02/openApi/UppMtlyInfoService/getMaxWind", 
                            list(year="2023", month="01"), list(station="47102")) 
})
test_that("get_upper_air_temp_humidity_levels works", {
  test_upper_air_xml_json_service("get_upper_air_temp_humidity_levels", 
                            "https://apihub.kma.go.kr/api/typ02/openApi/UppMtlyInfoService/getTaHmLevel", 
                            list(year="2023", month="01"), list(station="47102"))
})
test_that("get_upper_air_wind_levels works", {
  test_upper_air_xml_json_service("get_upper_air_wind_levels", 
                            "https://apihub.kma.go.kr/api/typ02/openApi/UppMtlyInfoService/getWindLevel", 
                            list(year="2023", month="01"), list(station="47102"))
})
