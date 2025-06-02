library(testthat)
# library(KMAapiR) # Assumed loaded by test_check or devtools::load_all() in a real session

# Helper to set a dummy auth key for tests and ensure .kma_api_env is accessible
# This version ensures KMAapiR:::set_kma_auth_key can find .kma_api_env
with_mock_auth <- function(expr) {
  # Ensure the .kma_api_env is available in the KMAapiR namespace for direct access
  # This might only be truly effective if KMAapiR is loaded (e.g. devtools::load_all())
  # For isolated test runs, direct environment manipulation might be tricky if not loaded.
  # However, set_kma_auth_key and get_kma_auth_key are designed to handle this.

  old_key_env_var <- Sys.getenv("KMA_API_KEY", unset = NA) # Store if NA if not set
  
  # Check if .kma_api_env and auth_key within it exist
  session_key_exists <- FALSE
  old_key_session <- NULL
  if (exists("set_kma_auth_key", where = "package:KMAapiR", mode = "function") && 
      exists(".kma_api_env", where = asNamespace("KMAapiR"))) {
    kma_env <- KMAapiR:::.kma_api_env
    if (exists("auth_key", envir = kma_env)) {
      old_key_session <- kma_env$auth_key
      session_key_exists <- TRUE
    }
  }

  Sys.setenv(KMA_API_KEY = "testmockkey_surface_obs")
  # If KMAapiR is loaded, set_kma_auth_key will use its internal environment.
  # If not loaded, this might not persist correctly for the tested functions unless they also
  # manage to find .kma_api_env through the namespace.
  # The get_kma_auth_key() function prioritizes Sys.getenv, so that's the primary mock here.

  on.exit({
    if (is.na(old_key_env_var)) {
      Sys.unsetenv("KMA_API_KEY")
    } else {
      Sys.setenv(KMA_API_KEY = old_key_env_var)
    }
    
    if (exists("set_kma_auth_key", where = "package:KMAapiR", mode = "function") &&
        exists(".kma_api_env", where = asNamespace("KMAapiR"))) {
      kma_env <- KMAapiR:::.kma_api_env
      if (session_key_exists) {
        kma_env$auth_key <- old_key_session
      } else {
        if (exists("auth_key", envir = kma_env)) {
          rm("auth_key", envir = kma_env)
        }
      }
    }
  }, add = TRUE)
  
  # Execute the expression
  eval.parent(substitute(expr))
}
    
# Mocking httr::GET / make_kma_request
# These tests primarily check parameter validation and structure.
# Actual API calls would require httptest::with_mock_api.

test_that("get_sfc_hourly_data input validation and structure", {
  expect_error(KMAapiR::get_sfc_hourly_data(tm = "20231027"), "Parameter 'tm' must be in YYYYMMDDHHMM format.")
  
  with_mock_auth({
    # Mock make_kma_request for this specific function call
    mockery::stub(KMAapiR::get_sfc_hourly_data, 'make_kma_request', 
                  function(base_url, params) {
                    expect_equal(base_url, "https://apihub.kma.go.kr/api/typ01/url/kma_sfctm2.php")
                    expect_true("tm" %in% names(params))
                    expect_equal(params$tm, "202301010000")
                    expect_equal(params$stn, "108")
                    expect_equal(params$authKey, "testmockkey_surface_obs") # Check if authKey was added by make_kma_request
                    # Mock a response object that httr::content can handle
                    return(structure(list(status_code = 200, content = charToRaw("Mocked CSV data for hourly")), class = "response"))
                  })
    expect_equal(KMAapiR::get_sfc_hourly_data(tm = "202301010000", stn = "108"), "Mocked CSV data for hourly")
  })
})

test_that("get_sfc_hourly_data_period input validation and structure", {
  expect_error(KMAapiR::get_sfc_hourly_data_period(tm1 = "20231027", tm2 = "202310280000"), "Parameter 'tm1' must be in YYYYMMDDHHMM format.")
  expect_error(KMAapiR::get_sfc_hourly_data_period(tm1 = "202310270000", tm2 = "20231028"), "Parameter 'tm2' must be in YYYYMMDDHHMM format.")
  
  with_mock_auth({
    mockery::stub(KMAapiR::get_sfc_hourly_data_period, 'make_kma_request',
                  function(base_url, params) {
                    expect_equal(base_url, "https://apihub.kma.go.kr/api/typ01/url/kma_sfctm3.php")
                    expect_equal(params$tm1, "202301010000")
                    expect_equal(params$tm2, "202301010100")
                    expect_equal(params$authKey, "testmockkey_surface_obs")
                    return(structure(list(status_code = 200, content = charToRaw("Mocked period CSV data")), class = "response"))
                  })
    expect_equal(KMAapiR::get_sfc_hourly_data_period(tm1 = "202301010000", tm2 = "202301010100"), "Mocked period CSV data")
  })
})

test_that("get_sfc_daily_data input validation and structure", {
  expect_error(KMAapiR::get_sfc_daily_data(tm = "202310"), "Parameter 'tm' must be in YYYYMMDD format.")
  
  with_mock_auth({
    mockery::stub(KMAapiR::get_sfc_daily_data, 'make_kma_request',
                  function(base_url, params) {
                    expect_equal(base_url, "https://apihub.kma.go.kr/api/typ01/url/kma_sfcdd.php")
                    expect_equal(params$tm, "20230101")
                    expect_equal(params$disp, "1")
                    expect_equal(params$authKey, "testmockkey_surface_obs")
                    return(structure(list(status_code = 200, content = charToRaw("Mocked daily CSV data")), class = "response"))
                  })
    expect_equal(KMAapiR::get_sfc_daily_data(tm = "20230101", disp = "1"), "Mocked daily CSV data")
  })
})

test_that("get_sfc_daily_data_period input validation and structure", {
  expect_error(KMAapiR::get_sfc_daily_data_period(tm1 = "202310", tm2 = "20231028", obs = "TA"), "Parameter 'tm1' must be in YYYYMMDD format.")
  expect_error(KMAapiR::get_sfc_daily_data_period(tm1 = "20231027", tm2 = "202310", obs = "TA"), "Parameter 'tm2' must be in YYYYMMDD format.")
  with_mock_auth({
    mockery::stub(KMAapiR::get_sfc_daily_data_period, 'make_kma_request',
                  function(base_url, params) {
                    expect_equal(base_url, "https://apihub.kma.go.kr/api/typ01/url/kma_sfcdd3.php")
                    expect_equal(params$tm1, "20230101")
                    expect_equal(params$obs, "TA")
                    expect_equal(params$authKey, "testmockkey_surface_obs")
                    return(structure(list(status_code = 200, content = charToRaw("Mocked daily period CSV")), class = "response"))
                  })
    expect_equal(KMAapiR::get_sfc_daily_data_period(tm1 = "20230101", tm2 = "20230102", obs = "TA"), "Mocked daily period CSV")
  })
})

test_that("get_sfc_elemental_data input validation and structure", {
  expect_error(KMAapiR::get_sfc_elemental_data(tm2 = "202310A", obs = "TA"), "Parameter 'tm2' must be in YYYYMMDD or YYYYMMDDHHMM format.")
  expect_error(KMAapiR::get_sfc_elemental_data(tm2 = "20231010", obs = "TA", tm1 = "invalid"), "Parameter 'tm1' must be in YYYYMMDD or YYYYMMDDHHMM format if provided.")
  with_mock_auth({
    mockery::stub(KMAapiR::get_sfc_elemental_data, 'make_kma_request',
                  function(base_url, params) {
                    expect_equal(base_url, "https://apihub.kma.go.kr/api/typ01/url/kma_sfctm5.php")
                    expect_equal(params$tm2, "202301011000")
                    expect_null(params$tm1) # Check tm1 is not passed if NULL by make_kma_request's Filter
                    expect_equal(params$authKey, "testmockkey_surface_obs")
                    return(structure(list(status_code = 200, content = charToRaw("Mocked elemental CSV")), class = "response"))
                  })
    expect_equal(KMAapiR::get_sfc_elemental_data(tm2 = "202301011000", obs = "TA"), "Mocked elemental CSV")

    # Test with tm1
    mockery::stub(KMAapiR::get_sfc_elemental_data, 'make_kma_request',
                  function(base_url, params) {
                    expect_equal(params$tm1, "202301010000")
                    expect_equal(params$tm2, "202301011000")
                    expect_equal(params$authKey, "testmockkey_surface_obs")
                    return(structure(list(status_code = 200, content = charToRaw("Mocked elemental period CSV")), class = "response"))
                  })
    expect_equal(KMAapiR::get_sfc_elemental_data(tm2 = "202301011000", obs = "TA", tm1 = "202301010000"), "Mocked elemental period CSV")
  })
})

test_that("get_sfc_normal_data input validation and structure", {
  expect_error(KMAapiR::get_sfc_normal_data(norm = "X", tmst = "2021", mm1="1",dd1="1",mm2="1",dd2="1"), "Parameter 'norm' must be one of 'D', 'S', 'M', 'Y'.")
  with_mock_auth({
    mockery::stub(KMAapiR::get_sfc_normal_data, 'make_kma_request',
                  function(base_url, params) {
                    expect_equal(base_url, "https://apihub.kma.go.kr/api/typ01/url/sfc_norm1.php")
                    expect_equal(params$norm, "D")
                    expect_equal(params$MM1, "1") # API uses MM1
                    expect_equal(params$authKey, "testmockkey_surface_obs")
                    return(structure(list(status_code = 200, content = charToRaw("Mocked normal CSV")), class = "response"))
                  })
    expect_equal(KMAapiR::get_sfc_normal_data(norm = "D", tmst = "2021", mm1 = "1", dd1 = "1", mm2 = "1", dd2 = "10"), "Mocked normal CSV")
  })
})

# Helper for SfcYearlyInfoService and SfcMtlyInfoService tests
test_sfc_xml_json_service <- function(func_name_str, base_url_expected, required_params, optional_params = list()) {
  # Test with XML
  with_mock_auth({
    call_args_xml <- c(list(page_no=1, num_of_rows=10, data_type="XML"), required_params, optional_params)
    
    # Need to qualify func_name_str with package for mockery::stub
    qualified_func_name_str <- paste0("KMAapiR::", func_name_str)

    mockery::stub(where = qualified_func_name_str, what = 'make_kma_request', 
                  how = function(base_url, params) {
                    expect_equal(base_url, base_url_expected)
                    expect_equal(params$dataType, "XML")
                    for(p_name in names(required_params)) expect_equal(params[[p_name]], required_params[[p_name]])
                    if (length(optional_params) > 0) {
                        for(p_name in names(optional_params)) expect_equal(params[[p_name]], optional_params[[p_name]])
                    }
                    expect_equal(params$authKey, "testmockkey_surface_obs")
                    return(structure(list(status_code = 200, content = charToRaw("<xml>Mocked XML</xml>")), class = "response"))
                  })
    expect_equal(do.call(eval(parse(text=qualified_func_name_str)), call_args_xml), "<xml>Mocked XML</xml>")
  })

  # Test with JSON
  with_mock_auth({
    call_args_json <- c(list(page_no=1, num_of_rows=10, data_type="JSON"), required_params, optional_params)
    qualified_func_name_str <- paste0("KMAapiR::", func_name_str)
    mockery::stub(where = qualified_func_name_str, what = 'make_kma_request',
                  how = function(base_url, params) {
                    expect_equal(base_url, base_url_expected)
                    expect_equal(params$dataType, "JSON")
                    expect_equal(params$authKey, "testmockkey_surface_obs")
                    return(structure(list(status_code = 200, content = charToRaw("{\"json\":\"Mocked JSON\"}")), class = "response"))
                  })
    expect_equal(do.call(eval(parse(text=qualified_func_name_str)), call_args_json), "{\"json\":\"Mocked JSON\"}")
  })
  
  # Test input validation for common parameters
  func_to_call <- eval(parse(text=paste0("KMAapiR::", func_name_str)))

  call_args_invalid_dt <- c(list(page_no=1, num_of_rows=10, data_type="TXT"), required_params, optional_params)
  expect_error(do.call(func_to_call, call_args_invalid_dt), "'data_type' must be 'XML' or 'JSON'.")
  
  if("year" %in% names(required_params)) {
    # Create a valid list of args first, then modify the year
    valid_year_args <- c(list(page_no=1, num_of_rows=10, data_type="XML"), required_params, optional_params)
    invalid_year_args <- valid_year_args
    invalid_year_args$year <- "23" # Invalid year
    expect_error(do.call(func_to_call, invalid_year_args), "'year' must be in YYYY format.")
  }
  if("month" %in% names(required_params)) {
    valid_month_args <- c(list(page_no=1, num_of_rows=10, data_type="XML"), required_params, optional_params)
    invalid_month_args <- valid_month_args
    invalid_month_args$month <- "1" # Invalid month
    expect_error(do.call(func_to_call, invalid_month_args), "'month' must be in MM format.")
  }
}

# SfcYearlyInfoService tests
test_that("get_sfc_yearly_summary_info1 works", {
  test_sfc_xml_json_service("get_sfc_yearly_summary_info1", 
                            "https://apihub.kma.go.kr/api/typ02/openApi/SfcYearlyInfoService/getYearSumry", 
                            list(year = "2023"))
})
test_that("get_sfc_yearly_summary_info2 works", {
  test_sfc_xml_json_service("get_sfc_yearly_summary_info2",
                            "https://apihub.kma.go.kr/api/typ02/openApi/SfcYearlyInfoService/getYearSumry2",
                            list(year = "2023"))
})
test_that("get_sfc_avg_temp_anomaly works", {
  test_sfc_xml_json_service("get_sfc_avg_temp_anomaly",
                            "https://apihub.kma.go.kr/api/typ02/openApi/SfcYearlyInfoService/getAvgTaAnamaly",
                            list(year = "2023"))
})
test_that("get_sfc_precip_anomaly works", {
  test_sfc_xml_json_service("get_sfc_precip_anomaly",
                            "https://apihub.kma.go.kr/api/typ02/openApi/SfcYearlyInfoService/getRnAnamaly",
                            list(year = "2023"))
})
test_that("get_sfc_stn_phenom_data1 works", {
  test_sfc_xml_json_service("get_sfc_stn_phenom_data1",
                            "https://apihub.kma.go.kr/api/typ02/openApi/SfcYearlyInfoService/getStnPhnmnData",
                            list(year = "2023"), list(station = "108"))
})
test_that("get_sfc_stn_phenom_data2 works", {
  test_sfc_xml_json_service("get_sfc_stn_phenom_data2",
                            "https://apihub.kma.go.kr/api/typ02/openApi/SfcYearlyInfoService/getStnPhnmnData2",
                            list(year = "2023"), list(station = "108"))
})
test_that("get_sfc_stn_phenom_data3 works", {
  test_sfc_xml_json_service("get_sfc_stn_phenom_data3",
                            "https://apihub.kma.go.kr/api/typ02/openApi/SfcYearlyInfoService/getStnPhnmnData3",
                            list(year = "2023"), list(station = "108"))
})

# SfcMtlyInfoService tests
test_that("get_sfc_monthly_report_notes works", {
  test_sfc_xml_json_service("get_sfc_monthly_report_notes",
                            "https://apihub.kma.go.kr/api/typ02/openApi/SfcMtlyInfoService/getNote",
                            list(year = "2023", month = "01"))
})
test_that("get_sfc_station_list_table works", {
  test_sfc_xml_json_service("get_sfc_station_list_table",
                            "https://apihub.kma.go.kr/api/typ02/openApi/SfcMtlyInfoService/getSfcStnLstTbl",
                            list(year = "2023", month = "01"))
})
test_that("get_sfc_monthly_summary1 works", {
  test_sfc_xml_json_service("get_sfc_monthly_summary1",
                            "https://apihub.kma.go.kr/api/typ02/openApi/SfcMtlyInfoService/getMmSumry",
                            list(year = "2023", month = "01"))
})
test_that("get_sfc_monthly_summary2 works", {
  test_sfc_xml_json_service("get_sfc_monthly_summary2",
                            "https://apihub.kma.go.kr/api/typ02/openApi/SfcMtlyInfoService/getMmSumry2",
                            list(year = "2023", month = "01"))
})
test_that("get_sfc_daily_weather_data_for_month works", {
  test_sfc_xml_json_service("get_sfc_daily_weather_data_for_month",
                            "https://apihub.kma.go.kr/api/typ02/openApi/SfcMtlyInfoService/getDailyWthrData",
                            list(year = "2023", month = "01"), list(station = "108"))
})

test_that("get_sfc_weather_phenomenon_chart works and returns raw content", {
  expect_error(KMAapiR::get_sfc_weather_phenomenon_chart(obs_type="ww_sfc", tm="20231027", val="1", stn="1", obj="mq", map_type="HR", grid="2", legend="1", size="600", itv="5", zoom_level="0", zoom_x="0", zoom_y="0"), 
               "Parameter 'tm' must be in YYYYMMDDHHMM format.")

  with_mock_auth({
    mockery::stub(KMAapiR::get_sfc_weather_phenomenon_chart, 'make_kma_request',
                  function(base_url, params) {
                    expect_equal(base_url, "https://apihub.kma.go.kr/api/typ03/php/alw/sfc/sfc_ww_pnt.php")
                    expect_equal(params$obs, "test_obs")
                    expect_equal(params$map, "test_map")
                    expect_equal(params$authKey, "testmockkey_surface_obs")
                    # Mock a response object with raw content
                    return(structure(list(status_code = 200, content = as.raw(c(0x01, 0x02, 0x03))), class = "response"))
                  })
    
    result <- KMAapiR::get_sfc_weather_phenomenon_chart(obs_type = "test_obs", tm = "202301010000", 
                                             val = "1", stn = "1", obj = "obj", map_type = "test_map",
                                             grid = "grid", legend = "1", size = "600", itv = "5",
                                             zoom_level = "0", zoom_x = "000", zoom_y = "000")
    expect_true(is.raw(result))
    expect_equal(result, as.raw(c(0x01, 0x02, 0x03)))
  })
})
