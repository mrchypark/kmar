library(testthat)
# Assuming KMAapiR is loaded via devtools or test_check
# And .kma_api_env is accessible for with_mock_auth if defined in KMAapiR namespace

# Fallback: define with_mock_auth if not found (e.g. during isolated testing)
# A more robust solution would be to place this in a helper file (e.g. tests/testthat/helper-auth.R)
# and ensure the KMAapiR package is loaded (e.g. via devtools::load_all()) before tests run.
if (!exists("with_mock_auth", mode="function")) {
    with_mock_auth <- function(expr) {
      # This simplified version primarily relies on Sys.setenv for get_kma_auth_key behavior.
      # Direct manipulation of .kma_api_env for set_kma_auth_key testing is more complex
      # without the package being fully loaded in the test environment.
      old_key_env_var <- Sys.getenv("KMA_API_KEY", unset = NA)
      Sys.setenv(KMA_API_KEY = "testmockkey_marine_obs_fallback") # Unique key for fallback
      
      # Attempt to use set_kma_auth_key if available (e.g. if package is loaded)
      # This part might not always work as expected in all test environments if KMAapiR isn't loaded
      # such that its namespace and internal environment .kma_api_env are properly accessible.
      # For the purpose of these tests, where make_kma_request is mocked,
      # the key check within make_kma_request (via get_kma_auth_key) will pick up the Sys.getenv.
      original_session_key <- NULL
      session_key_was_present <- FALSE
      if (exists("set_kma_auth_key", where = "package:KMAapiR", mode="function") &&
          exists(".kma_api_env", where = asNamespace("KMAapiR"))) {
            kma_env <- KMAapiR:::.kma_api_env
            if(exists("auth_key", envir = kma_env)) {
                original_session_key <- kma_env$auth_key
                session_key_was_present <- TRUE
            }
            # Set it for the duration of the test, get_kma_auth_key will prefer Sys.getenv anyway
            kma_env$auth_key <- "testmockkey_marine_obs_fallback"
      }

      on.exit({
        if (is.na(old_key_env_var)) {
          Sys.unsetenv("KMA_API_KEY")
        } else {
          Sys.setenv(KMA_API_KEY = old_key_env_var)
        }
        # Restore session key if it was originally present
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


test_that("get_marine_comprehensive_obs input validation and structure", {
  expect_error(KMAapiR::get_marine_comprehensive_obs(tm = "20231027"), "Parameter 'tm' must be in YYYYMMDDHHMM format.")
  with_mock_auth({
    mockery::stub(KMAapiR::get_marine_comprehensive_obs, 'make_kma_request', 
                  function(base_url, params) {
                    expect_equal(base_url, "https://apihub.kma.go.kr/api/typ01/url/sea_obs.php")
                    expect_equal(params$tm, "202301010000")
                    expect_equal(params$authKey, "testmockkey_marine_obs_fallback")
                    return(structure(list(status_code = 200, content = charToRaw("Mocked marine CSV")), class = "response"))
                  })
    expect_equal(KMAapiR::get_marine_comprehensive_obs(tm = "202301010000"), "Mocked marine CSV")
  })
})

test_that("get_marine_buoy_data_period input validation and structure", {
  expect_error(KMAapiR::get_marine_buoy_data_period(tm1 = "20231027", tm2 = "202310280000"), "Parameter 'tm1' must be in YYYYMMDDHHMM format.")
  expect_error(KMAapiR::get_marine_buoy_data_period(tm1 = "202310270000", tm2 = "20231028"), "Parameter 'tm2' must be in YYYYMMDDHHMM format.")
  with_mock_auth({
    mockery::stub(KMAapiR::get_marine_buoy_data_period, 'make_kma_request',
                  function(base_url, params) {
                    expect_equal(base_url, "https://apihub.kma.go.kr/api/typ01/url/kma_buoy2.php")
                    expect_equal(params$authKey, "testmockkey_marine_obs_fallback")
                    return(structure(list(status_code = 200, content = charToRaw("Mocked marine period CSV")), class = "response"))
                  })
    expect_equal(KMAapiR::get_marine_buoy_data_period(tm1 = "202301010000", tm2 = "202301010100"), "Mocked marine period CSV")
  })
})

test_that("get_marine_kma_buoy_data input validation and structure", {
  expect_error(KMAapiR::get_marine_kma_buoy_data(tm = "202310"), "Parameter 'tm' must be in YYYYMMDDHHMM format.")
  with_mock_auth({
    mockery::stub(KMAapiR::get_marine_kma_buoy_data, 'make_kma_request',
                  function(base_url, params) {
                    expect_equal(base_url, "https://apihub.kma.go.kr/api/typ01/url/kma_buoy.php")
                    expect_equal(params$authKey, "testmockkey_marine_obs_fallback")
                    return(structure(list(status_code = 200, content = charToRaw("Mocked KMA buoy CSV")), class = "response"))
                  })
    expect_equal(KMAapiR::get_marine_kma_buoy_data(tm = "202301010000"), "Mocked KMA buoy CSV")
  })
})

# Helper for SeaMtlyInfoService tests (XML/JSON based services)
test_marine_xml_json_service <- function(func_name_str, base_url_expected, required_params, optional_params = list()) {
  # Ensure func_name_str is a character string for the function name
  # Construct the fully qualified function name for use with do.call and mockery::stub
  qualified_func_name <- paste0("KMAapiR::", func_name_str)
  target_func <- eval(parse(text = qualified_func_name))


  # Test with XML
  with_mock_auth({
    call_args_xml <- c(list(page_no = 1, num_of_rows = 10, data_type = "XML"), required_params, optional_params)
    
    mockery::stub(where = qualified_func_name, what = 'make_kma_request', 
                  how = function(base_url, params) {
                    expect_equal(base_url, base_url_expected)
                    expect_equal(params$dataType, "XML")
                    for(p_name in names(required_params)) expect_equal(params[[p_name]], required_params[[p_name]])
                    if (length(optional_params) > 0) {
                        for(p_name in names(optional_params)) expect_equal(params[[p_name]], optional_params[[p_name]])
                    }
                    expect_equal(params$authKey, "testmockkey_marine_obs_fallback")
                    return(structure(list(status_code = 200, content = charToRaw("<xml>Mocked Marine XML</xml>")), class = "response"))
                  })
    expect_equal(do.call(target_func, call_args_xml), "<xml>Mocked Marine XML</xml>")
  })

  # Test with JSON
  with_mock_auth({
    call_args_json <- c(list(page_no = 1, num_of_rows = 10, data_type = "JSON"), required_params, optional_params)
    mockery::stub(where = qualified_func_name, what = 'make_kma_request',
                  how = function(base_url, params) {
                    expect_equal(base_url, base_url_expected)
                    expect_equal(params$dataType, "JSON")
                    expect_equal(params$authKey, "testmockkey_marine_obs_fallback")
                    return(structure(list(status_code = 200, content = charToRaw("{\"json\":\"Mocked Marine JSON\"}")), class = "response"))
                  })
    expect_equal(do.call(target_func, call_args_json), "{\"json\":\"Mocked Marine JSON\"}")
  })
  
  # Test input validation for common parameters
  call_args_invalid_dt <- c(list(page_no = 1, num_of_rows = 10, data_type = "TXT"), required_params, optional_params)
  expect_error(do.call(target_func, call_args_invalid_dt), "'data_type' must be 'XML' or 'JSON'.")
  
  # Create a base list for testing invalid year/month to avoid modifying required_params directly in loop
  base_test_args <- c(list(page_no = 1, num_of_rows = 10, data_type = "XML"), optional_params)

  if("year" %in% names(required_params)) {
    invalid_year_args <- c(base_test_args, required_params) 
    invalid_year_args$year <- "23" 
    expect_error(do.call(target_func, invalid_year_args), "'year' must be in YYYY format.")
  }
  if("month" %in% names(required_params)) {
    invalid_month_args <- c(base_test_args, required_params) 
    invalid_month_args$month <- "1" 
    if (!("year" %in% names(invalid_month_args) && invalid_month_args$year == "23")) { # Avoid re-testing year error if already invalid
        if (!("year" %in% names(invalid_month_args))) invalid_month_args$year <- "2023" # Add valid year if not present
    }
    expect_error(do.call(target_func, invalid_month_args), "'month' must be in MM format.")
  }
}

# --- SeaMtlyInfoService Tests ---
test_that("get_marine_monthly_report_notes works", {
  test_marine_xml_json_service("get_marine_monthly_report_notes", "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getNote", list(year="2023", month="01"))
})
test_that("get_marine_buoy_list_table works", {
  test_marine_xml_json_service("get_marine_buoy_list_table", "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getBuoyLstTbl", list(year="2023", month="01"))
})
test_that("get_marine_lhaws_list_table works", {
  test_marine_xml_json_service("get_marine_lhaws_list_table", "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getLhawsLstTbl", list(year="2023", month="01"))
})
test_that("get_marine_wave_buoy_list_table works", {
  test_marine_xml_json_service("get_marine_wave_buoy_list_table", "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getWaveBuoyLstTbl", list(year="2023", month="01"))
})
test_that("get_marine_obs_open_years works", {
  test_marine_xml_json_service("get_marine_obs_open_years", "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getObsOpenYear", list(year="2023", month="01"))
})

# Buoy Monthly Summaries Tests
test_that("get_marine_buoy_monthly_summary1 works", {
  test_marine_xml_json_service("get_marine_buoy_monthly_summary1", "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getBuoyMmSumry", list(year="2023", month="01"))
})
test_that("get_marine_buoy_monthly_summary2 works", {
  test_marine_xml_json_service("get_marine_buoy_monthly_summary2", "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getBuoyMmSumry2", list(year="2023", month="01"))
})
test_that("get_marine_daily_buoy_data_for_month works", {
  test_marine_xml_json_service("get_marine_daily_buoy_data_for_month", "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getDailyBuoy", list(year="2023", month="01"), list(station="22101"))
})

# Lighthouse AWS Monthly Summaries Tests
test_that("get_marine_lhaws_monthly_summary1 works", {
  test_marine_xml_json_service("get_marine_lhaws_monthly_summary1", "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getLhawsMmSumry", list(year="2023", month="01"))
})
test_that("get_marine_lhaws_monthly_summary2 works", {
  test_marine_xml_json_service("get_marine_lhaws_monthly_summary2", "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getLhawsMmSumry2", list(year="2023", month="01"))
})
test_that("get_marine_daily_lhaws_data_for_month works", {
  test_marine_xml_json_service("get_marine_daily_lhaws_data_for_month", "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getDailyLhaws", list(year="2023", month="01"), list(station="955"))
})

# Wave Buoy Monthly Summaries Tests
test_that("get_marine_wave_buoy_monthly_summary1 works", {
  test_marine_xml_json_service("get_marine_wave_buoy_monthly_summary1", "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getWaveBuoyMmSumry", list(year="2023", month="01"))
})
test_that("get_marine_wave_buoy_monthly_summary2 works", {
  test_marine_xml_json_service("get_marine_wave_buoy_monthly_summary2", "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getWaveBuoyMmSumry2", list(year="2023", month="01"))
})
test_that("get_marine_daily_wave_buoy_data_for_month works", {
  test_marine_xml_json_service("get_marine_daily_wave_buoy_data_for_month", "https://apihub.kma.go.kr/api/typ02/openApi/SeaMtlyInfoService/getDailyWaveBuoy", list(year="2023", month="01"), list(station="22441"))
})

test_that("get_marine_spatial_chart works and returns raw content", {
  # Test input validation
  expect_error(
    KMAapiR::get_marine_spatial_chart(proj="P",start_x="X",start_y="Y",end_x="X",end_y="Y",zoom_lvl="0",
                             tm="20231027", tm_st="202301010000", map_type="M",grid="G",itv="I",
                             data_dtl_cd="D",obs="O",stn="S",size="600",sel_ws="ms"),
    "Parameter 'tm' must be in YYYYMMDDHHMM format."
  )
  expect_error(
    KMAapiR::get_marine_spatial_chart(proj="P",start_x="X",start_y="Y",end_x="X",end_y="Y",zoom_lvl="0",
                             tm="202301010000", tm_st="20231027", map_type="M",grid="G",itv="I",
                             data_dtl_cd="D",obs="O",stn="S",size="600",sel_ws="ms"),
    "Parameter 'tm_st' must be in YYYYMMDDHHMM format."
  )

  with_mock_auth({
    mockery::stub(KMAapiR::get_marine_spatial_chart, 'make_kma_request',
                  function(base_url, params) {
                    expect_equal(base_url, "https://apihub.kma.go.kr/api/typ03/cgi/aws3/nph-sea_obs_imgp1")
                    expect_equal(params$PROJ, "test_proj")
                    expect_equal(params$map, "test_map") # API uses 'map'
                    expect_equal(params$dataDtlCd, "test_code") # API uses 'dataDtlCd'
                    expect_equal(params$selWs, "test_ws") # API uses 'selWs'
                    expect_equal(params$authKey, "testmockkey_marine_obs_fallback")
                    # Mock a response object with raw content
                    return(structure(list(status_code = 200, content = as.raw(c(0x0A, 0x0B, 0x0C))), class = "response"))
                  })
    
    result <- KMAapiR::get_marine_spatial_chart(
        proj="test_proj", start_x="X", start_y="Y", end_x="X", end_y="Y", zoom_lvl="0",
        tm="202301010000", tm_st="202301010000", map_type="test_map", grid="G", itv="I",
        data_dtl_cd="test_code", obs="O", stn="S", size="600", sel_ws="test_ws"
    )
    expect_true(is.raw(result))
    expect_equal(result, as.raw(c(0x0A, 0x0B, 0x0C)))
  })
})
