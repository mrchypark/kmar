library(testthat)
# Assuming KMAapiR is loaded and with_mock_auth is available
if (!exists("with_mock_auth", mode="function")) {
    with_mock_auth <- function(expr) {
      old_key_env_var <- Sys.getenv("KMA_API_KEY", unset = NA)
      Sys.setenv(KMA_API_KEY = "testmockkey_radar_fallback") 
      
      original_session_key <- NULL
      session_key_was_present <- FALSE
      if (exists("set_kma_auth_key", where = "package:KMAapiR", mode="function") &&
          exists(".kma_api_env", where = asNamespace("KMAapiR"))) {
            kma_env <- KMAapiR:::.kma_api_env
            if(exists("auth_key", envir = kma_env)) {
                original_session_key <- kma_env$auth_key
                session_key_was_present <- TRUE
            }
            kma_env$auth_key <- "testmockkey_radar_fallback"
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

test_that("get_radar_station_file_list works", {
  expect_error(KMAapiR::get_radar_station_file_list(tm="2023", stn="KWK", rdr="HSR"), "Parameter 'tm' must be in YYYYMMDD format.")
  with_mock_auth({
    mockery::stub(KMAapiR::get_radar_station_file_list, 'make_kma_request', 
                  function(base_url, params) {
                    expect_equal(base_url, "https://apihub.kma.go.kr/api/typ01/url/rdr_stn_file_list.php")
                    expect_equal(params$filter, "test")
                    expect_equal(params$authKey, "testmockkey_radar_fallback")
                    return(structure(list(status_code=200, content=charToRaw("file1.txt\nfile2.txt")), class="response"))
                  })
    expect_equal(KMAapiR::get_radar_station_file_list(tm="20230101", stn="KWK", rdr="HSR", filter_str="test"), "file1.txt\nfile2.txt")
  })
})

test_that("get_radar_composite_file_list works", {
  expect_error(KMAapiR::get_radar_composite_file_list(tm="2023111", cmp="HSR"), "Parameter 'tm' must be in YYYYMMDD format.")
  with_mock_auth({
    mockery::stub(KMAapiR::get_radar_composite_file_list, 'make_kma_request', 
                  function(base_url, params) {
                    expect_equal(base_url, "https://apihub.kma.go.kr/api/typ01/url/rdr_cmp_file_list.php")
                    expect_equal(params$authKey, "testmockkey_radar_fallback")
                    return(structure(list(status_code=200, content=charToRaw("cmpfile1.dat")), class="response"))
                  })
    expect_equal(KMAapiR::get_radar_composite_file_list(tm="20230101", cmp="HSR"), "cmpfile1.dat")
  })
})

test_that("get_radar_composite_info works", {
   expect_error(KMAapiR::get_radar_composite_info(tm="2023", cmp="HSR", qcd="MSK"), "Parameter 'tm' must be in YYYYMMDDHH\\(MM\\) format.")
  with_mock_auth({
    mockery::stub(KMAapiR::get_radar_composite_info, 'make_kma_request', 
                  function(base_url, params) {
                    expect_equal(base_url, "https://apihub.kma.go.kr/api/typ01/cgi-bin/url/nph-rdr_cmp_inf")
                    expect_equal(params$map, "HR") # API uses 'map'
                    expect_equal(params$authKey, "testmockkey_radar_fallback")
                    return(structure(list(status_code=200, content=charToRaw("Info text")), class="response"))
                  })
    expect_equal(KMAapiR::get_radar_composite_info(tm="202301010000", cmp="HSR", qcd="MSK", map_area="HR"), "Info text")
  })
})

test_that("get_radar_composite_data works for ASCII and Binary", {
  expect_error(KMAapiR::get_radar_composite_data(tm="2023", cmp="HSR", qcd="MSK", obs="ECHO"), "Parameter 'tm' must be in YYYYMMDDHH\\(MM\\) format.")
  expect_error(KMAapiR::get_radar_composite_data(tm="202301010000", cmp="HSR", qcd="MSK"), "Parameter 'obs' is required for data retrieval.")

  # Test ASCII
  with_mock_auth({
    mockery::stub(KMAapiR::get_radar_composite_data, 'make_kma_request', 
                  function(base_url, params) {
                    expect_equal(base_url, "https://apihub.kma.go.kr/api/typ01/cgi-bin/url/nph-rdr_cmp1_api")
                    expect_equal(params$disp, "A")
                    expect_equal(params$authKey, "testmockkey_radar_fallback")
                    return(structure(list(status_code=200, content=charToRaw("ASCII data")), class="response"))
                  })
    expect_equal(KMAapiR::get_radar_composite_data(tm="202301010000", cmp="HSR", qcd="MSK", obs="ECHO", disp="A"), "ASCII data")
  })

  # Test Binary
  with_mock_auth({
    mockery::stub(KMAapiR::get_radar_composite_data, 'make_kma_request', 
                  function(base_url, params) {
                    expect_equal(base_url, "https://apihub.kma.go.kr/api/typ01/cgi-bin/url/nph-rdr_cmp1_api")
                    expect_equal(params$disp, "B")
                    expect_equal(params$authKey, "testmockkey_radar_fallback")
                    return(structure(list(status_code=200, content=as.raw(c(1,2,3,4))), class="response"))
                  })
    expect_equal(KMAapiR::get_radar_composite_data(tm="202301010000", cmp="HSR", qcd="MSK", obs="ECHO", disp="B"), as.raw(c(1,2,3,4)))
  })
})
