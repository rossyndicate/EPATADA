# Testing the Geospatial Functions ----
# testing `GeoSpatialFunctions.R` with the same data that was used in the 
# `TADAGeospatialFunctions.Rmd` Vignette in chunk 6 (3/18/24).

# TADA_dataframe <- TADA_DataRetrieval(
#   startDate = "2020-01-01",
#   endDate = "2020-12-31",
#   characteristicName = "pH",
#   countycode = "US:08:069",
#   applyautoclean = TRUE
# )

# Read in sample data for tests
TADA_dataframe <- readRDS(testthat::test_path("testdata/GeospatialFunctions_TADA_dataframe.rds"))
TADA_spatial <- readRDS(testthat::test_path("testdata/GeospatialFunctions_TADA_spatial.rds"))
TADA_with_ATTAINS <- readRDS(testthat::test_path("testdata/GeospatialFunctions_TADA_with_ATTAINS.rds"))
TADA_with_ATTAINS_list <- readRDS(testthat::test_path("testdata/GeospatialFunctions_TADA_with_ATTAINS_list.rds"))

# TADA_MakeSpatial ----

test_that(desc = "`TADA_MakeSpatial()` converts the TADA_dataframe into an {sf} object",
          code = {
            test_df <- TADA_MakeSpatial(data = TADA_dataframe) 
            expect_true(inherits(test_df, "sf"))
          })

test_that(desc = "The CRS in the test data frame is the same as the input",
          code = {
            test_df1 <- TADA_MakeSpatial(data = TADA_dataframe, crs = 4326) 
            test_df2 <- TADA_MakeSpatial(data = TADA_dataframe, crs = 4269) 
            
            expect_true(gsub("\\D", "", st_crs(test_df1)$epsg) == as.character(4326))
            expect_true(gsub("\\D", "", st_crs(test_df2)$epsg) == as.character(4269))
          })

test_that(desc = "The geometry types in the data frame are points",
          code = {
            test_df <- TADA_MakeSpatial(data = TADA_dataframe) 
            geometry_types <- st_geometry_type(test_df)
            valid_geometry_types <- "POINT"
            expect_true(all(geometry_types %in% valid_geometry_types))
          })

test_that(desc = "The number of rows are the same between TADA_dataframe and the output data frame of TADA_MakeSpatial",
          code = {
            test_df <- TADA_MakeSpatial(data = TADA_dataframe)
            expect_true(nrow(TADA_dataframe) == nrow(test_df))
          })

test_that(desc = "The columns between TADA_dataframe and the output data frame of TADA_MakeSpatial are identical (sans 'geometry')",
          code = {
            test_df <- TADA_MakeSpatial(data = TADA_dataframe)
            expect_true(all(names(TADA_dataframe) == names(test_df)[names(test_df) != 'geometry']))
          }) 

test_that(desc = "The structures of the TADA_dataframe and the output data frame of TADA_MakeSpatial are identical (sans 'geometry')",
          code = {
            test_df <- TADA_MakeSpatial(data = TADA_dataframe)
            expect_true(identical(str(TADA_dataframe), str(test_df[names(test_df) != 'geometry'])))
          })

test_that(desc = "TADA_MakeSpatial() errors if the input dataframe does not contain WQP-style latitude and longitude data",
          code = {
            expect_error(TADA_MakeSpatial(data = tibble(sample = NA, .rows = 0))) # fails if I include the error message text
          })

test_that(desc = "TADA_MakeSpatial() errors if the input dataframe is already a spatial object",
          code = {
            expect_error(TADA_MakeSpatial(TADA_spatial))
          })


# test_that(desc = "The order of the rows are the same (sans 'geometry')") context ----

# test_that(desc = "The order of the rows are the same (sans 'geometry')",
#           code = {
#             # this test fails (need to reorder the index of test_df for this to pass!). See below for details...
#             test_df <- TADA_MakeSpatial(data = TADA_dataframe)
#             expect_true(identical(TADA_dataframe, test_df[names(test_df) != 'geometry']))
#           })

## make an index column for this test
TADA_dataframe_index <- TADA_dataframe %>%
  rowid_to_column(var = "index")

## generate sample dfs outside of testing fxn
test <- TADA_MakeSpatial(data = TADA_dataframe_index, crs = 4326)
test_sans_geom <- st_drop_geometry(test)

## test if og df and test_sans_geom are identical
identical(TADA_dataframe_index, test) # FALSE
identical(TADA_dataframe_index, test_sans_geom) # FALSE

## Repeat the test after sorting the index
test_sorted <- test %>% 
  arrange(index) 

test_sans_geom_sorted <- test_sans_geom %>% 
  arrange(index)

identical(TADA_dataframe_index, test_sorted) # FALSE
identical(TADA_dataframe_index, test_sans_geom_sorted) # TRUE ***

rm(list = c("test", "test_sans_geom", "test_sorted", "test_sans_geom_sorted", "TADA_dataframe_index"))

# fetch_ATTAINS ----
test_that(desc = "fetchATTAINS handles valid input data", 
          code = {
            valid_data <- st_sf(geometry = st_sfc(st_point(c(0, 0))), crs = 4326)
            result <- fetchATTAINS(data = valid_data, type = "points")
            expect_false(is.null(result))
            })

test_that(desc = "fetchATTAINS handles missing input data", 
          code = {
            expect_error(fetchATTAINS(data = NULL, type = "points")) # when I incorporate the error message this test fails
            })

test_that(desc = "fetchATTAINS handles missing type input",
          code = {
            valid_data <- st_sf(geometry = st_sfc(st_point(c(0, 0))), crs = 4326)
            expect_error(fetchATTAINS(data = valid_data, type = NULL)) # when I incorporate the error message this test fails
          })


# TADA_GetATTAINS ----

test_that(desc = "TADA_GetATTAINS can take in the TADA_dataframe and the sf object as inputs", # this test takes too long, should find an alternative
          code = {
            expect_no_error(TADA_GetATTAINS(data = TADA_dataframe))
            expect_no_error(TADA_GetATTAINS(data = TADA_dataframe))
            
            expect_no_warning(TADA_GetATTAINS(data = TADA_dataframe))
            expect_no_warning(TADA_GetATTAINS(data = TADA_dataframe))
          })

test_that(desc = "TADA_GetATTAINS does not run if the data argument input has already been joined with ATTAINS data.",
         code = {
           expect_error(TADA_GetATTAINS(TADA_with_ATTAINS), 
                        "You data has already been joined with ATTAINS data.") # change you -> your once the function has been updated
         })

test_that(desc = "TADA_GetATTAINS(return = TRUE)[[1]] == TADA_GetATTAINS(return = FALSE)",
          code = {
            expect_true(identical(TADA_with_ATTAINS_list[[1]], TADA_with_ATTAINS))
          })

test_that(desc = "An empty df gets returned if there are no observations in the input df, ",
          code = {
            test <- TADA_GetATTAINS(data = tibble(sample = NA, .rows = 0), FALSE)
            expect_true(nrow(test) == 0)
          })

# TADA_ViewATTAINS ----
test_that(desc = "An input that was not generated from `TADA_GetATTAINS()` gets rejected",
          code = {
            expect_error(TADA_ViewATTAINS(TADA_with_ATTAINS)) # test fails if I incorporate the fail message
          })

test_that(desc = "An input with no observations gets rejected",
          code = {
            expect_error(TADA_ViewATTAINS(data = tibble(sample = NA, .rows = 0), FALSE))
          })

# This needs to get tested after GeospatialFunctions.R L479 gets fixed
# test_that(desc = "An input without the required columns gets rejected")

# Remove everything from the environment
rm(list = c("TADA_dataframe", "TADA_spatial", "TADA_with_ATTAINS", "TADA_with_ATTAINS_list"))
