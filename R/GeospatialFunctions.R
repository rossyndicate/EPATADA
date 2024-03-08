#' TADA_MakeSpatial
#' 
#' Transform a Water Quality Portal dataframe into a geospatial {sf} object.
#' 
#' @param data A dataframe created by `TADA_DataRetrieval()`.
#' @param crs The coordinate reference system (CRS) you would like the returned point features to be in. The default is CRS 4326 (WGS84).
#' 
#' @return The original TADA Water Quality Portal dataframe but as geospatial {sf} point objects. 
#' 
#' @seealso [TADA_DataRetrieval()]
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' tada_not_spatial <- TADA_DataRetrieval(startDate = "1995-01-01",
#'                                        endDate = "1995-12-31",
#'                                        characteristicName = "pH",
#'                                        statecode = "SC", 
#'                                        countycode = "Abbeville",
#'                                        applyautoclean = TRUE)
#' 
#' # make `tada_not_spatial` an {sf} object, projected in crs = 4269 (NAD83)                                 
#' tada_spatial <- TADA_MakeSpatial(data = tada_not_spatial, crs = 4269)
#' }

TADA_MakeSpatial <- function(data, crs = 4326){
  
  # must have all columns needed to make spatial
  if (!"LongitudeMeasure" %in% colnames(data) & !"LatitudeMeasure" %in% colnames(data)) {
    stop("The dataframe does not contain WQP-style latitude and longitude data (column names `HorizontalCoordinateReferenceSystemDatumName`, `LatitudeMeasure` and LongitudeMeasure` or `TADA.LongitudeMeasure` and `TADA.LatitudeMeasures`.")
  } else if (!is.null(data) & inherits(data, "sf")) { stop("Your data is already a spatial object.")
  }
  
  # Make a reference table for CRS and EPSG codes
  epsg_codes <- tribble(
    ~ HorizontalCoordinateReferenceSystemDatumName, ~ epsg,
    "NAD83", 4269,
    "WGS84", 4326,
    "NAD27", 4267,
    # For now assume these are WGS84. USGS has done this, too. 
    "UNKWN", 4326,
    "OTHER", 4326,
    "OLDHI", 4135
  )
  
  suppressMessages(suppressWarnings({
    # join our CRS reference table to our original WQP dataframe:
    sf <- data %>%
      mutate(lat = as.numeric(LatitudeMeasure),
             lon = as.numeric(LongitudeMeasure)) %>%
      # Add EPSG codes
      dplyr::left_join(x = .,
                       y = epsg_codes,
                       by = "HorizontalCoordinateReferenceSystemDatumName") %>%
      # Group by CRS:
      split(f = .$HorizontalCoordinateReferenceSystemDatumName) %>%
      # Transform and re-stack:
      purrr::map_df(.x = .,
                    .f = ~ .x %>%
                      sf::st_as_sf(coords = c("lon", "lat"),
                                   crs = unique(.x$epsg)) %>%
                      # transform to the selected CRS:
                      sf::st_transform(sf::st_crs(as.numeric(crs)))) %>%
      dplyr::select(-epsg)
  }))
  
  return(sf)
  
}


#' fetchATTAINS
#' 
#' Fetch ATTAINS features within a bounding box produced from a set of TADA spatial features.
#' 
#' @param data A dataframe developed using `TADA_DataRetrieval()` or `TADA_MakeSpatial()`.
#' @param type The type of ATTAINS data you would like to fetch ("lines", "points", "polygons", "catchments"). All ATTAINS features are returned in WGS84 (crs = 4326).
#' 
#' @return spatial features that are within the spatial bounding box of water quality observations.
#' 
#' @seealso [TADA_MakeSpatial()]
#' @seealso [TADA_DataRetrieval()]
#' 
#' @examples
#' \dontrun{
#'tada_data <- TADA_DataRetrieval(startDate = "1990-01-01",
#'                                endDate = "1995-12-31",
#'                                characteristicName = "pH",
#'                                statecode = "NV",
#'                                applyautoclean = TRUE)
#'                                  
#'nv_attains_lines <- fetchATTAINS(data = tada_data, type = "lines")
#' }

fetchATTAINS <- function(data, type = NULL) {
  
  if(is.null(data) | nrow(data) == 0){
    stop("There is no data in your `data` object to use as a bounding box for selecting ATTAINS features.")
  }
  
  # If data is already spatial, just make sure it is in the right CRS
  # and add an index as the WQP observations' unique IDs...
  if (!is.null(data) & inherits(data, "sf")) {
    data <- data %>%
      sf::st_transform(4326) 
  } else {
    # ... Otherwise transform into a spatial object then do the same thing:
    data <- data %>%
      #convert dataframe to a spatial object
      TADA_MakeSpatial(data = ., crs = 4326) 
  }
  
  if(is.null(type)){
    stop("Please select the type of ATTAINS data you would like to import ('catchments', 'lines', 'points', or 'polygons').")
  }
  
  # select baseurl based on type of ATTAINS data you want to download:
  if(type == "catchments"){
    baseurl <- "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/3/query?"
  }else if(type == "points"){
    baseurl <-  "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/0/query?"
  }else if(type == "lines"){
    baseurl <-  "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/1/query?"
  }else if(type == "polygons"){
    baseurl <-  "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/2/query?"
  }
  
  #starting at feature 1 (i.e., no offset):
  offset <- 0 
  # empty list to store all features in
  all_features <- list()
  
  # EPSG we want our ATTAINS data to be in (always 4326 for this function)
  epsg <- sf::st_crs(data)$epsg
  
  # bounding box (with some minor wiggle) of user's WQP data
  suppressMessages(suppressWarnings({bbox <- data %>% 
    sf::st_buffer(0.001) %>% 
    sf::st_bbox(data) %>%
    # convert bounding box to characters
    toString(.) %>% 
    # encode for use within the API URL
    urltools::url_encode(.)}))
  
  # The ATTAINS API has a limit of 2000 features that can be pulled in at once.
  # Therefore, we must split the call into manageable "chunks" using a moving
  # window of what features to pull in, then munging all the separate API calls 
  # together.
  
  repeat {
    
    query <- urltools::param_set(baseurl, key = "geometry", value = bbox) %>%
      urltools::param_set(key = "inSR", value = epsg) %>%
      # Total of 2000 features at a time...
      urltools::param_set(key = "resultRecordCount", value = 2000) %>%
      # ... starting at the "offset":
      urltools::param_set(key = "resultOffset", value = offset) %>%
      urltools::param_set(key = "spatialRel", value = "esriSpatialRelIntersects") %>%
      urltools::param_set(key = "f", value = "geojson") %>%
      urltools::param_set(key = "outFields", value = "*") %>%
      urltools::param_set(key = "geometryType", value = "esriGeometryEnvelope") %>%
      urltools::param_set(key = "returnGeometry", value = "true") %>%
      urltools::param_set(key = "returnTrueCurves", value = "false") %>%
      urltools::param_set(key = "returnIdsOnly", value = "false") %>%
      urltools::param_set(key = "returnCountOnly", value = "false") %>%
      urltools::param_set(key = "returnZ", value = "false") %>%
      urltools::param_set(key = "returnM", value = "false") %>%
      urltools::param_set(key = "returnDistinctValues", value = "false") %>%
      urltools::param_set(key = "returnExtentOnly", value = "false") %>%
      urltools::param_set(key = "featureEncoding", value = "esriDefault")
    
    # Fetch features within the offset window and append to list:
    features <- suppressMessages(suppressWarnings({tryCatch({
      geojsonsf::geojson_sf(query)
    }, error = function(e) {
      NULL
    })}))
    
    
    # Exit loop if no more features or error occurred
    if (is.null(features) || nrow(features) == 0) {
      break
    }
    
    all_features <- c(all_features, list(features))
    # once done, change offset by 2000 features:
    offset <- offset + 2000
    
    if(offset == 4000){print("Your TADA data covers a large spatial range. The ATTAINS pull may take a while.")}
    
  }
  
  all_features <- bind_rows(all_features)
  
  return(all_features)
  
}


#' TADA_GetATTAINS
#' 
#' Link catchment-based ATTAINS assessment unit data to Water Quality Portal observations, often imported via `TADA_DataRetrieval()`. This function returns the same raw objects that are mapped in `TADA_ViewATTAINS()`.
#' 
#' @param data A dataframe created by `TADA_DataRetrieval()` or the {sf} equivalent made by `TADA_MakeSpatial()`.
#' @param return Whether to add the associated ATTAINS_catchments, ATTAINS_lines, ATTAINS_points, and ATTAINS_polygons shapefile objects into your Global Environment. TRUE (yes, return) or FALSE (no, do not return). All ATTAINS features are in WGS84 (crs = 4326).
#' 
#' @return The original `TADA_DataRetrieval()` dataframe with additional columns associated with the ATTAINS assessment unit data. 
#' 
#' @seealso [TADA_DataRetrieval()]
#' @seealso [TADA_MakeSpatial()]
#' @seealso [TADA_ViewATTAINS()]
#' 
#' @export
#'
#' @examples
#' \dontrun{
#'tada_data <- TADA_DataRetrieval(startDate = "1990-01-01",
#'                                endDate = "1995-12-31",
#'                                characteristicName = "pH",
#'                                statecode = "NV",
#'                                applyautoclean = TRUE)
#'                                  
#'tada_attains <- TADA_GetATTAINS(data = tada_data, return = TRUE)
#' }

TADA_GetATTAINS <- function(data, return = FALSE){
  
  if(nrow(data) == 0){
    
    print("Your Water Quality Portal dataframe has no observations. Returning an empty dataframe.")
    
    # if no WQP observations, return a modified `data` with empty ATTAINS-related columns:
    no_WQP_data <- data %>%
      dplyr::mutate("ATTAINS.organizationid" = NA, "ATTAINS.submissionid" = NA, "ATTAINS.hasprotectionplan" = NA,
                    "ATTAINS.assessmentunitname" = NA, "ATTAINS.nhdplusid" = NA, "ATTAINS.tas303d" = NA,                                                  
                    "ATTAINS.isthreatened" = NA, "ATTAINS.state" = NA, "ATTAINS.on303dlist" = NA,                                               
                    "ATTAINS.organizationname" = NA, "ATTAINS.region" = NA, "ATTAINS.Shape_Length" = NA,                                             
                    "ATTAINS.reportingcycle" = NA, "ATTAINS.assmnt_joinkey" = NA, "ATTAINS.hastmdl" = NA,                                                  
                    "ATTAINS.orgtype" = NA, "ATTAINS.permid_joinkey" = NA, "ATTAINS.catchmentistribal" = NA,                                        
                    "ATTAINS.ircategory" = NA, "ATTAINS.waterbodyreportlink" = NA, "ATTAINS.assessmentunitidentifier" = NA,                                 
                    "ATTAINS.overallstatus" = NA, "ATTAINS.isassessed" = NA, "ATTAINS.isimpaired" = NA,                                               
                    "ATTAINS.has4bplan" = NA, "ATTAINS.huc12" = NA, "ATTAINS.hasalternativeplan" = NA,                                      
                    "ATTAINS.visionpriority303d" = NA, "ATTAINS.areasqkm" = NA, "ATTAINS.catchmentareasqkm" = NA,                                       
                    "ATTAINS.catchmentstatecode" = NA, "ATTAINS.catchmentresolution" = NA, "ATTAINS.Shape_Area" = NA)
    
    return(no_WQP_data)
    
  }
  
  suppressMessages(suppressWarnings({
    
    sf::sf_use_s2(FALSE)
    
    # If data is already spatial, just make sure it is in the right CRS
    # and add an index as the WQP observations' unique IDs...
    if (!is.null(data) & inherits(data, "sf")) {
      TADA_DataRetrieval_data <- data %>%
        sf::st_transform(4326) %>%
        tibble::rowid_to_column(var = "index")
    } else {
      # ... Otherwise transform into a spatial object then do the same thing:
      TADA_DataRetrieval_data <- data %>%
        #convert dataframe to a spatial object
        TADA_MakeSpatial(data = ., crs = 4326) %>%
        # add unique WQP ID for identifying obs with more than one ATTAINS assessment unit
        tibble::rowid_to_column(var = "index")
    }
    
    # grab the ATTAINS catchments within our WQP bbox:
    nearby_catchments <- NULL
    # (Wrapped with "try" because it is possible that no ATTAINS data exists in the bbox.)
    try(nearby_catchments <- fetchATTAINS(type = "catchments", data = TADA_DataRetrieval_data) %>%
          # remove unnecessary columns:
          dplyr::select(-c(OBJECTID, GLOBALID)) %>%
          # select only catchments that have WQP observations in them:
          .[TADA_DataRetrieval_data, ] %>%
          # add prefix "ATTAINS" to ATTAINS data
          dplyr::rename_with(~ paste0("ATTAINS.", .), dplyr::everything()) %>%
          # get rid of dupes (as a precaution)
          dplyr::distinct(.keep_all = TRUE),
        silent = TRUE)
    
    # if no ATTAINS data, return original dataframe with empty ATTAINS columns:
    if(is.null(nearby_catchments)){
      
      print("There are no ATTAINS features associated with these WQP observations. Returning original dataframe with empty ATTAINS columns.")
      
      # return a modified `data` with empty ATTAINS-related columns:
      no_ATTAINS_data <- data %>%
        dplyr::mutate("ATTAINS.organizationid" = NA, "ATTAINS.submissionid" = NA, "ATTAINS.hasprotectionplan" = NA,
                      "ATTAINS.assessmentunitname" = NA, "ATTAINS.nhdplusid" = NA, "ATTAINS.tas303d" = NA,                                                  
                      "ATTAINS.isthreatened" = NA, "ATTAINS.state" = NA, "ATTAINS.on303dlist" = NA,                                               
                      "ATTAINS.organizationname" = NA, "ATTAINS.region" = NA, "ATTAINS.Shape_Length" = NA,                                             
                      "ATTAINS.reportingcycle" = NA, "ATTAINS.assmnt_joinkey" = NA, "ATTAINS.hastmdl" = NA,                                                  
                      "ATTAINS.orgtype" = NA, "ATTAINS.permid_joinkey" = NA, "ATTAINS.catchmentistribal" = NA,                                        
                      "ATTAINS.ircategory" = NA, "ATTAINS.waterbodyreportlink" = NA, "ATTAINS.assessmentunitidentifier" = NA,                                 
                      "ATTAINS.overallstatus" = NA, "ATTAINS.isassessed" = NA, "ATTAINS.isimpaired" = NA,                                               
                      "ATTAINS.has4bplan" = NA, "ATTAINS.huc12" = NA, "ATTAINS.hasalternativeplan" = NA,                                      
                      "ATTAINS.visionpriority303d" = NA, "ATTAINS.areasqkm" = NA, "ATTAINS.catchmentareasqkm" = NA,                                       
                      "ATTAINS.catchmentstatecode" = NA, "ATTAINS.catchmentresolution" = NA, "ATTAINS.Shape_Area" = NA)
      
      return(no_ATTAINS_data)
      
    } else {
      # ... otherwise link WQP features to the ATTAINS catchment feature(s) they land in:
      TADA_with_ATTAINS <- TADA_DataRetrieval_data %>%
        # left join = TRUE to preserve all observations (with or without ATTAINS features):
        sf::st_join(., nearby_catchments, left = TRUE)
      
      if(return == TRUE){
        
        # ... otherwise link WQP features to the ATTAINS catchment feature(s) they land in:
        TADA_data <- TADA_DataRetrieval_data %>%
          # left join = TRUE to preserve all observations (with or without ATTAINS features):
          sf::st_join(., nearby_catchments, left = TRUE)
        
        # CATCHMENT FEATURES 
        # use original catchment pull, but return column names to original
        ATTAINS_catchments <<- nearby_catchments
        colnames(ATTAINS_catchments) <- gsub("ATTAINS.", "", colnames(ATTAINS_catchments)) 
        # due to the rename, must re-set geometry column:
        sf::st_geometry(ATTAINS_catchments) <- "geometry"
        
        # POINT FEATURES - try to pull point AU data if it exists. Otherwise, move on...
        try(ATTAINS_points <<- fetchATTAINS(type = "points", data = TADA_DataRetrieval_data) %>%
              # subset to only ATTAINS point features in the same NHD HR catchments as WQP observations
              .[nearby_catchments,] %>%
              # make sure no duplicate features exist
              distinct(assessmentunitidentifier, .keep_all = TRUE),
            silent = TRUE)
        
        
        # LINE FEATURES - try to pull line AU data if it exists. Otherwise, move on...
        try(ATTAINS_lines <<- fetchATTAINS(type = "lines", data = TADA_DataRetrieval_data) %>%
              # subset to only ATTAINS line features in the same NHD HR catchments as WQP observations
              .[nearby_catchments,] %>%
              # make sure no duplicate line features exist
              distinct(assessmentunitidentifier, .keep_all = TRUE),
            silent = TRUE)
        
        # POLYGON FEATURES - try to pull polygon AU data if it exists. Otherwise, move on...
        try(ATTAINS_polygons <<- fetchATTAINS(type = "polygons", data = TADA_DataRetrieval_data) %>%
              # subset to only ATTAINS polygon features in the same NHD HR catchments as WQP observations
              .[nearby_catchments,] %>%
              # make sure no duplicate polygon features exist
              distinct(assessmentunitidentifier, .keep_all = TRUE),
            silent = TRUE)
      }
      
      return(TADA_with_ATTAINS)
      
    }
    
  }))
  
}


#' TADA_ViewATTAINS
#' 
#' Finds ATTAINS assessment unit data within the same catchment as water quality observations imported via `TADA_DataRetrieval()`.
#' 
#' @param data A dataframe, created by `TADA_DataRetrieval()`. 
#' 
#' @return A leaflet map visualizing the TADA water quality observations and the linked ATTAINS assessment units. All maps are in WGS84.
#' 
#' @seealso [TADA_DataRetrieval()]
#' @seealso [TADA_GetATTAINS()]
#' 
#' @export
#'
#' @examples
#' \dontrun{
#'tada_data <- TADA_DataRetrieval(startDate = "1990-01-01",
#'                                endDate = "1995-12-31",
#'                                characteristicName = "pH",
#'                                statecode = "NV",
#'                                applyautoclean = TRUE)
#'                                  
#'TADA_ViewATTAINS(data = tada_data)
#' }

TADA_ViewATTAINS <- function(data){
  
  if(nrow(data) == 0){stop("Your WQP dataframe has no observations.")}
  
  suppressMessages(suppressWarnings({
    
    sf::sf_use_s2(FALSE)
    
    # If data is already spatial, just make sure it is in the right CRS
    # and add an index...
    if (!is.null(data) & inherits(data, "sf")) {
      TADA_DataRetrieval_data <- data %>%
        sf::st_transform(4326) %>%
        tibble::rowid_to_column(var = "index")
    } else {
      #... otherwise, make it spatial then do the same thing.
      TADA_DataRetrieval_data <- data %>%
        TADA_MakeSpatial(data = ., crs = 4326) %>%
        # add index for identifying obs with more than one ATTAINS assessment unit
        tibble::rowid_to_column(var = "index")
    }
    
    nearby_catchments <- NULL
    # grab the ATTAINS catchment-level data:
    try(nearby_catchments <- fetchATTAINS(type = "catchments", data = TADA_DataRetrieval_data) %>%
          # remove unnecessary columns:
          dplyr::select(-c(OBJECTID, GLOBALID)) %>%
          # subset catchments to only those with user-supplied WQP observations in them:
          .[TADA_DataRetrieval_data,] %>%
          # tack on ATTAINS to the beginning of every column name:
          dplyr::rename_with(~ paste0("ATTAINS.", .), dplyr::everything()),
        silent = TRUE)
    
    if(is.null(nearby_catchments) == TRUE) {
      stop("There are no ATTAINS features associated with these WQP observations.")
    }
    
    
    # join TADA sf features to the ATTAINS catchment feature(s) they land on:
    TADA_with_ATTAINS <- TADA_DataRetrieval_data %>%
      sf::st_join(., nearby_catchments) %>%
      # drop spatial:
      sf::st_drop_geometry() 
    
    colors = data.frame(
      overallstatus = c("Not Supporting", "Fully Supporting", "Not Assessed"),
      col = c("#DC851E", "#059FA4", "#A1A522"),
      dark_col = c("#813B00", "#005258", "#4F5900"),
      priority = c(1, 2, 3))
    
    # POINT FEATURES - try to pull point AU data if it exists. Otherwise, move on...
    points <- NULL
    try(points <- fetchATTAINS(type = "points", data = TADA_DataRetrieval_data) %>%
          .[nearby_catchments,],
        silent = TRUE)
    try(points_mapper <- points %>%
          left_join(., colors, by = "overallstatus") %>%
          mutate(type = "Point Feature"),
        silent = TRUE)
    
    # LINE FEATURES - try to pull line AU data if it exists. Otherwise, move on...
    lines <- NULL
    try(lines <- fetchATTAINS(type = "lines", data = TADA_DataRetrieval_data) %>%
          .[nearby_catchments,],
        silent = TRUE)
    try(lines_mapper <- lines %>%
          left_join(., colors, by = "overallstatus") %>%
          mutate(type = "Line Feature"),
        silent = TRUE)
    
    # POLYGON FEATURES - try to pull polygon AU data if it exists. Otherwise, move on...
    polygons <- NULL
    try(polygons <- fetchATTAINS(type = "polygons", data = TADA_DataRetrieval_data) %>%
          .[nearby_catchments,],
        silent = TRUE)
    try(polygons_mapper <- polygons %>%
          left_join(., colors, by = "overallstatus") %>%
          mutate(type = "Polygon Feature"),
        silent = TRUE)
    
    # Rename WQP columns, depending on whether or not the user applied TADA's autoclean to the df:
    try(TADA_with_ATTAINS <- TADA_with_ATTAINS %>%
          rename(TADA.LatitudeMeasure = LatitudeMeasure,
                 TADA.LongitudeMeasure = LongitudeMeasure,
                 TADA.CharacteristicName = CharacteristicName), 
        silent = TRUE)
    # Develop WQP site stats (e.g. count of observations, parameters, per site)
    sumdat <- TADA_with_ATTAINS %>%
      dplyr::group_by(MonitoringLocationIdentifier, MonitoringLocationName, TADA.LatitudeMeasure, TADA.LongitudeMeasure) %>% 
      dplyr::summarize(Sample_Count = length(unique(ResultIdentifier)), 
                       Visit_Count = length(unique(ActivityStartDate)), 
                       Parameter_Count = length(unique(TADA.CharacteristicName)), 
                       Organization_Count = length(unique(OrganizationIdentifier)),
                       ATTAINS_AUs = as.character(list(unique(ATTAINS.assessmentunitidentifier)))) %>%
      mutate(ATTAINS_AUs = ifelse(is.na(ATTAINS_AUs), "None", ATTAINS_AUs),
             TADA.LatitudeMeasure = as.numeric(TADA.LatitudeMeasure),
             TADA.LongitudeMeasure = as.numeric(TADA.LongitudeMeasure))
    
    # Basemap for AOI:
    map <- leaflet::leaflet() %>% 
      leaflet::addProviderTiles("Esri.WorldTopoMap", 
                                group = "World topo",
                                options = leaflet::providerTileOptions(updateWhenZooming = FALSE, 
                                                                       updateWhenIdle = TRUE)) %>% 
      leaflet::clearShapes() %>% 
      leaflet::fitBounds(lng1 = min(sumdat$TADA.LongitudeMeasure), 
                         lat1 = min(sumdat$TADA.LatitudeMeasure), 
                         lng2 = max(sumdat$TADA.LongitudeMeasure), 
                         lat2 = max(sumdat$TADA.LatitudeMeasure)) %>% 
      leaflet.extras::addResetMapButton()  %>%
      leaflet::addLegend(position = "bottomright",
                         colors = c("#DC851E", "#059FA4", "#A1A522", "black", NA),
                         labels = c("ATTAINS: Not Supporting", "ATTAINS: Supporting", "ATTAINS: Not Assessed", "Water Quality Observation(s)",
                                    "NHD HR catchments containing water quality observations are represented as clear polygons with black outlines."),
                         opacity = 1,
                         title = "Legend")
    
    # Add ATTAINS catchment outlines (if they exist):
    try(map <- map %>%
          leaflet::addPolygons(data = nearby_catchments,
                               color = "black",
                               weight = 1, fillOpacity = 0,
                               popup = paste0("NHDPlus HR Catchment ID: ", nearby_catchments$ATTAINS.nhdplusid)),
        silent = TRUE)
    
    # Add ATTAINS polygon features (if they exist):
    try(map <- map %>%
          leaflet::addPolygons(data = polygons_mapper,
                               color = ~polygons_mapper$col,
                               fill = ~polygons_mapper$col,
                               weight = 3, fillOpacity = 1,
                               popup = paste0("Assessment Unit Name: ", polygons_mapper$assessmentunitname, 
                                              "<br> Assessment Unit ID: ", polygons_mapper$assessmentunitidentifier,
                                              "<br> Status: ", polygons_mapper$overallstatus,
                                              "<br> Assessment Unit Type: ", polygons_mapper$type,
                                              "<br> <a href=", polygons_mapper$waterbodyreportlink, " target='_blank'>ATTAINS Link</a>")),
        silent = TRUE)
    
    # Add ATTAINS lines features (if they exist):
    try(map <- map %>%
          leaflet::addPolylines(data = lines_mapper,
                                color = ~lines_mapper$col,
                                weight = 4, fillOpacity = 1,
                                popup = paste0("Assessment Unit Name: ", lines_mapper$assessmentunitname, 
                                               "<br> Assessment Unit ID: ", lines_mapper$assessmentunitidentifier,
                                               "<br> Status: ", lines_mapper$overallstatus,
                                               "<br> Assessment Unit Type: ", lines_mapper$type,
                                               "<br> <a href=", lines_mapper$waterbodyreportlink, " target='_blank'>ATTAINS Link</a>")),
        silent = TRUE)
    
    # Add ATTAINS point features (if they exist):
    try(map <- map %>%
          leaflet::addCircleMarkers(data = points_mapper, 
                                    lng = ~LongitudeMeasure, lat = ~LatitudeMeasure, 
                                    color = ~points_mapper$col, fillColor = ~points_mapper$col, 
                                    fillOpacity = 1, stroke = TRUE, weight = 1.5, radius = 5, 
                                    popup = paste0("Assessment Unit Name: ", points_mapper$assessmentunitname, 
                                                   "<br> Assessment Unit ID: ", points_mapper$assessmentunitidentifier,
                                                   "<br> Status: ", points_mapper$overallstatus,
                                                   "<br> Assessment Unit Type: ", points_mapper$type,
                                                   "<br> <a href=", points_mapper$waterbodyreportlink, " target='_blank'>ATTAINS Link</a>")),
        silent = TRUE)
    
    # Add WQP observation features (should always exist):
    try(map <- map %>%
          leaflet::addCircleMarkers(data = sumdat, 
                                    lng = ~sumdat$TADA.LongitudeMeasure, lat = ~sumdat$TADA.LatitudeMeasure, 
                                    color = "grey", fillColor = "black", 
                                    fillOpacity = 0.8, stroke = TRUE, weight = 1.5, radius = 6, 
                                    popup = paste0("Site ID: ", sumdat$MonitoringLocationIdentifier, 
                                                   "<br> Site Name: ", sumdat$MonitoringLocationName, 
                                                   "<br> Measurement Count: ", sumdat$Sample_Count, 
                                                   "<br> Visit Count: ", sumdat$Visit_Count, 
                                                   "<br> Characteristic Count: ", sumdat$Parameter_Count,
                                                   "<br> ATTAINS Assessment Unit(s): ", sumdat$ATTAINS_AUs)),
        silent = TRUE)
    
    if(is.null(lines) == TRUE & is.null(points) == TRUE & is.null(polygons) == TRUE) {
      print("No ATTAINS data associated with this Water Quality Portal data.")
    }
    
    # Return leaflet map of TADA WQ and its associated ATTAINS data
    return(map)
    
  }))
  
}

