unit_convert <- 1609.34

# Function to convert coordinates for all rows in a data.table and project crs
str2cor_tb <- function(ucalitems_activity, 
                       origin_crs,
                       projected_crs,
                       original_col = 'centroid', 
                       original_lon = 'lon',
                       original_lat = 'lat',
                       projected_lon = 'x',
                       projected_lat = 'y') {
  
  ucalitems_activity = copy(ucalitems_activity)
  #decode polyline
  ucalitems_activity[, eval(original_lon) := mapply(function(x){
    if (is.na(x) | x == "None") {
      return(0)
    } else { 
      coords <- decodePolyline(x)
      return(coords[2] %>% as.numeric())}}, 
    ucalitems_activity[[original_col]])]
  
  ucalitems_activity[, eval(original_lat) := mapply(function(x){
    if (is.na(x) | x == "None") {
      return(0)
    } else { 
      coords <- decodePolyline(x)
      return(coords[1] %>% as.numeric())}}, 
    ucalitems_activity[[original_col]])]
  
  #projection
  ##Convert to sf object
  temp_sf <- st_as_sf(ucalitems_activity,
                      coords = c(original_lon, original_lat),
                      crs = origin_crs)

  ##Project to the new CRS (unit=meter)
  temp_sf <- st_transform(temp_sf, crs = projected_crs)

  ##Extract x and y in meters
  ucalitems_activity[, c(projected_lon, projected_lat, 'geometry') :=
                       list(st_coordinates(temp_sf)[, 1],
                            st_coordinates(temp_sf)[, 2],
                            temp_sf$geometry)]

  ##convert 0 to NA
  ucalitems_activity[, c(original_lon, original_lat,
                         projected_lon, projected_lat) :=
                       lapply(.SD, function(x) ifelse(x == 0, NA_real_, x)),
                     .SDcols = c(original_lon, original_lat,
                                 projected_lon, projected_lat)]
  
  return(ucalitems_activity)
}

# Function to calculate convex hull
cal_convex_hull <- function(ucalitems_activity, buffer_dis_meter, 
                            dissolve_col = c("user_id", "start_date")) {
  result <- ucalitems_activity[,.(geometry = geometry %>% st_union() %>% st_convex_hull()),
                               by = dissolve_col]
  
  result[,geometry_type := st_geometry_type(geometry) %>% as.character(),
         by = dissolve_col]
  
  result[,area_meter := mapply(function(x,y){
    if(x == 'POINT'){
      return(0)
    } else if(x == 'LINESTRING'){
      return(st_area(st_buffer(y,dist = buffer_dis_meter)) %>% as.numeric())
    } else if(x == 'POLYGON'){
      return(st_area(y) %>% as.numeric())
    }}, result$geometry_type, result$geometry)]
  
  result[, area_mile := area_meter / (unit_convert**2), by = dissolve_col]
  
  return(result)
}

# Function to calculate standard deviation ellipse (SDE)
cal_sde <- function(ucalitems_activity, buffer_dis_meter, 
                    dissolve_col = c("user_id", "start_date")) {
  
  result <- ucalitems_activity[,.(geometry = geometry %>% st_union(),
                                  geometry_type = geometry %>% st_union() %>% 
                                    st_convex_hull() %>% st_geometry_type() %>%
                                    as.character()),
                               by = dissolve_col]
  
  for (i in 1:nrow(result)) {
    if(result$geometry_type[i]=='POLYGON'){
      sde = st_cast(result$geometry[i],"POINT") %>% std_dev_ellipse()
      result$sx[i] = sde$sx
      result$sy[i] = sde$sy
      result$theta[i] = sde$theta
      result$pnt[i] = sde$geometry
      result$sde_geometry[i] = st_ellipse(sde$geometry, sde$sx, sde$sy, sde$theta)
    } else{
      result$sx[i] = NA
      result$sy[i] = NA
      result$theta[i] = NA
      result$pnt[i] = NA
      result$sde_geometry[i] = NA

    }
  }
  
  result[geometry_type == "POINT", c("sx", "sy") := 0]
  result[geometry_type == "LINESTRING", 
         c("sx", "sy") := list(geometry %>% st_convex_hull() %>% st_length(), 
                               buffer_dis_meter)]
  result[, area_meter := pi * sx * sy]
  
  result[, c("sx_mile", "sy_mile") := list(sx / unit_convert, 
                                           sy / unit_convert)]
  result[, area_mile := pi * sx_mile * sy_mile]
  
  return(result)
}

