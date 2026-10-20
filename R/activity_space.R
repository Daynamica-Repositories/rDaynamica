#' Convert Polyline Coordinates and Project CRS for Activity Locations
#'
#' @param ucalitems_activity Data table of activities.
#' @param origin_crs Origin CRS integer or string (e.g. 4326).
#' @param projected_crs Projected CRS integer or string.
#' @param original_col Name of polyline column (default 'centroid').
#' @param original_lon Name for original longitude column.
#' @param original_lat Name for original latitude column.
#' @param projected_lon Name for projected X column.
#' @param projected_lat Name for projected Y column.
#' @return Data table with projected coordinates and sf geometry.
#' @export
str2cor_tb <- function(ucalitems_activity,
                       origin_crs,
                       projected_crs,
                       original_col = 'centroid',
                       original_lon = 'lon',
                       original_lat = 'lat',
                       projected_lon = 'x',
                       projected_lat = 'y') {

  unit_convert <- 1609.34
  ucalitems_activity <- data.table::copy(ucalitems_activity)

  ucalitems_activity[, (original_lon) := mapply(function(x) {
    if (is.na(x) || x == "None") return(0)
    coords <- gepaf::decodePolyline(x)
    return(as.numeric(coords[2]))
  }, ucalitems_activity[[original_col]])]

  ucalitems_activity[, (original_lat) := mapply(function(x) {
    if (is.na(x) || x == "None") return(0)
    coords <- gepaf::decodePolyline(x)
    return(as.numeric(coords[1]))
  }, ucalitems_activity[[original_col]])]

  temp_sf <- sf::st_as_sf(ucalitems_activity, coords = c(original_lon, original_lat), crs = origin_crs)
  temp_sf <- sf::st_transform(temp_sf, crs = projected_crs)

  coords <- sf::st_coordinates(temp_sf)
  ucalitems_activity[, c(projected_lon, projected_lat, 'geometry') :=
                       list(coords[, 1], coords[, 2], temp_sf$geometry)]

  ucalitems_activity[, c(original_lon, original_lat, projected_lon, projected_lat) :=
                       lapply(.SD, function(x) ifelse(x == 0, NA_real_, x)),
                     .SDcols = c(original_lon, original_lat, projected_lon, projected_lat)]

  return(ucalitems_activity)
}

#' Calculate Activity Space Convex Hull
#'
#' @param ucalitems_activity Data table with sf geometry.
#' @param buffer_dis_meter Buffer distance in meters.
#' @param dissolve_col Vector of grouping columns.
#' @return Data table with convex hull polygon geometry and areas.
#' @export
cal_convex_hull <- function(ucalitems_activity, buffer_dis_meter,
                            dissolve_col = c("user_id", "start_date")) {
  unit_convert <- 1609.34
  result <- ucalitems_activity[, .(geometry = sf::st_convex_hull(sf::st_union(geometry))), by = dissolve_col]
  result[, geometry_type := as.character(sf::st_geometry_type(geometry)), by = dissolve_col]

  result[, area_meter := mapply(function(x, y) {
    if (x == 'POINT') {
      return(0)
    } else if (x == 'LINESTRING') {
      return(as.numeric(sf::st_area(sf::st_buffer(y, dist = buffer_dis_meter))))
    } else if (x == 'POLYGON') {
      return(as.numeric(sf::st_area(y)))
    }
  }, result$geometry_type, result$geometry)]

  result[, area_mile := area_meter / (unit_convert^2), by = dissolve_col]
  return(result)
}

#' Calculate Activity Space Standard Deviation Ellipse (SDE)
#'
#' @param ucalitems_activity Data table with sf geometry.
#' @param buffer_dis_meter Buffer distance in meters.
#' @param dissolve_col Vector of grouping columns.
#' @return Data table with SDE metrics and geometry.
#' @export
cal_sde <- function(ucalitems_activity, buffer_dis_meter,
                    dissolve_col = c("user_id", "start_date")) {
  unit_convert <- 1609.34
  result <- ucalitems_activity[, .(
    geometry = sf::st_union(geometry),
    geometry_type = as.character(sf::st_geometry_type(sf::st_convex_hull(sf::st_union(geometry))))
  ), by = dissolve_col]

  for (i in seq_len(nrow(result))) {
    if (result$geometry_type[i] == 'POLYGON') {
      sde <- sfdep::std_dev_ellipse(sf::st_cast(result$geometry[i], "POINT"))
      result$sx[i] <- sde$sx
      result$sy[i] <- sde$sy
      result$theta[i] <- sde$theta
      result$pnt[i] <- sde$geometry
      result$sde_geometry[i] <- sfdep::st_ellipse(sde$geometry, sde$sx, sde$sy, sde$theta)
    } else {
      result$sx[i] <- NA
      result$sy[i] <- NA
      result$theta[i] <- NA
      result$pnt[i] <- NA
      result$sde_geometry[i] <- NA
    }
  }

  result[geometry_type == "POINT", c("sx", "sy") := 0]
  result[geometry_type == "LINESTRING", c("sx", "sy") := list(as.numeric(sf::st_length(sf::st_convex_hull(geometry))), buffer_dis_meter)]

  result[, area_meter := pi * sx * sy]
  result[, c("sx_mile", "sy_mile") := list(sx / unit_convert, sy / unit_convert)]
  result[, area_mile := pi * sx_mile * sy_mile]

  return(result)
}
