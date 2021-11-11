#' Returns details about the center of a regular grid provided the grid code.
#' Details include:
#'     the center latitude / longitude (regardless of the fraction of ocean area in the grid)
#'     the grid width and height (in degrees)
#'
#' @param grid_code A grid code
#' @return A named vector containing the center latitude (\code{y}) and longitude (\code{x}) plus the grid
#' width (\code{size_lat}) and height (\code{size_lon}) in degrees
#' @examples
#' center_from_grid_code("5206066")
#' center_from_grid_code("6205065")
#' @export
center_from_grid_code = function(grid_code) {
  #Grid sizes by gridCode[1], i.e.:
  #1 -> 30x30
  #2 -> 10x20
  #3 -> 10x10
  #4 -> 20x20
  #5 ->  1x1
  #6 ->  5x5

  #Height and width (in degrees) by type of grid (i.e. gridCode[0]) sorted by lexicographical first char code
  #(1 = 30x30, 2 = 10x20, 3 = 10x10, 4 = 20x20, 5 = 1x1, 6 = 5x5)
  sizes_lat = c(30, 10, 10, 20, 1, 5)
  sizes_lon = c(30, 20, 10, 20, 1, 5)

  grid_code = toString(grid_code)

  size_code = as.numeric(substring(grid_code, 1, 1))

  quadrant = as.numeric(substring(grid_code, 2, 2))

  lat = as.numeric(substring(grid_code, 3, 4))
  lon = as.numeric(substring(grid_code, 5, 7))

  latitudes.quadrant  = c(  1, -1, -1,  1)
  longitudes.quadrant = c(  1,  1, -1,  1)

  lat = latitudes.quadrant[quadrant]  * ( lat + sizes_lat[size_code] / 2 );
  lon = longitudes.quadrant[quadrant] * ( lon + sizes_lon[size_code] / 2 );

  return (c(x = lon, y = lat, size_lat = sizes_lat[size_code], size_lon = sizes_lon[size_code]))
}

#'Converts a pair of decimal coordinate into a CWP grid code for a specific grid type
#'@param lon Longitude (decimal coordinates)
#'@param lat Latitude (decimal coordinates)
#'@param grid_type_code The type of CWP grid (one among \code{\link{grid_1x1}}, \code{\link{grid_5x5}}, \code{\link{grid_10x10}}, \code{\link{grid_10x20}}, \code{\link{grid_20x20}} and \code{\link{grid_30x30}})
#'@return The CWP grid code for the provided coordinates and grid type
#'@export
#'@examples convert_to_CWP_grid(20, -10, grid_1x1)
convert_to_CWP_grid = function(lon, lat, grid_type_code = grid_5x5) {
  q = NA

  if     (lon >=0 && lat >=0) q = 1
  else if(lon >=0 && lat < 0) q = 2
  else if(lon < 0 && lat < 0) q = 3
  else if(lon < 0 && lat >=0) q = 4

  lat = floor(abs(lat))
  lon = floor(abs(lon))

  latS = 1
  lonS = 1

  grid = grid_char_1x1

  if     (grid_type_code == grid_5x5)   { lonS = latS =  5;     grid = grid_char_5x5 }
  else if(grid_type_code == grid_10x10) { lonS = latS = 10;     grid = grid_char_10x10 }
  else if(grid_type_code == grid_20x20) { lonS = latS = 20;     grid = grid_char_20x20 }
  else if(grid_type_code == grid_30x30) { lonS = latS = 30;     grid = grid_char_30x30 }
  else if(grid_type_code == grid_10x20) { lonS = 20; latS = 10; grid = grid_char_10x20 }

  lat = floor(lat / latS) * latS
  lon = floor(lon / lonS) * lonS

  return(paste0(grid, q, stri_pad(lat, 2, pad = "0"), stri_pad(lon, 3, pad = "0")))
}

#'Converts a CWP grid code into another CWP grid code of a given type
#'@param grid_code A CWP grid code
#'@param grid_type_code The type of CWP grid (one among \code{\link{grid_1x1}}, \code{\link{grid_5x5}}, \code{\link{grid_10x10}}, \code{\link{grid_10x20}}, \code{\link{grid_20x20}} and \code{\link{grid_30x30}})
#'@return The CWP grid code for the grid of type \code{grid_type_code} that contains the main corner of the original grid
#'@export
#'@examples convert_CWP_grid("5201123", grid_5x5)
#'@examples convert_CWP_grid("6205125", grid_1x1)
convert_CWP_grid = function(grid_code, target_grid_type_code = grid_1x1) {
  q = as.integer(substr(grid_code, 2, 2))

  qLon = qLat = 1

  if     (q == 2) { qLon =  1; qLat = -1 }
  else if(q == 3) { qLon = -1; qLat = -1 }
  else if(q == 4) { qLon = -1; qLat =  1 }

  lat = qLat * as.integer(substr(grid_code, 3, 4))
  lon = qLon * as.integer(substr(grid_code, 5, 7))

  return (convert_to_CWP_grid(lon, lat, target_grid_type_code))
}

#'Converts a CWP grid code into its four boundary points (NW, NE, SW and SE)
#'@param grid_code A CWP grid code
#'@return a data table containing the coordinates (LAT, LON) of each of the four boundary points for the grid
#'@export
#'@examples CWP_to_grid_coordinates("5201123")
#'@examples CWP_to_grid_coordinates("6205125")
CWP_to_grid_coordinates = function(grid_code) {
  s = as.integer(substr(grid_code, 1, 1))

  dx = dy = 1

  if     (s == 6) { dx =     dy =  5 }
  else if(s == 3) { dx =     dy = 10 }
  else if(s == 2) { dx = 20; dy = 10 }
  else if(s == 4) { dx =     dy = 20 }
  else if(s == 1) { dx =     dy = 30 }

  q =   as.integer(substr(grid_code, 2, 2))

  lat = as.integer(substr(grid_code, 3, 4))
  lon = as.integer(substr(grid_code, 5, 7))

  points = data.table(POS = character(), LON = numeric(), LAT = numeric())

  if (q == 1) {       #NE quadrant
    points = rbind(points, list("NW",  lon     ,  lat + dy))
    points = rbind(points, list("NE",  lon + dx,  lat + dy))
    points = rbind(points, list("SE",  lon + dx,  lat))
    points = rbind(points, list("SW",  lon     ,  lat))
  } else if(q == 2) { #SE quadrant
    points = rbind(points, list("NW",  lon     , -lat))
    points = rbind(points, list("NE",  lon + dx, -lat))
    points = rbind(points, list("SE",  lon + dx, -lat - dy))
    points = rbind(points, list("SW",  lon     , -lat - dy))
  } else if(q == 3) { #SW quadrant - doesn't really apply to the IO
    points = rbind(points, list("NW", -lon - dx, -lat))
    points = rbind(points, list("NE", -lon     , -lat))
    points = rbind(points, list("SE", -lon     , -lat - dy))
    points = rbind(points, list("SW", -lon - dx, -lat - dy))
  } else if(q == 4) { #NW quadrant - doesn't really apply to the IO
    points = rbind(points, list("NW", -lon - dx,  lat + dy))
    points = rbind(points, list("NE", -lon     ,  lat + dy))
    points = rbind(points, list("SE", -lon     ,  lat))
    points = rbind(points, list("SW", -lon - dx,  lat))
  }

  return(points)
}
