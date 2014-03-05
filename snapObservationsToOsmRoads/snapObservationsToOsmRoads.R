# difference to function from package: includes observationsIDs
# needed to establish relation to OSM-Street-IDs afterwards
importEnviroCar = function(file) {
  require(rjson) # fromJSON
  require(maptools) # spCbind
  require(rgdal) #readOGR
  require(RCurl) #getURL
  require(stringr) #str_replace_all
  
  # get ids of observations
  rawData = fromJSON(getURL(file, ssl.verifypeer = FALSE))
  observationIDs = lapply(rawData$features, function(X) X$properties$id)
  
  # read data as spatial object:
  layer = readOGR(getURL(file, ssl.verifypeer = FALSE), layer = "OGRGeoJSON")
  
  # convert time from text to POSIXct:
  layer$time = as.POSIXct(layer$time, format="%Y-%m-%dT%H:%M:%SZ")
  # the third column is JSON, we want it in a table (data.frame) form:
  # 1. form a list of lists
  l1 = lapply(as.character(layer[[3]]), fromJSON)
  # 2. parse the $value elements in the sublist:
  l2 = lapply(l1, function(x) as.data.frame(lapply(x, function(X) X$value)))
  # create a matrix with all columns and then convert it to a data frame
  # thanks to Kristina Helle!
  # dynamic parsing of phenomenon names and units
  phenomenonsUrl = "https://www.envirocar.org/api/stable/phenomenons"
  phenomenons = fromJSON(getURL(phenomenonsUrl, ssl.verifypeer = FALSE))
  colNames <- str_replace_all(sapply(phenomenons[[1]], "[[", "name"), pattern=" ", repl=".")
  
  resultMatrix = matrix(nrow=length(l2),ncol=length(colNames))
  dimnames(resultMatrix)[[2]]=colNames
  for (i in seq(along = l2))
    resultMatrix[i,names(l2[[i]])]=as.numeric(l2[[i]])
  result = as.data.frame(resultMatrix)
  
  # append observation id to data frame
  # unlist returns a vector of the list including observations
  result["observation_id"] = unlist(observationIDs)
  
  # set the units:
  units <- sapply(phenomenons[[1]], "[[", "unit")
  names(units)=colNames
  
  # add a units attribute to layer
  layer[[3]] = NULL
  # add the table as attributes to the spatial object 
  if (length(layer) == nrow(result)) {
    layer = spCbind(layer, result)
    # cbind (layer, id)
    attr(layer, "units") = units
    layer
  } else
    NULL
}

nearestPointOnSegment = function(s, p){
  # see http://pastebin.com/n9rUuGRh
  ap = c(p[1] - s[1,1], p[2] - s[1,2])
  ab = c(s[2,1] - s[1,1], s[2,2] - s[1,2])
  t = sum(ap*ab) / sum(ab*ab)
  t = ifelse(t<0,0,ifelse(t>1,1,t))
  x = s[1,1] + ab[1] * t 
  y = s[1,2] + ab[2] * t
  c(x, y, (x-p[1])^2 + (y-p[2])^2)  # Return nearest point and distance
}

nearestPointOnLine = function(coordsLine, coordsPoints){
  nearest_points = vapply(2:nrow(coordsLine), 
                          function(x) 
                            nearestPointOnSegment(coordsLine[(x-1):x,], coordsPoints),
                          FUN.VALUE=c(0,0,0))
  
  # Return coordinates of the nearest point in this line  
  nearest_points[1:2, which.min(nearest_points[3,])]  
}

# adaption of snapPointsToLines function from maptools
mySnapPointsToLines <- function( points, lines, maxDist=NA, withAttrs=TRUE) {
  
  require("rgeos")
  if (is(points, "SpatialPoints") && missing(withAttrs)) 
    withAttrs = FALSE
  if (is(points, "SpatialPoints") && withAttrs == TRUE) 
    stop("A SpatialPoints object has no attributes! Set withAttrs as FALSE.")
  if (!is.na(maxDist)) {
    w = gWithinDistance(points, lines, dist = maxDist, byid = TRUE)
    validPoints = apply(w, 2, any)
    validLines = apply(w, 1, any)
    points = points[validPoints, ]
    lines = lines[validLines, ]
  }
  
  # calculates the distance between the given point and lines geometries
  # Logical vector determining if the function should be applied across ids (TRUE)
  # or the entire object (FALSE) for points and lines
  d = gDistance(points, lines, byid = TRUE)
  # 2 indicates the column to apply the min function
  nearest_line_index = apply(d, 2, which.min)
  coordsLines = coordinates(lines)
  coordsPoints = coordinates(points)
  #This function calculates the coordinates of the nearest point on a line to a given point.
  # This function does not work with geographic coordinates.
  mNewCoords = vapply(1:length(points), function(x) nearestPointOnLine(coordsLines[[nearest_line_index[x]]][[1]], 
                                                                       coordsPoints[x, ]), FUN.VALUE = c(0, 0))
  
  if (!is.na(maxDist)) 
    nearest_line_id = as.numeric(rownames(d)[nearest_line_index]) + 1
  else
    nearest_line_id = nearest_line_index
  
  if (withAttrs) 
    df = cbind(points@data, nearest_line_id)
  else
    
    print(coordsPoints)
  df = data.frame(nearest_line_id, osm_id = lines$id[nearest_line_id], matched = t(mNewCoords), unmatched.X = coordsPoints[,1], unmatched.Y = coordsPoints[,2], observation.id = points$observation_id, row.names = names(nearest_line_index))
  print(df)
  SpatialPointsDataFrame(coords = t(mNewCoords), data = df, proj4string = CRS(proj4string(points)))
  
}

require ("osmar")
require ("data.table")

mapquestApiUrl <- "http://open.mapquestapi.com/xapi/api/0.6/"

# include only car relevant line features to avoid snapping to wrong lines features
# refer to http://wiki.openstreetmap.org/wiki/DE:Key:highway for filtering osm features
filter <- "way[highway=motorway|motorway_link|trunk|trunk_link|primary|primary_link|secondary|secondary_link|tertiary|tertiary_link|living_street|residential|unclassified|service|road]"

# boundingbox represents Muenster (ms)
bbox <- "[bbox=7.576829,51.923651,7.687158,51.988192]"
ms <- getURL(paste(mapquestApiUrl, filter, bbox, sep=""))
ms_as_osmar <- as_osmar(xmlParse(ms))
ms_as_sp <- as_sp(ms_as_osmar, what = c("points", "lines", "polygons"), crs = osm_crs(), simplify = TRUE)



require(rgdal)
crs = CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +ellps=bessel +datum=potsdam +units=m +no_defs")
lines = spTransform(ms_as_sp$lines, crs)

#url of example track
url <- "https://envirocar.org/api/stable/tracks/52529d64e4b04f4d08f10077"

# points 
points <- importEnviroCar(url)
points = spTransform(points, crs)
# SpatialPointsDataFrame
res <- mySnapPointsToLines(points, lines)

# plotting raw points 
plot(points, col="blue")
# adding OSM roads
plot(lines, add=TRUE)
# adding map matched points
plot(res, add=TRUE, col="red")

# improve matching results from mySnapToPoints function
# using "looking forward"-approach
improveMatching = function (res, lineset, points){
  linesToBeRemoved <- numeric()
  for (i in 3:nrow(res)-1){
    osmid = res[i,][2]$osm_id
    pre_osmid = res[i-1,][2]$osm_id
    post_osmid = res[i+1,][2]$osm_id
    
    if (identical(pre_osmid, post_osmid) 
        && !identical(osmid, post_osmid)
        && !identical(osmid, pre_osmid)) {
      linesToBeRemoved <- append(linesToBeRemoved, c(osmid), after = length(linesToBeRemoved))
    }      
  }
  
  for (i in 6:nrow(res)-3){
    osmid = res[i,][2]$osm_id
    pre_pre_osmid = res[i-2,][2]$osm_id
    pre_osmid = res[i-1,][2]$osm_id
    post_osmid = res[i+1,][2]$osm_id
    post_post_osmid = res[i+2,][2]$osm_id
    
    if (identical(pre_osmid, pre_pre_osmid) 
        && !identical(osmid, post_osmid)
        && !identical(osmid, pre_osmid)
        && identical(post_osmid, post_post_osmid)) {
      linesToBeRemoved <- append(linesToBeRemoved, c(osmid), after = length(linesToBeRemoved))
    }      
  }
  
  #   for (i in 9:nrow(res)-5){
  #     osmid_mid1 = res[i,][2]$osm_id
  #     osmid_mid2 = res[i-1,][2]$osm_id
  #     pre_pre_osmid = res[i-3,][2]$osm_id
  #     pre_osmid = res[i-2,][2]$osm_id
  #     post_osmid = res[i+1,][2]$osm_id
  #     post_post_osmid = res[i+2,][2]$osm_id
  #     
  #     if (identical(pre_osmid, pre_pre_osmid) 
  #         && !identical(osmid_mid1, pre_osmid)
  #         && !identical(osmid_mid2, post_osmid)
  #         && identical(post_osmid, post_post_osmid)) {
  #       linesToBeRemoved <- append(linesToBeRemoved, c(osmid_mid1, osmid_mid2), after = length(linesToBeRemoved))
  #     }      
  #   }
  
  #print(linesToBeRemoved)
  newLineSet <- lineset[ ! lineset$id %in% linesToBeRemoved, ]
  newRes <- mySnapPointsToLines(points, newLineSet)
  #newRes
  plot(points, col="blue")
  plot(lineset, add=TRUE)
  plot(res, add=TRUE, col="red")
  plot(newRes, add=TRUE, col="green")
}

improveMatching(res, lines, points)

