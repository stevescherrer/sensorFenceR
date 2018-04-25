#' runSensorFenceR
#'
#' This Function Places Static Sensors Along a Linear Transect

#' @param upperDepthLimit The shallowest depth at which to place a sensor. Note, depth is a negative number.
#' @param lowerDepthLimit The deepest depth at which to place a sensor. Note, depth is a negative number.
#' @param heightOfSensor The distance off the bottom (in meters) that the sensor is positioned
#' @param minimumDetectionThreshold  The radius (in meters) from the sensor where 1/2 of the minimum acceptable detection range is achieved. This is generally determined through range testing.
#' @param halfOfminimumDetectionThreshold The radius (in meters) from the sensor where 1/2 of the minimum acceptable detection range is achieved. For example, if the minimum allowable detection percentage is 50\%, this would be the distance from the sensor at which the detection threshold is 25\%. This is typically determined from range testing.
#' @param numberOfSensorsToPlace The number of sensors available from which to construct the fence line (integer)
#' @param minimumFenceHeight The height (m) off the seafloor that a transmisson must be detectable. Leave NULL if not specified.
#' @param swimSpeedOfStudySpecies  # The speed at which the transmitter may cross the sensor fence (m/s)
#' @param bathymetry Inherited from the parent function
#' @param fenceStart Inherited from the parent function
#' @param fenceEnd Inherited from the parent function
#' @keywords Place static sensors along a linear transect
#' @export
#' @examples
#' runSensorFenceR(numberOfSensorsToPlace = 10, fenceStart = c(-157.68330, 21.28333), fenceEnd = c(-157.53330, 21.41667), upperDepthLimit = -100, lowerDepthLimit = -500, minimumDetectionThreshold = 500, halfOfminimumDetectionThreshold = 650, minimumFenceHeight = 100, swimSpeedOfStudySpecies = .5)


runSensorFenceR = function(
  bathymetry = NULL,
  fenceStart = NULL, # c(lon, lat)
  fenceEnd = NULL, # c(lon, lat)
  upperDepthLimit = NULL,  # in meters
  lowerDepthLimit = NULL,  # in meters
  heightOfSensor = NULL,  # in meters
  minimumDetectionThreshold = NULL,  # Radius of receiver detection sphere where 25 percent of transmissions are picked up from range test data
  halfOfminimumDetectionThreshold = NULL,  # Radius of receiver detection sphere where 12.5 percent of transmissions are picked up from range testing data
  numberOfSensorsToPlace = NULL,
  minimumFenceHeight = 150,  # Distance (m) from the benthos that a detection sphere must encompass. Leave NULL if not specified.
  swimSpeedOfStudySpecies = .5,  # In meters / second
  plot_results = TRUE
){

  ####

  #### Specifying NULL parameters

  ### If a user has not specified the bathymetry for the region they want to place their fence, or end point coordiantes, we bother them about it.
  if(any(is.null(fenceStart) | is.null(fenceEnd)) & is.null(bathymetry)){
    print("You must specify the general area that encompasses where you would like to place your sensor fence.")
    lon1 = as.numeric(readline(prompt = "Enter the first longitude coordinate (decimal degrees): "))
    lon2 = as.numeric(readline(prompt = "Enter a second longitude coordinate (decimal degrees): "))
    lat1 = as.numeric(readline(prompt = "Enter the first latitude coordinate (decimal degrees): "))
    lat2 = as.numeric(readline(prompt = "Enter a second latitude coordinate (decimal degrees): "))
  }

  if(is.null(upperDepthLimit)){
    upperDepthLimit = as.numeric(readline(prompt = "Enter the shallowest depth that a sensor should be placed (m): "))
    if(upperDepthLimit > 0){
      upperDepthLimit = upperDepthLimit * -1
    }
  }

  if(is.null(lowerDepthLimit)){
    lowerDepthLimit = as.numeric(readline(prompt = "Enter the deepest depth that a sensor should be placed (m): "))
    if(lowerDepthLimit > 0){
      lowerDepthLimit = lowerDepthLimit * -1
    }
  }

  if(is.null(heightOfSensor)){
    heightOfSensor = as.numeric(readline(prompt = "How high off the seafloor should a sensor be placed (m): "))
    if(heightOfSensor < 0){
      heightOfSensor = abs(heightOfSensor)
    }
  }

  if(is.null(minimumDetectionThreshold)){
    min_detection_percent = as.numeric(readline(prompt = 'Enter the minimum % of transmissions the sensor fence should reliably detect (%): '))
    minimumDetectionThreshold = as.numeric(readline(prompt = paste('Enter the distance from the sensor where the detection threshold falls below ', min_detection_percent, '% (m): ')))
    halfOfminimumDetectionThreshold = as.numeric(readline(prompt = paste('Enter the distance from the sensor where the detection threshold falls below ', as.numeric(min_detection_percent)/2, '% (m): ')))
  }

  while(minimumDetectionThreshold > halfOfminimumDetectionThreshold){
    print('minimumDetectionThreshold must be larger than the value of halfOfMinimumDetectionTreshold. For example, if 25% of transmissions are detected 500m from a sensor, the distance at which only 12.5% of transmissions are detected must be farther from the sensor. Note: These values are often discovered through range testing.')
    minimumDetectionThreshold = as.numeric(readline(prompt = paste('Enter the distance from the sensor where the detection threshold falls below ', min_detection_percent, '% (m): ')))
    halfOfminimumDetectionThreshold = as.numeric(readline(prompt = paste('Enter the distance from the sensor where the detection threshold falls below ', as.numeric(min_detection_percent)/2, '% (m): ')))
  }

  if(is.null(numberOfSensorsToPlace)){
    numberOfSensorsToPlace = as.numeric(readline(prompt = paste('Enter the distance from the sensor where the detection threshold falls below ', min_detection_percent, '% (m): ')))
  }

  ### Get Bathymetry and transect data
  trans_bath = getBathyTransect(bathymetry = bathymetry, startFence = fenceStart, endFence = fenceEnd)
  transectData = trans_bath$transect
  bathymetry = trans_bath$bathy

  ### Now for some geometry
  ## Idealized receiver spacing places receivers so that detection thresholds intersect at 45 degree angles
  IdealizedReceiverSpacing = hypotonuseToLeg(halfOfminimumDetectionThreshold)
  ## Multiplying this length by two to acount for a detection range on either side of the receiver in the center of the circle
  IdealReceiverSpacing = IdealizedReceiverSpacing*2

  ### Setting up output repositories
  FixedNumerOfReceivers = c()
  FixedNumberOfReceiversRequired = c()

  ## What is the radius of the receiver footprint at the benthos (if center of circle is above benthos, it curves back under itself so less than horizontal)
  DistanceOfReceiverRangeAccountingForBottom25 = sqrt((minimumDetectionThreshold^2)-(heightOfSensor^2));
  DistanceOfReceiverRangeAccountingForBottom12 = sqrt((halfOfminimumDetectionThreshold^2)-(heightOfSensor^2));

  ## What is the diameter footprint of a receiver at the benthos for 1. end_receivers and 2. interior receivers
  diameter_end_receiver = DistanceOfReceiverRangeAccountingForBottom25 + DistanceOfReceiverRangeAccountingForBottom12
  diameter_interior_receiver = 2 * DistanceOfReceiverRangeAccountingForBottom12

  #### ix. Subsetting full transect data, bound by user specified upper and lower depth limits. Output into TransectBounds matrix -----
  TransectIndex = which((transectData$depth<upperDepthLimit)+(transectData$depth>=lowerDepthLimit)==2)
  if(length(TransectIndex) == 0){
    TransectIndex = c(which(transectData$depth>=lowerDepthLimit)[length(which(transectData$depth>=lowerDepthLimit))], which(transectData$depth<upperDepthLimit)[1])
  }else{
  ## Padding out the TransectIndex to include a point shallower and a point deeper than the depth ranges specified to ensure adaquate coverage
  TransectIndex=c(min(TransectIndex)-1, TransectIndex, max(TransectIndex)+1)
  }
  ## Creating new dataframe with data only for points in the specified depth range
  TransectBounds=as.data.frame(cbind((transectData$dist.km[TransectIndex]*1000) , transectData$depth[TransectIndex], rep(0, length(TransectIndex)), rep(0, length(TransectIndex)), rep(0, length(TransectIndex)), transectData$lat[TransectIndex], transectData$lon[TransectIndex])) ## Also converst distances between successive points from km to m
  colnames(TransectBounds) = c('dist_total_horiz', 'elevation', 'delta_dist_horiz', 'dist_total_euclid', 'delta_dist_euclid', 'lat', 'lon')
  ## Standardizing re-zeroing TransectBounds$dist_total_horiz to start at 0
  TransectBounds$dist_total_horiz=TransectBounds$dist_total_horiz - min(TransectBounds$dist_total_horiz)
  ## Creating a new column for change in elevation
  TransectBounds$delta_elevation = c(0, diff(TransectBounds$elevation))
  ## Filling in column 3 - delta_dist_horiz by taking the difference between each successive point.
  TransectBounds$delta_dist_horiz = c(0, diff(TransectBounds$dist_total_horiz))
  ## Filling in column 5 - delta_dist_euclid using pathagorian's theorum for the hypotonuse distance using elevation change and distance between successive points.
  for(i in 1:length(TransectBounds$delta_dist_horiz)){
    TransectBounds$delta_dist_euclid[i] = sqrt(TransectBounds$delta_dist_horiz[i]^2 + TransectBounds$delta_elevation[i]^2)
  }
  ## Filling in column 4 - dist_total_eculid by summing all prior delta_dist_euclid values
  for(i in 1:length(TransectBounds$delta_dist_euclid)){
    TransectBounds$dist_total_euclid[i] = sum(TransectBounds$delta_dist_euclid[1:i])
  }

  ### Determining the start and end points for the receiver transect
  ### Accounting for 25% detection range on ends of receiver line being smaller than the 12% detection ranges making up overlaptransmission receivers
  # Getting the number of datapoints in TransectBounds (number of rows)
  R = nrow(TransectBounds)
  StartSightedTransect = TransectBounds$delta_dist_euclid[1] + DistanceOfReceiverRangeAccountingForBottom25
  EndSightedTransect = TransectBounds$dist_total_euclid[R] - DistanceOfReceiverRangeAccountingForBottom25

  # A Receiver is to be placed at the two following points (ends of the fence)
  IndexStart = which.min(abs(TransectBounds$dist_total_euclid - StartSightedTransect)) - 1 # one index shallower to account for rounding
  IndexStop = which.max(which(TransectBounds$dist_total_euclid < EndSightedTransect)) + 1 # one index deeper to account for rounding

  ## Now that we've placed a receiver at each end, we can place remaining receivers with uniform euclidian spacing between them
  # Subsetting TransectBounds for only the positions between the two end receivers
  ReceiverLineIndex = IndexStart:IndexStop
  ReceiverLine = TransectBounds[ReceiverLineIndex, ]# for only putting in distance that need to be spanned by the receiver fence
  R = dim(ReceiverLine)[1] # Getting the number of datapoints in TransectBounds (number of rows)

  #### If there was not a number of receivers specified for the fence, the number of receivers for "Optimal spacing" is determined
  if(is.null(numberOfSensorsToPlace) == TRUE){ # if there is no restriction no receivers specified for number of receivers to place
    # The number of receivers to place is the total distance of the fence that must be spanned + (2 * the length of the 25% receiver detection  radius at the benthos (DistanceOfReceiverRangeAccountingForBottom25) because the total distance the fence spans is total distance between end receivers plus each of their truncated ranges for the end of the fence.
    DistanceFenceSpans = (ReceiverLine$dist_total_euclid[R]-ReceiverLine$dist_total_euclid[1]) + (2 * DistanceOfReceiverRangeAccountingForBottom25)
    # Begin by placing two receivers on each end
    numberOfSensorsToPlace = 2
    DistanceSpanned = 2 * DistanceOfReceiverRangeAccountingForBottom12
    # Until the distance spanned by the receivers is greater than or equal to the distance the fence needs to span, add additional interior receivers
    while(DistanceSpanned < DistanceFenceSpans){
      numberOfSensorsToPlace = numberOfSensorsToPlace + 1
      DistanceSpanned = DistanceSpanned + diameter_interior_receiver
    }
  }

  #### Placing receivers equidistant from one another along a line. If user specified numberOfSensorsToPlace as an integer, that number of recievers is used. If this parameter was left blank, the optimal number of receivers is used.
  DistancesOfReceiverLine = seq(from = ReceiverLine$dist_total_euclid[1], to = ReceiverLine$dist_total_euclid[R], length.out = numberOfSensorsToPlace)

  #### Placement occurs by finding the closest transact data (subset in ReceiverLine) points to either side of the ideal position of a receiver. The change between these two points is calculated and then multiplied by ((distance_point1 - ideal_receiver) / (distance_point1 - distance_point2). This is then added to the transact data point under evaluation to get an estimated depth, latitude, and logitude for each receiver deployment. The contents of this is written out to a file "fenceLine.csv" if the fence created meets or exceeds the performance of a fence given the user specified criteria

  fenceLine = as.data.frame(matrix(NA, nrow = 0, ncol = 5))
  # %%Notes on fenceLine
  # %%Column 1=sequential receiver number
  # %%Column 2=Distance between subsequent receivers
  # %%Column 3=Depth of that placement
  # %%Coulmn 4=Longitude of that placement
  # %%Column 5=Latitude of that Placement
  for (i in 1:length(DistancesOfReceiverLine)){
    # Comparing the each receiver's position to values from the transect data set, and getting one value on either side
    LocationIndex1 = max(which(ReceiverLine$dist_total_euclid <= DistancesOfReceiverLine[i]))
    LocationIndex2 = LocationIndex1 + 1
    if (i == 1){ # If placing the first receiver
      PercentageOfTotalDistance = 0 # where does receiver fall between successive points. In this specific case, receiver is at 0 percent, or right on top of first location
      DistanceToLocationIndex1 = 0 # Because the receiver is on top of a data pint, the distance to that data point is zero
      DistanceToLastReceiver = 0 # There is no previous receiver to compare it to
      DistanceBetweenLocations = ReceiverLine$dist_total_euclid[LocationIndex2] - ReceiverLine$dist_total_euclid[LocationIndex1] # Distance spanned between subsequent data points (m)
      DepthAtPlacement = ReceiverLine$elevation[LocationIndex1] # Depth where first receiver is placed (m)
      ReceiverLatitude = ReceiverLine$lat[LocationIndex1] # Latitude coordinate of first receiver position
      ReceiverLongitude = ReceiverLine$lon[LocationIndex1]  # Longitude coordinate of first receiver position
      DistanceToReceiverIndex2 = DistanceBetweenLocations # Distance to second transact datapoint
      OldIndex2 = LocationIndex2 # Storing former LocationIndex2
    }else if (i == length(DistancesOfReceiverLine)){ # If placing the last receiver
      LocationIndex1 = LocationIndex1-1 # Decriment index1
      LocationIndex2 = LocationIndex2-1 # Decriment index2
      DistanceBetweenOldReceiverAndCurrentReceiverIndex1 = (ReceiverLine$dist_total_euclid[LocationIndex1] - ReceiverLine$dist_total_euclid[OldIndex2]) + DistanceToReceiverIndex2 # Distance between previously placed receiver and new receiver
      DistanceBetweenLocations = ReceiverLine$dist_total_euclid[LocationIndex2]-ReceiverLine$dist_total_euclid[LocationIndex1] # Distance between two transect data points under evaluation
      DistanceOfReceiverToLocation1 = DistancesOfReceiverLine[i] - ReceiverLine$dist_total_euclid[LocationIndex1] # Distance between the receiver and the current transect data point under evaluation
      PercentageOfTotalDistance = DistanceOfReceiverToLocation1/DistanceBetweenLocations # What percent of the distance between the two transect data points does this represent. For the case when the last receiver is under evaluation, this is equal to 1.
      DistanceToLastReceiver = DistanceBetweenOldReceiverAndCurrentReceiverIndex1 + DistanceOfReceiverToLocation1 # Distance between the previously receiver and the current receiver
      DepthAtPlacement = (ReceiverLine$elevation[LocationIndex2]-ReceiverLine$elevation[LocationIndex1]) * PercentageOfTotalDistance + ReceiverLine$elevation[LocationIndex1] # depth at which receiver is to be deployed, calculated as the elevation change between the two data points multiplied by the percentage of the distance between those points spanned, added to the depth of the first location point under eval
      ReceiverLatitude = (ReceiverLine$lat[LocationIndex2]-ReceiverLine$lat[LocationIndex1])*PercentageOfTotalDistance+ReceiverLine$lat[LocationIndex1] # The change in Latitude between two transect data points multiplied by the % of the distance between those points spanned added to the depth of the first data point
      ReceiverLongitude=(ReceiverLine$lon[LocationIndex2]-ReceiverLine$lon[LocationIndex1])*PercentageOfTotalDistance+ReceiverLine$lon[LocationIndex1] # The change in Longitude between two transect data points multiplied by the % of the distance between those points spanned added to the depth of the first data point
    }else{ ## If placing interior fence receivers, that is, those that are neither the first nor the last
      DistanceBetweenOldReceiverAndCurrentReceiverIndex1 = (ReceiverLine$dist_total_euclid[LocationIndex1] - ReceiverLine$dist_total_euclid[OldIndex2]) + DistanceToReceiverIndex2 # Distance between current first transect point currently under evaluation and the previous second transect point under evaluation added with the distance between the previous receiver placed relative to the previous second transect point evaluated
      DistanceBetweenLocations = ReceiverLine$dist_total_euclid[LocationIndex2] - ReceiverLine$dist_total_euclid[LocationIndex1] # Euclidean distance between two transect points under evaluation
      DistanceOfReceiverToLocation1 = DistancesOfReceiverLine[i] - ReceiverLine$dist_total_euclid[LocationIndex1] # distance (m) between first transact point under evaluation and the position of the current receiver
      PercentageOfTotalDistance = DistanceOfReceiverToLocation1 / DistanceBetweenLocations # determining what the percentage of the distance between first and second transact points under evaluation the receiver placement represents
      DistanceToLastReceiver = DistanceBetweenOldReceiverAndCurrentReceiverIndex1 + DistanceOfReceiverToLocation1 # The distance between the previous receiver and the current receiver
      DepthAtPlacement = (ReceiverLine$elevation[LocationIndex2]-ReceiverLine$elevation[LocationIndex1]) * PercentageOfTotalDistance + ReceiverLine$elevation[LocationIndex1] # Depth at which receiver is to be deployed, calculated as the elevation change between the two data points multiplied by the percentage of the distance between those points spanned, added to the depth of the first location point under eval
      ReceiverLatitude = (ReceiverLine$lat[LocationIndex2] - ReceiverLine$lat[LocationIndex1]) * PercentageOfTotalDistance + ReceiverLine$lat[LocationIndex1] # The change in Latitude between two transect data points multiplied by the % of the distance between those points spanned added to the Latitute of the first data point
      ReceiverLongitude=(ReceiverLine$lon[LocationIndex2] - ReceiverLine$lon[LocationIndex1]) * PercentageOfTotalDistance + ReceiverLine$lon[LocationIndex1] # The change in Longitude between two transect data points multiplied by the % of the distance between those points spanned added to the Longitute of the first data point
      DistanceToReceiverIndex2 = DistanceBetweenLocations - DistanceOfReceiverToLocation1
      OldIndex2 = LocationIndex2
    }
    write_line = cbind(i, round(DistanceToLastReceiver), round(DepthAtPlacement), round(ReceiverLongitude, digits = 3), round(ReceiverLatitude, digits = 3)) # Creating new line for fenceLine table.
    colnames(write_line) = paste('V', 1:length(write_line), sep = "") # Standardizing column names because rbind is a useless function
    fenceLine = rbind(fenceLine, write_line) # adding new line to fenceLine dataframe
  }
  colnames(fenceLine) = c("ReceiverNumber", "deltaDistEuclid", "receiverDepth", "receiverLon", "receiverLat") # Naming fenceLine columns after rbinding everything because god forbid whoever wrote it let you be flexible

  #### xi. Testing fence produced against the minimum performance criteria outlined by the user ----

  R = dim(fenceLine)[1]

  ### Getting the radial distance of the circle formed by two overlaptransmission receiver detection spheres
  RadiusOfOverlaptransmissionSphere = 0
  if(is.nan(sqrt((halfOfminimumDetectionThreshold^2)-((fenceLine[R,2]/2)^2))) == FALSE){
    RadiusOfOverlaptransmissionSphere = sqrt((halfOfminimumDetectionThreshold^2)-((fenceLine[R,2]/2)^2))
    # Radius is equivilant to solving pathagorean theorum for the vertical leg of a  45° 45° 90° triangle with a hypotenuse the length of the smaller detection radius and the horizontal leg 1/2 the euclidian distance between two receivers.
  }

  # If not otherwise specified, the height of the fence is the half of the length of the vertical leg of a 45° 45° 90° triangle formed from horizontal detection distance for a receiver with a hypotenuse the length of the smaller detection radius of the receiver. This is the vertical height through the detection sphere that will be used to calculate the minimum transmission interval for a tag.
  if (is.null(minimumFenceHeight) == TRUE){
    minimumFenceHeight = (0.5 * RadiusOfOverlaptransmissionSphere) + heightOfSensor
  }

  ## Determining the horizontal cord length across a circle produced when two detection spheres overlap, across the height of the fence. This is 2x the length of a radial leg from a 45° 45° 90° formed with a hypotenuse from the radius of the overlaptransmission sphere and a vertical leg that is the total height of the fence from the bottom - receiver off the bottom, or the distance between the receiver's position in the water column to the height of the final fence
  LengthOfAcceptableDetection = 2 * sqrt(RadiusOfOverlaptransmissionSphere^2 - (minimumFenceHeight - heightOfSensor)^2)

  if (RadiusOfOverlaptransmissionSphere == 0){
    print('Insufficient receivers to place a fence across the perscribed distance')
    return()
  }else if (RadiusOfOverlaptransmissionSphere <= minimumFenceHeight){
    print('Insufficient receivers to place a fence of the perscribed height')
    return()
  }else{
    MaximumTagInterval = LengthOfAcceptableDetection/swimSpeedOfStudySpecies;
    print(paste('The maximum transmission interval in this configuration is ' , as.character(floor(MaximumTagInterval)), ' seconds', sep = ""))
  }

    #### Writing results file -----

    #### xii. Writing the Results file ----
    ### If fence passes all criteria, write fenceLine out to fenceLine.csv

    ## Writing contents of fenceLine to fenceLine.csv
    ## Notes On Output File
    ## Column 1 = sequential receiver number
    ## Column 2 = Distance between subsequent receivers
    ## Column 3 = Depth of that placement
    ## Column 4 = Longitute
    ## Column 5 = Latitude

  plot(bathymetry)
  text(fenceLine$receiverLat ~ fenceLine$receiverLon, col = 'red', labels = fenceLine$ReceiverNumber)

  return(fenceLine)
}

