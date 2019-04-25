# sensorFenceR
An R package for consecutive linear placement of overlapping sensor units relative to marine bathymetry

This repository houses the package 'sensorFenceR'

Getting Started
These instructions will get you a copy of the package up and running on your local machine.

You will need to download and install the following software: 
  - R (https://www.r-project.org) 
  - RStudio (http://rstudio.com)
  
 After intializing an instance of R, you will need to install and source the 'dev.tools' package using the commands
  install.packages('devtools')
  library('devtools')
  
 You can now install the sensorFenceR package using the command
  install_github("stevescherrer/sensorFenceR")
 
 Usage
 To design your sensor fence, call the function "runSensorFenceR()"
 The function will prompt users to answer some basic questions using the console to paramterizing the model.
 Users will also need to click the start and end points of their transect (where the fence should span) on the interactive map that will appear.
 
 Following this, sensorFenceR will return the ideal location for deployment of your sensors.
