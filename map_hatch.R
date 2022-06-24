library(tidyverse)
library(raster)
source("calc_hatch_function_slow.R")


# current year
cyear = format(Sys.Date(), "%Y")

# where to download
downloadfolder = paste0("../silo/", cyear)

# make function to download silo data into folders sorted by year
download.silo = function(var, date, downloadfolder = "./") {
  date = as.Date(date, origin="1970-01-01")
  cat(paste("downloading", date, var, "\n"))
  dir.create(file.path(downloadfolder, var), showWarnings = F)
  year  = format(date, "%Y") 
  month = format(date, "%m") 
  day   = format(date, "%d") 
  filename = paste0(year, month, day, ".", var,".tif")
  filepath = file.path(downloadfolder, var, filename)
  url = file.path(
    "https://s3-ap-southeast-2.amazonaws.com/silo-open-data/Official/daily",var, year, filename)
  print(url)
  if(!file.exists(filepath)){
    tryCatch(
    download.file(url, filepath, quiet = TRUE, mode = "wb"),
    error =  function(e) e
    )
  } 
}

# download new climate data for current year
vars = c("daily_rain","min_temp","max_temp")

warning("here!")
dates = seq.Date(as.Date(paste0(cyear,"-01-01")), Sys.Date()-1, by = 1)
# dates = seq.Date(as.Date(paste0(cyear,"-01-01")), as.Date("2022-05-29"), by = 1)

for(date in dates){
  for(var in vars){
    download.silo(var, date, downloadfolder)
  }  
}

# build stack of climate data for current year
rainfiles = list.files(file.path(downloadfolder,"/daily_rain/"), full.names=T)
tminfiles = list.files(file.path(downloadfolder,"/min_temp/"), full.names=T)
tmaxfiles = list.files(file.path(downloadfolder,"/max_temp/"), full.names=T)


# set the simulation day by restricting raster stack here for historical sims  
RAIN = stack(rainfiles)
TMIN = stack(tminfiles)
TMAX = stack(tmaxfiles)

# load previous hatch simulation
hatchrasterfiles = list.files("plots/hatchraster/", pattern = "tif", full.names = T)
hatchrasterfile = hatchrasterfiles[length(hatchrasterfiles)]
if(length(hatchrasterfiles) == 0) hatchrasterfile = NULL

# run new simulation with new climate data
hatch = calc_hatch(TMIN, TMAX, RAIN, hatchrasterfile = hatchrasterfile)

# save model output as raster
h = RAIN[[1]]
h[] = hatch 
plot(h)

# find the min max date available for each climate data
findmaxdate = function(filelocation) {
  filelocation |>
  stringr::str_extract("\\d{8}") |>
  as.integer() |>
  max()
}
maxsilodate = min(
  findmaxdate(rainfiles), 
  findmaxdate(tminfiles), 
  findmaxdate(tmaxfiles))

writeRaster(h, sprintf("./plots/hatchraster/hatch_day_at_%s.tif", maxsilodate))


