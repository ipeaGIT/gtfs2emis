#
# test for detroit
#
rm(list=ls())
library(magrittr)
library(data.table)
library(ggplot2)
library(sf)
library(mapview)
#
# gtfs2gps
#
gtfs_data <- gtfs2gps::read_gtfs("../../data-raw/gtfs/all/detroit/week.gtfs.zip")
#gtfs_data <- gtfs2gps::filter_by_shape_id(gtfs_data,shape_ids = "51020")
gps_data <- gtfs2gps::gtfs2gps(gtfs_data,filepath = "test_joao/gps/detroit/")
input_gps_fname <- list.files("test_joao/gps/detroit/",full.names = TRUE) # full name
input_gps_sname <- list.files("test_joao/gps/detroit/",full.names = FALSE) # short name
# gps_as_sflinestring
gps2line_data <- lapply(seq_along(input_gps_fname),function(i){ # i = 1
  message(input_gps_sname[i])
  dt <- gtfs2emis::gps_as_sflinestring(input_file = input_gps_fname[i]) %>% 
    data.table::as.data.table()
  outputname <- paste0("test_joao/gps_linestring/detroit/",gsub(".txt",".rds",input_gps_sname[i]))
  readr::write_rds(dt,outputname)
})
#
# fleet
#
det_fleet_files <- list.files("../../../../Dropbox/IPEA/data-raw/fleet/detroit/data",
                              full.names = TRUE)
det_fleet <- lapply(det_fleet_files,function(i){
  data.table::fread(i)
}) %>% data.table::rbindlist()
#
# fleet allocation
#
det_fleet[,time_start := data.table::as.ITime("00:00:00")]
det_fleet[,time_end := data.table::as.ITime("00:00:00")]

lapply(input_gps_sname,function(j){ # j = "51020.txt"
  message(j)
  dt <- readr::read_rds(paste0("test_joao/gps_linestring/detroit/",gsub(".txt",".rds",j))) 
  gps_fleet_aloc <- lapply(unique(dt$trip_id),function(i){ 
    # i = unique(dt$trip_id)[1]
    # departure_time of each trip_id
    temp_dpt <- setDT(dt)[trip_id %in% i,departure_time]
    # available fleet for each trip_id
    temp_fleet <- det_fleet[time_end < first(temp_dpt) | 
                              time_start > last(temp_dpt),]
    # gps data from trip_id
    temp_gps2line <- dt[trip_id %in% i,]
    # sample one vehicle from temp_fleet  
    fleet_sample <- temp_fleet[sample(seq_along(temp_fleet$Fleet_Number),1),]
    fleet_sample[,temp_trip_id := i][,time_start := NULL][,time_end := NULL]
    # allocate sampled in gps2line
    output <- temp_gps2line[fleet_sample,on = c("trip_id" = "temp_trip_id")]
    # update avaliable fleet
    det_fleet[Fleet_Number %in% fleet_sample$Fleet_Number,`:=`(
      time_start = data.table::as.ITime(first(temp_dpt)),
      time_end = data.table::as.ITime(last(temp_dpt))
    )] 
    # export output
    return(output)
  }) %>% data.table::rbindlist()
  # write
  outputname <- paste0("test_joao/gps_line_fleet/detroit/",gsub(".txt",".rds",j))
  readr::write_rds(gps_fleet_aloc,outputname)
  return(NULL)
})

#
# emissions
#
# ---
# emission factor 'CO'
lapply(input_gps_sname,function(j){ # j = "51040.txt"
  message(j)
  dt <- readr::read_rds(paste0("test_joao/gps_line_fleet/detroit/",gsub(".txt",".rds",j)))
  dt[,dist := units::set_units(dist,m)]
  FE_local <- lapply(unique(dt$Fleet_Number),function(i){ 
    # i = unique(dt$Fleet_Number)[1] 
    # temp_gps data per fleet_number
    # message(i)
    temp_gps <- dt[Fleet_Number %in% i,]
    
    #print(head(temp_gps,1))
    temp_ef <- ef_emfac(pol = "CO",calendar_year = "2019",
                        model_year = unique(temp_gps$Build_Date),
                        speed = units::set_units(temp_gps$speed,km/h),
                        fuel = "Diesel") 
    return(temp_ef)
  }) %>% unlist() %>% units::set_units("g/km")
  # emission
  
  # writedt[,CO_emissions := units::set_units(dist,km) * FE_local]
  outputname <- paste0("test_joao/emi/detroit/",gsub(".txt",".rds",j))
  readr::write_rds(dt,outputname)
  return(NULL)
})
# --
# create grid
boundary <- sf::read_sf("../../../../Dropbox/IPEA/data-raw/boundary/detroit/6c717e99-58c3-4711-9577-ad8df26739ae202045-1-kkyyti.h9ul.shp")
boundary <- sf::st_transform(boundary,4326)
grid_gps <- vein::make_grid(spobj = boundary,width =  0.25/102.47) #500 mts
mapview(boundary$geometry)
mapviewOptions(platform = "leafgl")
mapview(grid_gps$geometry)
readr::write_rds(grid_gps,"../../../../Dropbox/IPEA/data-raw/gridded_boundary/detroit/detroit.rds")
# allocate emissions into grid
gridded <- lapply(input_gps_sname,function(j){ # j = "51040.txt"
  message(j)
  dt <- readr::read_rds(paste0("test_joao/emi/detroit/",gsub(".txt",".rds",j)))
  # main
  dt <- dt %>% sf::st_as_sf() %>% sf::st_transform(4326) 
  e_grid <- data.table::setDT(vein::emis_grid(spobj = dt[c("CO_emissions")], g = grid_gps))
  e_grid[,shape_id := gsub(".txt",".rds",j)]
  return(e_grid)
}) %>% data.table::rbindlist()
readr::write_rds(gridded,paste0("test_joao/emi/detroit/all.rds"))
# merge lines 
gridded <- gridded[,CO_emissions := sum(CO_emissions),by = id][,.SD[1],by=id]
gridded <- gridded %>% sf::st_as_sf()
gridded <- gridded[as.numeric(gridded$CO_emissions) > 0, ]
mapview(gridded["CO_emissions"])
head(gridded,1)
