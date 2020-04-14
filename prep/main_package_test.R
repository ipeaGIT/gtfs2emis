# -----
# create a script called utils.R
usethis::use_testthat()
usethis::use_test("gps_to_linestring") #
devtools::load_all()
devtools::document() # it creates a man, updates namespace
devtools::check()
devtools::install()

data.table::setDTthreads(threads = 1)
 
# ----
# 
ef_hdv_speed(vel = 10,veh = "Urban Buses Midi <=15 t",
             fuel = "Diesel",euro = "Euro III",tech = NA,pol = "CO",show.equation = TRUE)

gtfs2gps(raw_gtfs = "inst/extdata/gtfs_cur.zip",filepath = "test_joao/gps/",filter_weekdays = TRUE)

gps2line(input_filepath = "test_joao/gps/",output_filepath = "test_joao/lines/",
                  fleet_path = "inst/extdata/cur_fleet.tar.xz",overwrite = TRUE)

emis(pol_list = c("CO","NOx"),input_folder = "test_joao/lines",output_folder = "test_joao/emis",overwrite = FALSE)
