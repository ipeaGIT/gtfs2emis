
test_that("slope_class_europe_emep", {
  gtfs_file <- system.file("extdata/bra_cur_gtfs.zip", package = "gtfs2emis")
  gtfs <- gtfstools::read_gtfs(gtfs_file) 
  # keep a single trip_id to speed up this example
  gtfs_small <- gtfstools::filter_by_trip_id(gtfs, trip_id ="4451136")
    
  # run transport model
  tp_model <- transport_model(gtfs_data = gtfs_small,
                              min_speed = 2,
                              max_speed = 80,
                              new_speed = 20,
                              spatial_resolution = 100,
                              parallel = FALSE)
  
  # read raster file
  raster_cur <- system.file("extdata/bra_cur-srtm.tif", package = "gtfs2emis")
  
  tp_model_slope <- slope_class_europe_emep(tp_model,raster_cur)
  
  expect_equal(tp_model_slope$slope_class, c(0.0,0.0), 0.001)
  expect_equivalent(nrow(tp_model_slope), 2)
  expect_equivalent(ncol(tp_model_slope), 15)
  
  tp_model_slope_true <- slope_class_europe_emep(tp_model,raster_cur,keep = TRUE)
  expect_equivalent(nrow(tp_model_slope_true), 2)
  expect_equivalent(ncol(tp_model_slope_true), 19)
  })
