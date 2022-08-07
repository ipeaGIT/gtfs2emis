test_that("ef_europe_emep", {
  # GTFS2gps filter
  
  # gtfs2emis
  EF_europe <- ef_europe_emep(pollutant = c("CO", "PM10"),
                              speed = tp_model_irl$speed,
                              veh_type = fleet_data_europe$veh_type,
                              tech = "SCR",
                              euro = fleet_data_europe$euro)
  
  # Expect equal -----
  expect_equal(length(EF_europe), 9)
  expect_equal(length(EF_europe$pollutant), 6)
  expect_equal(length(EF_europe$veh_type), 6)
  expect_equal(length(EF_europe$euro), 6)
  expect_equal(units::deparse_unit(EF_europe$EF$CO_Euro_II), "g km-1")
 # expect_equal(sum(EF_europe$EF,na.rm = TRUE), 1188.842,0.1)
  
  
  # Expect error-----
  # speed
  expect_error(
    ef_europe_emep(pollutant = c("CO", "PM")
                   ,speed = units::set_units(c(NA,1,2),"km/h")
                   ,veh_type = fleet_data_europe$veh_type
                   ,tech = "SCR"
                   ,euro = fleet_data_europe$euro)
  )
  expect_error(
    ef_europe_emep(pollutant = c("CO", "PM")
                   ,speed = units::set_units(c(1,2,200),"km/h")
                   ,veh_type = fleet_data_europe$veh_type
                   ,tech = "SCR"
                   ,euro = fleet_data_europe$euro)
  )
  expect_error(
    ef_europe_emep(pollutant = c("CO", "PM")
                   ,speed = units::set_units(c(1,2,200),"km")
                   ,veh_type = fleet_data_europe$veh_type
                   ,tech = "SCR"
                   ,euro = fleet_data_europe$euro)
  )
  # veh_type
  expect_error(
    ef_europe_emep(pollutant = c("CO", "PM")
                   ,speed = units::set_units(c(1,2,20),"km/h")
                   ,veh_type = c("Ubus Midi <=15 t",NA)
                   ,tech = "SCR"
                   ,euro = fleet_data_europe$euro)
  )
  expect_error(
    ef_europe_emep(pollutant = c("CO", "PM")
                   ,speed = units::set_units(c(1,2,20),"km/h")
                   ,veh_type = c("banana")
                   ,tech = "SCR"
                   ,euro = fleet_data_europe$euro)
  )
  expect_error(
    ef_europe_emep(pollutant = c("CO", "PM")
                   ,speed = units::set_units(c(1,2,20),"km/h")
                   ,veh_type = 23213
                   ,tech = "SCR"
                   ,euro = fleet_data_europe$euro)
  )
  # euro
  expect_error(
    ef_europe_emep(pollutant = c("CO", "PM")
                   ,speed = units::set_units(c(1,2,20),"km/h")
                   ,veh_type = "Ubus Midi <=15 t"
                   ,tech = "SCR"
                   ,euro = c("III",NA))
  )
  expect_error(
    ef_europe_emep(pollutant = c("CO", "PM")
                   ,speed = units::set_units(c(1,2,20),"km/h")
                   ,veh_type = "Ubus Midi <=15 t"
                   ,tech = "SCR"
                   ,euro = c("banana"))
  )
  expect_error(
    ef_europe_emep(pollutant = c("CO", "PM")
                   ,speed = units::set_units(c(1,2,20),"km/h")
                   ,veh_type = "Ubus Midi <=15 t"
                   ,tech = "SCR"
                   ,euro = 123)
  )
  # fuel
  expect_error(
    ef_europe_emep(pollutant = c("CO", "PM")
                   ,speed = units::set_units(c(1,2,20),"km/h")
                   ,veh_type = "Ubus Midi <=15 t"
                   ,tech = "SCR"
                   ,euro = c("III"),fuel = c(NA,"D"))
  )
  expect_error(
    ef_europe_emep(pollutant = c("CO", "PM")
                   ,speed = units::set_units(c(1,2,20),"km/h")
                   ,veh_type = "Ubus Midi <=15 t"
                   ,tech = "SCR"
                   ,euro = c("III"),fuel = c("banana"))
  )
  expect_error(
    ef_europe_emep(pollutant = c("CO", "PM")
                   ,speed = units::set_units(c(1,2,20),"km/h")
                   ,veh_type = "Ubus Midi <=15 t"
                   ,tech = "SCR"
                   ,euro = "III",fuel = 1231)
  )
  # pollutant
  expect_error(
    ef_europe_emep(pollutant = c("CO", NA)
                   ,speed = units::set_units(c(1,2,20),"km/h")
                   ,veh_type = "Ubus Midi <=15 t"
                   ,tech = "SCR"
                   ,euro = c("III"))
  )
  expect_error(
    ef_europe_emep(pollutant = c("CO", "banana")
                   ,speed = units::set_units(c(1,2,20),"km/h")
                   ,veh_type = "Ubus Midi <=15 t"
                   ,tech = "SCR"
                   ,euro = c("III"))
  )
  expect_error(
    ef_europe_emep(pollutant = c("CO", 2132)
                   ,speed = units::set_units(c(1,2,20),"km/h")
                   ,veh_type = "Ubus Midi <=15 t"
                   ,tech = "SCR"
                   ,euro = "III")
  )
  # tech
  expect_error(
    ef_europe_emep(pollutant = c("CO")
                   ,speed = units::set_units(c(1,2,20),"km/h")
                   ,veh_type = "Ubus Midi <=15 t"
                   ,tech = c("SCR",NA)
                   ,euro = c("III"))
  )
  expect_error(
    ef_europe_emep(pollutant = c("CO")
                   ,speed = units::set_units(c(1,2,20),"km/h")
                   ,veh_type = "Ubus Midi <=15 t"
                   ,tech = "banana"
                   ,euro = c("III"))
  )
  expect_error(
    ef_europe_emep(pollutant = c("CO")
                   ,speed = units::set_units(c(1,2,20),"km/h")
                   ,veh_type = 1231
                   ,tech = "SCR"
                   ,euro = "III")
  )
  # as_list
  expect_error(
    ef_europe_emep(pollutant = c("CO")
                   ,speed = units::set_units(c(1,2,20),"km/h")
                   ,veh_type = "Ubus Midi <=15 t"
                   ,tech = "SCR"
                   ,euro = "III",as_list = 123)
  )
  # lengths
  expect_error(
    ef_europe_emep(pollutant = c("CO")
                   ,speed = units::set_units(c(1,2,20),"km/h")
                   ,veh_type = "Ubus Midi <=15 t"
                   ,tech = "SCR"
                   ,euro = "III",slope = rep(0.3,2))
  )
  expect_error(
    ef_europe_emep(pollutant = c("CO")
                   ,speed = units::set_units(c(1,2,20),"km/h")
                   ,veh_type = "Ubus Midi <=15 t"
                   ,tech = "SCR"
                   ,euro = "III",load = rep(0.3,2))
  )
})
