
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GTFS2EMIS

Respository of the R package to estimate public transport emissions
based on GTFS
files.

### Installation

``` r
devtools::install_github("rafapereirabr/gtfs2emis",auth_token = "94c88a642a2f62fce15e4c05cc0b6ead55ff5851")
```

    ## 
    ##      checking for file ‘/tmp/RtmpAtZbN8/remotes6a1a784bd9d/rafapereirabr-gtfs2emis-db6231ebb3d5639601afa826ab06797603dca9f5/DESCRIPTION’ ...  ✓  checking for file ‘/tmp/RtmpAtZbN8/remotes6a1a784bd9d/rafapereirabr-gtfs2emis-db6231ebb3d5639601afa826ab06797603dca9f5/DESCRIPTION’
    ##   ─  preparing ‘gps2emis’:
    ##      checking DESCRIPTION meta-information ...  ✓  checking DESCRIPTION meta-information
    ##   ─  checking for LF line-endings in source and make files and shell scripts
    ##   ─  checking for empty or unneeded directories
    ## ─  looking to see if a ‘data/datalist’ file should be added
    ##        NB: this package now depends on R (>= 3.5.0)
    ##        WARNING: Added dependency on R >= 3.5.0 because serialized objects in  serialize/load version 3 cannot be read in older versions of R.  File(s) containing such objects:  'gps2emis/inst/extdata/fleet/cur/cur.rds'
    ##   ─  building 'gps2emis_0.0.0.9000.tar.gz'
    ##      
    ## 

### Usage

For now, the function are a little bit adapted to Curitiba’s GTFS and
fleet characteristics. Also, it still possible to simulate emissions
only for CO and
NOx.

``` r
# ef_hdv_speed(vel = units::set_units(10,km/h),veh = "Urban Buses Midi <=15 t",
#             fuel = "Diesel",euro = "Euro III",tech = NA,pol = "CO",show.equation = TRUE)
#
# gtfs2gps(raw_gtfs = "inst/extdata/gtfs_cur.zip",filepath = "test_joao/gps/",filter_weekdays = TRUE)
#
# gps_to_linestring(input_filepath = "test_joao/gps/",output_filepath = "test_joao/lines/",
#                  fleet_data = "inst/extdata/cur_fleet.tar.xz",overwrite = FALSE)
```

### Preparation scripts

##### 01\) `prep/01_*.R` - prep scripts

1 - download gps cur

2 - prep fleet cur

3 - check valid shapeids (perhaps it could be add into gtfs2gps package)

4 - create hexagons (not working so far I dont know why)

5 - read\_gps (from points data to linestring)

##### 02\) `prep/02_*.R` - emission

1 - emission estimation

##### 03\) `prep/03_*.R` - main

1 - main script

##### 04\) `prep/08_*.R` - plot and visualization

used only for check data purposes

##### 05\) `prep/09_*.R` - auxiliar script / internal functions

1 - emission factor from EMEEA

### Project

The main steps of the development should be presented in `projects`
sheet
