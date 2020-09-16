Introduction to gtfs2emis
================
Joao Bazzo, Rafael H. M. Pereira, Pedro R. Andrade

7 August 2020

# Introduction

The package `gtfs2emis` estimates hot exhaust emissions for public
transport networks at high spatial and temporal resolutions based on
GTFS data. This vignette introduces the main functions of the package.
Here are the following packages we will use:

``` r
library(gtfs2emis)
library(magrittr)
library(ggplot2)
```

# GTFS data

To estimate emissions from GTFS data, we need first to compute `speed`
and distance (`dist`) of each trip between pairs of public transport
stops/stations. This information is created when we convert GTFS data to
GPS-like records, using `gtfs2gps::gtfs2gps()`. The example below uses a
sample of the GTFS data from São Paulo municipality, in Brazil. In this
example we will only process the trips of two `shape_id`s to speedup
processing time.

``` r
spo <- gtfs2gps::read_gtfs(system.file("extdata/saopaulo.zip", package = "gtfs2gps")) %>%
  gtfs2gps::filter_by_shape_id(c("51982", "50784"))
spo_gps <- gtfs2gps::gtfs2gps(spo)

# the ouput is a data.table, like this
head(spo_gps)
#>    id shape_id   trip_id trip_number route_type shape_pt_lon shape_pt_lat
#> 1:  1    50784 407E-10-0           1          3    -46.44303    -23.63370
#> 2:  2    50784 407E-10-0           1          3    -46.44290    -23.63336
#> 3:  3    50784 407E-10-0           1          3    -46.44285    -23.63315
#> 4:  4    50784 407E-10-0           1          3    -46.44277    -23.63292
#> 5:  5    50784 407E-10-0           1          3    -46.44270    -23.63269
#> 6:  6    50784 407E-10-0           1          3    -46.44269    -23.63263
#>    departure_time   stop_id stop_sequence      dist   cumdist   cumtime
#> 1:       00:00:59 770007651             1 39.873149  39.87315  59.33129
#> 2:       00:01:36      <NA>            NA 24.678450  64.55160  96.05286
#> 3:       00:01:41 770007650             2 26.396777  90.94838 101.30573
#> 4:       00:01:47      <NA>            NA 26.396777 117.34515 106.55860
#> 5:       00:01:48      <NA>            NA  6.587615 123.93277 107.86952
#> 6:       00:01:54      <NA>            NA 33.248587 157.18135 114.48588
#>        speed
#> 1:  2.419353
#> 2:  2.419353
#> 3: 18.090750
#> 4: 18.090750
#> 5: 18.090750
#> 6: 18.090750
```

The GPS-like data can be converted into line segments (`linestring`).
This conversion breaks the trips into separate segments between
consecutive pairs of stops. This will give us a much smaller data set to
work on, and allows to capture the spatial-temporal variation of speeds
and emissions levels.

``` r
spo_gpslines <- gtfs2gps::gps_as_sflinestring(spo_gps) %>% dplyr::select(trip_id,speed, dist, departure_time)

dim(spo_gps)
#> [1] 124900     15
dim(spo_gpslines)
#> [1] 8923    5

# the bus routes look like this:
plot(spo_gpslines['speed'])
```

![](/home/sergio/models/gtfs2emis/vignettes/gtfs2emis-vignette_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

We will also update units of measurement of the attributes in our data.

``` r
spo_gpslines$dist <- units::set_units(spo_gpslines$dist, "km")

# The data shows the average speed and distance between pairs of stops
head(spo_gpslines)
#> Simple feature collection with 6 features and 4 fields
#> geometry type:  LINESTRING
#> dimension:      XY
#> bbox:           xmin: -46.44696 ymin: -23.6337 xmax: -46.44269 ymax: -23.62561
#> geographic CRS: WGS 84
#>     trip_id     speed            dist departure_time
#> 1 407E-10-0  2.419353 0.03969249 [km]       00:00:59
#> 2 407E-10-0 18.090750 0.33483783 [km]       00:01:41
#> 3 407E-10-0 15.862761 0.29423847 [km]       00:02:58
#> 4 407E-10-0  5.761980 0.08712751 [km]       00:04:29
#> 5 407E-10-0 10.585952 0.19520212 [km]       00:05:35
#> 6 407E-10-0  7.569150 0.12146853 [km]       00:06:56
#>                         geometry
#> 1 LINESTRING (-46.44303 -23.6...
#> 2 LINESTRING (-46.44285 -23.6...
#> 3 LINESTRING (-46.44359 -23.6...
#> 4 LINESTRING (-46.44568 -23.6...
#> 5 LINESTRING (-46.44617 -23.6...
#> 6 LINESTRING (-46.44626 -23.6...
```

# Fleet data

To compute emission estimates, we need some basic information on the age
and technology of the public transport fleet. The user can input fleet
data organized in two different ways. We call them *regional data* and
*detailed data*.

## Regional fleet data

Regional fleet data input is a simple `data.frame` with information on
the **relative composition of the fleet in terms of age and
technology**. For many cities, this is the only data from registry of
vehicles made available by local transport agencies. In this typical
situation, there is no information on which vehicles (age and/or
technology) are assigned to specific routes in the GTFS. In this case,
the package `gtfs2emis` considers that, on average, all vehicles have
the same probability to operate on every route of the transport network.
Here is an example of how a *regional fleet data* looks like:

``` r
total_fleet <- data.table::data.table(year = c(2005, 2010:2012, 2014:2015, 2017:2019),
                                      bus = c(1, 61, 50, 1, 45, 18, 62, 27, 31),
                                      veh_type_euro = "Urban Buses Standard 15 - 18 t",
                                      euro_stage = c("II", "IV", "IV", rep("V", 6)))

total_fleet[,fleet_composition := bus / sum(bus)]

# The input data looks like this
head(total_fleet)
#>    year bus                  veh_type_euro euro_stage fleet_composition
#> 1: 2005   1 Urban Buses Standard 15 - 18 t         II       0.003378378
#> 2: 2010  61 Urban Buses Standard 15 - 18 t         IV       0.206081081
#> 3: 2011  50 Urban Buses Standard 15 - 18 t         IV       0.168918919
#> 4: 2012   1 Urban Buses Standard 15 - 18 t          V       0.003378378
#> 5: 2014  45 Urban Buses Standard 15 - 18 t          V       0.152027027
#> 6: 2015  18 Urban Buses Standard 15 - 18 t          V       0.060810811

sum(total_fleet$fleet_composition)
#> [1] 1
```

## Detailed fleet data

Alternatively, the user can also pass as an input a `data.frame` with
detailed data on which vehicle categories (age/technology) are assigned
to each route on the GTFS feed. Here is an example of how this input
looks like.

``` r
det_fleet <- data.table::data.table(shape_id = unique(spo$shapes$shape_id),
                                    bus_age = c("2010", "2011", "2012", "2013"),
                                    bus_fuel = "Diesel")

head(det_fleet)
#>    shape_id bus_age bus_fuel
#> 1:    50784    2010   Diesel
#> 2:    51982    2011   Diesel
#> 3:    50784    2012   Diesel
#> 4:    51982    2013   Diesel
```

# Emission factor

Before we estimate emission levels, we need first need to calculate what
are the emission factors for each pollutant given a combination of fleet
characteristics. For Europe and the United States, we can use more
rigorous functions that also account for speed information as well. The
`gtfs2emis` package generates emission factors expressed in `g/km` for
each trip segment between consecutive pairs of stops. The emission
factors currently available in `gtfs2emis` are from Europe, US, and
Brazil, see below.

## European emission factors for buses

The `ef_europe` function can be used to estimate the emission factors
for multiple pollutants considering different types of vehicles and
technologies listed in the [EMEP/EEA air pollutant emission inventory
guidebook 2019](https://www.eea.europa.eu/themes/air/air-pollution-sources-1/emep-eea-air-pollutant-emission-inventory-guidebook).
See documentation for list of options `?ef_europe`. Supported
pollutants: CO, NOx, HC, PM, CH4, NMHC, CO2, SO2, Pb, FC , NO, and NO2.

The function works like this:

``` r
EF_europe <- ef_europe(pollutant = c("CO", "PM"),
                       speed = vein::Speed(spo_gpslines$speed),
                       veh_type = total_fleet$veh_type_euro,
                       tech = "SCR",
                       euro = total_fleet$euro_stage,
                       fcorr = rnorm(9, 0.5, 0.1))
#> no technology associated with Euro II
#> no technology associated with Euro II
head(EF_europe, 3)
#>         CO_Euro II       CO_Euro IV       CO_Euro IV       CO_Euro V
#> 1: 1.461709 [g/km] 1.1405010 [g/km] 0.6821520 [g/km] 4.148589 [g/km]
#> 2: 1.026624 [g/km] 0.7786811 [g/km] 0.4657417 [g/km] 1.311601 [g/km]
#> 3: 1.130609 [g/km] 0.8537907 [g/km] 0.5106660 [g/km] 1.462192 [g/km]
#>          CO_Euro V       CO_Euro V       CO_Euro V       CO_Euro V
#> 1: 6.806037 [g/km] 5.661368 [g/km] 5.386490 [g/km] 7.628262 [g/km]
#> 2: 2.151769 [g/km] 1.789876 [g/km] 1.702971 [g/km] 2.411721 [g/km]
#> 3: 2.398824 [g/km] 1.995379 [g/km] 1.898497 [g/km] 2.688622 [g/km]
#>          CO_Euro V        PM_Euro II        PM_Euro IV        PM_Euro IV
#> 1: 5.593590 [g/km] 0.11595638 [g/km] 0.03759811 [g/km] 0.02248803 [g/km]
#> 2: 1.768447 [g/km] 0.08326247 [g/km] 0.02922220 [g/km] 0.01747827 [g/km]
#> 3: 1.971491 [g/km] 0.08980496 [g/km] 0.03114395 [g/km] 0.01862770 [g/km]
#>            PM_Euro V         PM_Euro V         PM_Euro V         PM_Euro V
#> 1: 0.04278901 [g/km] 0.07019823 [g/km] 0.05839199 [g/km] 0.05555687 [g/km]
#> 2: 0.02842759 [g/km] 0.04663736 [g/km] 0.03879369 [g/km] 0.03691013 [g/km]
#> 3: 0.03083938 [g/km] 0.05059407 [g/km] 0.04208494 [g/km] 0.04004158 [g/km]
#>            PM_Euro V         PM_Euro V
#> 1: 0.07867876 [g/km] 0.05769292 [g/km]
#> 2: 0.05227154 [g/km] 0.03832925 [g/km]
#> 3: 0.05670625 [g/km] 0.04158110 [g/km]
```

## United States emission factors for buses

For the United States, the `ef_usa` function estimates emission factors
for multiple pollutants considering different types of fuel, speed and
fleet age information based on the [California EMission Factor model
(EMFAC2017)](https://arb.ca.gov/emfac/). See documentation for list of
options `?ef_usa`. Supported pollutants: CO, NOx, Hydrocarbons as TOG
(total organic gases), ROG (reactive organic gases), THC, CH4, PM10,
PM2.5, SOx, CO2, N2O and CH4.

``` r
EF_usa <- ef_usa(pollutant = c("CO", "PM10"),
                 calendar_year = "2019",
                 model_year = total_fleet$year,
                 speed = vein::Speed(spo_gpslines$speed),
                 fuel = "Diesel")
head(EF_usa, 3)
#>             CO_2005          CO_2010          CO_2011          CO_2012
#> 1: 0.8215654 [g/km] 0.4812028 [g/km] 0.2549770 [g/km] 0.2382385 [g/km]
#> 2: 0.3725280 [g/km] 0.2041763 [g/km] 0.1115174 [g/km] 0.1058134 [g/km]
#> 3: 0.6421423 [g/km] 0.3286603 [g/km] 0.1762283 [g/km] 0.1664479 [g/km]
#>              CO_2014           CO_2015          CO_2017          CO_2018
#> 1: 0.22037526 [g/km] 0.20540820 [g/km] 0.2407551 [g/km] 0.2302044 [g/km]
#> 2: 0.09785266 [g/km] 0.09121356 [g/km] 0.1064519 [g/km] 0.1021209 [g/km]
#> 3: 0.15384723 [g/km] 0.14343118 [g/km] 0.1676797 [g/km] 0.1606730 [g/km]
#>             CO_2019          PM10_2005          PM10_2010          PM10_2011
#> 1: 0.2674933 [g/km] 0.005034035 [g/km] 0.006037529 [g/km] 0.004285711 [g/km]
#> 2: 0.1140830 [g/km] 0.004397409 [g/km] 0.005003436 [g/km] 0.003435218 [g/km]
#> 3: 0.1838292 [g/km] 0.004818207 [g/km] 0.005394625 [g/km] 0.003890010 [g/km]
#>             PM10_2012          PM10_2014          PM10_2015          PM10_2017
#> 1: 0.004297150 [g/km] 0.004100377 [g/km] 0.003935507 [g/km] 0.004268862 [g/km]
#> 2: 0.003392060 [g/km] 0.003236196 [g/km] 0.003106210 [g/km] 0.003378595 [g/km]
#> 3: 0.003929818 [g/km] 0.003748086 [g/km] 0.003597862 [g/km] 0.003897909 [g/km]
#>             PM10_2018          PM10_2019
#> 1: 0.004195573 [g/km] 0.004054468 [g/km]
#> 2: 0.003313239 [g/km] 0.003253489 [g/km]
#> 3: 0.003835032 [g/km] 0.003686085 [g/km]
```

## Brazil emission factors for buses

For buses in Brazil, the `ef_brazil` functions estimates emission
factors considering model year and types of buses. These estimates are
based on data from [CETESB 2017](https://cetesb.sp.gov.br/veicular/)
obtained via the [`vein` package](https://atmoschem.github.io/vein/).
Supported pollutants: CO, HC, NMHC, CH4, NOx, CO2, RCHO, ETOH, PM, N2O,
KML, FC, NO2, NO, gD/KWH, gCO2/KWH, RCHO. , CO\_0km, HC\_0km, NMHC\_0km,
NOx\_0km, NO2\_0km, NO\_0km, RCHO\_0km, ETOH\_0km, and FS (fuel sales).

*Acho que não é necessario mostrar os fatores 0km, pois só ussados os
deteriorados. Inclusive, os fatores deteriorados só aplicam a veículos
com moor Otto ou Flex, então não aplica incluir aqui. FS tambem não,
pois é pensado na estimativa de emissões evaporativas desde o veículo ao
ambiente aomomento de abastecimento*

``` r
EF_brazil <- ef_brazil(pollutant = c("CO", "CO2"),
                       veh_type = "BUS_URBAN_D",
                       years = total_fleet$year) # fleet_composition
head(EF_brazil, 3)
#> Units: [g/km]
#>      CO_2005  CO_2010 CO_2011   CO_2012   CO_2014   CO_2015   CO_2017   CO_2018
#> [1,]    1.41 1.827202 1.67243 0.6255739 0.5275977 0.5295905 0.4617687 0.5315306
#>        CO_2019 CO2_2005 CO2_2010 CO2_2011 CO2_2012 CO2_2014 CO2_2015 CO2_2017
#> [1,] 0.5315306 1278.175 1399.906 1424.399 1329.682 1341.547 1338.582 1334.626
#>      CO2_2018 CO2_2019
#> [1,] 1275.807 1275.807
```

# Emission

Now that we have route segment-level information on vehicles, distances
and emission factors, emissions levels can be estimated as a product
between distances (units `km`) and emission factors (units `g/km`).
Using the `emis` function we can estimate emissions at high spatial and
temporal resolutions.

Here is an example using using different emission factors from Europe,
USA and Brazil.

``` r
# USA
emi_usa <- gtfs2emis::emis(fleet_composition = total_fleet$fleet_composition,
                           dist = spo_gpslines$dist,
                           ef = EF_usa, 
                           prefix = "USA")

# EUROPE
emi_europe <- gtfs2emis::emis(fleet_composition = total_fleet$fleet_composition,
                              dist = spo_gpslines$dist,
                              ef = EF_europe,
                              prefix = "EU")

# BRAZIL (not speed dependent emission factor)
emi_brazil <- gtfs2emis::emis(fleet_composition = total_fleet$fleet_composition,
                              dist = spo_gpslines$dist,
                              ef = EF_brazil,
                              prefix = "BR")
#> Constant emission factor along the route
head(emi_brazil, 3)
#>       BR_CO_total  BR_CO2_total
#> 1: 0.03886771 [g]  53.69613 [g]
#> 2: 0.32788019 [g] 452.96975 [g]
#> 3: 0.28812444 [g] 398.04680 [g]
```

When we join emission estimates with `sf` spatial data, it looks like
this.

``` r
spo_emis <- cbind(spo_gpslines, emi_usa)
spo_emis <- cbind(spo_emis, emi_europe)
spo_emis <- cbind(spo_emis, emi_brazil)

head(spo_emis)
#> Simple feature collection with 6 features and 10 fields
#> geometry type:  LINESTRING
#> dimension:      XY
#> bbox:           xmin: -46.44696 ymin: -23.6337 xmax: -46.44269 ymax: -23.62561
#> geographic CRS: WGS 84
#>     trip_id     speed            dist departure_time   USA_CO_total
#> 1 407E-10-0  2.419353 0.03969249 [km]       00:00:59 0.01156056 [g]
#> 2 407E-10-0 18.090750 0.33483783 [km]       00:01:41 0.04236155 [g]
#> 3 407E-10-0 15.862761 0.29423847 [km]       00:02:58 0.05925146 [g]
#> 4 407E-10-0  5.761980 0.08712751 [km]       00:04:29 0.02537615 [g]
#> 5 407E-10-0 10.585952 0.19520212 [km]       00:05:35 0.03930829 [g]
#> 6 407E-10-0  7.569150 0.12146853 [km]       00:06:56 0.03537807 [g]
#>     USA_PM10_total    EU_CO_total      EU_PM_total    BR_CO_total  BR_CO2_total
#> 1 0.0001811507 [g] 0.02219217 [g] 0.0006650718 [g] 0.03886771 [g]  53.69613 [g]
#> 2 0.0012285936 [g] 0.11752732 [g] 0.0043200145 [g] 0.32788019 [g] 452.96975 [g]
#> 3 0.0012179394 [g] 0.11338741 [g] 0.0040501557 [g] 0.28812444 [g] 398.04680 [g]
#> 4 0.0003976372 [g] 0.04781831 [g] 0.0014598744 [g] 0.08531707 [g] 117.86639 [g]
#> 5 0.0008079989 [g] 0.10098948 [g] 0.0032599562 [g] 0.19114598 [g] 264.07009 [g]
#> 6 0.0005543645 [g] 0.06467514 [g] 0.0020352790 [g] 0.11894451 [g] 164.32303 [g]
#>                         geometry
#> 1 LINESTRING (-46.44303 -23.6...
#> 2 LINESTRING (-46.44285 -23.6...
#> 3 LINESTRING (-46.44359 -23.6...
#> 4 LINESTRING (-46.44568 -23.6...
#> 5 LINESTRING (-46.44617 -23.6...
#> 6 LINESTRING (-46.44626 -23.6...
```

# Analyzing public transport emission levels

To facilitate the analysis of these emissions estimates, the `gtfs2emis`
package also includes the `emis_post`, a handy function to aggregate
results by time at different temporal resolutions. This allows users to
examine how the transport emissions of a given public transport network
varies over the day.

## Hour time stamp

``` r
hour_emissions <- gtfs2emis::emis_post(data = spo_emis,
                                       emi = c('BR_CO_total','EU_CO_total'),
                                       time_class = "hour",
                                       time_column = 'departure_time')

ggplot(data = hour_emissions) + 
    geom_line(aes(x = departure_time, y = as.numeric(BR_CO_total)/1000), color = 'gray50') +
    geom_line(aes(x = departure_time, y = as.numeric(EU_CO_total)/1000)) +
    xlab("Hour of the day") + ylab("Total CO emitted in Kilograms") +
    theme_bw()
```

![](/home/sergio/models/gtfs2emis/vignettes/gtfs2emis-vignette_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## Hour-minute time stamp

Alternatively, the user can aggregate the results at every minute,
getting a much higher temporal resolution.

``` r
minute_emissions <- gtfs2emis::emis_post(data = spo_emis,
                                         emi = c('BR_CO_total','EU_CO_total'),
                                         time_class = "hour-minute",
                                         time_column = 'departure_time')

minute_emissions[, group_x := .GRP, by = departure_time]
breaks <- minute_emissions$group_x[seq(1, nrow(minute_emissions), length.out = 10)]
x_labels <- minute_emissions[breaks, departure_time]

ggplot(data = minute_emissions) +
  geom_line(aes(group = 1, x = group_x, y = as.numeric(BR_CO_total) / 1000), color = 'gray50') +
  geom_line(aes(group = 1, x = group_x, y = as.numeric(EU_CO_total) / 1000)) +
  xlab("Time of the day") + 
  ylab("Total CO emitted in Kilograms") +
  scale_x_continuous(breaks = breaks, labels = x_labels) +
  theme_bw()
```

![](/home/sergio/models/gtfs2emis/vignettes/gtfs2emis-vignette_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

## Spatialize

Finally, we can also map how emission levels varies across space and
time of the day. To do this, we need first to aggregate our emission
estimates on a spatial grid.

In this example, we are using some support functions from the `VEIN`
package, but this aggregation can be done using any spatial polygons,
including hexagonal grids, census tracts etc.

``` r
# PEDRO: funcao para escolher a resolucao? esta funcao deveria receber um objeto
# com unidade de medida, para casar com "km"

# Create a regular spatial grid
grid_gps <- vein::make_grid(spobj = spo_emis, width =  0.25 / 102.47) # 500 meters
#> Number of lon points: 107
#> Number of lat points: 84
plot(grid_gps["id"])
```

![](/home/sergio/models/gtfs2emis/vignettes/gtfs2emis-vignette_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Emissions can be allocated into grid through `emis_grid` function, which
sums the total emissions into a single grid cell.

``` r
# convert estimates to sf spatial object
spo_sf <- sf::st_as_sf(spo_emis)

# spatial aggregation
pol_gps <- gtfs2emis::emis_grid(data = spo_sf,
                                emi = c("EU_CO_total", "BR_CO_total"),
                                grid = grid_gps,
                                time_class = 'all periods')
#> input data "EU_CO_total" is in units: g
#> input data "BR_CO_total" is in units: g
#> Sum of street emissions
#> EU_CO_total = 990.98 g
#> BR_CO_total = 3205.7 g
#> Sum of gridded emissions
#> EU_CO_total = 990.98 g
#> BR_CO_total = 3205.7 g

ggplot() +
  geom_sf(data = pol_gps, aes(fill = as.numeric(EU_CO_total) / 1000), color = NA) +
  scale_fill_viridis_c(option = "plasma") +
  labs(fill = "Daily CO\nemitted locally\n(g)") +
  theme_bw()
```

![](/home/sergio/models/gtfs2emis/vignettes/gtfs2emis-vignette_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

User can also display gridded emissions by time and grid cell by
specifying `hour` or `hour-minute` in the `time_class` argument.

``` r
# # spatial aggregation by hour
pol_gps_hour <- gtfs2emis::emis_grid(data = spo_sf,
                                     emi = c("EU_CO_total", "BR_CO_total"),
                                     grid = grid_gps,
                                     time_class = 'hour',
                                     time_column = 'departure_time')
#> input data "EU_CO_total" is in units: g
#> input data "BR_CO_total" is in units: g
#> Sum of street emissions
#> EU_CO_total = 990.98 g
#> BR_CO_total = 3205.7 g
#> Sum of gridded emissions
#> EU_CO_total = 990.98 g
#> BR_CO_total = 3205.7 g

# subset hours of interest
pol_gps_hour <- subset(pol_gps_hour, time_column %in% c(8, 16, 23))
to_string <- as_labeller(c(`8` = "Hour 8", `16` = "Hour 16", `23` = "Hour 23"))

ggplot(data = pol_gps_hour) +
  geom_sf(aes(fill=as.numeric(EU_CO_total) / 1000), color = NA) +
  scale_fill_viridis_c(option = "plasma") +
  labs(fill = "Hourly CO\nemitted\n(kg)") +
  facet_wrap(facets = ~time_column, labeller = to_string) + 
  theme_minimal() +
  theme(axis.text = element_blank(),
        strip.background = element_rect(fill = "gray80", color = NA))
```

![](/home/sergio/models/gtfs2emis/vignettes/gtfs2emis-vignette_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r

    # theme(panel.background = element_rect(fill = "white", 
    #   colour = NA), 
    #   panel.border = element_rect(fill = NA, 
    #   colour = "grey20"), 
    #   axis.text.x =  NULL,
    #   panel.grid = element_line(colour = "grey92"), 
    #   panel.grid.minor = element_line(size = rel(0.5)), 
    #   strip.background = element_rect(fill = "grey85", 
        # colour = "grey20"), legend.key = element_rect(fill = "white", 
        # colour = NA), complete = TRUE) + 
```
