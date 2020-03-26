# gps2emis

Respository of the R package to estimate public transport emissions based on GTFS files.


`prep/01_*.R` - prep scripts 

1 - download gps cur

2 - prep fleet cur

3 - check valid shapeids (perhaps it could be add into gtfs2gps package)

4 - create hexagons (not working so far I dont know why)

5 - read_gps (from points data to linestring)

`prep/02_*.R` - emission

`prep/03_*.R` - main 

`prep/08_*.R` - plot and visualization

`prep/09_*.R` - auxiliar script / internal functions

