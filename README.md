# gps2emission
Estimation of bus emissions based on GPS data

## to do
- generate low gps resolution for `gtfs2gps` data set from SP, Fortaleza and Porto Alegre
- verify bus fleet on each city

## doing
- first results for emissions in SP, considering speed variation
- verifying code for plotting maps

## done
- first results for emission in SP, without speed consideration
  - emissions seems to be concentrated on bus terminals
  - 18h30 min to estimate emissions
  - the main reason for slow processing time is the high size of input data (gps for each `15` min)