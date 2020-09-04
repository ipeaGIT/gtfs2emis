#' @importFrom magrittr %>%
#' @importFrom data.table := .SD .I .GRP %like% setDT
NULL

`%nin%` = Negate(`%in%`)

`%nlike%` = Negate(`%like%`)

# Globals
utils::globalVariables(c('id', '.'))
utils::globalVariables(c('Euro.Standard', 'Fuel', 'Load', 'Pollutant', 'Road.Slope', 'Segment',
                         'Technology'))
utils::globalVariables(c('europe'))
utils::globalVariables(c('usa', 'Calendar Year', 'Model Year', 'EF'))
utils::globalVariables(c('lkm_inter', 'ratio', 'temp_lkm'))
utils::globalVariables(c('year','i.den','i.s','i.pah','i.cn','i.t95','den_base',
                         'pah_base','cn_base','t95_base','s_base','den_imp','pah_imp',
                         'cn_imp','t95_imp','s_imp'))
