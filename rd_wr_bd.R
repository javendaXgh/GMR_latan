library(dplyr)
global_r <- readRDS(paste0('/Users/josemiguelavendanoinfante/R/google_mobility_report_ggplot_graficos/',
                           list.files('/Users/josemiguelavendanoinfante/R/google_mobility_report_ggplot_graficos/') %>%
                             .[substr(.,1,5)=='latan']))
saveRDS(global_r,'global.rds')
write.csv(global_r,'global.csv')
