##### Data Cleaning
##### Daniel Dempsey

### Loading in packages
library( readxl ) # For reading in excel files
library( readr ) # For writing csv files

### Read in data
xp_dat <- read_xlsx( 'Data/initial_results_V3.xlsx' )

### Utility Function to create extra columns
add_section <- function( dat, ind, test_num, labels ) {
  
  for ( i in seq_along( labels ) ) {
    dat[[ind]][ dat$test %in% paste0('test_', test_num[[i]]) ] <- labels[[i]]
  }
  
  dat
  
}

### Add in Sections
xp_dat$section <- 1
section_test <- list( 9:12, 13, 14:15, 16:17, 18:19, 20:23 )
section_label <- as.list( 2:7 )
xp_dat_v2 <- add_section( xp_dat, 'section', section_test, section_label )

### Add in Signal Type
xp_dat_v2$signal_type <- 'drums'
signal_test <- list( c(2,3,6,7), 9:10, 11:12, 13, 14, 15, 16, 17, 18, 19, 22:23 )
signal_label <- list( 'speech', 'viola/violin', 'oboe/clarinet', 'pink noise', 'organ', 'choir',
                      'trio', 'choir', 'outdoor scene', 'indoor scene', 'speech' )
xp_dat_v3 <- add_section( xp_dat_v2, 'signal_type', signal_test, signal_label )

### Add in Source
xp_dat_v3$source <- 'NA'
source_test <- list( 1:4, 5:8 )
source_label <- list( 'horizontal', 'elevated' )
xp_dat_v4 <- add_section( xp_dat_v3, 'source', source_test, source_label )

### Change Decoder Names (Enda's Request)
stimulus <- factor( xp_dat_v4$stimulus )
levels( stimulus ) <- c('Anchor', 'Hybrid', 'Active', 'Passive', 'Reference')
xp_dat_v4$stimulus <- stimulus
xp_dat_v4$stimulus_signal <- paste0( xp_dat_v4$stimulus, '_', xp_dat_v4$signal_type )

### Save altered dataset
write_csv( xp_dat_v4, 'Data/new_dat.csv' )
write_csv( xp_dat_v4, 'Code/MUSHRA_Visualisation_App/new_dat.csv' )
