##### Data Visualisation
##### Daniel Dempsey

### Load libraries
library( dplyr )
library( readr )

### Load data
xp_dat <- read_csv('Data/new_dat.csv', col_types = 'cccddcc')
xp_dat$stimulus <- factor( xp_dat$stimulus, levels = c('Active', 'Hybrid', 'Passive', 'Anchor', 'Reference') )

### Set working directory
setwd( 'Output/DataViz/' )

### Set plotting parameters
# Colour pallette for plots, taken from: https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible
mycol <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",  
           "#0072B2", "#D55E00", "#CC79A7", "#999999")
scales::show_col(mycol)

# Plot titles
section_titles <- paste0( 'Section ', 1:7, ':\n', 
                          c('Localization Accuracy - single static source',
                            'Localization Accuracy - multiple static sources',
                            'Locatedness', 'Timbre', 'Spatial Impression/Envelopment',
                            'Naturalness/Presence',
                            'Localization Accuracy - single source, off-centre listener position') )

### Section boxplots
for ( i in unique(xp_dat$section) ) {
  
  pdf( paste0( 'Box_Section_', i, '.pdf' ) )
  sec_dat <- filter( xp_dat, section == i )
  #mean_vals <- sapply( split(sec_dat$result, sec_dat$stimulus), mean )
  plot( 0, 0, ylim = c(0, 101), xlim = c(0.5, 5.5), main = section_titles[i], 
        xlab = '', ylab = 'Score', xaxt = 'n', yaxt = 'n', type = 'n')
  grid()
  boxplot( result ~ stimulus, data = sec_dat, col = mycol[3], pch = 20, 
           add = TRUE )
  #points( 1:5, mean_vals, pch = 20, col = 'orange' )
  dev.off()
  
}

### Boxplots seperating the decoders and signals
for ( i in unique(xp_dat$section) ) {
  
  pdf( paste0( 'Box_Section_Signal_', i, '.pdf' ) )
  
  sec_dat <- filter( xp_dat, section == i )
  levs <- unique( sec_dat$stimulus_signal )
  active_vec <- levs[ which(grepl( 'Active', levs )) ]
  hybrid_vec <- levs[ which(grepl( 'Hybrid', levs )) ]
  passive_vec <- levs[ which(grepl( 'Passive', levs )) ]
  anchor_vec <- levs[ which(grepl( 'Anchor', levs )) ]
  reference_vec <- levs[ which(grepl( 'Reference', levs )) ]
  desired_levs <- c( active_vec, hybrid_vec, passive_vec, anchor_vec, reference_vec )
  sec_dat$stimulus_signal <- factor( sec_dat$stimulus_signal, desired_levs )
  use_signal <- unique( sec_dat$signal_type )
  
  signal_labels <- sub( ".*_", "", levels(sec_dat$stimulus_signal)[1:2] )
  
  #mean_vals <- sapply( split(sec_dat$result, sec_dat$stimulus_signal), mean )
  plot( 0, 0, ylim = c(0, 101), xlim = c(0.5, 10.5), main = section_titles[i], 
        xlab = '', ylab = 'Score', xaxt = 'n', type = 'n', yaxt = 'n')
  
  abline( h = seq(0, 100, 20), lty = 3, col = 'lightgray' )
  abline( v = seq(0.5, 10.5, 2), lty = 3 )
  
  boxplot( result ~ stimulus_signal, data = sec_dat, pch = 20, 
           add = TRUE, xaxt = 'n', col = mycol[1:2] )
  axis( 1, at = seq(1.5, 9.5, 2),
        labels = c('Active', 'Hybrid', 'Passive', 'Anchor', 'Reference') )
  #points( 1:10, mean_vals, pch = 20, col = 'orange' )
  
  legend( 'bottomright', legend = signal_labels, col = mycol[1:2], pch = 20 )
  
  dev.off()
  
}

### For section 1, draw boxplot within horizontal and elevation
sec1_source <- na.omit( unique( xp_dat$source ) )
for ( i in seq_along(sec1_source) ) {
  
  pdf( paste0( 'Sec_1_Box_', sec1_source[i], '.pdf' ) )
  
  sec_dat <- filter( xp_dat, (section == 1) & (source == sec1_source[i]) )
  levs <- unique( sec_dat$stimulus_signal )
  active_vec <- levs[ which(grepl( 'Active', levs )) ]
  hybrid_vec <- levs[ which(grepl( 'Hybrid', levs )) ]
  passive_vec <- levs[ which(grepl( 'Passive', levs )) ]
  anchor_vec <- levs[ which(grepl( 'Anchor', levs )) ]
  reference_vec <- levs[ which(grepl( 'Reference', levs )) ]
  desired_levs <- c( active_vec, hybrid_vec, passive_vec, anchor_vec, reference_vec )
  sec_dat$stimulus_signal <- factor( sec_dat$stimulus_signal, desired_levs )
  use_signal <- unique( sec_dat$signal_type )
  
  signal_labels <- sub( ".*_", "", levels(sec_dat$stimulus_signal)[1:2] )
  
  #mean_vals <- sapply( split(sec_dat$result, sec_dat$stimulus_signal), mean )
  plot( 0, 0, ylim = c(0, 101), xlim = c(0.5, 10.5), 
        main = paste0( section_titles[i], ' - ', sec1_source[i] ),
        xlab = '', ylab = 'Score', xaxt = 'n', type = 'n', yaxt = 'n')
  
  abline( h = seq(0, 100, 20), lty = 3, col = 'lightgray' )
  abline( v = seq(0.5, 10.5, 2), lty = 3 )
  
  boxplot( result ~ stimulus_signal, data = sec_dat, pch = 20, 
           add = TRUE, xaxt = 'n', col = mycol[1:2] )
  axis( 1, at = seq(1.5, 9.5, 2),
        labels = c('Active', 'Hybrid', 'Passive', 'Anchor', 'Reference') )
  #points( 1:10, mean_vals, pch = 20, col = 'orange' )
  
  legend( 'bottomright', legend = signal_labels, col = mycol[1:2], pch = 20 )
  
  dev.off()
  
}

### For section 2, draw boxplot within signal
sec2_source <- na.omit( unique( filter( xp_dat, section == 2 )$signal_type ) )
for ( i in seq_along(sec2_source) ) {
  
  pdf( paste0( 'Sec_2_Box_', i, '.pdf' ) )
  
  sec_dat <- filter( xp_dat, (section == 2) & (signal_type == sec2_source[i]) )
  
  plot( 0, 0, ylim = c(0, 101), xlim = c(0.5, 5.5), 
        main = paste0( section_titles[i], ' - ', sec2_source[i] ), 
        xlab = '', ylab = 'Score', xaxt = 'n', yaxt = 'n', type = 'n')
  grid()
  boxplot( result ~ stimulus, data = sec_dat, col = mycol[3], pch = 20, 
           add = TRUE )
  
  dev.off()
  
}


