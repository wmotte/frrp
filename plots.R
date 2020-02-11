#!/usr/bin/env Rscript
#
# w.m.otte@umcutrecht.nl
#########################

library( 'ggplot2' )
library( 'plyr' )
library( 'Hmisc' )
library( "cowplot" )

###################################################
# FUNCTIONS
###################################################

###
# Get proportional CI
##
get.prop.ci <- function( k, n, lower = TRUE )
{

	if( k > n ) stop( "*** ERROR ***: could not calculate proportional CI as k > n!" )

	p <- prop.test( x = k, n = n )

	if( lower )
	{
		return( p$conf.int[ 1 ] )
	} else {
		return( p$conf.int[ 2 ] )
	}
}

number_ticks <- function( n ) { function( limits ) pretty( limits, n ) }
errs <- function(x, na.rm=FALSE) { if (na.rm) x <- na.omit(x); sqrt(var(x)/length(x)) }

###
# Plot cont.
##
plot.facet <- function( type = 'cont', df.cont, var2, label2, var1, label1, outdir, fillcolour = '#3690c0' )
{
	xlabel = 'Year'
    
    ylimits <- c( 0, 95 )
    if( type == 'cont' )
        ylimits <- c( 0, 65 )

	# get means and 95% confidence intervals
	df <- df.cont
	df$x <- as.factor( df.cont$year_group )
	df$y <- df.cont[, var1 ]
	df$group <- df.cont[, var2 ]

	if( type == 'cont' )
	{
		# get means and 95% confidence interval - with linear regression
		ds <- ddply( df, .( x, group ), summarise, mean = 100 * mean( y, na.rm = T ), 
												lower = 100 * ( mean( y, na.rm = T ) - 1.96 * errs( y, na.rm = T ) ), 
												upper = 100 * ( mean( y, na.rm = T ) + 1.96 * errs( y, na.rm = T ) ) )
	} else {
		# percentage and 95% CI
		ds <- ddply( df, .( x, group ), summarise, 
				mean = 100 * sum( y, na.rm = TRUE ) / length( y ),							
				lower = 100 * get.prop.ci( k = sum( y, na.rm = TRUE ), n = length( y ), lower = TRUE ),
				upper = 100 * get.prop.ci( k = sum( y, na.rm = TRUE ), n = length( y ), lower = FALSE ) )	
	}

	levels( ds$x ) <- c( '<1990', '1990-1995', '1995-2000', '2000-2005', '2005-2010', '2010-2018' )

	# Default bar plot
	p <- ggplot( ds, aes( x = x, y = mean, fill = group ) ) + 
	  		geom_bar( stat = "identity", colour = 'gray20', position = position_dodge() ) +
			geom_errorbar( aes( ymin = lower, ymax = upper ), width = 0.2, position = position_dodge( 0.9 ) ) +
			scale_y_continuous( breaks = number_ticks( 8 ), limits = ylimits ) +
		   	scale_fill_manual( values = c( '#E69F00', '#d95f0e' ) ) +
			ylab( label1 ) +
			xlab( xlabel ) +
			theme_classic( base_size = 14 ) +
			theme( legend.position = 'top', axis.title = element_text( face = "bold" ), axis.text.x = element_text( angle = 45, hjust = 1 ) ) + labs( fill = label2 )

	# write to file
	ggsave( p, file = paste( outdir, '/', var2, '_', var1, '.png', sep = '' ), dpi = 200, width = 4, height = 7 )
	write.csv( ds, file = paste( outdir, '/', var2, '_', var1, '.csv', sep = '' ) )

	return( p )
}

###
# Plot number of trials
##
plot_n_pub <- function( df.cont, var = 'number_of_trials', ylabel = 'Trials', outdir, fillcolour = '#31a354' )
{
	print( fillcolour )
	xlabel = 'Year'
	df.cont$x <- df.cont$year_group
	df.cont$y <- 1

	# get means and 95% confidence intervals
	df <- df.cont
	df$x <- as.factor( df.cont$year_group )
	df$y <- 1

	# get means and 95% confidence interval - with linear regression
	ds <- ddply( df, .( x ), summarise, mean = sum( y, na.rm = T ) )
	levels( ds$x ) <- c( '<1990', '1990-1995', '1995-2000', '2000-2005', '2005-2010', '2010-2018' )

	p <- ggplot( ds, aes( x = x, y = mean ) ) +
	  		geom_bar( stat = "identity", colour = 'gray20', position = position_dodge(), fill = fillcolour ) +
			scale_y_continuous( breaks = number_ticks( 8 ), limits = c( 0, 95000 ) ) +
		   	ylab( paste0( ylabel, "" ) ) +
			xlab( xlabel ) +
			theme_classic( base_size = 14 ) +
			theme( legend.position = 'none', axis.title = element_text( face = "bold" ), axis.text.x = element_text( angle = 45, hjust = 1 ) )

	# write to file
	ggsave( p, file = paste( outdir, '/', gsub( '%', '_perc_', gsub( ' ', '_', ylabel ) ), '.png', sep = '' ), dpi = 200, width = 4, height = 7 )

	# write corresponding number to csv
	write.csv( ds, file = paste( outdir, '/', gsub( '%', '_perc_', gsub( ' ', '_', ylabel ) ), '.csv', sep = '' ) )

	return( p )
}

###
# Plot cont.
##
plot.cont <- function( df.cont, var, ylabel = 'Y', outdir, fillcolour = '#3690c0', limits = NULL )
{
	print( fillcolour )
	xlabel = 'Year'
	df.cont$x <- df.cont$year_group
	df.cont$y <- df.cont[, var ]

	# get means and 95% confidence intervals
	df <- df.cont
	df$x <- as.factor( df.cont$year_group )
	df$y <- df.cont[, var ]

	# get means and 95% confidence interval - with linear regression
	ds <- ddply( df, .( x ), summarise, mean = mean( y, na.rm = T ), lower = mean( y, na.rm = T ) - 1.96 * errs( y, na.rm = T ), upper = mean( y, na.rm = T ) + 1.96 * errs( y, na.rm = T ) )
	levels( ds$x ) <- c( '<1990', '1990-1995', '1995-2000', '2000-2005', '2005-2010', '2010-2018' )

	p <- ggplot( ds, aes( x = x, y = mean, ymin = lower, ymax = upper ) ) +
	  		geom_bar( stat = "identity", colour = 'gray20', position = position_dodge(), fill = fillcolour ) +
			geom_errorbar( aes( ymin = lower, ymax = upper ), width = 0.2, position = position_dodge( 0.9 ) ) +
		   	ylab( paste0( ylabel, "" ) ) +
			xlab( xlabel ) +
			theme_classic( base_size = 12 ) +
			theme( legend.position = 'none', axis.title = element_text( face = "bold" ), axis.text.x = element_text( angle = 45, hjust = 1 ) )

	if( is.null( limits ) ) {
		p <- p + scale_y_continuous( breaks = number_ticks( 7 ) )
	} else {
		p <- p + scale_y_continuous( breaks = number_ticks( 7 ), limits = limits )
	}

	# write to file
	ggsave( p, file = paste( outdir, '/', var, '.png', sep = '' ), dpi = 200, width = 4, height = 7 )

	# write corresponding number to csv
	write.csv( ds, file = paste( outdir, '/', var, '.csv', sep = '' ) )

	return( p )
}


###################################################
# END FUNCTIONS
###################################################


# make output directory
outdir <- 'out.plots'
dir.create( outdir, showWarnings = FALSE )

# input file
infile <- 'data/data.csv'  

# read data
all <- read.csv( infile, row.names = 1 )

# add helper column
all$all <- as.factor( 'all' )

# convert prob to percentage
all$gender_propFem <- all$gender_propFem * 100

###########################
# I. Figure 2
###########################

p1 <- plot_n_pub( all, 'trials', 'Trials', outdir )
p2 <- plot.cont( all, "nAuthors", 'Authors', outdir )
p3 <- plot.cont( all, "gender_propFem", 'Proportion female', outdir )
p4 <- plot.cont( all, "h.last", 'H-index (last)', outdir )
p5 <- plot.cont( all, "ncountries", 'Countries', outdir )
p6 <- plot.cont( all, "ninstitution", 'Institutions', outdir )
p7 <- plot.cont( all, "neg.rate", 'Negative words', outdir, limits = c( 0, 0.06 ) )
p8 <- plot.cont( all, "pos.rate", 'Positive words', outdir, limits = c( 0, 0.06 ) )


# arrange the three plots in a single row
p <- plot_grid(
	p1 + theme( legend.position = "none", axis.title = element_blank() ),
	p2 + theme( legend.position = "none", axis.title = element_blank() ),
	p3 + theme( legend.position = "none", axis.title = element_blank() ),
	p4 + theme( legend.position = "none", axis.title = element_blank() ),
	p5 + theme( legend.position = "none", axis.title = element_blank() ),
	p6 + theme( legend.position = "none", axis.title = element_blank() ),
	p7 + theme( legend.position = "none", axis.title = element_blank() ),
	p8 + theme( legend.position = "none", axis.title = element_blank() ),
	align = 'vh',
	labels = c( "A", "B", "C", "D", "E", "F", "G", "H" ),
	label_size = 13,
	hjust = -1,
	nrow = 2
)

# save to disk
save_plot( paste0( outdir, "/Figure_2.png" ), p, base_height = NULL, base_width = 14, base_asp = 1.4 )


###################################
## II. Combine bias plots
###################################

p1 <- plot.facet( 'cont', all, 'all', 'All', 'RoB_allocation_prob', 'Allocation bias', outdir )
p2 <- plot.facet( 'cont', all, 'all', 'All', 'RoB_random_prob', 'Randomization bias', outdir )
p3 <- plot.facet( 'cont', all, 'all', 'All', 'RoB_blinding_pts_prob', 'Blinding of people bias', outdir )
p4 <- plot.facet( 'cont', all, 'all', 'All', 'RoB_blinding_outcome_prob', 'Blinding of outcome bias', outdir )
p5 <- plot.facet( 'bin', all, 'all', 'All', 'trial_registered', 'In database register (%)', outdir )
p6 <- plot.facet( 'bin', all, 'all', 'All', 'consort_mentioned', 'With CONSORT statement (%)', outdir )

# arrange the three plots in a single row
pcombined <- plot_grid(

	p1 + theme( legend.position = "none", axis.title = element_blank() ),
	p2 + theme( legend.position = "none", axis.title = element_blank(), axis.text.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank() ),
	p3 + theme( legend.position = "none", axis.title = element_blank() ),
	p4 + theme( legend.position = "none", axis.title = element_blank(), axis.text.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank() ),
	p5 + theme( legend.position = "none", axis.title = element_blank() ),
	p6 + theme( legend.position = "none", axis.title = element_blank(), axis.text.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank() ),

	align = 'vh',
	labels = c( "A", "B", "C", "D", "E", "F" ),
	label_size = 14,
	hjust = -1,
	nrow = 3
)

# save
save_plot( paste0( outdir, "/Suppl.Figure_2.png" ), pcombined, base_height = NULL, base_width = 6, base_asp = 0.5 )


######################################
## III. Iterate over JIF 3, 5 and 10
######################################
for( jif in c( 'impact_factor_cat3', 'impact_factor_cat5', 'impact_factor_cat10' ) )
{
	# Message 5
	p1 <- plot.facet( 'cont', all, jif, 'Impact factor', 'RoB_allocation_prob', 'Allocation bias', outdir )
	p2 <- plot.facet( 'cont', all, jif, 'Impact factor', 'RoB_random_prob', 'Randomization bias', outdir )
	p3 <- plot.facet( 'cont', all, jif, 'Impact factor', 'RoB_blinding_pts_prob', 'Blinding of people bias', outdir )
	p4 <- plot.facet( 'cont', all, jif, 'Impact factor', 'RoB_blinding_outcome_prob', 'Blinding of outcome bias', outdir )
	p5 <- plot.facet( 'bin', all, jif, 'Impact factor', 'trial_registered', 'In database register (%)', outdir )
	p6 <- plot.facet( 'bin', all, jif, 'Impact factor', 'consort_mentioned', 'With CONSORT statement (%)', outdir )

	# arrange the three plots in a single row
	pcombined <- plot_grid(

		p1 + theme( legend.position = "none", axis.title = element_blank() ),
		p2 + theme( legend.position = "none", axis.title = element_blank(), axis.text.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank() ),
		p3 + theme( legend.position = "none", axis.title = element_blank() ),
		p4 + theme( legend.position = "none", axis.title = element_blank(), axis.text.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank() ),
		p5 + theme( legend.position = "none", axis.title = element_blank() ),
		p6 + theme( legend.position = "none", axis.title = element_blank(), axis.text.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank() ),

		align = 'vh',
		labels = c( "A", "B", "C", "D", "E", "F" ),
		label_size = 14,
		hjust = -1,
		nrow = 3
	)

	# extract a legend that is laid out horizontally
	legend_b <- get_legend( p1 + guides( color = guide_legend( nrow = 1 ) ) + theme( legend.position = "top" ) )

	# add the legend underneath the row we made earlier. Give it 10% of the height of one plot (via rel_heights).
	p <- plot_grid( pcombined, legend_b, ncol = 1, rel_heights = c(1, .05))

	# save
	save_plot( paste0( outdir, "/Suppl.Figure__", jif, ".png" ), p, base_height = NULL, base_width = 7, base_asp = 0.6 )
}


