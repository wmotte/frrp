#!/usr/bin/env Rscript
#
# Stacked plots.
# w.m.otte@umcutrecht.nl
#
#########################

library( 'ggplot2' )
library( 'plyr' )
library( 'Hmisc' )
library( "reshape2" )

###################################################
# FUNCTIONS
###################################################

###
# ggplot style function.
##
number_ticks <- function( n ) { function( limits ) pretty( limits, n ) }

###################################################
# END FUNCTIONS
###################################################

# make output directory
outdir <- 'out.med.disciplines'
dir.create( outdir, showWarnings = FALSE )

# read prepared data
infile <- 'data/data.csv'  

# get data
all <- read.csv( infile, row.names = 1 )

###########################################################################################################################  

# only keep last 10 years
df <- all
df <- df[ df$year_group > 2007, ]
df$time <- '2005-2018'

df$disciplines <- gsub( "_", " & ", df$disciplines )
df$group <- Hmisc::capitalize( as.character( df$disciplines ) )
df <- df[ ! is.na( df$group ), ]
df <- df[ , colnames( df ) %in% c( "time", "group", "RoB_random_prob", "RoB_allocation_prob", "RoB_blinding_pts_prob", "RoB_blinding_outcome_prob" ) ]

# wide to long
df_long <- reshape2::melt( df, id = c( "time", "group" ) ) 
df_long$time <- NULL

###
# Get proportional CI.
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

# dichotomize into low and unclear-high bias
df_long$bias <- factor( ifelse( df_long$value < 0.5, "Low", "Unclear-High" ) )

# data for table
tdf <- ddply( df_long, .( group, variable ), summarise, 
			n_studies = length( bias ),
			n_studies_low_bias = sum( bias == 'Low', na.rm = TRUE ),
			perc_low_bias = round( 100 * sum( bias == 'Low', na.rm = TRUE ) / length( bias ), 1 ),							
			lb = round( 100 * get.prop.ci( k = sum( bias == 'Low', na.rm = TRUE ), n = length( bias ), lower = TRUE ), 1 ),
			ub = round( 100 * get.prop.ci( k = sum( bias == 'Low', na.rm = TRUE ), n = length( bias ), lower = FALSE ), 1 ) )
levels( tdf$variable ) <- c( "random", "allocation", "blinding of people", "blinding outcome" )

# parsing summary
tdf$cis <- paste0( tdf$perc_low_bias, "% (", tdf$lb, '-', tdf$ub, "%)" )

# write table
write.csv( tdf, file = 'Suppl.Table.csv', quote = TRUE )


# get mean
ds <- ddply( df_long, .( group, variable ), summarise, mean = 100 * mean( value, na.rm = T ) )
levels( ds$variable ) <- c( "random", "allocation", "blinding of people", "blinding outcome" )
ds_sum <- ddply( ds, .( group ), summarise, sum = sum( mean, na.rm = T ) )
comb <- merge( ds, ds_sum )

# Stacked bias for medical disciplines
p <- ggplot( data = comb, aes( x = reorder( group, sum ), y = mean, group = variable, fill = variable ) ) + geom_col( colour = 'gray20' ) +
			scale_y_continuous( breaks = number_ticks( 12 ) ) +
			scale_fill_manual( values = c( "#f4a582", "#92c5de", "#0571b0", '#E69F00', '#d95f0e' ) ) +
			ylab( "Risk (%)" ) +
			xlab( "Medical discipline" ) +
			theme_classic( base_size = 14 ) +
			theme( legend.position = 'top', axis.title = element_text( face = "bold" ), axis.text.x = element_text( angle = 65, hjust = 1 ) ) + labs( fill = "Bias" ) +
			geom_text( aes( label = round( mean, 0 ) ), size = 4, position = position_stack( vjust = 0.5 ) ) + 
			theme( axis.text.y = element_blank(), axis.ticks.y = element_blank() )

# write to file
ggsave( p, file = paste0( outdir, '/medical_disciplines__RoBs.png' ), dpi = 200, width = 7, height = 8 )
write.csv( comb, file = paste0( outdir, '/medical_disciplines__RoBs.csv' ) )


###########################################################################################################################  
# CONSORT / registration (stacked)
##################################

# only keep last 10 years
df <- all
df <- df[ df$year_group > 2007, ]
df$time <- '2005-2018'

df$disciplines <- gsub( "_", " & ", df$disciplines )
df$group <- Hmisc::capitalize( as.character( df$disciplines ) )
df <- df[ ! is.na( df$group ), ]
df <- df[ , colnames( df ) %in% c( "time", "group", "trial_registered", "consort_mentioned" ) ]

# wide to long
df_long <- reshape2::melt( df, id = c( "time", "group" ) ) 

# get means and 95% confidence interval - with linear regression
ds <- ddply( df_long, .( time, group, variable ), summarise, mean = 100 * sum( value, na.rm = TRUE ) / length( value ) )
 
levels( ds$variable ) <- c( "RCT registration", "CONSORT Statement" )

ds_sum <- ddply( ds, .( time, group ), summarise, sum = sum( mean, na.rm = T ) )
comb <- merge( ds, ds_sum )

# Stacked bias for medical disciplines"#f4a582", 
#p <- ggplot( data = comb, aes( x = reorder( group, 2 - sum ), y = mean, group = variable, fill = variable ) ) + geom_col( colour = 'gray20' ) +

p <- ggplot( data = comb, aes( x = reorder( group, 2 - sum ), y = mean, group = variable, fill = variable ) ) + geom_col( colour = 'gray20' ) +
			facet_wrap( ~variable, nrow = 2 ) +
			scale_y_continuous( breaks = number_ticks( 12 ) ) +
			scale_fill_manual( values = c( "#92c5de", "#0571b0", '#E69F00', '#d95f0e' ) ) +
			ylab( "Present (%)" ) +
			xlab( "Medical discipline" ) +
			theme_classic( base_size = 14 ) +
			theme( legend.position = 'none', axis.title = element_text( face = "bold" ), axis.text.x = element_text( angle = 65, hjust = 1 ) ) + labs( fill = "Data" ) +
			geom_text( aes( label = round( mean, 0 ) ), size = 4, position = position_stack( vjust = 0.5 ) ) 
			

#theme( axis.text.y = element_blank(), axis.ticks.y = element_blank() )

# write to file
ggsave( p, file = paste0( outdir, '/medical_disciplines__reg-consort.png' ), dpi = 200, width = 7, height = 8 )
write.csv( comb, file = paste0( outdir, '/medical_disciplines__reg-consort.csv' ) )

 

 
 

