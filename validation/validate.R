#!/usr/bin/env Rscript
# Determine correspondance between RR and human ratings
#######################################################
library( 'plyr' )
library( 'ggplot2' )
library( 'caret' )
library( 'cowplot' )

#######################################################
# FUNCTIONS
#######################################################

###
# ggplot style function.
##
number_ticks <- function( n ) { function( limits ) pretty( limits, n ) }

## get data
get_data <- function()
{
	# read combined data
	df <- read.csv( 'data/data.csv.gz', row.names = 1 )
	
	df$judgement_binary <- 'Low_risk'
	df[ df$judgement != 'Low risk', 'judgement_binary' ] <- 'High-Unclear_risk'
	
	return( df )
}

###
# Process RoB domain
##
process <- function( df, tag, cl, title )
{
	# select tag
	sub <- df[ df$bias %in% tag, ]	

	# select col
	sub$cl <- sub[ , cl ]
	
	# remove NA
	sub <- sub[ !is.na( sub$cl ), ]
	
	# summary data
	sum <- ddply( sub, c( "judgement_binary" ), summarise, median = median( cl ), K = length( cl ) )
	sum$N <- nrow( sub )
	sum$domain <- tag
	
	# generate binary RoB outcome
	sub$class1 <- 'Low_risk'
	sub$class1[ sub$cl > 0.5 ] <- 'High-Unclear_risk'
	
	# confusion matrix data
	acc <- caret::confusionMatrix( as.factor( sub$judgement_binary ), as.factor( sub$class1 ) )

	acc1 <- round( 100 * acc$overall[ c( 'Kappa', 'Accuracy', 'AccuracyLower', 'AccuracyUpper' ) ], 1 )
	acc2 <- round( 100 * acc$byClass[ c( 'Sensitivity', 'Specificity' ) ], 1 )
	
	sum$acc <- paste0( acc1[ 'Accuracy' ], " (CI: ", acc1[ 'AccuracyLower' ], '–', acc1[ 'AccuracyUpper' ], ")" )
	sum$sens <- acc2[ 'Sensitivity' ]
	sum$spec <- acc2[ 'Specificity' ]
	sum$kappa <- acc1[ 'Kappa' ]
	
	# set factor
	sub$judgement_binary <- as.factor( sub$judgement_binary )
	levels( sub$judgement_binary ) <- c( 'High-Unclear', 'Low' )
	
	# plot
	fillcolour <- '#31a354' 
	p <- ggplot( data = sub, aes( x = judgement_binary, y = cl ) ) + 
		geom_violin( fill = fillcolour ) +
		geom_hline( yintercept = 0.5, colour = 'gray30', linetype = "dashed" ) +
		scale_y_continuous( breaks = number_ticks( 6 ), limits = c( 0, 0.95 ) ) +
		ylab( "Predicted risk" ) +
		xlab( title ) +
		theme_classic( base_size = 10 ) +
		theme( legend.position = 'none', axis.title = element_text( face = "bold" ), axis.text.x = element_text( angle = 0, hjust = 0.5 ) )
	
	out <- list( text = sum, plot = p )
	
	return( out )
}

#######################################################
# END FUNCTIONS
#######################################################

# generate output dir
outdir <- 'out.validate'
dir.create( outdir, showWarnings = FALSE )

# read data
df <- get_data()

# 17,394
uniq_pmids <- length( unique( df$pmid ) )
write.csv( uniq_pmids, file = paste0( outdir, '/uniq_pmids.csv' ) )

# get accuracy numbers
m_allocation <- process( df, tag = "2. Allocation concealment?", cl = "RoB_allocation_prob", title = "Allocation bias" )
m_random <- process( df, tag = "1. Random sequence generation?", cl = "RoB_random_prob", title = "Randomization bias" )
m_blinding_pts <- process( df, tag = "3. Blinding of participants and personnel?", cl = "RoB_blinding_pts_prob", title = "Blinding of people bias" )
m_blinding_outcome <- process( df, tag = "4. Blinding of outcome assessment?", cl = "RoB_blinding_outcome_prob", title = "Blinding of outcome bias" )

# combine
all <- rbind( rbind( rbind( m_allocation$text, m_random$text ), m_blinding_pts$text ), m_blinding_outcome$text )

# write to disk
write.csv( all, file = paste0( outdir, '/accuracies.csv' ) )

# total 41,358
N <- sum( all$K )
write.csv( N, file = paste0( outdir, '/N.csv' ) )

# arrange the three plots in a single row
p <- plot_grid(
	m_random$plot + theme( legend.position = "none", axis.title = element_blank() ) + 
		annotate("text", -Inf, Inf, label = "  Randomization bias", hjust = 0, vjust = 2, size = 3, fontface = 2, colour = 'gray40' ),
	m_allocation$plot + theme( legend.position = "none", axis.title = element_blank() ) + 
		annotate("text", -Inf, Inf, label = "  Allocation bias", hjust = 0, vjust = 2, size = 3, fontface = 2, colour = 'gray40' ),
	m_blinding_pts$plot + theme( legend.position = "none", axis.title = element_blank() ) + 
		annotate("text", -Inf, Inf, label = "  Blinding of people bias", hjust = 0, vjust = 2, size = 3, fontface = 2, colour = 'gray40' ),
	m_blinding_outcome$plot + theme( legend.position = "none", axis.title = element_blank() ) + 
		annotate("text", -Inf, Inf, label = "  Blinding of outcome bias", hjust = 0, vjust = 2, size = 3, fontface = 2, colour = 'gray40' ),
	
	align = 'vh',
	labels = c( "A", "B", "C", "D" ),
	label_size = 10,
	hjust = -1,
	vjust = 2,
	nrow = 2,
	ncol = 2
)

# save to disk
save_plot( paste0( outdir, "/Figure_validation.png" ), p, base_height = NULL, base_width = 6, base_asp = 0.8 ) 

