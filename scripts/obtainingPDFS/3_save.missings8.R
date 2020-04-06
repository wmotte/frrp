################################################################################
# Aim 1: Download 'OVID' full text pdfs, given PMID
# Aim 2: Download PMC full text pdfs, given PMID
# 
# Wim Otte (w.m.otte@umcutrecht.nl)
# 6 April 2017
#############################

setwd( "/mnt/cortexraid/hlamberink" ) 
.libPaths( c(.libPaths(), "/mnt/cortexraid/hlamberink/RLibrary" ) ) 

library( 'xml2' ) # used by rvest package
library( 'rvest' ) # web scraping package
library( "curl" )
library( "XML" )
library( "pbapply" ) # power bar during sapply

###################################
# FUNCTIONS
###################################


###
# get data of missing pmids
##
get.missings <- function( pmid, url, outdr = outdir )
{
    # create link to check
    outpdf <- paste0( outdr, '/', pmid, '.pdf' )
    
    dataframe <- NA
    
    if( ! file.exists( outpdf ) )
    {
        dataframe <- data.frame( pmid = pmid,
                                 url = url,
                                 stringsAsFactors = FALSE )
    }
    
    return( dataframe )
}


###################################
# END FUNCTIONS
###################################

# output directory to store full text pdf
outdir <- 'pdf/pdfs'

# read data, list of pmids and urls
p <- read.csv2( "pdf/all_rcts.csv", stringsAsFactors = FALSE, header = FALSE, skip = 350000, nrows = 50000 )
names(p) <- c( "pmids", "url" )
print( length( p$pmids ) )

# exclude animal studies
animals <- read.csv2( "animalstudies_exclude.csv", stringsAsFactors = FALSE )
p <- p[ !( p$pmids %in% animals$pmids), ]
print( length( p$pmids ) )




# determine which pdfs are missing
missings <- NULL

for( i in 1:length( p$pmids ) )
{
  pmid <- p$pmids[i]
  url <- p$url[i]
  mis <- get.missings(pmid = pmid, url = url )
  
  if( !is.na( mis ) )
  {
    missings <- rbind( missings, mis )
  }
}
  
# write information on missing pdfs
write.csv2( missings, "missings_8.csv" )