
################################################################################
#
## aim: get missing urls
## date: 2017-11-30
## herm.lamberink@gmail.com
#
################################################################################

setwd( "/mnt/cortexraid/hlamberink" ) 
.libPaths( c(.libPaths(), "/mnt/cortexraid/hlamberink/RLibrary" ) ) 

## load required libraries
library( 'rentrez' )
library( "curl" )
library( 'xml2' ) # used by rvest package
library( 'rvest' ) # web scraping package
library( 'XML' )


################################################################################
# BEGIN FUNCTIONS								#
################################################################################


###
# Convert pmid to specific url of the publisher, from which the pdf will be downloaded
#
####
pmid.to.full.text.url <- function( pmid )
{ 
  # prevent overloading of the system by inserting a small break after every entry
  Sys.sleep( 0.5 )
  
  # prevent fatal error when one of the steps fails using tryCatch
  # --> insertion of error messages instead of stopping the system
  out <- tryCatch( 
    { 
      # Just to highlight: if you want to use more than one 
      # R expression in the "try" part then you'll have to 
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression 
      # in case the "try" part was completed successfully
      
      # url that links to page of provider 
      # documentation: https://www.ncbi.nlm.nih.gov/books/NBK25499/
      url <- paste0( 'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&id=', pmid, '&cmd=prlinks' )
      
      # get html and fake Mozilla as download client (to prevent non-closed connections)
      #html_url <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Mozilla/5.0" ) ) )
      xml_url <- xml2::read_xml( curl( url, handle = curl::new_handle( "useragent" = "Mozilla/5.0" ) ) )
      
      # parse from list into xml document
      xmlfull <- xmlParse( xml_url )
      rootNode <- xmlRoot(xmlfull)
      
      # extract url from xml
      out <- as.character( xpathSApply( rootNode, "//ObjUrl/Url", xmlValue ) )
      if( length( out ) > 1 ) out <- out[1]
      if( out == "character(0)" ) out <- NA
      
      return( out )
      #  on.exit( close(out), add=FALSE )
    },
    error=function(err) {
      #message(paste("URL does not seem to exist:", url))
      #message("Here's the original error message:")
      #message(err)
      # Choose a return value in case of error
      out <- NA
      return(out)
    },
    warning=function(war) {
      message(paste("URL caused a warning:", url))
      message("Here's the original warning message: ")
      message(paste( war, "\n" ) )
      # Choose a return value in case of warning
      return(out)
    },
    finally={
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you 
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>' 
      #message(paste("Processed URL:", url))
      #message("Some other message at the end")
      
    }
  )   
  return(out)
}


################################################################################
# END FUNCTIONS									#
################################################################################

# pdf dir
outdir <- "pdf"
dir.create( outdir, showWarnings = FALSE )

# read data
p <- read.csv2( "url_NA.csv", stringsAsFactors = FALSE, skip = 120000, nrows = 20000,
                col.names = c( "set", "row", "pmid", "urlbase", "urltail" ) )

# set progress bar
progbar <- txtProgressBar( min = 0, max = length( p$pmid ), style = 3 )

# print starting time
starttime <- Sys.time(); print( paste( "starting time:", starttime ) )

# set starting number (start where previous system ended)
startnr <- 1

if( file.exists( paste0( outdir, '/all_rctsNA7.csv' ) ) )
{
  a <- read.csv2(paste0( outdir, '/all_rctsNA7.csv' ) )
  startnr <- length( a[,1] ) + 2
}

# for every pmid, add url
for( i in startnr:length( p$pmid ) )
{
  setTxtProgressBar( progbar, i )
  
  # add url
  pp <- data.frame( pmid = p$pmid[ i ], stringsAsFactors = FALSE )
  pp$url <- pmid.to.full.text.url( pp$pmid )
  
  # write to file
  write.table( pp, file = paste0( outdir, '/all_rctsNA7.csv' ), 
               sep = ";", row.names = FALSE, col.names = FALSE, append = TRUE )
  
  # print elapsed time
  if( i == length( p$pmid ) ) 
  {
    timez <- Sys.time()-starttime
    print( paste0( "elapsed time: " ) )
    print( timez )
  }
}

