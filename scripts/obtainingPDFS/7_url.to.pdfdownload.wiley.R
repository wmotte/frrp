
################################################################################
# Aim: Download full text pdfs, given PMID
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


###################################
# FUNCTIONS
###################################



###
# Get pdf from given pmid
##
get.pdf <- function( pmid, outdr = outdir, df = pp  )
{
  # prevent the function from shutting down due to an error
  v <- tryCatch(
    {
      # output pdf
      outpdf <- paste0( outdr, '/', pmid, '.pdf' )
      
      if( !file.exists( outpdf ) )
      {
        # get proper link to full text page
        url <- df[ df$pmid == pmid, "url" ]
        
        # set empty pdflink
        pdflink <- NA
        
        #######################
        # pdflink per publisher
        #######################
        pdflink <- get.pdflink.wiley( url )


        #######################
        # downoad pdf
        #######################
        
        # write pdf to output if link is available
        if( !is.na( pdflink ) )
        {
          # download pdf (only if output is yet downloaded)
          download.file( url = pdflink, destfile = outpdf, 
                         mode = "wb", quiet = TRUE )
        }
      }
      return( NA )
    },
    
    error=function(err) {
      #message(paste("URL does not seem to exist:", url))
      #message("Here's the original error message:")
      #message(err)
      # Choose a return value in case of error
      return( paste( pmid, "URL does not seem to exist" ) )
    },
    
    warning=function(war) {
      #message(paste("URL caused a warning:", url))
      #message("Here's the original warning message: ")
      #message(paste( war, "\n" ) )
      # Choose a return value in case of warning
      return( paste( pmid, "warning, test if downloaded" ) )
    }
    
    #finally={
    # NOTE:
    # Here goes everything that should be executed at the end,
    # regardless of success or error.
    # If you want more than one expression to be executed, then you 
    # need to wrap them in curly brackets ({...}); otherwise you could
    # just have written 'finally=<expression>' 
    #message(paste("Processed URL:", url))
    #message("Some other message at the end")
    #}
  )
} 



###
# Get full text pdf link from wiley full text website.
##
get.pdflink.wiley <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf
  css <- 'meta[name="citation_pdf_url"]'
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  return( pdflink ) 
}  





################################################################################
# END FUNCTIONS									#
################################################################################

# outdir
outdir <- "pdf/pdfs"

# read data
wiley <- read.csv2( "missingwiley.csv", stringsAsFactors = FALSE )

# download pdfs
# set progress bar
progbar <- txtProgressBar( min = 0, max = length( wiley$pmid ), style = 3 )

# print starting time
starttime <- Sys.time(); print( paste( "starting time:", starttime ) )


# for every pmid, add url
for( i in 1:length( wiley$pmid ) )
{
  setTxtProgressBar( progbar, i )
  
  # add url
  pp <- data.frame( pmid = wiley$pmid[ i ],
                    url = wiley$urlfull[ i ],
                    stringsAsFactors = FALSE )
  get.pdf( pp$pmid )
  
  
  # print elapsed time
  if( i == length( wiley$pmid ) ) 
  {
    timez <- Sys.time()-starttime
    print( paste0( "elapsed time: " ) )
    print( timez )
  }
}
