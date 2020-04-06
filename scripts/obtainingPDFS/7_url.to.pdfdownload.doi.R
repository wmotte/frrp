
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

        # url contains 10.1038 (nature publishers)
        if( grepl( "doi.org/10.1038", url ) )
        {
          pdflink <- get.pdflink.nature( url )
        }
        
        # url conains 10.1089 (acm journal)
        if( grepl( "doi.org/10.1089", url ) )
        {
          pdflink <- get.pdflink.acm( url )
        }
        
        # url conains 10.1111 (acm journal)
        if( grepl( "doi.org/10.1111", url ) )
        {
          pdflink <- get.pdflink.wiley( url )
        }
        
        # url conains 10.1002 (acm journal)
        if( grepl( "doi.org/10.1002", url ) )
        {
          pdflink <- get.pdflink.wiley( url )
        }
        
        # all other urls
        if( !grepl( "doi.org/10.1038", url ) &
            !grepl( "doi.org/10.1089", url ) &
            !grepl( "doi.org/10.1111", url ) &
            !grepl( "doi.org/10.1002", url ) )
        {
          pdflink <- get.pdflink.other( url )
        }
        
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
# Get full text pdf link from nature full text website.
##
get.pdflink.nature <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf
  css <- 'meta[name="citation_pdf_url"]'
  
  # save pdflink
  pdflink <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  
  if( identical( pdflink, character(0) ) )
  {
    css <- 'a[class="inline-block block-link pa10 pl0"]'
    intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
    if( !identical( intermed1, character(0)))
    {
      pdflink <- paste0( "https://www.nature.com", intermed1[1] )
      return( pdflink )
    }
  }
}  
  
  
###
# Get full text pdf link from acm full text website.
##
get.pdflink.acm <- function( url )
{ 
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # css of pdf
  css <- '.pdfprint a'
  
  # save pdflink
  intermed <- page %>% html_nodes( css = css ) %>% html_attr( "href" )
  if( !identical( intermed, character(0) ) ) 
  {
    pdflink <- paste0( "http://online.liebertpub.com", intermed )
    return( pdflink ) 
  }
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


get.pdflink.sciencedirect <- function( url )
{
  # parse page
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  css <- 'input[name="redirectURL"]'
  intermed1 <- page %>% html_nodes( css = css ) %>% html_attr( "value" )
  intermed2 <- URLdecode(intermed1)
  
  page <- xml2::read_html( curl( intermed2, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # xpath of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css = 'meta[name="citation_pdf_url"]'
  
  # get href to pdfLink
  intermed3 <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  pdflink <- paste0( "https://www.sciencedirect.com", intermed3 )
  
  return( pdflink )
}


###
# Get full text pdf link from other full text website.
##
get.pdflink.other <- function( url )
{ 
  pdflink <- get.pdflink.acm( url )
  
  if( is.null( pdflink ) ) pdflink <- get.pdflink.nature( url )
  if( is.null( pdflink ) ) pdflink <- get.pdflink.wiley( url )
  if( identical( pdflink, character(0) ) ) pdflink <- get.pdflink.sciencedirect( url )
  
  return( pdflink ) 
}


################################################################################
# END FUNCTIONS									#
################################################################################

# outdir
outdir <- "pdf/pdfs"

# read data
doi <- read.csv2( "missingdoi.csv", stringsAsFactors = FALSE )

# download pdfs
# set progress bar
progbar <- txtProgressBar( min = 0, max = length( doi$pmid ), style = 3 )

# print starting time
starttime <- Sys.time(); print( paste( "starting time:", starttime ) )


# for every pmid, add url
for( i in 1:length( doi$pmid ) )
{
  setTxtProgressBar( progbar, i )
  
  # add url
  pp <- data.frame( pmid = doi$pmid[ i ],
                    url = doi$urlfull[ i ],
                    stringsAsFactors = FALSE )
  get.pdf( pp$pmid )
  
  
  # print elapsed time
  if( i == length( doi$pmid ) ) 
  {
    timez <- Sys.time()-starttime
    print( paste0( "elapsed time: " ) )
    print( timez )
  }
}
