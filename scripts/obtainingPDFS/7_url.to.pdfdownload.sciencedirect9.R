
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
        if( !is.na( url ) ) pdflink <- get.pdflink.linkinghub( url )

        #######################
        # downoad pdf
        #######################
        
        # write pdf to output if link is available
        if( !is.na( pdflink ) )
        {
          # download pdf (only if output is yet downloaded)
          download.file( url = pdflink1, destfile = outpdf, 
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




# for springerlink, retrieve the correct url
get.pdflink.linkinghub <- function( url )
{
  # parse url further and get the specific node with the URL
  page <- xml2::read_html( curl( url, handle = curl::new_handle( "useragent" = "Mozilla/5.0" ) ) )
  parsedfull <- htmlParse( page )
  rootnode <- xmlRoot( parsedfull )
  o <- getNodeSet( rootnode, "//input[@name='redirectURL']" )[[1]]
  
  # convert to character
  o2 <- capture.output(o)
  
  # extract URL from character string
  o3 <- data.frame( col = strsplit( o2, split = " " )[[1]] )
  o4 <- separate( o3, col = "col", into = c("a", "b"), sep = "=", fill = "right" )
  http <- o4[ o4$a == "value", "b" ]
  http <- gsub( "\"", "", http )
  outurl <- URLdecode(http)
  

  # parse page
  page <- xml2::read_html( curl( outurl, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  
  # xpath of pdf element selected with Selector Gadget Google Chrome plugin [http://selectorgadget.com/] 
  css = 'meta[name="citation_pdf_url"]'
  
  # get href to pdfLink
  intermed3 <- page %>% html_nodes( css = css ) %>% html_attr( "content" )
  intermed4 <- sub( "amp;", "", intermed3 )
  pdflink1 <- paste0( "https://www.sciencedirect.com", intermed4 )
  
  page2 <- xml2::read_html( curl( pdflink1, handle = curl::new_handle( "useragent" = "Chrome/55.0" ) ) )
  css2 = 'meta[content^="0;URL"]'
  
  intermed6 <- page2 %>% html_nodes( css = css2 ) %>% html_attr( "content" )
  pdflink <- strsplit(intermed6, "URL=")[[1]][2]
  
  return( pdflink )
}




################################################################################
# END FUNCTIONS									#
################################################################################

# outdir
outdir <- "pdf/pdfs"

# read data
sciencedirect <- read.csv2( "pdf/all_rctsNA9.csv", stringsAsFactors = FALSE, header = TRUE )
names( sciencedirect ) <- c( "pmid", "url" )

# download pdfs
# set progress bar
progbar <- txtProgressBar( min = 0, max = length( sciencedirect$pmid ), style = 3 )

# print starting time
starttime <- Sys.time(); print( paste( "starting time:", starttime ) )


# for every pmid, add url
for( i in 1:length( sciencedirect$pmid ) )
{
  setTxtProgressBar( progbar, i )
  
  # add url
  pp <- data.frame( pmid = sciencedirect$pmid[ i ],
                    url = sciencedirect$url[ i ],
                    stringsAsFactors = FALSE )
  get.pdf( pp$pmid )
  
  
  # print elapsed time
  if( i == length( sciencedirect$pmid ) ) 
  {
    timez <- Sys.time()-starttime
    print( paste0( "elapsed time: " ) )
    print( timez )
  }
}

