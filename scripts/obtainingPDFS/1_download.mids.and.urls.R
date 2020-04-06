#!/home1/wim/R-3.2.3/bin/Rscript --no-save --no-restore
#
# contact: w.m.otte@umcutrecht.nl
# date: 01-04-2017
#
# aim: download full text pdf from all clinical trials available
#################################################################

setwd( "/mnt/cortexraid/hlamberink" ) #"O:/psychiatrie/predictie verantwoordonderzoek/4.download_pdf" )
.libPaths( c(.libPaths(), "/mnt/cortexraid/hlamberink/RLibrary" ) )        #, "O:/kinderneuro/240AED withdrawal/R-libraries/V3") )

library( 'rentrez' )
library( 'pbapply' )  # adding progress bar to *apply function
library( 'xml2' ) # used by rvest package
library( 'rvest' ) # web scraping package
library( "curl" )
library( 'XML' )


######################################
# FUNCTIONS
######################################

###
# Return pubmed ids for RCTs with Pubmed Central links to full text pfs.
##
get.all.rct.pmids <- function()
{
  	# search term
	sterm <- "Randomized controlled trial[pt] NOT (randomized controlled trial[pt] NOT (animals[mh] NOT humans[mh]) )"
  
	# 435,000 papers (all clinical trials in PubMed) \\ will only store the first 20 ids (retmax = 20)
	rct_search <- entrez_search( db = "pubmed", term = sterm )
	print( rct_search$count )
	
	# re-run with retmax set to all
	rct_search <- entrez_search( db = "pubmed", term = sterm, retmax = rct_search$count )

	# 63,000 papers (all clinical trials in PubMed with open access text)
	# rct_search_pmc <- entrez_search( db = "pubmed", term = "all[sb] and \"Randomized Controlled Trial\"[pt] and \"pubmed pmc\"[Filter]" )
	# print( rct_search_pmc$count )

	# store pmids
	pmids <- rct_search$ids

	return( pmids )
}



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
      # [ e.g. "http://www.sciencedirect.com/science/article/pii/S1474442215002550?np=y" ]
      out <- as.character( xpathSApply( rootNode, "//ObjUrl/Url", xmlValue ) )
      if( length( out ) > 1 ) out <- out[1]
      
      
      return( out )
      on.exit( close(out), add=FALSE )
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




######################################
# END FUNCTIONS
######################################

# pdf dir
outdir <- "pdf"
dir.create( outdir, showWarnings = FALSE )

# get pmids for full text RCTs
#pmids <- as.character( get.all.rct.pmids() )
#p <- as.data.frame( pmids, stringsAsFactors = FALSE )
#write.csv2( p, file = paste0( outdir, '/all_rcts_pmidsonly_2017-11-17.csv' ), row.names = FALSE )
p <- read.csv2( file = paste0( outdir, '/all_rcts_pmidsonly_2017-11-17.csv' ), stringsAsFactors = FALSE )


# set progress bar
progbar <- txtProgressBar( min = 0, max = length( p$pmids ), style = 3 )
  
# print starting time
starttime <- Sys.time(); print( paste( "starting time:", starttime ) )
  
# set starting number (start where previous system ended)
startnr <- 1
  
if( file.exists( paste0( outdir, '/all_rcts.csv' ) ) )
{
  a <- read.csv2(paste0( outdir, '/all_rcts.csv' ) )
  startnr <- length( a[,1] ) + 2
}
  
# for every pmid, add url
for( i in startnr:length( p$pmids ) )
  {
  setTxtProgressBar( progbar, i )
  
  # add url
  pp <- data.frame( pmids = p$pmids[ i ], stringsAsFactors = FALSE )
  pp$url <- pmid.to.full.text.url( pp$pmids )
  
  # write to file
  write.table( pp, file = paste0( outdir, '/all_rcts.csv' ), 
             sep = ";", row.names = FALSE, col.names = FALSE, append = TRUE )
  
  # print elapsed time
  if( i == length( p$pmids ) ) 
    {
    timez <- Sys.time()-starttime
    print( paste0( "elapsed time: " ) )
    print( timez )
  }
}
