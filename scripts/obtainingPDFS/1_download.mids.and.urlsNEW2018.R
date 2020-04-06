#################################################################
#
# contact: h.j.lamberink@umcutrecht.nl
# date: 19-03-2018
#
# aim: download full text pdf from all clinical trials available
#################################################################

.libPaths( c(.libPaths(), "/mnt/data/live02/stress/hlamberink/RLibrary" ) ) 

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
get.new.rct.pmids <- function( pmidsPrevious )
{
  # search term
	sterm1 <- "Randomized controlled trial[pt] NOT (animals[mh] NOT humans[mh])"
	sterm2 <- "Randomized controlled trial[pt] NOT (animals[mh] NOT humans[mh]) AND 2017/11/17:2018/08[DP]"
  
	# 435,000 papers (all clinical trials in PubMed) \\ will only store the first 20 ids (retmax = 20)
	rct_search1 <- entrez_search( db = "pubmed", term = sterm1 )
	rct_search2 <- entrez_search( db = "pubmed", term = sterm2 )
	print( rct_search1$count )
	print( rct_search2$count )
	
	# re-run with retmax set to all
	rct_search1 <- entrez_search( db = "pubmed", term = sterm1, retmax = rct_search1$count )
	rct_search2 <- entrez_search( db = "pubmed", term = sterm2, retmax = rct_search2$count )

	# store new pmids
	pmidsTotal <- rct_search1$ids
	pmidsNEW <- rct_search2$ids
	
	# select pmids that were not already in file on 2017/11/17
	'%ni%' <- Negate( '%in%' )
	pmidsNEW <- data.frame(
	  pmid = pmidsTotal[ pmidsTotal %ni% pmidsPrevious[ , 1 ] ] 
	  )

	return( pmidsNEW )
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
      
      if( length( out ) == 0 ) out <- NA
      
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
outdir <- "pdfNEW"
dir.create( outdir, showWarnings = FALSE )

# read pmids already in files
pmidsPrevious <- NULL
for( i in 1:22 )
{
  pm <- read.csv2( paste0( "../csv/csvout/pubmeddata_limited", i, ".csv" ) )
  pm <- data.frame( pmid = pm[ , 1 ] )
  pmidsPrevious <- rbind( pmidsPrevious, pm )
}


# get new pmids 
#p <- get.new.rct.pmids( pmidsPrevious )


# get pmids for full text RCTs
#write.csv2( p, file = paste0( outdir, '/new_rcts_pmidsonly_2017-03-19.csv' ), row.names = F )
p <- read.csv2( file = paste0( outdir, '/new_rcts_pmidsonly_2017-03-19.csv' ) )

# set progress bar
progbar <- txtProgressBar( min = 0, max = length( p$pmid ), style = 3 )
 
  
# set starting number (start where previous system ended)
startnr <- 1
  
if( file.exists( paste0( outdir, '/new_rcts.csv' ) ) )
{
  a <- read.csv2(paste0( outdir, '/new_rcts.csv' ) )
  startnr <- length( a[,1] ) + 2
}

print( startnr )

  
# for every pmid, add url
for( i in startnr:length( p$pmid ) )
  {
  setTxtProgressBar( progbar, i )
  
  # add url
  pp <- data.frame( pmid = p$pmid[ i ], stringsAsFactors = FALSE )
  pp$url <- pmid.to.full.text.url( pp$pmid )
  
  # write to file
  write.table( pp, file = paste0( outdir, '/new_rcts.csv' ), 
             sep = ";", row.names = FALSE, col.names = FALSE, append = TRUE )
  

}

# quit R session
q( save = "no" )