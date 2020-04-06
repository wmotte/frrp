#
# contact: h.j.lamberink@umcutrecht.nl
# date: 2018-02-21
#
# aim:  (1) exclude also new excluded xmls (pilot and feasibility)
#       (2) select xml files that contain results section
#################################################################

.libPaths( c(.libPaths(), "/mnt/data/live02/stress/hlamberink/RLibrary" ) ) 

library( 'XML' )
library( 'plyr' ); library( 'dplyr' )

######################################
# FUNCTIONS
######################################

select_xmldata <- function( xmlfile ) 
{
  caughtTry <- tryCatch(
    {
      # parse xml file
      doc <- xmlParse( xmlfile )
 
      # store rootnodes
      top <- xmlRoot( doc )
      
      # gather document headers
      body <- top[["text"]][["body"]]
      divs <- body[ names(body) == "div" ]
      heads <- lapply( divs, function(x) x[["head"]] )
      
      # does a header contain "result"
      hh <- paste( lapply(heads, xmlValue) )
      if( sum( grepl( "[rR]esult", hh ) | 
               grepl( "RESULT", hh ) | 
               grepl( "analysis", hh ) |
               grepl( "[s]ubject", hh ) ) == 0 ){ stop( "does not contain results" ) }
      
      keep <- data.frame( remove = "keep",
                           file = filename,
                           errormessage = NA,
                           stringsAsFactors = FALSE )
      
      return( keep )
    },
    
    error=function(err) {
      #message(paste("URL does not seem to exist:", url))
      #message("Here's the original error message:")
      #message(err)
      # Choose a return value in case of error
      ch <- as.character(err)
      errdf <- data.frame( remove = "remove", 
                           file = filename, 
                           errormessage = ch,
                           stringsAsFactors = FALSE )
      return( errdf )
    },
    
    warning=function(war) {
      #message(paste("URL caused a warning:", url))
      #message("Here's the original warning message: ")
      #message(paste( war, "\n" ) )
      # Choose a return value in case of warning
      #return( paste( pmid, "warning, test if downloaded" ) )
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

move.files <- function( data, outdir )
{
  
  for( i in 1:nrow( data ) )
  {
    file <- data[ i, ]
    filename <- file$file
    
    # move files that are indicated as 'notPDF'
    if( file$remove == "remove" )
    {
      # prepare bash command
      cmnd <- paste0( "mv ", filename, " ", outdir, file )
      
      # move files
      system( cmnd, intern = FALSE )
    }
  }
}



######################################
# END FUNCTIONS
######################################

######### Step 1 #########################################
### make list of pmids to exclude based on extra_filter.R
######### ###### #########################################

# read file names and extract pmids
filez <- list.files( "xmlout" )
pmids <- gsub( "\\.xml", "", filez )

# check which pmids should have been removed after extra_filter
dftotal <- NULL
for( i in 1:22 )
{
  df <- read.csv( paste0( "../csv/csvfinal/pmids_selected", i, ".csv" ), stringsAsFactors = F )
  dftotal <- rbind(dftotal,df)
}
pmidsIn <- pmids[ pmids %in% dftotal[ , 1 ] ]
'%ni%' <- Negate( '%in%' ); pmidsEx <- pmids[ pmids %ni% dftotal[ , 1 ] ]

######### Step 2 #########################################
### make list of files to exclude b/c no results section
######### ###### #########################################


# vehicle
remove <- NULL

# loop over filez
for( i in 1:length( filez ) )
{
  # select xml file
  f <- filez[ i ]
  filename <- paste0( "xmlout/", f )
  xmlfile <- readLines( filename )
  
  # determine: remove or keep?
  removables <- select_xmldata( xmlfile )
  
  # bind
  remove <- rbind( remove, removables )
}

print( table( remove$remove ) )
head(remove )


######### Step 3 #########################################
### combine lists from .1 and .2, and move files to 'xmlExcluded'
######### ###### #########################################

# change 'keep' into 'remove' for all files in pmidsEx
remove$pmid <- gsub( "xmlout/", "", remove$file )
remove$pmid <- gsub( "\\.xml", "", remove$pmid )
remove[ remove$pmid %in% pmidsEx, "remove" ] <- "remove"
remove[ remove$pmid %in% pmidsEx, "errormessage" ] <- "extra_filter"

print( table( remove$remove ) )
head(remove )


# write output
write.csv2( remove[ remove$remove == "keep", ], "xmlsToKeep.csv", row.names = FALSE )
write.csv2( remove[ remove$remove == "remove", ], "xmlsToRemove.csv", row.names = FALSE )


# move new files to different folder
# in below command something goes wrong, files are not moved to their full name but to the file 'remove', which is then overwritten by the next file
#move.files( data = remove, outdir = "xmlExcluded/" ) 



# quit R session
q( save = "no" )