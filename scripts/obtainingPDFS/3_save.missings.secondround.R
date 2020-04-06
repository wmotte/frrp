################################################################################
# Aim 1: Download 'OVID' full text pdfs, given PMID
# Aim 2: Download PMC full text pdfs, given PMID
# 
# Wim Otte (w.m.otte@umcutrecht.nl)
# 6 April 2017
#############################

setwd( "/mnt/cortexraid/hlamberink" ) 
.libPaths( c(.libPaths(), "/mnt/cortexraid/hlamberink/RLibrary" ) ) 



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
p <- read.csv2( "pdf/all_rcts.csv", stringsAsFactors = FALSE, header = FALSE )
names(p) <- c( "pmids", "url" )
print( length( p$pmids ) )

# exclude animal studies
animals <- read.csv2( "pdf/animalstudies_exclude.csv", stringsAsFactors = FALSE )
p <- p[ !( p$pmids %in% animals$pmids), ]
print( length( p$pmids ) )


###################################
# first add new URLs for linkinghub
###################################
newurl1 <- read.csv2( "pdf/all_rctsNA1.csv", stringsAsFactors = FALSE, header = FALSE )
newurl2 <- read.csv2( "pdf/all_rctsNA2.csv", stringsAsFactors = FALSE, header = FALSE )
newurl3 <- read.csv2( "pdf/all_rctsNA3.csv", stringsAsFactors = FALSE, header = FALSE )
newurl4 <- read.csv2( "pdf/all_rctsNA4.csv", stringsAsFactors = FALSE, header = FALSE )
newurl5 <- read.csv2( "pdf/all_rctsNA5.csv", stringsAsFactors = FALSE, header = FALSE )
newurl6 <- read.csv2( "pdf/all_rctsNA6.csv", stringsAsFactors = FALSE, header = FALSE )
newurl7 <- read.csv2( "pdf/all_rctsNA7.csv", stringsAsFactors = FALSE, header = FALSE )
newurl8 <- read.csv2( "pdf/all_rctsNA8.csv", stringsAsFactors = FALSE, header = FALSE )
newurl9 <- read.csv2( "pdf/all_rctsNA9.csv", stringsAsFactors = FALSE, header = FALSE )
newurl10 <- read.csv2( "pdf/all_rctsNA10.csv", stringsAsFactors = FALSE, header = FALSE )

newurl <- rbind(newurl1, newurl2, newurl3, newurl4, newurl5, newurl6, newurl7, newurl8, newurl9, newurl10)
names(newurl) <- c( "pmids", "url" )
print( length( newurl$pmids ) )
print( "length should be 194341" )

pnew <- merge(p, newurl, by = "pmids", all = TRUE)
print( length( pnew$pmids ) == length( p$pmids ) )

pnew[ is.na(pnew$url.x), "url.x"] <- pnew[is.na(pnew$url.x), "url.y"]
pnew$url.y <- NULL
names(pnew) <- c("pmids", "url")


write.csv2(pnew, "pdf/all_rcts_newurls.csv", row.names = FALSE)
  
###################################
# determine which pdfs are missing
###################################
missings <- NULL

for( i in 1:length( pnew$pmids ) )
{
  pmid <- pnew$pmids[i]
  url <- pnew$url[i]
  mis <- get.missings(pmid = pmid, url = url )
  
  if( !is.na( mis ) )
  {
    missings <- rbind( missings, mis )
  }
}
  
# write information on missing pdfs
write.csv2( missings, "pdf/missings_secondround.csv" )