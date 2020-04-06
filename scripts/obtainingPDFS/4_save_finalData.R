################################################################################
# Aim: gather all csv data from pubmed into single files
# 
# Contact: h.j.lamberink@umcutrecht.nl
# Date: March 23, 2018
################################################################################

# outdir
outdir <- "data"

# create vehicles
dfTiab <- dfData <- dfPmids <- NULL

# loop over 23 files for each
for( i in 1:23 )
{
  # read data
  tiab <- read.csv2( paste0( "csvfinal/tiabselect", i, ".csv" ), stringsAsFactors = FALSE )
  data <- read.csv2( paste0( "csvfinal/dataselect", i, ".csv" ), stringsAsFactors = FALSE )
  pmids <- read.csv2( paste0( "csvfinal/pmids_selected", i, ".csv" ), stringsAsFactors = FALSE )
  
  # bind data
  dfTiab <- rbind( dfTiab, tiab )
  dfData <- rbind( dfData, data )
  dfPmids <- rbind( dfPmids, pmids )
}



dim(dfTiab)
dim(dfData)
dim(dfPmids)

# create some extra subfiles (smaller)
authors <- dfData[ , c("pmid", "authors", "firstnames", "affiliations") ]
journals <- dfData[ , c( "pmid", "journal", "jcountry", "yearpub" ) ]

# save data in outdir
write.csv( dfTiab, paste0( outdir, "/tiabtotal.csv" ), row.names = F )
write.csv( dfData, paste0( outdir, "/datatotal.csv" ), row.names = F )
write.csv( dfPmids, paste0( outdir, "/pmidstotal.csv" ), row.names = F )
write.csv2( authors, paste0( outdir, "/authors.csv" ), row.names = F )
write.csv2( journals, paste0( outdir, "/journals.csv" ), row.names = F )


# quit R session
q( save = "no" )