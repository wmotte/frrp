#
# contact: h.j.lamberink@umcutrecht.nl
# date: 2018-03-19
#
# aim: analyze missing files and test re-download
#################################################################

.libPaths( c(.libPaths(), "/mnt/data/live02/stress/hlamberink/RLibrary" ) ) 



######################################
# FUNCTIONS
######################################


######################################
# END FUNCTIONS
######################################

# read data missing files
missings <- read.csv( "U:/data/pdf/missingpdfs.csv" )
missings <- data.frame( pmid = missings[,2])

# read original file with urls
urlfile <- read.csv2( "U:/data/pdf/oldData/all_rcts.csv", stringsAsFactors = F, header = F )
urlfile <- urlfile[ urlfile$V1 %in% missings$pmid,  ]

# read second files with urls (10 in total)
urlfileNew <- NULL
for( i in 1:10 )
{
  uNew <- read.csv2( paste0( "U:/data/pdf/oldData/all_rctsNA", i, ".csv"), stringsAsFactors = F, header = F )
  
  urlfileNew <- rbind( urlfileNew, uNew )
  rm( uNew )
}

# keep only files of which pdf is missing
urlfileNew <- urlfileNew[ urlfileNew$V1 %in% missings$pmid, ]

# make urlfile complete
for( i in 1:length(urlfile$V1))
{
  # skip all rows with existing URL
  if( !is.na( urlfile[ i, "V2" ] ) ) next()
  
  pmid <- urlfile[ i, "V1" ]
  urlfile[ i, "V2" ] <- urlfileNew[ urlfileNew$V1 == pmid, "V2" ]
  
  print(urlfile[i,])

}

print( length( urlfile[ !complete.cases(urlfile), 1 ] ) )

# write file of missings WITH url
write.csv2( urlfile, "U:/data/pdf/missingsWithURL.csv", row.names = F)
