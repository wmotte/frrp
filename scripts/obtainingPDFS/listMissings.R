################################################################################
# aim: list files that could have been downloaded but have not been
# aim2: make sublist of all failed Elsevier downloads
# 
# contact: Herm Lamberink, h.j.lamberink@umcutrecht.nl
# date: March 22, 2018
################################################################################

.libPaths( c(.libPaths(), "/mnt/data/live02/stress/hlamberink/RLibrary" ) ) 

# read data of missing pdfs
missings <- read.csv2( "missingsWithURL.csv", stringsAsFactors = F )
withurl <- missings[ complete.cases(missings), ]
dim(withurl)

# read data of new pdfs
new <- read.csv2( "pdfNEW/new_rcts.csv", stringsAsFactors = F, header = F )
newwithurl <- new[ complete.cases(new), ]
dim(newwithurl)

# read downloaded pdfs (previously missing)
pdfs2 <- list.files( "/mnt/data/live02/stress/hlamberink/pdf/pdfNEW/pdfs2" )
pdfs2 <- as.numeric( gsub( '.pdf', '', pdfs2 ) )
length( pdfs2 )
head( pdfs2 )

# read downloaded pdfs (new)
pdfs1 <- list.files( "/mnt/data/live02/stress/hlamberink/pdf/pdfNEW/pdfs1" )
pdfs1 <- as.numeric( gsub( '.pdf', '', pdfs1 ) )
length( pdfs1 )
head( pdfs1 )

# bind files
pdfstotal <- c( pdfs1, pdfs2 )
urlstotal <- rbind( withurl, newwithurl )
dim(urlstotal)
length(pdfstotal)

# change names variables
names(urlstotal) <- c( "pmid", "url" )

# make selection of rcts with url which were not downloaded
'%ni%' <- Negate( '%in%' )
wanted <- urlstotal[ urlstotal$pmid %ni% pdfstotal, ]
alreadydownloaded <- urlstotal[ urlstotal$pmid %in% pdfstotal, ]
  
# print summary data
length(wanted[,1])
length(alreadydownloaded[,1])

# is this equal to the following?
length(urlstotal[,1])
# YES! now we can continue




# write 2 datafiles: (1) all wanted and (2) only linkinghub files
# first, combine pmids&urls with doi
df <- NULL
for( i in 1:23 )
{
  dd <- read.csv2( paste0( "/mnt/data/live02/stress/hlamberink/csv/csvfinal/limitedselect", i, ".csv" ), stringsAsFactors = F )
  
  df <- rbind( df, dd )
}

# then, combine wanted with df and write csv files
tosave <- merge( wanted, df, by = "pmid", all = TRUE )
linkinghubs <- tosave[ !is.na( tosave$url ), ]
linkinghubs <- linkinghubs[ grepl( "linkinghub", linkinghubs$url ), ]
linkinghWithDOI <- linkinghubs[ !is.na( linkinghubs$doi ), ]
write.csv2( tosave, "listMissingPDFS.csv", row.names = FALSE )
write.csv2( linkinghubs, "listMissingElsevier.csv", row.names = FALSE )
write.csv2( linkinghWithDOI, "listMissingElsevierDOI.csv", row.names = FALSE )

q( save = 'no' )