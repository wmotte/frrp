# aim: remove pdfs without XML that should not have been downloaded
# date: 2018-03-21
####################################################################

# read list of included pdf files
pdfs <- list.files( "/mnt/data/live02/stress/hlamberink/pdf/pdfs" )
length(pdfs)
head(pdfs)

# read list of formerly new pdf files
pdfsnew <- list.files( "/mnt/data/live02/stress/hlamberink/pdf/pdfs_new" )
length(pdfsnew)
head(pdfsnew)

# read list of formerly replace pdf files
pdfsrep <- list.files( "/mnt/data/live02/stress/hlamberink/pdf/pdfs_replace" )
length(pdfsrep)
head(pdfsrep)



# are xmlsnew already in xmls ?
wereincluded <- pdfsnew[ pdfsnew %in% pdfs ]
length(wereincluded)

# which xmls are not present in the full dataset and why?
'%ni%' <- Negate( '%in%' )
notincluded <- pdfsnew[ pdfsnew %ni% pdfs ]
length(notincluded)



# are xmlsnew already in xmls ?
wereincluded2 <- pdfsrep[ pdfsrep %in% pdfs ]
length(wereincluded2)

# which xmls are not present in the full dataset and why?
'%ni%' <- Negate( '%in%' )
notincluded2 <- pdfsrep[ pdfsrep %ni% pdfs ]
length(notincluded2)



# quit R session
q(save = 'no' )