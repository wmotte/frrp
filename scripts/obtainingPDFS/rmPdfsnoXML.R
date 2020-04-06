# aim: remove pdfs without XML that should not have been downloaded
# date: 2018-03-21
####################################################################

# read list of included xml files
xmls <- list.files( "/mnt/data/live02/stress/hlamberink/grobid/xmlout" )
length(xmls)
head(xmls)

# read list of formerly new xml files
xmlsnew <- list.files( "/mnt/data/live02/stress/hlamberink/grobid/xmloutnew" )
length(xmlsnew)
head(xmls)

# are xmlsnew already in xmls ?
wereincluded <- xmlsnew[ xmlsnew %in% xmls ]
length(wereincluded)

# which xmls are not present in the full dataset and why?
'%ni%' <- Negate( '%in%' )
notincluded <- xmlsnew[ xmlsnew %ni% xmls ]
print(notincluded)


# quit R session
q(save = 'no' )