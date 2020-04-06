# aim: remove NEW pdfs that should not have been downloaded
# date: 2018-03-21
####################################################################

# read list of downloaded pdf files, remove '.pdf' and store as numeric variable
pdfs <- list.files( "/mnt/data/live02/stress/hlamberink/pdf/pdfNEW/pdfs1" )
pdfs <- as.numeric( gsub( ".pdf", "", pdfs ) )
length(pdfs)

# read included files after extra_filterNEW.R
included <- read.csv2( "/mnt/data/live02/stress/hlamberink/csv/csvfinal/dataselect23.csv" )
pincluded <- included$pmid
length(pincluded)

# which should be kept?
keep <- pdfs[ pdfs %in% pincluded ]
length( keep )

# which should be removed?
'%ni%' <- Negate( '%in%' )
remove <- pdfs[ pdfs %ni% pincluded ]
length(remove)

for( i in 1:length(remove) )
{
  f <- remove[ i ]
  f <- paste0( "/mnt/data/live02/stress/hlamberink/pdf/pdfNEW/pdfs1/", f, ".pdf" )
  
  cmd <- paste( "rm", f )
  
  system( cmd, intern = FALSE )
}

# for Flow-chart: how many of 'included' did not have a url?
urls <- read.csv2( "/mnt/data/live02/stress/hlamberink/pdf/pdfNEW/new_rcts.csv", header = F )
urlselect <- urls[ urls$V1 %in% pincluded, ]
length(urlselect[,1])

length( urlselect[ !complete.cases(urlselect), 1 ] )

# quit R session
q(save = 'no' )