#!/home1/wim/R-3.2.3/bin/Rscript --no-save --no-restore
#
# contact: w.m.otte@umcutrecht.nl
# date: 01-04-2017
#
# aim: download pmids of animal studies that have to be excluded
#################################################################

setwd( "/mnt/cortexraid/hlamberink" ) 
.libPaths( c(.libPaths(), "/mnt/cortexraid/hlamberink/RLibrary" ) )        

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
get.all.animal.pmids <- function()
{
  # search term
  sterm <- "randomized controlled trial[pt] NOT (randomized controlled trial[pt] NOT (animals[mh] NOT humans[mh]) ) "
  
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


######################################
# END FUNCTIONS
######################################

# pdf dir
outdir <- "pdf"
dir.create( outdir, showWarnings = FALSE )

# get pmids for full text RCTs
pmids <- as.character( get.all.animal.pmids() )
p <- as.data.frame( pmids, stringsAsFactors = FALSE )
write.csv2( p, file = paste0( outdir, '/animalstudies_exclude.csv' ), row.names = FALSE )
#p <- read.csv2( file = paste0( outdir, '/all_rcts_pmidsonly_2017-11-17.csv' ), stringsAsFactors = FALSE )