#
# contact: h.j.lamberink@umcutrecht.nl
# date: 2017-11-28
#
# aim: remove animal studies from data
#################################################################



######################################
# FUNCTIONS
######################################




######################################
# END FUNCTIONS
######################################

# pdf dir
outdir <- "U:/pdf"
dir.create( outdir, showWarnings = FALSE )

# read list of pmids
p <- read.csv2( "all_rcts_pmidsonly_2017-11-17.csv", stringsAsFactors = FALSE )

# exclude animal studies
animals <- read.csv2( "animalstudies_exclude.csv", stringsAsFactors = FALSE )
d <- data.frame( pmids = p[ !( p$pmids %in% animals$pmids), ],
                 stringsAsFactors = FALSE )

# write new datafile
write.csv2( d, "all_rcts_pmidsonly_2017-11-17_animalsexcluded.csv", row.names = FALSE )
