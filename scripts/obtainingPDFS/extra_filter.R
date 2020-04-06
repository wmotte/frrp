#
# contact: h.j.lamberink@umcutrecht.nl
# date: 2017-12-21
#
# aim: make stricter selection of articles
#################################################################

.libPaths( c(.libPaths(), "/mnt/data/live02/stress/hlamberink/RLibrary" ) ) 


######################################
# FUNCTIONS
######################################


######################################
# END FUNCTIONS
######################################

indata <- "csvout"
outdata <- "csvfinal"

# read full list of pmids
pmidlist <- read.csv2( "all_rcts_pmidsonly_2017-11-17.csv", stringsAsFactors = FALSE )


# loop over 22 datasets, afterward summarize the total
for( i in 1:22 )
{
  print( paste( "filtering dataset:", i ) ) 
  
  # read 20000 records
  limited <- read.csv2( paste0( indata, "/pubmeddata_limited", i, ".csv" ), stringsAsFactors = FALSE )
  basic <- read.csv2( paste0( indata, "/pubmeddata", i, ".csv" ), stringsAsFactors = FALSE, quote = "" )
  basic <- as.data.frame( lapply( basic, gsub, pattern = "\"", replacement = "" ), stringsAsFactors = FALSE )
  names( basic ) <- c( "pmid", "doi", "authors", "firstnames", "yearpub", "articletitle",  
                     "journal", "jcountry", "volume", "issue", "pages", "lang",
                     "meshHeadings", "keyword", "grantAgency", "grantNumber", "grantCountry", "ptype",
                     "affiliations", "collective", "funding", "databankname", "databanknumber" )
  class( basic$pmid ) <- class( basic$yearpub ) <- "integer"
  
  # read tiab file, clean and write away
  #txt <- readLines( paste0( indata, "/pubmeddata_tiab", i, ".csv" ) )
  #tiabtest <- gsub( "\";\"", "%%%", txt )
  #tiabtest <- gsub( ";", ":", tiabtest ) 
  #tiabtest <- gsub( "%%%", "\";\"", tiabtest)
  #if( i == 9 ) tiabtest <- sub( ":", ";", tiabtest )  ## the 9th dataset has classified 'pmid' as numeric and therefore has no quoatations around first column. 
  #writeLines( tiabtest, paste0( indata, "/pubmeddata_tiab", i, "_new.csv" ) )
  
  # read in clean file 
  tiab <- read.csv2( paste0( indata, "/pubmeddata_tiab", i, "_new.csv" ), stringsAsFactors = FALSE, quote = "" )
  tiab <- as.data.frame( lapply( tiab, gsub, pattern = "\"", replacement = "" ), stringsAsFactors = FALSE )
  names( tiab ) <- c( "pmid", "articletitle", "abstract", "journal" )
  class( tiab$pmid ) <- "integer"
  rm(tiabtest,txt)
  
  # save full list of pmids
  pmidOrig <- basic$pmid
  
  # first filter on title and abstract
  tiab$protocol <- grepl( "[Ss]tudy protocol", tiab$articletitle )
  tiab$random <- grepl( "[Rr]andom", tiab$articletitle ) | grepl( "[Rr]andom", tiab$abstract ) | grepl( "[Aa]ssign", tiab$abstract ) | grepl( "[Aa]llocat", tiab$abstract ) | grepl( "[Pp]lacebo", tiab$abstract ) | grepl( "[Dd]ouble-blind", tiab$abstract )
  tiab$abstractNA <- grepl( "NA", tiab$abstract )
  tiab$pilot <- grepl( "[Pp]ilot", tiab$articletitle ) | grepl( "[Ff]easibility", tiab$articletitle )
  tiab$selected <- tiab$random
  if( sum( tiab$protocol ) > 0 ) { tiab[ tiab$protocol == TRUE, ]$selected <- FALSE }
  if( sum( tiab$pilot ) > 0 ) { tiab[ tiab$pilot == TRUE, ]$selected <- FALSE }
  
  # save pmids left after TIAB selection
  pmidTiabSelected <- tiab[ tiab$selected == TRUE, ]$pmid
  
  # second filter on rest of information: exclude non-english text, exclude pt==letter
  basic$english <- grepl( "eng", basic$lang )
  pmidEngSelected <- basic[ basic$english == TRUE, ]$pmid
  
  # final selection pmids (tiab + english)
  pmidSelectedFinal <- pmidEngSelected[ pmidEngSelected %in% pmidTiabSelected ]
  
  # write summary statistics of excluded languages
  languages <- as.data.frame( table( basic$lang ) )
  names( languages ) <- c( "lang", paste0( "freq_", i ) )
  if( file.exists( paste0( outdata, "/excluded_languages.csv" ) ) )
  {
    langorig <- read.csv2( paste0( outdata, "/excluded_languages.csv" ), stringsAsFactors = FALSE )
    languages <- merge( langorig, languages, all = TRUE, by.x = "lang", by.y = "lang" )
  }
  
  write.csv2( languages, paste0( outdata, "/excluded_languages.csv" ), row.names = FALSE )
  
  # write summary statistics of excluded files
  excl_tiab <- data.frame( dataset = i,
                           totalOrig = length(tiab$pmid),
                           excl_nonEng = length(tiab$pmid) - length(pmidEngSelected), 
                           excl_protocol = sum(tiab$protocol),
                           excl_absNA = sum(tiab$abstractNA),
                           excl_random = sum(!tiab$random),
                           excl_protocolANDtiab = sum(!tiab$selected),
                           excl_pilot = sum(tiab$pilot),
                           totalSelected = length(pmidSelectedFinal),
                           stringsAsFactors = FALSE )
  if( i == 1 ) 
  {
    write.table( excl_tiab, paste0( outdata, "/excluded_categories.csv" ), sep = ";", row.names = FALSE, append = TRUE )
  } else {
    write.table( excl_tiab, paste0( outdata, "/excluded_categories.csv" ), sep = ";", row.names = FALSE, col.names = FALSE, append = TRUE )
  }
    
  
  # write selected pmids back to file & write filtered files 
  write.csv2( pmidSelectedFinal, paste0( outdata, "/pmids_selected", i, ".csv" ), row.names = FALSE )
  write.csv2( tiab[ tiab$pmid %in% pmidSelectedFinal, ], paste0( outdata, "/tiabselect", i, ".csv" ), row.names = FALSE )
  write.csv2( tiab[ !tiab$pmid %in% pmidSelectedFinal, ], paste0( outdata, "/tiabexcluded", i, ".csv" ), row.names = FALSE )
  write.csv2( basic[ basic$pmid %in% pmidSelectedFinal, ], paste0( outdata, "/dataselect", i, ".csv" ), row.names = FALSE )
  write.csv2( basic[ !basic$pmid %in% pmidSelectedFinal, ], paste0( outdata, "/dataexcluded", i, ".csv" ), row.names = FALSE )
  write.csv2( limited[ limited$pmid %in% pmidSelectedFinal, ], paste0( outdata, "/limitedselect", i, ".csv" ), row.names = FALSE )
  write.csv2( limited[ !limited$pmid %in% pmidSelectedFinal, ], paste0( outdata, "/limitedexcluded", i, ".csv" ), row.names = FALSE )
}

# exclLang: summarize and write file
exclLang <- read.csv2( paste0( outdata, "/excluded_languages.csv" ), stringsAsFactors = FALSE )
exclLang$sum <- rowSums(exclLang[, -1], na.rm = T)
exclLang$perc <- round( exclLang$sum / sum(exclLang$sum), digits = 5 )
write.csv2( exclLang, paste0( outdata, "/excluded_languages.csv" ), row.names = FALSE )

# exclCat: summarize
exclCat <- read.csv2( paste0( outdata, "/excluded_categories.csv" ), stringsAsFactors = FALSE )
exclCat[ "total", ] <- as.data.frame( t( as.data.frame( colSums( exclCat ) ) ) )
exclCat[ "total", "dataset" ] <- "total"
exclCat[ "percent", ] <- as.data.frame( cbind( "percent", 
                                               round( exclCat["total", -1 ] / exclCat["total", "totalOrig"], 4 ) ) )
write.csv2( exclCat, paste0( outdata, "/excluded_categories.csv" ), row.names = FALSE )

## summarize: how many studies included in cochrane reviews are included in our data?
# First, summarize original selection (before filter)
cochranePmids <- read.csv( "unique.pmids.in.reviews.csv", row.names = 1, stringsAsFactors = FALSE )
class( cochranePmids$x ) <- "integer"
cochranePmidsSelected_orig <- cochranePmids[ cochranePmids$x %in% pmidlist$pmids, "x" ]
cochranePmidsNotSelected_orig <- cochranePmids[ !( cochranePmids$x %in% pmidlist$pmids ), "x" ]

# Second, summarize selection after applying filter
# create vehicle
pmidSelectedTotal <- NULL

# loop over 22 files of selected pmids and bind into one vector
for( i in 1:22 )
{
  df <- read.csv2( paste0( outdata, "/pmids_selected", i, ".csv" ) )
  pmidSelectedTotal <- rbind( pmidSelectedTotal, df)
}

# summarize cochrane and bind all info to dataframe
cochranePmidsAfterselection <- cochranePmids[ cochranePmids$x %in% pmidSelectedTotal$x, "x" ]
cochranePmidsRemoved <- cochranePmidsSelected_orig[ !( cochranePmidsSelected_orig %in% pmidSelectedTotal$x ) ]
cochraneSummary <- data.frame( original_studies_cochrane = length( cochranePmids$x ),
                               studies_in_original_selection = length( cochranePmidsSelected_orig ),
                               studies_selected_after_filter = length( cochranePmidsAfterselection ),
                               stringsAsFactors = FALSE )


# create vehicles
tiab <- NULL
lang <- NULL

# loop over 22 tiab and basic files and select those that were excluded
for( i in 1:22 )
{
  print( paste( "analyzing dataset:", i ) )
  df <- read.csv2( paste0( indata, "/pubmeddata_tiab", i, "_new.csv" ), stringsAsFactors = FALSE, quote = "" )
  df <- as.data.frame( lapply( df, gsub, pattern = "\"", replacement = "" ), stringsAsFactors = FALSE )
  names( df ) <- c( "pmid", "articletitle", "abstract", "journal" )
  class( df$pmid ) <- "integer"
  
  df <- df[ df$pmid %in% cochranePmidsRemoved, ]
  tiab <- rbind( tiab, df )
  
  
  df2 <- read.csv2( paste0( indata, "/pubmeddata", i, ".csv" ), stringsAsFactors = FALSE, quote = "" )
  df2 <- as.data.frame( lapply( df2, gsub, pattern = "\"", replacement = "" ), stringsAsFactors = FALSE )
  names( df2 ) <- c( "pmid", "doi", "authors", "firstnames", "yearpub", "articletitle",  
                     "journal", "jcountry", "volume", "issue", "pages", "lang",
                     "meshHeadings", "keyword", "grantAgency", "grantNumber", "grantCountry", "ptype",
                     "affiliations", "collective", "funding", "databankname", "databanknumber" )
  class( df2$pmid ) <- class( df2$yearpub ) <- "integer"
  
  df2 <- df2[ df2$pmid %in% cochranePmidsRemoved, c( "pmid", "lang", "yearpub" ) ]
  lang <- rbind( lang, df2 )
} 

# bind to dataframe
cochrane_excluded<- cbind( tiab, lang)

# write to file
write.csv2( cochraneSummary, paste0( outdata, "/cochrane_summary.csv" ), row.names = FALSE )
write.csv2( cochrane_excluded, paste0( outdata, "/cochrane_excluded.csv" ), row.names = FALSE )



# quit R session
q( save = "no" )