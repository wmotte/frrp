
################################################################################
#
## aim: summarize missing data
## date: 2017-11-30
## herm.lamberink@gmail.com
#
################################################################################

## load required libraries
library( tidyr )
library( plyr ); library( dplyr )


################################################################################
# BEGIN FUNCTIONS								#
################################################################################




################################################################################
# END FUNCTIONS									#
################################################################################

# read data
miss <- read.csv2( "missings_total.csv", stringsAsFactors = FALSE )

# split URL column into urlhead, urltail
# split 1
miss <- miss %>% separate( url, c( "urlbase", "url" ), "://" )

# split 2 + reunite: replace first slash by '_', use this to split variable; then reunite main with base
miss$url <- sub( "/", "&_&", miss$url)
miss <- miss %>% separate( url, c( "urlmain", "urltail" ), "&_&" )
miss[ !is.na( miss$urlbase ), ] <- miss[ !is.na( miss$urlbase ), ] %>% unite( urlhead, urlbase, urlmain, sep = "://" )

# remove urltail, rename urlmain -> urltail
miss$urltail <- NULL
names( miss )[ 5 ] <- "urltail"

# summarize data
miss.summary <- as.data.frame( table( miss$urlbase, useNA = "ifany" ), stringsAsFactors = FALSE )
miss.summary <- arrange( miss.summary, -Freq )
miss.summary$percent <- miss.summary$Freq / sum( miss.summary$Freq )
miss.summary$cumpercent <- NA
miss.summary$cumpercent[1] <- miss.summary$percent
for( i in 2:length( miss.summary$percent ) )
{
  d <- miss.summary[ i, ]
  d$cumpercent <- miss.summary[ i-1, "cumpercent" ] + d$percent
  
  miss.summary[ i, "cumpercent" ] <- d$cumpercent
}

# write output
write.csv2( miss.summary, "missings_summary.csv", row.names = FALSE )

# write separate datafiles
write.csv2( miss[ is.na( miss$urlbase ), ], "url_NA.csv", row.names = FALSE )

################################################################################
# split up doi journals
################################################################################
# select all doi urls
doi <- miss[ miss$urlbase == "http://dx.doi.org", ]
doi <- doi[complete.cases(doi),]

# recreate full link
doi <- doi %>% unite( urlfull, urlbase, urltail, sep = "/", remove = FALSE)

# split urltail
doi <- doi %>% separate( col = urltail, into = c( "tail1", "tail2" ), sep = "/", remove = FALSE)

# doi summary
doi.summary <- as.data.frame( table(doi$tail1), stringsAsFactors = FALSE )
doi.summary <- arrange( doi.summary, -Freq )

# write output
write.csv2( doi, "missingdoi.csv", row.names = FALSE )
write.csv2( doi.summary, "missingdoi_summary.csv", row.names = FALSE )

### only the first four items have more than 1000 (actually more than 2900) papers. Continue 
### with those: 10.1111, 10.1002, 10.1038, and 10.1089
doibjc <- doi[ doi$tail1 == 10.1038, ]      # bjc
doiacm <- doi[ doi$tail1 == 10.1089, ]      # acm
doiwiley <- doi[ doi$tail1 == 10.1111 | doi$tail1 == 10.1002, ] # wiley
doirest <- doi[ doi$tail1 == 10.1016, ]     # sciencedirect, wiley, nature
