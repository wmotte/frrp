# aim: compare pre-registered number of planned inclusions with actual RCT inclusions
# contact: h.j.lamberink@umcutrecht.nl
# date: 2018-11-07
####################################################################################


.libPaths( c(.libPaths(), "/mnt/data/live02/stress/hlamberink/RLibrary" ) ) 


# load libraries
library(plyr); library(dplyr)
library(rvest)
library(RCurl)
library(stringr)

##############################
# FUNCTIONS
##############################


###
# extract basic (most recent) information from CT.gov
##
ctgov_download_basic <- function( nct ) {
  url <- paste0( "https://clinicaltrials.gov/ct2/results/download_fields?cond=&term=", nct, "&down_fmt=csv&down_flds=all" )
  tmp <- tempfile( "data", fileext = ".csv" )
  download.file( url, tmp, quiet = TRUE )
  df <- read.csv( tmp, stringsAsFactors = F )
  unlink(tmp)
  
  return( df)
}


###
# extract planned sample size from the historical protocol that was uploaded before or around 
# the start date of the trial
##
extract_planned_sample_size <- function( nct, startdate, enddate, status ) {
  
  # set all output items to NA
  N_planned <- N_actual <- st.d <- en.d <- A <- s <- e <- NA
  
  # prepare URL
  url <- paste0("https://clinicaltrials.gov/ct2/history/", nct, "?A=1&B=10000&C=side-by-side")
  
  #######TODO####### # determine whether all necessary
  xpathOutcomesInResults <- '//*[(@id = "ProtocolOutcomeMeasures")]'
  xpathTrack <- '//*[@id="controls"]/div/div/div[2]/form/div/fieldset/table'
  xpathOutcomes <- '//*[@id="form_OutcomeMeasures"]/div/fieldset/div/table'
  xpathEnrollment <- '//div[@id="StudyDesignBody"]/table'
  
  # scrape full page
  htmltemp <- read_html(url)
  
  # are results posted?
  outcomesinresults <-  htmltemp %>%
    html_nodes(xpath = xpathOutcomesInResults) 
  
  # full track of protocol changes on ct.gov
  track <- htmltemp %>%
    html_nodes( xpath = xpathTrack ) %>%
    html_table( fill = TRUE )
  if( length(track) != 0 ) {
    track <- track[[1]]
    track$Date <- gsub("January ", "01-", track$Date)
    track$Date <- gsub("February ", "02-", track$Date)
    track$Date <- gsub("March ", "03-", track$Date)
    track$Date <- gsub("April ", "04-", track$Date)
    track$Date <- gsub("May ", "05-", track$Date)
    track$Date <- gsub("June ", "06-", track$Date)
    track$Date <- gsub("July ", "07-", track$Date)
    track$Date <- gsub("August ", "08-", track$Date)
    track$Date <- gsub("September ", "09-", track$Date)
    track$Date <- gsub("October ", "10-", track$Date)
    track$Date <- gsub("November ", "11-", track$Date)
    track$Date <- gsub("December ", "12-", track$Date)
    
    track$Date <- as.Date(track$Date, format = "%m-%d, %Y")
  } 
  
  # number of changed posted on ct.gov
  tracklength <- nrow(track)
  
  # scrape number of planned inclusions in protocol version around starting date
  # determine which row to use: the protocol version before or around start date
  if(grepl("^[[:alpha:]]+ [[:digit:]]{4}", startdate)){
    st.d <- gsub("January ", "01-", startdate)
    st.d <- gsub("February ", "02-", st.d)
    st.d <- gsub("March ", "03-", st.d)
    st.d <- gsub("April ", "04-", st.d)
    st.d <- gsub("May ", "05-", st.d)
    st.d <- gsub("June ", "06-", st.d)
    st.d <- gsub("July ", "07-", st.d)
    st.d <- gsub("August ", "08-", st.d)
    st.d <- gsub("September ", "09-", st.d)
    st.d <- gsub("October ", "10-", st.d)
    st.d <- gsub("November ", "11-", st.d)
    st.d <- gsub("December ", "12-", st.d)
    st.d <- paste0("15-", st.d)
    st.d <- as.Date(st.d, format = "%d-%m-%Y")
  } else if(grepl("^[[:alpha:]]+ [[:digit:]]{1,2}, [[:digit:]]{4}", startdate)) {
    st.d <- gsub("January ", "01-", startdate)
    st.d <- gsub("February ", "02-", st.d)
    st.d <- gsub("March ", "03-", st.d)
    st.d <- gsub("April ", "04-", st.d)
    st.d <- gsub("May ", "05-", st.d)
    st.d <- gsub("June ", "06-", st.d)
    st.d <- gsub("July ", "07-", st.d)
    st.d <- gsub("August ", "08-", st.d)
    st.d <- gsub("September ", "09-", st.d)
    st.d <- gsub("October ", "10-", st.d)
    st.d <- gsub("November ", "11-", st.d)
    st.d <- gsub("December ", "12-", st.d)
    st.d <- as.Date(st.d, format = "%m-%d, %Y")
  }
  
  if(grepl("^[[:alpha:]]+ [[:digit:]]{4}", enddate)){
    en.d <- gsub("January ", "01-", enddate)
    en.d <- gsub("February ", "02-", en.d)
    en.d <- gsub("March ", "03-", en.d)
    en.d <- gsub("April ", "04-", en.d)
    en.d <- gsub("May ", "05-", en.d)
    en.d <- gsub("June ", "06-", en.d)
    en.d <- gsub("July ", "07-", en.d)
    en.d <- gsub("August ", "08-", en.d)
    en.d <- gsub("September ", "09-", en.d)
    en.d <- gsub("October ", "10-", en.d)
    en.d <- gsub("November ", "11-", en.d)
    en.d <- gsub("December ", "12-", en.d)
    en.d <- paste0("15-", en.d)
    en.d <- as.Date(en.d, format = "%d-%m-%Y")
  } else if(grepl("^[[:alpha:]]+ [[:digit:]]{1,2}, [[:digit:]]{4}", startdate)) {
    en.d <- gsub("January ", "01-", enddate)
    en.d <- gsub("February ", "02-", en.d)
    en.d <- gsub("March ", "03-", en.d)
    en.d <- gsub("April ", "04-", en.d)
    en.d <- gsub("May ", "05-", en.d)
    en.d <- gsub("June ", "06-", en.d)
    en.d <- gsub("July ", "07-", en.d)
    en.d <- gsub("August ", "08-", en.d)
    en.d <- gsub("September ", "09-", en.d)
    en.d <- gsub("October ", "10-", en.d)
    en.d <- gsub("November ", "11-", en.d)
    en.d <- gsub("December ", "12-", en.d)
    en.d <- as.Date(en.d, format = "%m-%d, %Y")
  }
  
  # compare dates in posted protocol versions with starting date
  comp.dates.s <- as.numeric(track$Date - st.d)
  comp.dates.e <- as.numeric(track$Date - en.d)
  
  # choose protocol version before or max 31 days (1 month) after start date
  A <- sum(comp.dates.s < 32)
  if(A == 0) {
    s <- min(comp.dates.s)
    e <- min(comp.dates.e)
    A <- 1
  } else {
    s <- comp.dates.s[A]
    e <- min(comp.dates.e)
  }
  
  if(A > 1) {
    url2 <- paste0("https://clinicaltrials.gov/ct2/history/", nct, "?A=", A, "&B=10000&C=side-by-side")
    htmltemp2 <- read_html(url2)
  } else {
    htmltemp2 <- htmltemp
  }
  
  # extract enrolment information
  enrollment <- htmltemp2 %>%
    html_nodes(xpath = xpathEnrollment) %>%
    html_table( fill = TRUE )
  if( length(enrollment) != 0 ) {
    enrollment <- enrollment[[1]]
    if(sum(grepl("Enrollment:", enrollment$X1)) > 0) {
      planned <- enrollment[grepl("Enrollment:", enrollment$X1), "X2"]
      actual <- enrollment[grepl("Enrollment:", enrollment$X1), "X3"]
      N_planned <- as.numeric(str_extract(planned, "[[:digit:]]+"))
      N_actual <- as.numeric(str_extract(actual, "[[:digit:]]+"))
      if(!grepl("Actual", actual)) N_actual <- NA
    }
  } 
  
  # create output file
  dfout <- data.frame( nct = nct,
                       status = status,
                       startdate = st.d,
                       enddate = en.d,
                       protocoldate = track$Date[A],
                       relative_start_days = s,
                       relative_end_days = e,
                       N_planned = N_planned,
                       N_actual = N_actual,
                       stringsAsFactors = F)
  
  return( dfout )
}

##############################
# END FUNCTIONS
##############################

#### FOR ON O:/
## read data actualized sample size
#df <- read.csv("sample_size_abstract.csv", stringsAsFactors = F)
#
## read data clinicaltrials.gov registration numbers
#NCT <- read.csv("../../9.registry/NCT/nctnumbersOverall.csv", stringsAsFactors = F)
#NCT <- NCT[,c("pmid", "yearpub", "fulltext", "source", "NCT")]
#NCT <- NCT[!is.na(NCT$NCT),]

# read data actualized sample size
df <- read.csv("sample_size_abstract.csv", stringsAsFactors = F)

# read data clinicaltrials.gov registration numbers
NCT <- read.csv("../ctgov/NCT/nctnumbersOverall.csv", stringsAsFactors = F)
NCT <- NCT[,c("pmid", "yearpub", "fulltext", "source", "NCT")]
NCT <- NCT[!is.na(NCT$NCT),]




####### clinicaltrials.gov
# obtain planned sample size from CT.gov, from the most recent protocol before the start date of the trial

# prepare vehicle
dfout <- NULL

# loop over df
tstart <- Sys.time()
for(i in 1:length(NCT[,1])) {

  planned_enrol <- basic <- NULL
  
  nct <- NCT[i,]$NCT
  pmid <- NCT[i,]$pmid
  
  print(paste(i, nct))
  if(nct == "multiple") next()
  
  # dowlnoad basic information
  tryCatch( basic <- ctgov_download_basic( nct ),
            error = function(e) print(e) )
  if(length(basic) == 0) next()
  if(dim(basic)[1] > 1) basic <- basic[basic$NCT.Number == nct,]
  if(dim(basic)[1] != 1){
    head(basic)
    next()
  } 
  
  startd <- basic$Start.Date
  endd <- basic$Primary.Completion.Date
  if(endd == "null") endd <- basic$Completion.Date
  status <- basic$Status
  
  if( endd == "null" | startd == "null" ) next()
  
  # download planned sample size
  tryCatch( planned_enrol <- extract_planned_sample_size(nct, startd, endd, status),
            error = function(e) print(e) )
  if(length(planned_enrol) == 0) next()
  
  planned_enrol$pmid <- pmid
  
  # bind rows
  dfout <- rbind(dfout, planned_enrol)
}
print(Sys.time() - tstart)



#######TODO####### # read data WHO registry registration numbers
#######TODO####### # obtain planned sample size from WHO trial registries


# merge dataframes
sizes <- merge(df, dfout,
               by = "pmid",
               all.x = T)
sizes$proportion_target <- sizes$N_actual / sizes$N_planned

write.csv(sizes, "sample_size_obtained.csv", row.names = F)

# quit R session
q("no")