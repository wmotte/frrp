# aim: determine gender of first and last author
# date: 2018-07-18
# contact: h.j.lamberink@umcutrecht.nl
#
###############################################################

.libPaths( c( .libPaths(), "/mnt/data/live02/stress/hlamberink/RLibrary" ) )


library( 'httr' )
library( 'rvest' )
library( 'pbapply' )

####################################
# FUNCTIONS
####################################

###
# adjusted version of rscopus::replace_non_ascii
##
replace_non_ascii2 <- function (string) 
{
  raws = structure(list(S = as.raw(c(197, 160)), s = as.raw(c(197,161)), 
                        Z = as.raw(c(197, 189)), z = as.raw(c(197, 190)), 
                        A = as.raw(c(195, 128)), A = as.raw(c(195, 129)), 
                        A = as.raw(c(195, 130)), A = as.raw(c(195, 131)), 
                        A = as.raw(c(195, 132)), A = as.raw(c(195, 133)), 
                        A = as.raw(c(195, 134)), C = as.raw(c(195, 135)), 
                        E = as.raw(c(195, 136)), E = as.raw(c(195, 137)), 
                        E = as.raw(c(195, 138)), E = as.raw(c(195, 139)), 
                        I = as.raw(c(195, 140)), I = as.raw(c(195, 141)), 
                        I = as.raw(c(195, 142)), I = as.raw(c(195, 143)), 
                        N = as.raw(c(195, 145)), O = as.raw(c(195, 146)), 
                        O = as.raw(c(195, 147)), O = as.raw(c(195, 148)), 
                        O = as.raw(c(195, 149)), O = as.raw(c(195, 150)), 
                        O = as.raw(c(195, 152)), U = as.raw(c(195, 153)), 
                        U = as.raw(c(195, 154)), U = as.raw(c(195, 155)), 
                        U = as.raw(c(195, 156)), Y = as.raw(c(195, 157)), 
                        B = as.raw(c(195, 158)), Ss = as.raw(c(195, 159)), 
                        a = as.raw(c(195, 160)), a = as.raw(c(195, 161)), 
                        a = as.raw(c(195, 162)), a = as.raw(c(195, 163)), 
                        a = as.raw(c(195, 164)), a = as.raw(c(195, 165)), 
                        a = as.raw(c(195, 166)), c = as.raw(c(195, 167)), 
                        e = as.raw(c(195, 168)), e = as.raw(c(195, 169)), 
                        e = as.raw(c(195, 170)), e = as.raw(c(195, 171)), 
                        i = as.raw(c(195, 172)), i = as.raw(c(195, 173)), 
                        i = as.raw(c(195, 174)), i = as.raw(c(195, 175)), 
                        o = as.raw(c(195, 176)), n = as.raw(c(195, 177)), 
                        o = as.raw(c(195, 178)), o = as.raw(c(195, 179)), 
                        o = as.raw(c(195, 180)), o = as.raw(c(195, 181)), 
                        o = as.raw(c(195, 182)), o = as.raw(c(195, 184)), 
                        u = as.raw(c(195, 185)), u = as.raw(c(195, 186)), 
                        u = as.raw(c(195, 187)), y = as.raw(c(195, 189)), 
                        y = as.raw(c(195, 189)), b = as.raw(c(195, 190)), 
                        y = as.raw(c(195, 191)),
                        c = as.raw(c(196, 135)), u = as.raw(c(195, 188))), 
                   .Names = c("S", "s", "Z", "z", "A", "A", 
                              "A", "A", "A", "A", "A", "C", "E", "E", "E", "E", "I", 
                              "I", "I", "I", "N", "O", "O", "O", "O", "O", "O", "U", 
                              "U", "U", "U", "Y", "B", "Ss", "a", "a", "a", "a", "a", 
                              "a", "a", "c", "e", "e", "e", "e", "i", "i", "i", "i", 
                              "o", "n", "o", "o", "o", "o", "o", "o", "u", "u", "u", 
                              "y", "y", "b", "y", "c", "u"))
  unwanted_array = names(raws)
  unwanted_array_names = sapply(raws, rawToChar)
  names(unwanted_array) = unwanted_array_names
  for( i in 1:length( unwanted_array ) )
  {
    string <- gsub( unwanted_array_names[i], unwanted_array[i], string)
  } 
  
  return(string)
}


###
# for names like 'Bob B G', extract 'Bob'
##
extract.first.names <- function(namez) { 
  
  namez <- gsub( "-", " ", namez )
  namesSplit <- strsplit(namez, split = "|", fixed = TRUE)

  newnames <- lapply( namesSplit, function(x) { 
    
    for( i in 1:length(x)) {
      if( !is.na(x[i]) & nchar( x[i] ) == 1 ) {
        x <- NA
        next()
      } 
      
      if( grepl( " ", x[i] ) & x[i] != " " ) {
        
        fnSplit <- strsplit(x[i], split = " ")[[1]]
        fnSplit[fnSplit == ""] <- NA
        fnSplit <- fnSplit[!is.na(fnSplit)]
        
        # 1: if only initials, set to NA
        if( sum(nchar(fnSplit)) == length(fnSplit) ) {
          x <- NA
          next()
        } 
        
        # 2: if name + initials, remove initials
        fnSplit <- fnSplit[ nchar(fnSplit) > 1 ]
        
        # 3: if multiple names, choose first one
        if( length(fnSplit) > 1 ) {
          x[i] <- fnSplit[1]
        } else {
          x[i] <- fnSplit
        }
      }
    }
    return(x)
  })
  
  nnames <- sapply(newnames, function(x) {
    paste0( x, collapse = "|" )
  })
  
  nnames[nnames == "NA"] <- NA
  
  return(nnames)
}


###
# get results from genderize.io API using country abbreviations
##
get.genders.country <- function(dfrow) {
  # loop over publications in calling this function
  # loop over authors within publications
  fnames <- dfrow['fnames']
  country <- dfrow['country_abbrev']
  pmid <- dfrow['pmid']
  
  if( !is.na(fnames) ) {
    
    # split names
    nameSplit <- strsplit( fnames, split = "\\|" )[[1]]
    
    # if country is missing, set to NULL
    if(is.na(country)) {
      country <- NULL
    }
    
    
    # use get.genderize.result
    gendz <- lapply(nameSplit, get.genderize.result, country = country) 
    genderz <- t(matrix(unlist(gendz), nrow = 5, ncol = length(nameSplit))) 
    
    namez <- paste0( genderz[,1], collapse = "|" )
    gender <- paste0( genderz[,3], collapse = "|" )
    genderprop <- paste0( genderz[,4], collapse = "|" )
    country <- paste0( genderz[,2], collapse = "|" )
    count <- paste0( genderz[,5], collapse = "|" )
  } else {
    count <- country <- genderprop <- gender <- namez <- NA
  }
  
  
  gdf <- data.frame( names = namez,
                      gender = gender,
                      genderprop = genderprop,
                      count = count,
                     pmid = pmid,
                      stringsAsFactors = F)
  
  write.table(gdf, "partial_output_country.csv", sep = ",", 
              col.names = F, row.names = F, append = T)
  
  return(gdf)

}


###
# get results from genderize.io API not using country abbreviations
##
get.genders.nocountry <- function(dfrow) {
  # loop over publications in calling this function
  # loop over authors within publication
  fnames <- dfrow['fnames']
  pmid <- dfrow['pmid']
  
  if( !is.na(fnames) ) {
    
    # split names
    nameSplit <- strsplit( fnames, split = "\\|" )[[1]]

    
    
    # use get.genderize.result
    gendz <- lapply(nameSplit, get.genderize.result, country = NULL) 
    genderz <- t(matrix(unlist(gendz), nrow = 5, ncol = length(nameSplit))) 
    
    namez <- paste0( genderz[,1], collapse = "|" )
    gender <- paste0( genderz[,3], collapse = "|" )
    genderprop <- paste0( genderz[,4], collapse = "|" )
    count <- paste0( genderz[,5], collapse = "|" )
  } else {
    count <- genderprop <- gender <- namez <- NA
  }
  
  
  gdf <- data.frame( names = namez,
                     gender = gender,
                     genderprop = genderprop,
                     count = count,
                     pmid = pmid,
                     stringsAsFactors = F)
  
  write.table(gdf, "partial_output.csv", sep = ",", 
              col.names = F, row.names = F, append = T)
  
  return(gdf)
  
}



###
# get results from genderize.io API
##
get.genderize.result <- function( name, country = NULL, 
                                  api_key = "6156bedb2803700429ea8af42496daae",
                                  http = "https://api.genderize.io/" ) {
  
  # create query with name, country & api key
  query <- paste0( "?name=", name )
  if( !is.null(country) ) query <- paste0( query, "&country_id=", country )
  query <- paste0( query, "&apikey=", api_key )
  
  # combine all to api URL
  link <- paste0( http, query )
  
  # get output for name/country combination
  out <- GET( link )
  if( out$status_code == 429 ) stop("...error: quota exeeded.\nWait until next month...")
  
  result <- content( out )
  if( length( result ) == 4 ) {
    gender <- result$gender
    probability <- result$probability
    count <- result$count
  } else {
    gender <- NA
    probability <- NA
    count <- 0
  }
  if(is.null(country)) country <- NA
  
  return(list(name,country,gender,probability,count))
}




####################################
# END FUNCTIONS
####################################

# get data
df <- read.csv("../country/affiliations_first_author.csv", stringsAsFactors = F)

# normalize names to ascii
df$authors <- replace_non_ascii2(df$authors)
df$firstnames <- replace_non_ascii2(df$firstnames)

# extract single first names
df$fnames <- extract.first.names(df$firstnames)


firsttime <- FALSE

if(firsttime == TRUE){
  # write empty table (only headers)
  tb <- data.frame(names = "names",
                   gender = "gender",
                   genderprop = "genderprop",
                   count = "count",
                   pmid = "pmid")
  write.table(tb, "partial_output.csv", sep = ",", col.names = F, row.names = F)
  
  # extract genders
  gender_out <- pbapply(df, 1, get.genders.nocountry)
  
  nn <- lapply(gender_out, function(x) x[1])
  gg <- lapply(gender_out, function(x) x[2])
  pp <- lapply(gender_out, function(x) x[3])
  cc <- lapply(gender_out, function(x) x[4])
  id <- lapply(gender_out, function(x) x[5])
  out <- data.frame(names = unlist(nn),
                    gender = unlist(gg),
                    genderprop = unlist(pp),
                    count = unlist(cc),
                    pmid = unlist(id),
                    stringsAsFactors = F)
}

if(firsttime == FALSE){
  gender_out1 <- read.csv("partial_output.csv", stringsAsFactors = F)
  
  if( gender_out1$pmid[length(gender_out1[,1])] == df$pmid[length(gender_out1[,1])] ) {
    
    dfnew <- df[length(gender_out1[,1])+1:length(df[,1]),]
    dfnew <- dfnew[!is.na(dfnew$pmid),]
    
    gender_out2 <- pbapply(dfnew, 1, get.genders.nocountry)
    
    nn <- lapply(gender_out2, function(x) x[1])
    gg <- lapply(gender_out2, function(x) x[2])
    pp <- lapply(gender_out2, function(x) x[3])
    cc <- lapply(gender_out2, function(x) x[4])
    id <- lapply(gender_out2, function(x) x[5])
    out <- data.frame(names = unlist(nn),
                      gender = unlist(gg),
                      genderprop = unlist(pp),
                      count = unlist(cc),
                      pmid = unlist(id),
                      stringsAsFactors = F)
    
  } else {
    stop( "...check old and new data, order of files do not match..." )
  }
  
  gender_out <- rbind( gender_out1, out)
  
}




# write full output data
write.csv(out, "genders_full_data.csv", row.names = F)
### TODO when finished: compare with partial_output.csv

# add genders to dataframe
df <- merge(df, out[,c("pmid", "genders", "genderprop")],
            by = "pmid",
            all.x = T)


write.csv(df, "author_country_gender.csv", row.names = F)




# quit R session
q("no")