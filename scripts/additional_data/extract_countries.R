# aim: extract country of first (and last when possible) author affiliation
# date: 2018-07-16
# contact: h.j.lamberink@umcutrecht.nl
# inspiration script: https://stackoverflow.com/questions/5318076/extracting-country-name-from-author-affiliations



library(maps)
library(plyr)
library(rvest)

######################################################
# FUNCTIONS
######################################################

###
# select affiliations of first author (pubmed data)
##
aff_split <- function(aff) {
  affiliationsplit <- strsplit( aff, "|", fixed = TRUE )
  
  firstaffiliation <- lapply(affiliationsplit, function(x) x[[1]][1])
  
  return(firstaffiliation)
}

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
# replace state abbreviations, names, and some other country names by standardised country names
##
replace.states.by.countries <- function(affiliation) {

  # replace us state abbreviation by state name
  usabb <- html_table(read_html("state_abbreviations.txt"))[[1]] # source of table: http://www.softschools.com/social_studies/state_abbreviations/
  usabb$State <- tolower(usabb$State)
  usabb$Abbreviation <- tolower(usabb$Abbreviation)
  for( i in 1:length(usabb[,1])) {
    abbr <- paste0( ", ", usabb[i,2], "(\\.|,)")
    state <- paste0( ", ", usabb[i,1] )
    state <- gsub( " ","", state)
    
    affiliation <- gsub( abbr, state, affiliation)
    
    abbr2 <- paste0( ", ", usabb[i,2], "$" )
    affiliation <- gsub( abbr2, state, affiliation)
    
    abbr3 <- paste0( ", ", usabb[i,2], "\\s" )
    state2 <- paste0( state, " " )
    affiliation <- gsub( abbr3, state2, affiliation)
    
  }
  
  
  # replace the us state name in affiliation by usa
  for( i in unique(usstates$name) )
  {
    affiliation <- gsub( i, "usa", affiliation )
  }
  
  # replace state abbreviations by state name
  castates <- data.frame( name = c( "alberta", "british columbia", 			
                                    "manitoba", "new brunswick", 
                                    "newfoundland and labrador", 	
                                    "northwest territories", "nova scotia",				
                                    "nunavut", "ontario", 
                                    "prince edward island", "quebec",
                                    "saskatchewan", "yukon" ),
                          abb = c( "ab",
                                   "bc",
                                   "mb",
                                   "nb",
                                   "nl",
                                   "nt",
                                   "ns",
                                   "nu",
                                   "on",
                                   "pe",
                                   "qc",
                                   "sk",
                                   "yt" ) )
  
  
  
  # replace the Canadian state name by canada
  for( i in unique(castates$name))
  {
    affiliation <- gsub( i, "canada", affiliation, ignore.case = T)
  }
  for( i in 1:length(castates[,1])) {
    abbr <- paste0( ", ", castates[i,2], "(\\.|,)")
    state <- paste0( ", ", castates[i,1] )
    state <- gsub( " ","", state)
    
    affiliation <- gsub( abbr, state, affiliation)
    
    abbr2 <- paste0( ", ", castates[i,2], "$" )
    affiliation <- gsub( abbr2, state, affiliation)
    
    abbr3 <- paste0( ", ", castates[i,2], "\\s" )
    state2 <- paste0( state, " " )
    affiliation <- gsub( abbr3, state2, affiliation)
    
  }
  
  # replace state abbreviation for full state name
  for( i in state.abb )
  {
    affiliation <- gsub( paste0( ", ", i ), ", usa", affiliation )
  }
  
  # change "United Kingdom" into "UK" in the aff file and United States to USA
  affiliation <- gsub( "unitedkingdom", "uk", affiliation )
  affiliation <- gsub( "england", "uk", affiliation )
  affiliation <- gsub( "scotland", "uk", affiliation )
  affiliation <- gsub( "newsouthwales", "australia", affiliation ) #otherwise "wales" is not correct
  affiliation <- gsub( "wales", "uk", affiliation )
  affiliation <- gsub( "unitedstates", "usa", affiliation )
  affiliation <- gsub( "brasil", "brazil", affiliation)
  affiliation <- gsub( "ivorycoast", "coted'ivoire", affiliation)
  
  return(affiliation)
}


replace.extensions <- function(affiliation, cextensions) {
  
  
  # replace the extension by the country name
  for( i in 1:length(cextensions$extension))
  {
    
    abbr <- paste0( "[a-z]\\", cextensions[i,3], "\\s")
    country <- paste0( ", ", cextensions[i,1], " " )
    
    affiliation <- gsub( abbr, country, affiliation)
    
    abbr2 <- paste0( "[a-z]\\", cextensions[i,3], "$" )
    affiliation <- gsub( abbr2, country, affiliation)
    
    abbr3 <- paste0( "[a-z]\\", cextensions[i,3], "(\\.|,)")
    affiliation <- gsub( abbr3, country, affiliation)
    
  }
  return(affiliation)
}

###
# get table with country abbreviations
##
get.country.abbreviations <- function() {
  ctable <- html_table(read_html("../country/country_abbreviations.txt"), header = T)[[1]] # source table: https://www.worldatlas.com/aatlas/ctycodes.htm
  ctable$COUNTRY <- tolower(ctable$COUNTRY)
  ctable$country_abbrev <- tolower(ctable$`A2 (ISO)`)
  ctable$fullname <- gsub( " \\((.)*\\)", "", ctable$COUNTRY)
  
  # replace country names by generic country names
  ctable$fullname <- gsub( "brunei darussalam", "brunei", ctable$fullname )
  ctable$fullname <- gsub( "iran, islamic republic of", "iran", ctable$fullname )
  ctable$fullname <- gsub( "macedonia, the former yugoslav republic of", "macedonia", ctable$fullname )
  ctable$fullname <- gsub( "moldova, republic of", "moldova", ctable$fullname )
  ctable$fullname <- gsub( "palestine, state of", "palestine", ctable$fullname )
  ctable$fullname <- gsub( "russian federation", "russia", ctable$fullname )
  ctable$fullname <- gsub( "syrian arab republic", "syria", ctable$fullname )
  ctable$fullname <- gsub( "united republic of tanzania", "tanzania", ctable$fullname )
  ctable$fullname <- gsub( "viet nam", "vietnam", ctable$fullname )
  ctable$fullname <- gsub( "united states", "usa", ctable$fullname )
  ctable$fullname <- gsub( "united kingdom", "uk", ctable$fullname )
  ctable$fullname <- gsub( "korea, democratic people's republic of", "korea", ctable$fullname )
  ctable$fullname <- gsub( "lao people's democratic republic", "laos", ctable$fullname )
  ctable[length(ctable[,1])+1,] <- ctable[ctable$fullname == "serbia",]
  ctable[length(ctable[,1]),]$fullname <- "serbiaandmontenegro"
  ctable[ctable$fullname == "namibia",]$country_abbrev <- "na"
  
  names(ctable) <- c("country", "a2_iso", "a3_un", "num_un", "dialing_code", 
                     "country_abbrev", "fullname")
  
  ctable <- rbind( ctable, data.frame(country = "netherlands antilles", 
                                      a2_iso = "AN",
                                      a3_un = "NLD",
                                      num_un = 528,
                                      dialing_code = 31,
                                      country_abbrev = "an",
                                      fullname = "netherlandsantilles"))
  
  # remove all spaces like in the other data frames
  ctable$fullname <- gsub( " ", "", ctable$fullname)
  
  
  return(ctable)
}

###
# get table with country names and internet extensions
##
get.extensions <- function() {
  cextensions <- html_table(read_html("country_extensions.txt"), header = T )[[1]]
  names(cextensions) <- c("country", "extension")
  cextensions$country <- as.character(cextensions$country)
  cextensions$extension <- as.character(cextensions$extension)
  cextensions$country <- tolower(cextensions$country)
  cextensions$extension <- gsub( " \\((.)*\\)", "", cextensions$extension)
  cextensions$country <- gsub( " \\((.)*\\)", "", cextensions$country)
  cextensions$extension <- gsub( ".sx / .an", ".sx", cextensions$extension)
  cextensions[cextensions == "not assigned yet"] <- NA
  
  cextensions$country <- gsub( " ", "", cextensions$country )
  
  cextensions$country <- gsub( "china,people'srepublicof", "china", cextensions$country)
  cextensions$country <- gsub( "czechia", "czechrepublic", cextensions$country)
  cextensions$country <- gsub( "southkorea", "korea", cextensions$country)
  cextensions$country <- gsub( "northkorea", "korea", cextensions$country)
  cextensions$country <- gsub( "macedonia,republicof", "macedonia", cextensions$country)
  cextensions$country <- gsub( "unitedstatesofamerica", "usa", cextensions$country)
  cextensions$country <- gsub( "unitedkingdom", "uk", cextensions$country)
  
  
  
  return(cextensions)
}


###
# extract country names from affiliations
##
extract.country <- function(aff) {
  
  # in case a comma is not followwed by a space but by a word, insert a space
  aff[grepl(",[a-z]", aff)] <- gsub(",", ", ", aff[grepl(",[a-z]", aff)])
  aff[grepl("[a-z]\\(", aff)] <- gsub("\\(", " \\(", aff[grepl("[a-z]\\(", aff)])
  aff <- gsub("( ){2,}", " ", aff)
  
  # for affiliation, remove all punctuation and make element of every word
  caa <- gsub( "[[:punct:]\n]", "", aff )
  saa <- strsplit(caa, " " )
  
  # for affiliation, match on country in world.countries
  # assumes that if multiple matches, the first takes precedence, i.e. min()
  out <- lapply( saa, function( x ) x[ suppressWarnings( min( which( x %in% cities$country.etc ) ) ) ] )
  out <- lapply( out, function( x ) if( length( x ) == 0 ){ x <- NA } else { x } )
  countries <- unlist( out )
  
  return(countries)
}


######################################################
# END FUNCTIONS
######################################################


# load data on world cities from 'maps' data, usstates and canadian states
cities <- world.cities
data(state)
usstates <- as.data.frame(state.x77)
usstates$name <- row.names( usstates )

# country data to lower
cities$country.etc <- tolower( cities$country.etc )
usstates$name <- tolower( usstates$name )

# change "Korea North" and "Korea South" both to Korea
cities$country.etc <- gsub( "korea north", "korea", cities$country.etc )
cities$country.etc <- gsub( "korea south", "korea", cities$country.etc )

# add Hong Kong to cities$country
cities <- rbind( cities, data.frame( name = "hong kong", country.etc = "hong kong", pop = NA, lat = NA, long = NA, capital = 0 ))

# remove Madeira from countries
cities <- cities[cities$country.etc != "madeira",]

# read data
df <- read.csv("../../data/authors.csv", stringsAsFactors = F)


# replace non-ascii characters in affiliations
df$affiliations <- replace_non_ascii2(df$affiliations)

# extract first and last affiliation
df$firstaffiliation <- tolower(aff_split(df$affiliations))

# remove spaces from all country names such as "new zealand"
cities$country.etc <- as.factor( cities$country.etc )
withspace <- levels(cities$country.etc)[ grepl( " ", levels(cities$country.etc))]
withspace <- data.frame( withspace = withspace, 
                         nospace = gsub( " ", "", withspace ), 
                         stringsAsFactors = F )
firstaffiliation <- df$firstaffiliation
for( i in 1:length(withspace[,1])) {
  country <- withspace[ i, "withspace" ]
  replacement <- withspace[ i, "nospace" ]
  
  cities$country.etc <- gsub( pattern = country, replacement = replacement, x = cities$country.etc )
  firstaffiliation <- gsub( pattern = country, replacement = replacement, x = firstaffiliation )
}


# remove spaces from all usa state names such as "south carolina"
usstates$name <- as.factor( usstates$name )
withspace <- levels(usstates$name)[ grepl( " ", levels(usstates$name))]
withspace <- data.frame( withspace = withspace, nospace = gsub( " ", "", withspace ), stringsAsFactors = F )
for( i in 1:length(withspace[,1])) {
  state <- withspace[ i, "withspace" ]
  replacement <- withspace[ i, "nospace" ]
  
  usstates$name <- gsub( pattern = state, replacement = replacement, x = usstates$name )
  firstaffiliation <- gsub( pattern = state, replacement = replacement, x = firstaffiliation )
}

# replace states by countries
firstaffiliation <- replace.states.by.countries(firstaffiliation)

# potential to decrease missings: use email address extentions
cextensions <- get.extensions()
cextensions <- base::merge(data.frame(table(cities$country.etc)), cextensions,
                           by.x = "Var1", by.y = "country", all.x = T)
names(cextensions)[1] <- "country"
firstaffiliation <- replace.extensions(firstaffiliation, cextensions)


# extract countries
df$firstcountry <- extract.country(firstaffiliation)

# change some mistaken countries
df$firstcountry <- gsub( "sicily", "italy", df$firstcountry)

# make column with country abbreviation
country.abbrev <- get.country.abbreviations()
df <- merge( df, country.abbrev[,6:7],
                 by.x = "firstcountry", by.y = "fullname",
                 all.x = T)

# write file
write.csv( df, "affiliations_first_author.csv", row.names = FALSE )


#### check for missed countries 
check <- df[ !is.na( df$firstaffiliation ) & is.na( df$firstcountry ), ]
check2 <- df[ !is.na( df$firstcountry) & is.na( df$country_abbrev), ]
table(check2$firstcountry)
write.csv2( check, "tocheck.csv")





