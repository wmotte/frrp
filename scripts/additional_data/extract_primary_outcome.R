# aim: extract sample size and primary outcome from abstract
# contact: h.j.lamberink@umcutrecht.nl  
# date: 2018-11-01
################################################################################

# libraries
library(stringr)
library(plyr)
library(psych) # for computing kappa
library(tidyr)

#############################
# FUNCTIONS
#############################

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
# convert words to numbers
##
word2num <- function(word){
  wsplit <- strsplit(tolower(word)," ")[[1]]
  one_digits <- list(zero=0, one=1, two=2, three=3, four=4, five=5,
                     six=6, seven=7, eight=8, nine=9)
  teens <- list(eleven=11, twelve=12, thirteen=13, fourteen=14, fifteen=15,
                sixteen=16, seventeen=17, eighteen=18, nineteen=19)
  ten_digits <- list(ten=10, twenty=20, thirty=30, forty=40, fifty=50,
                     sixty=60, seventy=70, eighty=80, ninety=90)
  doubles <- c(teens,ten_digits)
  out <- 0
  i <- 1
  while(i <= length(wsplit)){
    j <- 1
    temp <- 0
    if(i==1 && wsplit[i]=="hundred")
      temp <- 100
    else if(i==1 && wsplit[i]=="thousand")
      temp <- 1000
    else if(wsplit[i] %in% names(one_digits))
      temp <- as.numeric(one_digits[wsplit[i]])
    else if(wsplit[i] %in% names(teens))
      temp <- as.numeric(teens[wsplit[i]])
    else if(wsplit[i] %in% names(ten_digits))
      temp <- (as.numeric(ten_digits[wsplit[i]]))
    if(i < length(wsplit) && wsplit[i+1]=="hundred"){
      if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
        out <- out + 100*temp
      else
        out <- 100*(out + temp)
      j <- 2
    }
    else if(i < length(wsplit) && wsplit[i+1]=="thousand"){
      if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
        out <- out + 1000*temp
      else
        out <- 1000*(out + temp)
      j <- 2
    }
    else if(i < length(wsplit) && wsplit[i+1] %in% names(doubles)){
      temp <- temp*100
      out <- out + temp
    }
    else{
      out <- out + temp
    }
    i <- i + j
  }
  #return(list(word,out))
  return(out)
}



###
# replace numbers written as words by actual numbers
##
replaceNumbers <- function(text){
  
  a <- str_extract_all(tolower(text), "\\b(twenty|thirty|fourty|forty|fifty|sixty|seventy|eighty|ninety|eleven|twelve|thirteen|fourteen|forteen|fifteen|sixteen|seventeen|eighteen|nineteen|one|two|three|four|five|six|seven|eight|nine|zero|ten|hundred|thousand)\\b((\\s|-)(and|twenty|thirty|fourty|forty|fifty|sixty|seventy|eighty|ninety|eleven|twelve|thirteen|fourteen|forteen|fifteen|sixteen|seventeen|eighteen|nineteen|one|two|three|four|five|six|seven|eight|nine|zero|ten|hundred|thousand)+){0,10}")[[1]]
  a2 <- gsub( "-", " ", a )
  
  a2 <- lapply(a2, 
               function(x) {
                 
                 if( !grepl("\\s", x) ) {
                   s <- x
                 } else if( !grepl("and", x) ) {
                   s <- x
                 } else { 
                   s <- str_split( x, " " )[[1]]
                   if( length(s[grep("and", s) -1]) > 0 ) {
                     if( s[grep("and", s) - 1] %in% c("hundred", "thousand")){
                       s <- paste(s, collapse = " ")
                     } else {
                       s <- s[-grep("and",s)]
                     }
                   } else {
                     s <- paste(s, collapse = " ")
                   }
                 }
               }
  )
  a2 <- unlist(a2)
  
  '
  # OLD PART
  a2 <- lapply(a2, 
               function(x) {
                 
                 if( !grepl("\\s", x) ) {
                   s <- x
                 } else if( !grepl("and", x) ) {
                   s <- x
                 } else { 
                   s <- str_split( x, " " )[[1]]
                   if( s[grep("and", s) - 1] %in% c("hundred", "thousand")){
                     s <- paste(s, collapse = " ")
                   } else {
                     s <- s[-grep("and",s)]
                   }
                 }
               }
  )
  a2 <- unlist(a2)
  '
  
  b <- sapply(a2, word2num)
  
  if( length(b) == 0 ){
    out <- text
  } else if( length(b) == 1 ) {
    out <- sub( a, b, text, ignore.case = T )
  } else {
    ifelse(length(b)==length(a), c <- a, c <- a2 )
    out <- tolower(text)
    for( i in 1:length(b) ) {
      out <- sub( c[i], b[i], out, ignore.case = T )
    }
  }
  return(out)
}

#text <- "thousand and one nights"
#text <- "|seven, three and four, respectively. Seven were for"
#text <- " one hundred twenty-one"
#text <- ". Fifty patients"
#text <- "overweight people" ## not replace to be 'overw8 people'
#text <- "thousand five hundred"
#text <- "one, thousand five hundred"
#text <- "patients who had undergone surgery"  ## not replace to be 'underg1 surgery'
text <- "Two hundred sixteen adult patients"
replaceNumbers(text)


#############################
# END FUNCTIONS
#############################


##########################
# !!!!!!!!!!!!!!!!!!!!!!!!
# some of the abstracts are corrupt
# for example pmid 26654135, says: Children (n=0). This should be Children (n=170)
# fix this!!!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!
##########################



df <- read.csv("../../data/tiabtotal.csv", stringsAsFactors = F)
df <- df[!is.na(df$abstract),]
abs <- replace_non_ascii2( df$abstract )
#abs <- tail(df$abstract, 1000)
#abs <- replace_non_ascii2( abs )
#abs <- abs[1:100]

# transform all written numbers to numerics
abs <- sapply( abs, replaceNumbers )

# remove decimals of thousands, replace decimal dots by comma's 
abs <- gsub("([[:digit:]]+)(\\,)([[:digit:]]+)", "\\1\\3", abs)
abs <- gsub("([[:digit:]]+)(\\.)([[:digit:]]+)", "\\1\\,\\3", abs)
abs <- gsub("([[:digit:]]+) ([[:digit:]]+)", "\\1\\,\\3", abs)

# remove strange characters in between n and = and digits AND in between digits
abs <- gsub("([[:digit:]]+)([^[:space:][:punct:][:digit:]]){1,3}([[:digit:]]+)", "\\1\\3", abs)
abs <- gsub("([nN])([^[:punct:]]){1,3}(=)([^[:punct:]]){1,3}([[:digit:]]+)", "\\1\\3\\5", abs)

# uniform 'n's', e.g. n = 3 --> n=3
abs <- gsub("n = ", "n=", abs)
abs <- gsub("N = ", "n=", abs)
abs <- gsub("N=", "n=", abs)



####################
# primary outcome measure
####################

outcm <- "(primary outcome|main outcome|primary end point|primary end-point|primary endpoint) "
ptn <- paste0( "\\.([^.])*", outcm, "([^.])*(\\.|secondary)")
sum(grepl(ptn, abs, ignore.case = T))
#pmids <- df[grepl(ptn, abs, ignore.case = T),]$pmid
pr_outcomes <- str_extract(abs, ptn)

pmids <- df[!is.na(pr_outcomes),]$pmid
pr_outcomes <- pr_outcomes[!is.na(pr_outcomes)]

# clean pr_outcomes and extract primary outcome measure
pr_outcomes <- gsub("^[^[:alpha:]]{1,5}", "", pr_outcomes)

after <- strsplit(pr_outcomes, "(primary outcome|main outcome|primary end point|primary end-point|primary endpoint) ([[:alpha:],]+ ){0,3}(was|is|were) ")
before <- strsplit(pr_outcomes, " (was|is|were) ([[:alpha:],]+ ){1,3}(primary outcome|main outcome|primary end point|primary end-point|primary endpoint)")


after_out <- sapply(after, function(x){
  if(length(x) == 2) out <- x[2]
  if(length(x) == 1) out <- NA
  return(out)
})

before_out <- sapply(before, function(x){
  if(length(x) == 2) out <- x[1]
  if(length(x) == 1) out <- NA
  return(out)
})

after_out[is.na(after_out)] <- before_out[is.na(after_out)]

df_out <- data.frame(pmid = pmids,
                     pr_outcome = after_out,
                     stringsAsFactors = F)

df_out <- df_out[!is.na(df_out$pr_outcome),]



######### TODO
# 4) find studies with NCT number, try to compare outcome measure
######### TODO
ctgov <- read.csv2("../../9.registry/michelle/Data_Herm_2018-7-10(data_scripts)/Outcome_data.csv", stringsAsFactors = F)
ctgov <- ctgov[ctgov$OutcomeType == "Primary",]

comparable <- df_out[df_out$pmid %in% ctgov$PMID,]
comparable <- merge(comparable,ctgov[,c("PMID", "OldOutcome", "NewOutcome")],
                    by.x = "pmid", by.y = "PMID", all.x = T)

write.csv2(comparable, "check_primary_outcomes.csv", row.names = F)

# idea: separate words in CTGOV outcome measure, check whether they exist in publication
words <- strsplit(tolower(comparable$OldOutcome), "( |/)")

for( i in 1:3 ) {
  wrds <- words[[i]]
  txt <- comparable[i,]$pr_outcome
  
  count <- 0
  for( j in 1:length(wrds) ) {
    if(grepl(paste0("\\b", wrds[j], "\\b"), txt)) {count <- count + 1
    print(wrds[j])}
  }
}


