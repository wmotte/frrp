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
abs <- gsub("([nN])([^[:punct:]]){1,3}(=)([^[:digit:]]){1,3}([[:digit:]]+)", "\\1\\3\\5", abs)

# uniform 'n's', e.g. n = 3 --> n=3
abs <- gsub("n = ", "n=", abs)
abs <- gsub("N = ", "n=", abs)
abs <- gsub("N=", "n=", abs)

####################
# sample size
####################

# compute new number based on 'notes' column
pts <- "(patients|participants|patients|participants|subjects|individuals|inpatients|outpatients|people|persons|volunteers|cases|controls|adults|children|women|men|males|females|students|boys|girls|elderly|adolescents|mothers|parents|eyes|babies|neonates|infants|survivors|surgeons|employees|MSM)"
rndm <- "(enrolled|included|randomised|randomized|assigned|allocated|evaluated|recruited|followed)"
adject <- "(on|in|by|among|among the)"


ptrn1 <- paste0("[[:digit:]]+ ", "([[:alpha:],]+ ){0,4}", pts, "([^\\.:])*", "(were|was) ", "([^\\.:])*", rndm, "([^\\.]* (and|the other) [[:digit:]]+ ", pts, ")*")
ptrn2 <- paste0(rndm, " [[:digit:]]+ ", "([[:alpha:]]+ ){0,4}", pts)
ptrn5 <- paste0(pts, " \\(n=[[:digit:]]+\\)", "([^\\.])*", " were ", rndm)
ptrn3 <- paste0("[[:digit:]]+ ", "([[:alpha:],]+ ){0,4}", pts, "([^\\.])*", "completed")
ptrn4 <- paste0(adject, " [[:digit:]]+ ", "([[:alpha:],]+ ){0,10}", pts)
ptrn8 <- paste0("[[:digit:]]+ ", "([[:alpha:],]+ ){0,4}", pts, " in each group")
ptrn9 <- "arm \\(([^\\)])*n=[[:digit:]]+\\)([^\\.])*arm \\(([^\\)])*n=[[:digit:]]+\\)"
ptcpnts <- paste0("[[:digit:]]+ ", pts)

ptrns <- data.frame(
  pmid = df$pmid,
  manually = NA,
  #patients = nrs_check$patients,
  patients = str_extract(abs, "[[:digit:]]+ patients"),
  #notes = nrs_check$notes,
  notes = NA,
  abs = abs,
  p1 = str_extract(tolower(abs), ptrn1),
  p2 = str_extract(tolower(abs), ptrn2),
  p3 = str_extract(tolower(abs), ptrn3),
  p4 = str_extract(tolower(abs), ptrn4),
  p5 = str_extract(tolower(abs), ptrn5),
  ptcpnts = str_extract(tolower(abs), ptcpnts),
  p8 = str_extract(tolower(abs), ptrn8),
  stringsAsFactors = F
)
ptrns[ptrns == ""] <- NA

# extract the first number from included texts
ptrns[,c(3,6:12)]<- lapply(ptrns[,c(3,6:12)], function(x) {
  str_extract(x, "[[:digit:]]+")
})

# extract all numbers and multiply by 2 (because extracted 'in each group')
# p8 is not accurate enough, so skip this step
#ptrns$p8 <- unlist( lapply(ptrns$p8, function(x) {
#  a <- str_extract_all(x, "[[:digit:]]*")
#  a <- unlist( lapply(a, paste, collapse = "") )
#  a[a == "NA"] <- NA
#  
#  b <- lapply(a, as.integer)
#  b <- unlist( lapply(b, function(x) x*2) )
#  
#  return(b)
#}))


# combine columns 2,1,5
# p3 and p4 are not accurate enough
ptrns$cbd <- ptrns$p2
ptrns[is.na(ptrns$cbd),]$cbd <- ptrns[is.na(ptrns$cbd),]$p5
#ptrns[is.na(ptrns$cbd),]$cbd <- ptrns[is.na(ptrns$cbd),]$p4
#ptrns[is.na(ptrns$cbd),]$cbd <- ptrns[is.na(ptrns$cbd),]$p3
ptrns$combined <- ptrns$p1
ptrns[is.na(ptrns$p1),]$combined <- ptrns[is.na(ptrns$p1),]$cbd

# combine all available data, starting with prcpnts
ptrns$combined2 <- ptrns$ptcpnts
ptrns[is.na(ptrns$combined2),]$combined2 <- ptrns[is.na(ptrns$combined2),]$combined

# combine all available data, starting with combined
ptrns$combined3 <- ptrns$combined
ptrns[is.na(ptrns$combined3),]$combined3 <- ptrns[is.na(ptrns$combined3),]$ptcpnts


sum(is.na(ptrns$combined))
sum(is.na(ptrns$combined2))
sum(is.na(ptrns$combined3))







# check performance of different columns.
# first, manually note sample size of 100 cases
#set.seed(7654)
#write.csv2(ptrns[sample(1:323402, 100),], "compute_kappa3.csv", row.names = F)
comp.kappa <- read.csv2("compute_kappa3.csv", stringsAsFactors = F)

#set.seed(8765)
#write.csv2(ptrns[sample(1:323402, 100),], "compute_kappa4.csv", row.names = F)
comp.kappa2 <- read.csv2("compute_kappa4.csv", stringsAsFactors = F)
comp.kappa <- rbind(comp.kappa, comp.kappa2)

suppressWarnings(cohen.kappa(comp.kappa[,c("manually", "combined")]))
suppressWarnings(cohen.kappa(comp.kappa[,c("manually", "combined3")]))
suppressWarnings(cohen.kappa(comp.kappa[,c("manually", "p1")]))
suppressWarnings(cohen.kappa(comp.kappa[,c("manually", "p2")]))
#suppressWarnings(cohen.kappa(comp.kappa[,c("manually", "patientsnum")]))
#suppressWarnings(cohen.kappa(comp.kappa[,c("manually", "ptcpnts")]))
#suppressWarnings(cohen.kappa(comp.kappa[,c("manually", "combined2")]))


View(dplyr::select(ptrns, pmid, manually, combined3, combined2, combined, notes, abs))

### conclusions sample size based on first draw of 100 pts
# kappa combined   = 0.92 (95%CI 0.84-1.00) (n=41)
# kappa combined3  = 0.87 (95%CI 0.79-0.96) (n=65)
# kappa p1         = 0.94 (95%CI 0.87-1.00) (n=36)
# kappa p2         = 0.69 (95%CI 0.25-1.00) (n=4 )
### conclusions sample size

### conclusions sample size based on second draw of 100 pts
# kappa combined   = 0.97 (95%CI 0.90-1.00) (n=30)
# kappa combined3  = 0.83 (95%CI 0.75-0.92) (n=73)
# kappa p1         = 0.96 (95%CI 0.88-1.00) (n=25)
# kappa p2         = 1.00 (95%CI 1.00-1.00) (n=6 )
### conclusions sample size

### conclusions sample size based on both draws combined
# kappa combined   = 0.94 (95%CI 0.89-0.94) (n=71)
# kappa combined3  = 0.85 (95%CI 0.79-0.91) (n=138)
# kappa p1         = 0.95 (95%CI 0.89-1.00) (n=61)
# kappa p2         = 0.89 (95%CI 0.69-1.00) (n=10 )
### conclusions sample size

##########OVERALL CONCLUSTIONS SAMPLE SIZE########
#'combined' works best with highest yield (but is not perfect!)
# NOTE: this variable has been changed after computing abovementioned kappas
# combined is only a combination of p1, p2, and p5; the others are too vague
##########OVERALL CONCLUSTIONS SAMPLE SIZE########


# write output
dfout <- ptrns[,c("pmid", "combined")]
names(dfout) <- c("pmid", "sample_size_abstract")

write.csv(dfout, "sample_size_abstract.csv", row.names = F)
