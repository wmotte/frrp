# aim: download university ranking


library(rvest)
library(plyr);library(dplyr)
library(tidyr)

shurl1 <- "http://www.shanghairanking.com/ARWU2018.html" # 1-500
shurl2 <- "http://www.shanghairanking.com/ARWU2018Candidates.html" # 501-1000 
turl1 <- "https://www.timeshighereducation.com/world-university-rankings/2019/world-ranking#!/page/0/length/-1/sort_by/rank/sort_order/asc/cols/stats"
turl2 <- "https://www.timeshighereducation.com/world-university-rankings/2019/world-ranking#!/page/0/length/-1/sort_by/rank/sort_order/asc/cols/scores"


########## SHANGHAI ###########
###############################

# read full page, add country information to column, and write as html
sh500 <- read_html(shurl1) 
sh500 <- gsub("(<img src=\"image/flag/)([[:alpha:]-]+)(\\.png)\"(>)", "\\2", as.character(sh500))
writeLines(sh500, "shanghai_2018_500.html")
sh500 <- read_html(sh500)

sh1000 <- read_html(shurl2) 
writeLines(as.character(sh1000), "shanghai_2018_501_1000_orig.html")
sh1000 <- gsub("(<img title=\")([[:print:] ]+)(\" src=\"image/flag/)([[:alpha:]-]+)(\\.png)\"(>)", "\\4", as.character(sh1000))
writeLines(sh1000, "shanghai_2018_501_1000.html")
sh1000 <- read_html(sh1000)


# extract table
sht <- html_nodes(sh500, xpath = '//table[@id="UniversityRanking"]') %>%
  html_table(fill = T)
sht <- sht[[1]]
names(sht) <- c("rank", "institution", "country", "rank_country", "score_total", "score_alumni", "score_award", "score_HiCi", "score_N_S", "score_PUB", "score_PCP")

sht2 <- html_nodes(sh1000, xpath = '//table[@id="UniversityRanking"]') %>%
  html_table(fill = T)
sht2 <- sht2[[1]]
names(sht2) <- c("rank", "institution", "country", "score_alumni", "score_award", "score_HiCi", "score_N_S", "score_PUB", "score_PCP")
sht2$rank_country <- sht2$score_total <- NA 

# merge shanghai tables
sht <- rbind(sht, sht2)

# replace China-hk and China-tw
sht[sht$country == "China-hk",]$country <- "HongKong"
sht[sht$country == "China-tw",]$country <- "Taiwan"

write.csv(sht, "shanghai_2018_full.csv", row.names = F)

########## Times HE ###########
###############################

# read full page as manually downloaded from turl1
the <- readLines("times_2018.html") 
the2 <- readLines("times_2018_2.html")

# make sure country is separable from university
the <- gsub("([[:alpha:] ]+)(</a></span></div>)", "|\\1\\2", the)
the2 <- gsub("([[:alpha:] ]+)(</a></span></div>)", "|\\1\\2", the2)

writeLines(the, "times_2018_use.html")
writeLines(the2, "times_2018_2_use.html")

the <- read_html("times_2018_use.html")
the2 <- read_html("times_2018_2_use.html")

# extract table
thet <- the %>% html_nodes(xpath = '//table[@id="datatable-1"]') %>%
  html_table(fill = T)
thet <- thet[[1]]

thet2 <- the2 %>% html_nodes(xpath = '//table[@id="datatable-1"]') %>%
  html_table(fill = T)
thet2 <- thet2[[1]]

# merge the two files
thet$rn <- as.numeric(row.names(thet))
the_table <- merge(thet, thet2[,names(thet2) != "Rank"], by = "Name") %>%
  select(Rank, everything()) %>%
  arrange(rn)

# remove 'Explore' after country
the_table$Name <- gsub("Explore", "", the_table$Name)

# split university and country
unisplit <- strsplit(the_table$Name, "\\|")
total <- NULL
for(i in unisplit) {
  dd <- data.frame( name = i[1],
                    country = i[2] )
  total <- rbind(total,dd)
}

names(the_table) <- tolower(names(the_table))
the_table$country <- total$country
the_table$name <- total$name

write.csv(the_table, "timesHigherEducation_2018.csv", row.names = F)
