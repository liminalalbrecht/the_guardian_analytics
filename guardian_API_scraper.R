### (I) Import and Preparation

#set your wd
#setwd("SET_YOUR_WORKING DIRECTORY")


#libraries
library(dplyr)
library(lubridate)
library(readr)
library(httr)
library(tidyverse)
library(jsonlite)

#API
guardian_api_key<-"bb7240ec-a6c0-47dd-8562-f809cdeaa9e7"


#webscraping function
get_guardian_articles <- function(query, max_pages = 10, from = "1970-01-01", to = Sys.Date()) {
  # parameters
  base <- 'http://content.guardianapis.com/search'
  params <- list(
    'api-key' = guardian_api_key,
    'from-date' = from,
    'to-date' = to,
    'page-size' = 50,
    'page' = 1,
    'q' = query,
    'show-fields' = "bodyText"
  )
  # first request
  req <- GET(base, query = params)
  data <- fromJSON(content(req, 'text'))
  df <- data$response$results
  # remaining pages
  for (page in 2:data$response$pages) {
    if (page <= max_pages) {
      params$page <- page
      req <- GET(base, query = params)
      data <- fromJSON(content(req, 'text'))
      df <- bind_rows(df, data$response$results)
    }
  }
  return(df)
}



### (II) Fetch all Articles for the following keywords:
query_terms= "china (OR Macau OR Chengdu OR Shenzhen OR guangzhou OR shanghai OR hu OR jiang OR xi OR chinese OR beijing OR jinping OR jintao OR zemin OR hong OR prc OR ccp)"
#PS: fetching happens on a yearly basis because the API otherwise does not allow to extract all articles for a year


#check 1989-1999
#df_china_1989_1998 <-get_guardian_articles(query="china (OR Deng OR Xiaoping OR Macau OR Chengdu OR Shenzhen OR guangzhou OR shanghai OR hu OR jiang OR xi OR chinese OR beijing OR jinping OR jintao OR zemin OR hong OR prc OR ccp)", max_pages = 1900, from = "1989-01-01", to = "1998-12-31")
#nrow(df_china_1989_1998)
#df_china_1989_1998 <-data.frame(date=df_china_1989_1998$webPublicationDate, title=df_china_1989_1998$webTitle, url=df_china_1989_1998$webUrl, sectionID=df_china_1989_1998$sectionId, sectionName=df_china_1989_1998$sectionName, pillarID=df_china_1989_1998$pillarId, pillarName=df_china_1989_1998$pillarName, type=df_china_1989_1998$type, text=df_china_1989_1998$fields$bodyText)
#df_china_1989_1998 <- df_china_1989_1998 %>%
  #filter(grepl("Macau|Chengdu|Shenzhen|Guangzhou|Shanghai|Hu|Jiang|Xi|China|Chinese|Beijing|Jinping|Jintao|Zemin|Hong|PRC|CCP", title))
#nrow(df_china_1989_1998)



#1999
df_china_1999 <-get_guardian_articles(query=query_terms, max_pages = 1900, from = "1999-01-01", to = "1999-12-31")
nrow(df_china_1999)
# old sample: 1875

#2000
df_china_2000 <-get_guardian_articles(query=query_terms, max_pages = 1900, from = "2000-01-01", to = "2000-12-31")
nrow(df_china_2000)
# old sample: 2096

#2001
df_china_2001 <-get_guardian_articles(query=query_terms, max_pages = 1900, from = "2001-01-01", to = "2001-12-31")
nrow(df_china_2001)
# old sample: 2748

#2002
df_china_2002 <-get_guardian_articles(query=query_terms, max_pages = 1900, from = "2002-01-01", to = "2002-12-31")
nrow(df_china_2002)
# old sample 2875

#2003
df_china_2003 <-get_guardian_articles(query= query_terms, max_pages = 1900, from = "2003-01-01", to = "2003-12-31")
nrow(df_china_2003)
# old sample 3478

#2004
df_china_2004 <-get_guardian_articles(query= query_terms, max_pages = 1900, from = "2004-01-01", to = "2004-12-31")
nrow(df_china_2004)
# old 3860

#2005
df_china_2005 <-get_guardian_articles(query=query_terms, max_pages = 1900, from = "2005-01-01", to = "2005-12-31")
nrow(df_china_2005)
# old sample: 4806

#2006
df_china_2006 <-get_guardian_articles(query=query_terms, max_pages = 1900, from = "2006-01-01", to = "2006-12-31")
nrow(df_china_2006)
# old sample 5352

#2007
df_china_2007 <-get_guardian_articles(query=query_terms, max_pages = 1900, from = "2007-01-01", to = "2007-12-31")
nrow(df_china_2007)
# old sample: 6231

#2008
df_china_2008 <-get_guardian_articles(query=query_terms, max_pages = 1900, from = "2008-01-01", to = "2008-12-31")
nrow(df_china_2008)
# old sample: 8466

#2009
df_china_2009 <-get_guardian_articles(query=query_terms, max_pages = 1900, from = "2009-01-01", to = "2009-12-31")
nrow(df_china_2009)
# old sample 5943

#2010
df_china_2010 <-get_guardian_articles(query=query_terms, max_pages = 1900, from = "2010-01-01", to = "2010-12-31")
nrow(df_china_2010)
# old sample 6026

#2011
df_china_2011 <-get_guardian_articles(query=query_terms, max_pages = 1900, from = "2011-01-01", to = "2011-12-31")
nrow(df_china_2011)
# old sample: 7148

#2012
df_china_2012 <-get_guardian_articles(query=query_terms, max_pages = 1900, from = "2012-01-01", to = "2012-12-31")
nrow(df_china_2012)
# old sample 7810

#2013
df_china_2013 <-get_guardian_articles(query=query_terms, max_pages = 1900, from = "2013-01-01", to = "2013-12-31")
nrow(df_china_2013)
# old sample: 8381

#2014
df_china_2014 <-get_guardian_articles(query=query_terms, max_pages = 1900, from = "2014-01-01", to = "2014-12-31")
nrow(df_china_2014)
# old sample 8378

#2015
df_china_2015 <-get_guardian_articles(query=query_terms, max_pages = 1900, from = "2015-01-01", to = "2015-12-31")
nrow(df_china_2015)
# old sample 9020

#2016
df_china_2016 <-get_guardian_articles(query=query_terms, max_pages = 1900, from = "2016-01-01", to = "2016-12-31")
nrow(df_china_2016)
# old sample: 8646

#2017
df_china_2017 <-get_guardian_articles(query=query_terms, max_pages = 1900, from = "2017-01-01", to = "2017-12-31")
nrow(df_china_2017)
# old sample: 6881

#2018
df_china_2018 <-get_guardian_articles(query=query_terms, max_pages = 1900, from = "2018-01-01", to = "2018-12-31")
nrow(df_china_2018)
# old sample: 6307

#2019
df_china_2019 <-get_guardian_articles(query=query_terms, max_pages = 1900, from = "2019-01-01", to = "2019-12-31")
nrow(df_china_2019)
# old sample: 7048

#2020
df_china_2020 <-get_guardian_articles(query=query_terms, max_pages = 1900, from = "2020-01-01", to = "2020-12-31")
nrow(df_china_2020)
# old sample: 9619

#2021
df_china_2021 <-get_guardian_articles(query=query_terms, max_pages = 1900, from = "2021-01-01", to = "2021-12-31")
nrow(df_china_2021)
# old sample: 7512

#2022
df_china_2022 <-get_guardian_articles(query=query_terms, max_pages = 1900, from = "2022-01-01", to = "2022-12-31")
nrow(df_china_2022)
# old sample: 7373




#rbind years
guardian_china <- rbind(df_china_1999, df_china_2000, df_china_2001, df_china_2002, df_china_2003, df_china_2004,
                        df_china_2005, df_china_2006, df_china_2007, df_china_2008, df_china_2009, df_china_2010,
                        df_china_2011, df_china_2012, df_china_2013, df_china_2014, df_china_2015, df_china_2016,
                        df_china_2017, df_china_2018, df_china_2019, df_china_2020, df_china_2021, df_china_2022)


nrow(guardian_china) #163,994



### (III) Data Wrangling

#create data frame and choose columns
colnames(guardian_china)
guardian_china <-data.frame(date=guardian_china$webPublicationDate, title=guardian_china$webTitle, url=guardian_china$webUrl, sectionID=guardian_china$sectionId, sectionName=guardian_china$sectionName, pillarID=guardian_china$pillarId, pillarName=guardian_china$pillarName, type=guardian_china$type, text=guardian_china$fields$bodyText)

nrow(guardian_china) #163,994
ncol(guardian_china)

#save raw data
#write.csv(guardian_china, file="./datasets/china_dataset.csv")


#check data types
str(guardian_china)

#transform date string to date object
guardian_china$date <- as.POSIXct(guardian_china$date)
str(guardian_china)

#remove duplicated
sum(duplicated(guardian_china))
guardian_china_clean <- distinct(guardian_china)

sum(duplicated(guardian_china_clean))
View(guardian_china_clean[duplicated(guardian_china_clean$title), ])
nrow(guardian_china_clean) #159,162


#filter according to text == empty
sum(is.na(guardian_china_clean$text)) # no NA

number <- guardian_china_clean[guardian_china_clean$text=="", ] # but empty string
nrow(number)

guardian_china_clean <- guardian_china_clean[!guardian_china_clean$text=="", ]
nrow(guardian_china_clean) #157,273


#check for type
unique(guardian_china_clean$type)

#types <- guardian_china_clean %>%
  #filter(type %in% c("picture", "interactive", "gallery", "video", "audio"))


#parse article text + article title
colnames(guardian_china_clean)
guardian_china_clean$header_and_body <- paste(guardian_china_clean$title, guardian_china_clean$text)


#extract year
guardian_china_clean$year <- year(guardian_china_clean$date)

#extract month
guardian_china_clean$month <- month(guardian_china_clean$date)


#determinate articles/year
check_years <- guardian_china_clean %>%
  group_by(year) %>%
  count() 

mean(check_years$n)
median(check_years$n)
plot(check_years$year, check_years$n)


#create ID column
colnames(guardian_china_clean)

guardian_china_clean <- guardian_china_clean %>%
  arrange(date) %>%
  mutate(id = row_number())


#save DF
nrow(guardian_china_clean) #144,847
#write.csv(guardian_china_clean, file="./datasets/china_dataset_clean.csv")
