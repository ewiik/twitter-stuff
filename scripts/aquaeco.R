## do some analytics on what AquaEco has tweeted for Thursday job day!
## NOTE that since Feb-March there seems to be some twitter inconsistency - now #jobthursday not
##    #thursdayjobday AND sometimes this isn't hashtagged... nrr is there phd or postdoc hashtag ID
## the saved file here was from a search 12.03.2019
## NOTE that obvs not completely clean - there will be some tweets where the right number may not be
##    captured or someone has used the hashtags without actually advertising a position; AND
##    where instead of 'year' yr or y is used, or when number of positions is written out (two)

## get packages
library(tidyr)
library(rtweet)
library(stringr)


## authenticate via access token
## NOTE these are my private access tokens so will not be public; to reproduce, create your own token
source("../token-create.R")

if(!file.exists("../dat-mod/thursjob-raw.rds")) {
  fr <- get_timeline("@BES_AquaEco", n=3000)
  frnames <- names(fr)
  
  thurs <- fr[grep("thursdayjobday" ,fr$hashtags, ignore.case = TRUE),]
  df <- as.data.frame(thurs)
  saveRDS(df, "../dat-mod/thursjob-raw.rds")
}
df <- readRDS("../dat-mod/thursjob-raw.rds")

vec <- df$urls_url
vec[duplicated(vec) | duplicated(vec, fromLast=TRUE)] # includes NAs..

dups <- which(duplicated(vec) | duplicated(vec, fromLast=TRUE)) # includes NAs..
dups <- as.data.frame(df[dups,c('urls_url','text')])
dups$urls_url <- unlist(dups$urls_url)
sdups <- dups[-which(is.na(dups$urls_url)),]
#write.csv(file="~/not-to-backup/dups.csv", x= sdups, row.names = FALSE)
# most dups are real dups but unfortunately some not - can't automate that

dupin <- which(duplicated(df$urls_url))
dupin <- dupin[!dupin %in% which(is.na(df$urls_url))]
df <- df[-dupin,]

## create indicator of which tweets mention PhD and postdoc positions
df$PhD <- 0
df$PhD[grep("#PhD",df$text, ignore.case = TRUE)] <- 1
df$postdoc <- 0
df$postdoc[grep("#postdoc",df$text, ignore.case = TRUE)] <- 1

##locate phd vs postdoc jobs as indices of start and end position in text string
pd<- str_locate_all(pattern ='#postdoc',tolower(df$text))
pd <- lapply(pd,as.data.frame)
pd <- lapply(pd, function(x){if(nrow(x) == 0) {x[1,1:2]<-0}; return(x)})
pd <- lapply(pd, function(x){if(nrow(x) > 1) {x<-x[1,]}; return(x)}) # keep 1st instance only; likely to be the hit
pd <- do.call(rbind, pd)

ph<- str_locate_all(pattern ='#phd',tolower(df$text))
ph <- lapply(ph,as.data.frame)
ph <- lapply(ph, function(x){if(nrow(x) == 0) {x[1,1:2]<-0}; return(x)})
ph <- lapply(ph, function(x){if(nrow(x) > 1) {x<-x[1,]}; return(x)})
ph <- do.call(rbind, ph)
names(ph) <- c('startph','endph')
pd <- cbind(pd, ph)

## create substrings that will capture the numbers relating to PhDs and postdocs, respectively
df$pdnums <- NA

pdlastphfirst <- which(pd$start > pd$startph & pd$startph!=0)
df$pdnums[pdlastphfirst] <-
  substring(df$text[pdlastphfirst], 
            first=pd$endph[pdlastphfirst], #start from where PhD ended
            last=pd$end[pdlastphfirst]) # finish at end of postdoc

pdnophd <- which(pd$start > 0 & pd$startph==0)
df$pdnums[pdnophd] <-
  substring(df$text[pdnophd], 
            first=1,  # start at beinning of string
            last=pd$end[pdnophd]) # end at end of postdoc

pdfirstphdlast <- which(pd$start > 0 & pd$start < pd$startph)
df$pdnums[pdfirstphdlast] <-
  substring(df$text[pdfirstphdlast], 
            first=1, # start at beginning of string
            last=pd$end[pdfirstphdlast]) # finish at end of postdoc

# not 100% but for simplicity remove numbers associated with years
#   .. (some have info on both how many and how many years....)
df$pdnums[grep('year', tolower(df$pdnums))] <-NA
df$pdnums[grep('10 tips', tolower(df$pdnums))] <- NA # happened on another rogue
df$pdnums[grep("this is the australian story", tolower(df$pdnums))] <- NA # and another - url before postdoc
#    which includes number

## do same for phd positions
df$phnums <- NA

phlastpdfirst <- which(pd$startph > pd$start & pd$start!=0)
df$phnums[phlastpdfirst] <-
  substring(df$text[phlastpdfirst], 
            first=pd$end[phlastpdfirst], # start where postdoc ended
            last=pd$endph[phlastpdfirst]) # end where phd ends

phnopd <- which(pd$startph > 0 & pd$start==0)
df$phnums[phnopd] <-
  substring(df$text[phnopd], 
            first=1, # note that pd$startph[phnopd]-5 would capture different noise; some strings have
            #   long gaps btw number and PhD but others list MSC positions first...etc.
            last=pd$endph[phnopd])

phfirstpdlast <- which(pd$startph > 0 & pd$startph < pd$start)
df$phnums[phfirstpdlast] <-
  substring(df$text[phfirstpdlast], 
            first=1, # start at beginning
            last=pd$endph[phfirstpdlast]) # end at end of phd

## create numeric from searching numbers within the substrings 
df$pdnumz <- as.numeric(str_extract_all(df$pdnums,"\\(?[0-9]+\\)?"))
df$pdnumz[is.na(df$pdnumz)] <- 0


pdnumz<- str_extract_all(df$pdnums,"\\(?[0-9]+\\)?")
pdnumz <- lapply(pdnumz, function(x){if(length(x) == 0) {x[1]<-0}; return(x)})
pdnumz <- lapply(pdnumz, function(x){if(length(x) > 1) {x<-x[2]}; return(x)}) # keep 2nd instance only
#   but this may not always appropriate
pdnumz <- lapply(pdnumz, as.numeric)
pdnumz <- do.call(rbind, pdnumz)


phnumz<- str_extract_all(df$phnums,"\\(?[0-9]+\\)?")
phnumz <- lapply(phnumz, function(x){if(length(x) == 0) {x[1]<-0}; return(x)})
phnumz <- lapply(phnumz, function(x){if(length(x) > 1) {x<-x[2]}; return(x)}) # keep 2nd instance only
#   but this may not always appropriate
phnumz <- lapply(phnumz, as.numeric)
phnumz <- do.call(rbind, phnumz)

## translate numbers into respective columns
df$PhD[which(phnumz>df$PhD)] <- phnumz[which(phnumz>df$PhD)]
df$postdoc[which(pdnumz>df$postdoc)] <- pdnumz[which(pdnumz>df$postdoc)]

sum(df$PhD)
sum(df$postdoc)

