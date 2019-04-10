#############################################################
## Text Mining  - QA
## Author : Wen Lee
## 4 April 2019
## Data Dependencies : Data

## Version control : 1.0
##############################################################
###############################################################
## Reading Raw Libraries required
###############################################################

library("tm")
library("slam")
library("wordcloud")
library(Rmpfr)
library(ggplot2)
library(tidyr)
library(proxy)
library(LDAvis)
library(topicmodels)
library(dplyr)
library(stringi)
library(tidytext)
library(plotly)
library("SnowballC")
library(lubridate)
library(xlsx)

##############################################################

## Raw Data Read in
###############################################################

#setwd("C:/Users/17wenl/Documents/text-mining-trial-master")

mydata<-read.csv("xxx.csv", header = TRUE)
df <- data.frame(mydata)

##checking data if needed
#names(mydata)
#head(mydata)

##Date Creation - formating date data

date<-as.Date(mydata$fdtmWhen,format="%d-%B-%y %H:%M:%S")

## Retrieving only subject message in <span> doc

docs <- gsub(".*<span>|</span>.*", "", mydata$fstrMessage)
#length(docs)
#docs[[1]]

##Converting file to Windows readable format

docs<-iconv(docs, "UTF-8", "WINDOWS-1252")

##Cleaning up the docs by removing weird letters and keywords

files<-gsub("[^[:alnum:][:blank:] + ? / \\ - ]", " ", docs)
files2<-gsub("&#39;m","",files)
files3 <-gsub("â ","",files2)
files4 <-gsub("Ä ","",files3)
files5 <-gsub("ï ","",files3)

##creating corpus which is main doc - should be around 38k rows
doc.vec <- VectorSource(files4)
corpus <-Corpus(doc.vec)

##Removing now stop words like to from etc..

#Setting the stopwords dictionaries and custom words
myStopwords <- c(stopwords("english"), "my", "custom", "words")

#Pre-processing and tranforming the Corpus
#myStopwords <- c(stopwords("english"), stopwords("SMART"))
your_corpus <- tm_map(corpus, content_transformer(tolower))
your_corpus <- tm_map(your_corpus, removeWords, myStopwords) 
your_corpus <- tm_map(your_corpus, removeNumbers)
your_corpus <- tm_map(your_corpus, removePunctuation)
your_corpus <- tm_map(your_corpus, stripWhitespace)
###################################################################################
## Creating of Document to Matrix which is a key step before creating any models

#Create a document term matrix (containing between 3 & Infinite characters)

myDtm <- DocumentTermMatrix(your_corpus, control=list(wordLengths=c(3,100)))

#sparse terms removed here - terms that dont occur frequently
myDtm2<- removeSparseTerms(myDtm, 0.999) 

#Now there are text where no words have occured - i.e rare sentences remove these

rowTotals <- apply(myDtm2 , 1, sum) #Removing 0 documents from Dtm2
dtm.new   <- myDtm2[rowTotals > 0, ] 

###################################################################################

##############     WORD CLOUD TIME    #######################

#####################################################
##############################################


#convert document term matrix to a term document matrix
myTdm <- t(dtm.new)
findFreqTerms(myTdm, 10000)

#To calculate which rows do not have any documents
m <- as.matrix(dtm.new)
rw0<-which(rowSums(m)==0)

#inspect(myTdm[467,201:1000])

#define tdm as matrix
m <- as.matrix(myTdm)

# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE)

# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

# plot wordcloud with maximum of 200 words
wordcloud(dm$word, dm$freq,  max.words=200,min.freq=1000,
random.order=FALSE,rot.per=.35, colors=brewer.pal(8, "Dark2"))

##############################################################################################################

#########################  MACHINE LEARNING LDA ####################################

#########################################################################################
##########################################################################################




## Trying a range of topics from 2 - 30 and pushing the system

seqk <- seq(2, 30, 1)
burnin <- 100
iter <- 100
keep <- 50
system.time(fitted_many <- lapply(seqk, function(k) topicmodels::LDA(dtm.new, k = k,
                                                     method = "Gibbs",control = list(burnin = burnin,
                                                                         iter = iter, keep = keep) )))

# extract logliks from each topic
logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])

harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

# compute harmonic means
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

ldaplot <- ggplot(data.frame(seqk, hm_many), aes(x=seqk, y=hm_many)) + geom_path(lwd=1.5) +
   theme(text = element_text(family= NULL),
         axis.title.y=element_text(vjust=1, size=16),
         axis.title.x=element_text(vjust=-.5, size=16),
         axis.text=element_text(size=16),
         plot.title=element_text(size=20)) +
   xlab('Number of Topics') +
   ylab('Harmonic Mean') +
      annotate("text", x = 25, y = -80000, label = paste("The optimal number of topics is", seqk[which.max(hm_many)])) +
   ggtitle("Latent Dirichlet Allocation Analysis of eCorrie")

## Ploting out the optimal number of topics

ldaplot1<-ggplotly(ldaplot)
ldaplot1

###########################################################################

######## SENSE OF THE TOPICS ############################################

#########################################################################3

#Making sense of the topics - I am taking based on the chart the number of topics to be 10

system.time(llis.model <- topicmodels::LDA(dtm.new, 10, method = "Gibbs", control = list(iter=1000, seed = 0622)))
 llis.topics <- topicmodels::topics(llis.model, 1)
 llis.terms <- as.data.frame(topicmodels::terms(llis.model, 10), stringsAsFactors = TRUE)
 llis.terms[1:10]

## Merging the topics back to the raw data fields

doctopics.df <- as.data.frame(llis.topics)
doctopics.df <- doctopics.df %>% mutate(id = row_number())
doctopics.df$id <- as.integer(doctopics.df$id)

raw_docs_dataframe <- data.frame(docs[rowTotals > 0] ,date[rowTotals > 0])
raw.df <- raw_docs_dataframe %>% mutate(id = row_number())
raw.df$id <- as.integer(raw.df$id)
llis.display <- merge(raw.df , doctopics.df, by = "id")

write.table(llis.display, "mydata.csv", sep=",")

## Outputing the topics with the raw data into an excel file

## Viewing the topics over time

words_by_time <- llis.display %>%
  mutate(time_floor = floor_date(date.rowTotals...0., unit = "1 day")) %>%
  mutate(topics =as.character(llis.topics)) %>%
  count(time_floor, topics) %>%
  group_by(time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(topics) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n)

words_by_time

timeplot<- words_by_time %>% ggplot(aes(time_floor, count/time_total, color = topics)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Topic frequency")

p<-ggplotly(timeplot)
p

#####################################################################################

##Visualizing it using LDA Vis as an interactive dashboard
##llis.model,corpus,myDtm,
##

####################################################################################

term.table <- sort(table(unlist(dtm.new)), decreasing = TRUE)

#term.frequency <- as.integer(myDtm$dimnames$Terms)


phi <- posterior(llis.model)$terms %>% as.matrix
 theta <- posterior(llis.model)$topics %>% as.matrix
  vocab <- colnames(phi)
    doc_length <- vector()

     for (i in 1:length(corpus)) {
          temp <- paste(corpus[[i]]$content, collapse = ' ')
          doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
     }

doc_length_n   <- doc_length[rowTotals > 0] 

temp_frequency=slam::col_sums(dtm.new) ##fixed bug

freq_matrix <- data.frame(ST = names(temp_frequency),
Freq = temp_frequency)
 
json_lda <- createJSONl(phi = phi, theta = theta,
                                    vocab = vocab,
                                    doc.length = doc_length_n,
                                    term.frequency = freq_matrix$Freq,
						reorder.topics = FALSE)



serVis(json_lda)

serVis(json_lda,outdir="sample1",open.browser=FALSE)





##########################################################################

########################################################################


topicTerms <- tidyr::gather(llis.terms, Topic)
topicTerms <- cbind(topicTerms, Rank = rep(1:30))
topTerms <- dplyr::filter(topicTerms, Rank < 4)
topTerms <- dplyr::mutate(topTerms, Topic = stringr::word(Topic, 2))
topTerms$Topic <- as.numeric(topTerms$Topic)

topicLabel <- data.frame()
for (i in 1:10){
     z <- dplyr::filter(topTerms, Topic == i)
     l <- as.data.frame(paste(z[1,2], z[2,2], z[3,2], sep = " " ), stringsAsFactors = FALSE)
     topicLabel <- rbind(topicLabel, l)

}
colnames(topicLabel) <- c("Label")
topicLabel


## corellated topics

theta <- as.data.frame(topicmodels::posterior(llis.model)$topics)
head(theta[1:5])

x <- as.data.frame(row.names(theta), stringsAsFactors = FALSE)
colnames(x) <- c("id")
x$LessonId <- as.numeric(x$id)
theta2 <- cbind(x, theta)



theta2 <- dplyr::left_join(theta2, FirstCategorybyLesson, by = "id")



llistopic.dtm <- tm::DocumentTermMatrix(your_corpus, control = list(stemming = TRUE, stopwords = TRUE,
                                    minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE))


    yy <- data.frame(ST = colnames(j),
                               Freq = colSums(j))

