


library("tm")
library("slam")
library("wordcloud")
library(Rmpfr)
library(ggplot2)
library(tidyr)
library(proxy)
library(LDAvis)
library(topicmodels,lib.loc="C:/NotBackedUp/R-3.4.3/library")

     library(dplyr)
     library(stringi)
     library(LDAvis)

setwd("C:/NotBackedUp")

mydata<-read.csv("transcript.csv", header = TRUE)
df <- data.frame(mydata)
names(mydata)
head(mydata)
docs = mydata[,"FreeText"] 
length(docs)
docs[[1]]

files<-gsub("[^[:alnum:][:blank:] + ? / \\ - ]", " ", docs)


doc.vec <- VectorSource(files)

corpus <-Corpus(doc.vec)

#Load the text mining package(s)
library("SnowballC")

#Setting the stopwords dictionaries and custom words
myStopwords <- c(stopwords("english"), "my", "custom", "words")

#Pre-processing and tranforming the Corpus
#myStopwords <- c(stopwords("english"), stopwords("SMART"))
your_corpus <- tm_map(corpus, content_transformer(tolower))
your_corpus <- tm_map(your_corpus, removeWords, myStopwords) 
your_corpus <- tm_map(your_corpus, removeNumbers)
your_corpus <- tm_map(your_corpus, removePunctuation)
your_corpus <- tm_map(your_corpus, stripWhitespace)

#Create a document term matrix (containing between 3 & Infinite characters)
myDtm <- DocumentTermMatrix(your_corpus, control=list(wordLengths=c(3,Inf)))

#show the document terms matrix summary
myDtm

#Find frequent terms of atleast 500 occurence
findFreqTerms(myDtm, 500)

#slam: provides data structures and algorithms for sparse arrays and matrices


#convert document term matrix to a term document matrix
myTdm <- t(myDtm)

#To calculate which rows do not have any documents
m <- as.matrix(myDtm)

rw0<-which(rowSums(m)==0)

#inspect(myTdm[467,201:1000])

#define tdm as matrix
m <- as.matrix(myTdm)

# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE)

# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

# plot wordcloud with maximum of 200 words
wordcloud(dm$word, dm$freq,  max.words=200, random.order=FALSE,rot.per=.2, colors=brewer.pal(9, "Dark2"))


##############################################################################################################

##trying another way

seqk <- seq(2, 100, 1)
burnin <- 1000
iter <- 1000
keep <- 50
system.time(fitted_many <- lapply(seqk, function(k) topicmodels::LDA(myDtm, k = k,
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
   ggtitle(expression(atop("Latent Dirichlet Allocation Analysis of NEN LLIS", atop(italic("How many distinct topics in the abstracts?"), ""))))


system.time(llis.model <- topicmodels::LDA(myDtm, 5, method = "Gibbs", control = list(iter=2000, seed = 0622)))
 llis.topics <- topicmodels::topics(llis.model, 1)
 llis.terms <- as.data.frame(topicmodels::terms(llis.model, 30), stringsAsFactors = FALSE)
 llis.terms[1:5]

#Making sense of the topics

doctopics.df <- as.data.frame(llis.topics)
doctopics.df <- dplyr::transmute(doctopics.df, id = rownames(doctopics.df), Topic = llis.topics)
doctopics.df$id <- as.integer(doctopics.df$id)
labels<-c("id","FreeText")
llis.display <- dplyr::inner_join(mydata[,labels] , doctopics.df, by = "id")

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





##Visualizing it
##llis.model,corpus,myDtm,
##



term.table <- sort(table(unlist(myDtm)), decreasing = TRUE)

term.frequency <- as.integer(myDtm$dimnames$Terms)


phi <- posterior(llis.model)$terms %>% as.matrix
 theta <- posterior(llis.model)$topics %>% as.matrix
  vocab <- colnames(phi)
    doc_length <- vector()

     for (i in 1:length(corpus)) {
          temp <- paste(corpus[[i]]$content, collapse = ' ')
          doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
     }

temp_frequency=slam::col_sums(myDtm) ##fixed bug

freq_matrix <- data.frame(ST = names(temp_frequency),
Freq = temp_frequency)
 
json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                    vocab = vocab,
                                    doc.length = doc_length,
                                    term.frequency = freq_matrix$Freq)



serVis(json_lda)


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

