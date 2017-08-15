### References:
### https://rstudio-pubs-static.s3.amazonaws.com/163802_0f005a14bcfb4c4b8ee17ac8a8e6c3e9.html
### http://davidmeza1.github.io/2015/07/20/topic-modeling-in-R.html
###

library(tm)
library(SnowballC)
library(topicmodels)
library(Rmpfr)

# Load the responses
setwd('~/EE Survey Analysis')

# Load virtual experience survey response data
virtual_xp_data <- readLines(file('Virtual_exp_responses.txt'))

# Remove missing information
virtual_xp_data <- grep('n/a', virtual_xp_data, ignore.case = TRUE, value = TRUE, invert = TRUE)
virtual_xp_data <- virtual_xp_data[virtual_xp_data != '']

# Create corpus
vs <- VectorSource(virtual_xp_data)
response_corpus <- VCorpus(vs, readerControl = list(readPlain, language = 'en', load = TRUE))

# Preview the data
topten_words <- function(dtm) {
  dtm.matrix <- as.matrix(dtm)
  wordcount <- colSums(dtm.matrix)
  topten <- head(sort(wordcount, decreasing=TRUE), 10)
  
  library(reshape2)
  library(ggplot2)
  
  dfplot <- as.data.frame(melt(topten))
  dfplot$word <- dimnames(dfplot)[[1]]
  dfplot$word <- factor(dfplot$word,
                        levels=dfplot$word[order(dfplot$value,
                                                 decreasing=TRUE)])
  
  fig <- ggplot(dfplot, aes(x=word, y=value)) + geom_bar(stat="identity")
  fig <- fig + xlab("Word in Corpus")
  fig <- fig + ylab("Count")
  print(fig)
}

# Quick dirty view
response_dtm <- DocumentTermMatrix(response_corpus)
topten_words(response_dtm)
view_text <- data.frame(text=unlist(sapply(response_corpus, `[`, "content")), stringsAsFactors=F)
view_text[1:20,]

# Data preproessing
response_corpus <- tm_map(response_corpus, content_transformer(tolower))
response_corpus <- tm_map(response_corpus, removeWords, stopwords('SMART'))
response_corpus <- tm_map(response_corpus, removePunctuation)
response_corpus <- tm_map(response_corpus, removeNumbers)
response_corpus <- tm_map(response_corpus, stemDocument)

# Create DTM
response_dtm <- DocumentTermMatrix(response_corpus)
topten_words(response_dtm)
view_text <- data.frame(text=unlist(sapply(response_corpus, `[`, "content")), stringsAsFactors=F)
view_text[1:20,]

# Calculate tf-idf score
response_tfidf <- tapply(response_dtm$v/slam::row_sums(response_dtm)[response_dtm$i], response_dtm$j, mean) *
  log2(nDocs(response_dtm)/slam::col_sums(response_dtm > 0))
summary(response_tfidf)

# Potentially try to remove terms with very low tf-idf score

# Remove all documents with no words
rowTotals <- apply(response_dtm, 1, sum)
response_dtm <- response_dtm[rowTotals > 0,]

# Choosing K topics
harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

# Fitting LDA model
k <- 5
burnin <- 1000
iter <- 1000
keep <- 50
fitted <- LDA(response_dtm, k = k, method = "Gibbs", control = list(burnin = burnin, iter = iter, keep = keep) )
## assuming that burnin is a multiple of keep
logLiks <- fitted@logLiks[-c(1:(burnin/keep))]

## This returns the harmomnic mean for k = 5 topics.
harmonicMean(logLiks)

# Fitting models with different values of k
seqk <- seq(2, 50, 1)
burnin <- 1000
iter <- 1000
keep <- 50
system.time(fitted_many <- lapply(seqk, function(k) LDA(response_dtm, k = k, 
                                                        method = "Gibbs", control = list(burnin = burnin,
                                                        iter = iter, keep = keep) )))

# extract logliks from each topic
logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])

# compute harmonic means
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))


# Plot the harmonic means vs k
ldaplot <- ggplot(data.frame(seqk, hm_many), aes(x=seqk, y=hm_many)) + geom_path(lwd=1.5) +
  theme(text = element_text(family= NULL),
        axis.title.y=element_text(vjust=1, size=16),
        axis.title.x=element_text(vjust=-.5, size=16),
        axis.text=element_text(size=16),
        plot.title=element_text(size=20)) +
  xlab('Number of Topics') +
  ylab('Harmonic Mean') +
  # annotate("text", x = 25, y = -80000, label = paste("The optimal number of topics is", seqk[which.max(hm_many)])) +
  ggtitle(expression(atop("Latent Dirichlet Allocation Analysis of Virtual Experience Survey", 
                     atop(italic("How many distinct topics in the abstracts?"), ""))))
ldaplot

# Best value of k
k_best <- seqk[which.max(hm_many)]
# 11

number_of_topics <- 5
terms_per_topic <- 10

# Fit the optimal model
response.model <- LDA(response_dtm, k = number_of_topics, method = "Gibbs", control = list(iter=2000, seed = 0622))

# Exploring the topics
virt_response.topics <- topics(response.model, 1)
# Top 10 terms for each topic
virt_response.terms <- as.data.frame(terms(response.model, terms_per_topic), stringsAsFactors = FALSE)
virt_response.terms[1:number_of_topics]







############################################
#Alternative productivity tools
############################################

# Load virtual experience survey response data
alt_tools_data <- readLines(file('Alternative_tools_responses.txt'))

# Remove missing information
alt_tools_data <- grep('n/a', alt_tools_data, ignore.case = TRUE, value = TRUE, invert = TRUE)
alt_tools_data <- alt_tools_data[alt_tools_data != '']

# Create corpus
alt_vs <- VectorSource(alt_tools_data)
alt_response_corpus <- VCorpus(alt_vs, readerControl = list(readPlain, language = 'en', load = TRUE))

# Quick dirty view
alt_response_dtm <- DocumentTermMatrix(alt_response_corpus)
topten_words(alt_response_dtm)
alt_view_text <- data.frame(text=unlist(sapply(alt_response_corpus, `[`, "content")), stringsAsFactors=F)
alt_view_text[1:20,]

# Data preproessing
alt_response_corpus <- tm_map(alt_response_corpus, content_transformer(tolower))
alt_response_corpus <- tm_map(alt_response_corpus, removeWords, stopwords('SMART'))
alt_response_corpus <- tm_map(alt_response_corpus, removePunctuation)
alt_response_corpus <- tm_map(alt_response_corpus, removeNumbers)
alt_response_corpus <- tm_map(alt_response_corpus, stemDocument)

# Create DTM
alt_response_dtm <- DocumentTermMatrix(alt_response_corpus)
topten_words(alt_response_dtm)
alt_view_text <- data.frame(text=unlist(sapply(alt_response_corpus, `[`, "content")), stringsAsFactors=F)
alt_view_text[1:20,]

# Remove all documents with no words
rowTotals <- apply(alt_response_dtm, 1, sum)
alt_response_dtm <- alt_response_dtm[rowTotals > 0,]

alt_fitted <- LDA(alt_response_dtm, k = k, method = "Gibbs", control = list(burnin = burnin, iter = iter, keep = keep) )
## assuming that burnin is a multiple of keep
logLiks <- alt_fitted@logLiks[-c(1:(burnin/keep))]

## This returns the harmomnic mean for k = 5 topics.
harmonicMean(logLiks)

# Fitting models with different values of k
seqk <- seq(2, 50, 1)
burnin <- 1000
iter <- 1000
keep <- 50
system.time(alt_fitted_many <- lapply(seqk, function(k) LDA(alt_response_dtm, k = k, 
                                                        method = "Gibbs", control = list(burnin = burnin,
                                                                                         iter = iter, keep = keep) )))

# extract logliks from each topic
alt_logLiks_many <- lapply(alt_fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])

# compute harmonic means
alt_hm_many <- sapply(alt_logLiks_many, function(h) harmonicMean(h))


# Plot the harmonic means vs k
ldaplot <- ggplot(data.frame(seqk, alt_hm_many), aes(x=seqk, y=alt_hm_many)) + geom_path(lwd=1.5) +
  theme(text = element_text(family= NULL),
        axis.title.y=element_text(vjust=1, size=16),
        axis.title.x=element_text(vjust=-.5, size=16),
        axis.text=element_text(size=16),
        plot.title=element_text(size=20)) +
  xlab('Number of Topics') +
  ylab('Harmonic Mean') +
  # annotate("text", x = 25, y = -80000, label = paste("The optimal number of topics is", seqk[which.max(hm_many)])) +
  ggtitle(expression(atop("Latent Dirichlet Allocation Analysis of Virtual Experience Survey", 
                          atop(italic("How many distinct topics in the abstracts?"), ""))))
ldaplot

# Best value of k
k_best <- seqk[which.max(alt_hm_many)]
# 11

number_of_topics <- 5
terms_per_topic <- 10

# Fit the optimal model
alt.response.model <- LDA(alt_response_dtm, k = number_of_topics, method = "Gibbs", control = list(iter=2000, seed = 0622))

# Exploring the topics
alt_response.topics <- topics(alt.response.model, 1)
# Top 10 terms for each topic
alt_response.terms <- as.data.frame(terms(alt.response.model, terms_per_topic), stringsAsFactors = FALSE)
alt_response.terms[1:number_of_topics]

