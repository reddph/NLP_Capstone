require(tm)
require(doParallel)
require(SnowballC)
require(dplyr)
require(ggplot2)
require(grid)

options(java.parameters = "-Xmx4g") ## This is a runtime java option to prevent Java GC() from throwing an exception
library("RWeka")

registerDoParallel(cores=4)

setwd("C:\\Users\\phanindra.reddigari\\Documents\\R_Practice\\Capstone")

dataFeed <- getwd()

## Delete sample files for blogs, news, and twitter feeds
sapply(list.files(path="./SampleFeeds",pattern="*.sample.txt",full.names=TRUE),
       function(x) unlink(x, force=TRUE))

#################### Sample the twitter feed text feed ########################
con <- file("en_US.twitter.txt", "r") ### read binary mode
content <- readLines(con, encoding = "UTF-8") ## Read the first line of text 
close(con) ## It's important to close the connection when you are done

## Distribution of line lengths in the twitter feed
numLines <- length(content)
 
# ## Percent of the lines chosen randomly from the text file with no replacement
con <- file("en_US.twitter.txt", "r") ### read binary mode
content <- readLines(con, encoding = "UTF-8") ## Read the first line of text 
close(con) ## It's important to close the connection when you are done

## Distribution of line lengths in the twitter feed
numLines <- length(content)

## Percent of the lines chosen randomly from the text file with no replacement
samplePercent <- 30.0

sampleSize <- ceiling(numLines*samplePercent/100)

set.seed(1234)
randLineNum <- sample(seq(1,numLines), sampleSize, replace=FALSE)
sample.twitter.txt <- content[randLineNum]

#sapply(sample.twitter.txt,function(x))
con <- file("./SampleFeeds/en_US.twitter.sample.txt", "w") 
writeLines(sample.twitter.txt, con)
close(con)

###################### Sample the blogs text feed ########################
con <- file("en_US.blogs.txt", "r") 
content <- readLines(con,encoding = "UTF-8") ## Read the first line of text 
close(con) ## It's important to close the connection when you are done

numLines <- length(content)

sampleSize <- ceiling(numLines*samplePercent/100)

set.seed(1234)
randLineNum <- sample(seq(1,numLines), sampleSize, replace=FALSE)
sample.blogs.txt <- content[randLineNum]

con <- file("./SampleFeeds/en_US.blogs.sample.txt", "w") 
writeLines(sample.blogs.txt, con)
close(con)
# #################### Sample the news feed ########################
con <- file("en_US.news.txt", "r") 
content <- readLines(con,encoding = "UTF-8") ## Read the first line of text 
close(con) ## It's important to close the connection when you are done

numLines <- length(content)

sampleSize <- ceiling(numLines*samplePercent/100)

set.seed(1234)

randLineNum <- sample(seq(1,numLines), sampleSize, replace=FALSE)
sample.news.txt <- content[randLineNum]

con <- file("./SampleFeeds/en_US.news.sample.txt", "w") 
writeLines(sample.news.txt, con)
close(con)

#sampleText <- sent_detect_nlp(sample30pct) # splitting of text paragraphs into sentences.

###########################################################################

sampleFeed <- paste(getwd(),"SampleFeeds",sep="/")

corpus.temp <- VCorpus(DirSource(sampleFeed),
                       readerControl = list(reader=readPlain))

inspect(corpus.temp[1])

# Replace certain distracting patterns and to reduce the computing load
f <- content_transformer(function(x, pattern, replacement) gsub(pattern, " ", x))
g <- content_transformer(function(x, pattern, replacement) gsub(pattern, replacement, x))

## Read the profanities from the file
con <- file("profanities.txt", "r") ### read binary mode
profanities <- readLines(con, encoding = "UTF-8") ## Read the first line of text 
close(con) ## It's important to close the connection when you are done

#corpus.temp <- tm_map(corpus.temp, f, "/")
#corpus.temp <- tm_map(corpus.temp, f, "@")
#corpus.temp <- tm_map(corpus.temp, f, "\\|")

stopWords <- stopwords("english")

corpus.temp <- tm_map(corpus.temp, content_transformer(tolower))
corpus.temp <- tm_map(corpus.temp, removeNumbers)
#corpus.temp <- tm_map(corpus.temp, removeWords, stopWords)
corpus.temp <- tm_map(corpus.temp, removeWords, profanities)
corpus.temp <- tm_map(corpus.temp, removePunctuation)
corpus.temp <- tm_map(corpus.temp, stripWhitespace)

# Step 6: Heuristically complete stemmed words.
#corpus.final <- tm_map(corpus.final, stemCompletion, dictionary = usEnCorpus)

# tdm <- TermDocumentMatrix(corpus.temp)
# saveRDS(tdm,file="./tdm.rds")

tdm <- readRDS(file="./tdm.rds")

# Extract the matrix of word sparsity from term docuent matrix
mat <- as.matrix(tdm)

# Aggregate the word frequency from each document and arrange the word frequency in descenting order
# (highest frequency to lowest frequency)
freqSingle <- sort(rowSums(mat),decreasing=TRUE)

# Formulate a data frame of the word frequency and word name for further processing
topWords <- data.frame(single=names(freqSingle),freq=freqSingle,stringsAsFactors=FALSE)

# List out the top and bottom 100 words in the denser tdm
head(topWords, 30)
tail(topWords, 30)

# require(wordcloud)
# 
# set.seed(1234)
# wordcloud(words = topWords$word, freq = topWords$freq, min.freq=1,
#           max.words=500, random.order=FALSE, rot.per=0.35, 
#           colors=brewer.pal(8,"Dark2"))

topWords$Percent <- topWords$freq / sum(topWords$freq)
topWords$CumPercent <- cumsum(topWords$Percent)
topWords$RowNum=seq(1,nrow(topWords),1)

# Check the frequency of the most frequent words from the word cloud
inspect(tdm[as.vector(topWords[topWords$RowNum <= 30,]$single),])

# Compare with the frequency of the most frequent words from the word cloud for comparison with the sparser tdm
#inspect(tdmDense[as.vector(topWords[topWords$RowNum <= 30,]$single),])

picsDir <- "./Charts"

wcRef <- data.frame(perc95WC = max(topWords[topWords$CumPercent < 0.9501,]$RowNum),
                    perc99WC = max(topWords[topWords$CumPercent < 0.9901,]$RowNum),
                    perc992WC = max(topWords[topWords$CumPercent < 0.9921,]$RowNum))

wcRef

xxa <- topWords[topWords$CumPercent < 0.990001,]

jpeg(filename = paste(picsDir,"TopNWords.jpeg",sep="/"),pointsize=6,
     width = 1700, height = 1080,quality=100)

ggplot(xx,aes(y=CumPercent,x=RowNum)) + 
  geom_bar(stat="identity") +
  geom_vline(data=wcRef, aes(xintercept=medianWC),color="darkgreen",size=3.0) +
  geom_vline(data=wcRef, aes(xintercept=perc90WC),color="orange",size=3.0) +
  geom_vline(data=wcRef, aes(xintercept=perc95WC),color="red",size=3.0) +
  ggtitle("Cumulative Distribution of Top N words from all feeds") +
  theme(axis.title.y=element_text(size=25),axis.title.x=element_text(size=25),
        plot.title=element_text(size=35),
        axis.text.x=element_text(angle=90,size=20),
        axis.text.y=element_text(size=20),
        legend.position="none") +
  ylab("Cumulative Probability")+xlab("Top N word Count") +
  #coord_cartesian(ylim =c(0,1.0),xlim=c(1,max(xx$RowNum))) +
  coord_cartesian(ylim =c(0,1.0),xlim=c(0,25000)) +
  scale_y_continuous(breaks=seq(0,1.0,0.05)) + 
  #scale_x_continuous(breaks=seq(1,max(xx$RowNum),100))
  scale_x_continuous(breaks=seq(0,25000,1000)) +
  geom_text(data=wcRef, aes(x=medianWC+500, y=0.975, label="Median"),size=10) +
  geom_text(data=wcRef, aes(x=perc90WC+500, y=0.975, label="90 Percentile"),size=10) +
  geom_text(data=wcRef, aes(x=perc95WC+500, y=0.975, label="95 Percentile"),size=10)

dev.off()
####################################################################################
## Bigram tokenization and bigram distribution
####################################################################################

BigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}

#tdmBigram <- as.tdm(corpus.temp, control = list(tokenize = BigramTokenizer))

## Compute tdmBigram and then save it to disk as RDS file for subsequent loading
# tdmBigram <- TermDocumentMatrix(corpus.temp, control = list(tokenize = BigramTokenizer))
# saveRDS(tdmBigram,file="./tdmBigram.rds")

tdmBigram <- readRDS(file="./tdmBigram.rds")

dim(tdmBigram)

## Reduce the sparsity i.e., increase the incidence of terms in 2 out of 3 documents
## This will improve coverage with fewer words by removing words occurring mostly in one document
#tdmBigramDense <- removeSparseTerms(tdmBigram, 0.66)

# this reduces the sparsity to 22%
#matBigram <- as.matrix(tdmBigramDense)

matBigram <- as.matrix(tdmBigram)

# Aggregate the word frequency from each document and arrange the word frequency in descenting order
# (highest frequency to lowest frequency)
freqBigram <- sort(rowSums(matBigram),decreasing=TRUE)

# Formulate a data frame of the word frequency and word name for further processing
topBigrams <- data.frame(freq=freqBigram,stringsAsFactors=FALSE)

# List out the top and bottom 100 words in the denser tdm
head(topBigrams, 30)
tail(topBigrams, 30)

set.seed(1234)
wordcloud(words = topBigrams$word, freq = topBigrams$freq, min.freq=1,
          max.words=240, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8,"Dark2"))

topBigrams$Percent <- topBigrams$freq / sum(topBigrams$freq)
topBigrams$CumPercent <- cumsum(topBigrams$Percent)
topBigrams$RowNum=seq(1,nrow(topBigrams),1)

wcRef <- data.frame(medianWC = max(topBigrams[topBigrams$CumPercent < 0.75001,]$RowNum),
                    perc90WC = max(topBigrams[topBigrams$CumPercent < 0.90001,]$RowNum),
                    perc95WC = max(topBigrams[topBigrams$CumPercent < 0.95001,]$RowNum),
                    stringsAsFactors=FALSE)

wcRef

#xxb <- topBigrams[topBigrams$CumPercent < 0.75001,]
xxb <- topBigrams[topBigrams$freq > 7,]

jpeg(filename = paste(picsDir,"TopNBigrams.jpeg",sep="/"),pointsize=6,
     width = 1700, height = 1080,quality=100)

ggplot(xx,aes(y=CumPercent,x=RowNum)) + 
  geom_bar(stat="identity") +
  geom_vline(data=wcRef, aes(xintercept=medianWC),color="darkgreen",size=3.0) +
  geom_vline(data=wcRef, aes(xintercept=perc90WC),color="orange",size=3.0) +
  geom_vline(data=wcRef, aes(xintercept=perc95WC),color="red",size=3.0) +
  ggtitle("Cumulative Distribution of Top N Bigrams from all feeds") +
  theme(axis.title.y=element_text(size=25),axis.title.x=element_text(size=25),
        plot.title=element_text(size=35),
        axis.text.x=element_text(angle=90,size=20),
        axis.text.y=element_text(size=20),
        legend.position="none") +
  ylab("Cumulative Probability")+xlab("Top N Bigram Count") +
  #coord_cartesian(ylim =c(0,1.0),xlim=c(1,max(xx$RowNum))) +
  coord_cartesian(ylim =c(0,1.0),xlim=c(0,240000)) +
  scale_y_continuous(breaks=seq(0,1.0,0.1)) + 
  #scale_x_continuous(breaks=seq(1,max(xx$RowNum),100))
  scale_x_continuous(breaks=seq(0,240000,24000)) +
  geom_text(data=wcRef, aes(x=medianWC+500, y=0.975, label="Median"),size=10) +
  geom_text(data=wcRef, aes(x=perc90WC+500, y=0.975, label="90 Percentile"),size=10)

dev.off()
###################################################################################
## Trigram tokenization and trigram distribution
###################################################################################

TrigramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 3, max = 3))}

## Compute tdmBigram and then save it to disk as RDS file for subsequent loading
# tdmTrigram <- TermDocumentMatrix(corpus.temp, control = list(tokenize = TrigramTokenizer))
# saveRDS(tdmTrigram,file="./tdmTrigram.rds")

tdmTrigram <- readRDS(file="./tdmTrigram.rds")

dim(tdmTrigram)

## Reduce the sparsity i.e., increase the incidence of terms in 2 out of 3 documents
## This will improve coverage with fewer words by removing words occurring mostly in one document
tdmTrigramDense <- removeSparseTerms(tdmTrigram, 0.66)
#tdmTrigramDense <- tdmTrigram

tdmTrigramDense

matTrigram <- as.matrix(tdmTrigramDense)

# Aggregate the word frequency from each document and arrange the word frequency in descenting order
# (highest frequency to lowest frequency)
freqTrigram <- sort(rowSums(matTrigram),decreasing=TRUE)

# Formulate a data frame of the word frequency and word name for further processing
topTrigrams <- data.frame(freq=freqTrigram,stringsAsFactors=FALSE)

# List out the top and bottom 100 words in the denser tdm
head(topTrigrams, 10)
tail(topTrigrams, 10)

set.seed(1234)
wordcloud(words = topTrigrams$word, freq = topTrigrams$freq, min.freq=1,
          max.words=180, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8,"Dark2"))


topTrigrams$Percent <- topTrigrams$freq / sum(topTrigrams$freq)
topTrigrams$CumPercent <- cumsum(topTrigrams$Percent)
topTrigrams$RowNum=seq(1,nrow(topTrigrams),1)

# Check the frequency of the most frequent words from the word cloud for comparison with the orignal tdm
inspect(tdmTrigramDense[as.vector(topTrigrams[topTrigrams$RowNum <= 30,]),])

wcRef <- data.frame(medianWC = max(topTrigrams[topTrigrams$CumPercent < 0.4001,]$RowNum),
                    perc90WC = max(topTrigrams[topTrigrams$CumPercent < 0.9001,]$RowNum),
                    perc95WC = max(topTrigrams[topTrigrams$CumPercent < 0.9501,]$RowNum))

wcRef

xxc <- topTrigrams[topTrigrams$CumPercent < 0.75,]

jpeg(filename = paste(picsDir,"TopNTrigrams.jpeg",sep="/"),pointsize=6,
     width = 1700, height = 1080,quality=100)

ggplot(xx,aes(y=CumPercent,x=RowNum)) + 
  geom_bar(stat="identity") +
  geom_vline(data=wcRef, aes(xintercept=medianWC),color="darkgreen",size=3.0) +
  geom_vline(data=wcRef, aes(xintercept=perc90WC),color="orange",size=3.0) +
  geom_vline(data=wcRef, aes(xintercept=perc95WC),color="red",size=3.0) +
  ggtitle("Cumulative Distribution of Top N Trigrams from all feeds") +
  theme(axis.title.y=element_text(size=25),axis.title.x=element_text(size=25),
        plot.title=element_text(size=35),
        axis.text.x=element_text(angle=90,size=20),
        axis.text.y=element_text(size=20),
        legend.position="none") +
  ylab("Cumulative Probability")+xlab("Top N Trigram Count") +
  #coord_cartesian(ylim =c(0,1.0),xlim=c(1,max(xx$RowNum))) +
  coord_cartesian(ylim =c(0,1.0),xlim=c(0,40000)) +
  scale_y_continuous(breaks=seq(0,1.0,0.05)) + 
  #scale_x_continuous(breaks=seq(1,max(xx$RowNum),100))
  scale_x_continuous(breaks=seq(0,40000,4000)) +
  geom_text(data=wcRef, aes(x=medianWC+500, y=0.975, label="Median"),size=10) +
  geom_text(data=wcRef, aes(x=perc90WC+500, y=0.975, label="90 Percentile"),size=10) +
  geom_text(data=wcRef, aes(x=perc95WC+500, y=0.975, label="95 Percentile"),size=10)

dev.off()

###################################################################################
## 4-Gram tokenization and trigram distribution
###################################################################################

QuadgramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 4, max = 4))}

## Compute tdmBigram and then save it to disk as RDS file for subsequent loading
# tdmQuadgram <- TermDocumentMatrix(corpus.temp, control = list(tokenize = QuadgramTokenizer))
# saveRDS(tdmQuadgram,file="./tdmQuadgram.rds")

tdmQuadgram <- readRDS(file="./tdmQuadgram.rds")

dim(tdmQuadgram)

## Reduce the sparsity i.e., increase the incidence of terms in 2 out of 3 documents
## This will improve coverage with fewer words by removing words occurring mostly in one document
## tdmQuadgramDense <- removeSparseTerms(tdmQuadgram, 0.40)

tdmQuadgram

matQuadgram <- as.matrix(tdmQuadgram)

# Aggregate the word frequency from each document and arrange the word frequency in descenting order
# (highest frequency to lowest frequency)
freqQuadgram <- sort(rowSums(matQuadgram),decreasing=TRUE)

# Formulate a data frame of the word frequency and word name for further processing
topQuadgrams <- data.frame(freq=freqQuadgram,stringsAsFactors=FALSE)

# List out the top and bottom 100 words in the denser tdm
head(topQuadgrams, 10)
tail(topQuadgrams, 10)

set.seed(1234)
wordcloud(words = topQuadgrams$word, freq = topQuadgrams$freq, min.freq=1,
          max.words=180, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8,"Dark2"))

topQuadgrams$Percent <- topQuadgrams$freq / sum(topQuadgrams$freq)
topQuadgrams$CumPercent <- cumsum(topQuadgrams$Percent)
topQuadgrams$RowNum=seq(1,nrow(topQuadgrams),1)

# Check the frequency of the most frequent words from the word cloud for comparison with the orignal tdm
# To limit data size for the app, we got to draw a threshold. Let us choose freq to be 2 or higher for 4-grams
# xxd <- topQuadgrams[topQuadgrams$freq > 4,]
xxd <- topQuadgrams[topQuadgrams$CumPercent < 0.10,]
#############################################################################
## 5-Gram
#############################################################################

PentagramTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 5, max = 5))}

tdmPentagram <- TermDocumentMatrix(corpus.temp, control = list(tokenize = PentagramTokenizer))
saveRDS(tdmPentagram,file="./tdmPentagram.rds")

matPentagram <- as.matrix(tdmPentagram)

# Aggregate the word frequency from each document and arrange the word frequency in descenting order
# (highest frequency to lowest frequency)
freqPentagram <- sort(rowSums(matPentagram),decreasing=TRUE)

# Formulate a data frame of the word frequency and word name for further processing
topPentagrams <- data.frame(freq=freqPentagram,stringsAsFactors=FALSE)

# List out the top and bottom 100 words in the denser tdm
head(topPentagrams, 10)
tail(topPentagrams, 10)

# set.seed(1234)
# wordcloud(words = topPentagrams$word, freq = topPentagrams$freq, min.freq=1,
#           max.words=180, random.order=FALSE, rot.per=0.35, 
#           colors=brewer.pal(8,"Dark2"))

topPentagrams$Percent <- topPentagrams$freq / sum(topPentagrams$freq)
topPentagrams$CumPercent <- cumsum(topPentagrams$Percent)
topPentagrams$RowNum=seq(1,nrow(topPentagrams),1)

xxp <- topPentagrams[topPentagrams$CumPercent < 0.400001,]
############################################################################################

## Building referential constraints for hash lookups between 4-Gram, 3-Gram, 2-Gram, and 1-Gram data frames

## Step 1: Connecting 4-Grams to 3-Grams and the latest 1-Gram

test10 <- sapply(rownames(xxd),FUN=function(x){
  t <- strsplit(x,split=" ")
  paste(t[[1]][1],t[[1]][2],t[[1]][3],sep=" ")
})

xxd$trigram <- sapply(1:length(test10),FUN=function(i)test10[[i]])

test12 <- sapply(rownames(xxd),FUN=function(x) {
  t <- strsplit(x,split=" ")
  t[[1]][4]
})

xxd$single <- sapply(1:length(test12),FUN=function(i)test12[[i]])

# quadTrigrams <- paste(firstWord,secondWord,thirdWord,sep=" ")
# xxd$trigram <- quadTrigrams

diff1 <- setdiff(xxd$trigram,rownames(xxc))

subset1 <- topTrigrams[rownames(topTrigrams) %in% diff1,]
xxc <- rbind(xxc,subset1)

diff1 <- setdiff(xxd$trigram,rownames(xxc))

# Still there are some triples in quads which are not captured by the trigram subset
subset3 <- xxd[xxd$trigram %in% diff1,]

xxd <- xxd[!xxd$trigram %in% diff1,]

# Reconcile unmatched single indexes between 4-Grams and 1-Grams
diff2 <- setdiff(xxd$single,xxa$single)

# subset2 <- xxd[xxd$single %in% diff2,]
subset2 <- topWords[rownames(topWords) %in% diff2,]

# The result is a null subset. So we can treat them as Kneser-Ney words to be added to the singles list
n1 <- nrow(xxa)
extraSingles <- data.frame(single=diff2,freq=NA,Percent=NA,CumPercent=NA,RowNum=seq(n1+1,n1+length(diff2)),stringsAsFactors=FALSE)
xxa <- rbind(xxa,extraSingles)

#########################################################################################################################################
## Step 2: Connecting 3-Grams to 2-Grams and the latest 1-Gram
# xxc$single <- sapply(rownames(xxc),FUN=function(x)strsplit(x,split=" ")[[1]][3])
# firstWord <- sapply(rownames(xxc),FUN=function(x)strsplit(x,split=" ")[[1]][1])
# secondWord <- sapply(rownames(xxc),FUN=function(x)strsplit(x,split=" ")[[1]][2])
# 
# tgBigram <- paste(firstWord,secondWord,sep=" ")
# xxc$bigram <- tgBigram

test10 <- sapply(rownames(xxc),FUN=function(x){
  t <- strsplit(x,split=" ")
  paste(t[[1]][1],t[[1]][2],sep=" ")
})

xxc$bigram <- sapply(1:length(test10),FUN=function(i)test10[[i]])

test12 <- sapply(rownames(xxc),FUN=function(x) {
  t <- strsplit(x,split=" ")
  t[[1]][3]
})

xxc$single <- sapply(1:length(test12),FUN=function(i)test12[[i]])

## Reconcile indexes for bigrams in 3-Grams and 2-Gram hash lookup
diff1 <- setdiff(xxc$bigram,rownames(xxb))


subset1 <- topBigrams[rownames(topBigrams) %in% diff1,]

xxb <- rbind(xxb,subset1)

diff2 <- setdiff(xxc$single,xxa$single)
subset2 <- topWords[rownames(topWords) %in% diff2,]

# The subset2 is null

# Formulate a subset of xxa from xxc data frame based on the extra singles it has
n1 <- nrow(xxa)
extraSingles <- data.frame(single=diff2,freq=1,Percent=NA,CumPercent=NA,RowNum=seq(n1+1,n1+length(diff2)),stringsAsFactors=FALSE)

xxa <- rbind(xxa,extraSingles)

#####################################################################################################################################
## Step 3: Connecting 2-Grams to 1-Grams
xxb$single <- sapply(rownames(xxb),FUN=function(x)strsplit(x,split=" ")[[1]][2])
xxb$prefix <- sapply(rownames(xxb),FUN=function(x)strsplit(x,split=" ")[[1]][1])

diff1 <- setdiff(union(xxb$prefix,xxb$single),rownames(xxa))

n1 <- nrow(xxa)
extraSingles <- data.frame(single=diff1,freq=NA,Percent=NA,CumPercent=NA,RowNum=seq(n1+1,n1+length(diff1)),stringsAsFactors=FALSE)
rownames(extraSingles) <- extraSingles$single
xxa <- rbind(xxa,extraSingles)

## Compute cross-reference indexes between different N-Grams for fast hashlookup instead of grepping of the rownames
## The hash index lookups will be faster than grepping for the matching trigram lookup in quads, etc.

## Compute trigram reference index (index3) in 4-Gram hash from 3-Gram hash rows (foreign key relationship)
## index3 in 4-Gram hash is a lookup for matching trigram from the 3-Gram hash
xxDup <- xxd # create a backup just in case the join does not function as expected

xxRef <- data.frame(trigram=rownames(xxc),index3=row(xxc)[,1],stringsAsFactors=FALSE)
rownames <- rownames(xxd)
xxd <- left_join(xxd,xxRef)
rownames(xxd) <- rownames

sum(is.na(xxd$index3)) ## assert that this sum is zero

## Compute bigram reference index (index2) in 3-Gram hash from 2-Gram hash rows (foreign key relationship)
## index2 in 3-Gram hash is a lookup for matching bigram from the 2-Gram hash
xxDup <- xxc # create a backup just in case the join does not function as expected

xxRef <- data.frame(bigram=rownames(xxb),index2=row(xxb)[,1],stringsAsFactors=FALSE)
rownamesTriples <- rownames(xxc)
xxc <- left_join(xxc,xxRef)
rownames(xxc) <- rownamesTriples

sum(is.na(xxc$index2)) ## assert that this sum is zero

## Compute prefix reference index (index1) in 2-Gram hash from 1-Gram hash rows (foreign key relationship)
## index1 in 2-Gram hash is a lookup for matching prefix token from the 1-Gram hash
xxDup <- xxb # create a backup just in case the join does not function as expected

xxRef <- data.frame(prefix=rownames(xxa),index1=row(xxa)[,1],stringsAsFactors=FALSE)
rownamesDoubles <- rownames(xxb)
xxb <- left_join(xxb,xxRef)
rownames(xxb) <- rownamesDoubles

## Assert that the index1 is configured for all
sum(is.na(xxb$index1))

## Compute unigram reference index (index) in 4-Gram hash (foreign key relationship)
## index in 4-Gram hash is a lookup for matching unigram from the 1-Gram hash
xxDup <- xxd # create a backup just in case the join does not function as expected

xxRef <- data.frame(single=rownames(xxa),index=row(xxa)[,1],stringsAsFactors=FALSE)
rownamesQuads <- rownames(xxd)
xxd <- left_join(xxd,xxRef)
rownames(xxd) <- rownamesQuads

sum(is.na(xxd$index)) ## assert that this sum is zero

## Compute unigram reference index (index) in 3-Gram hash (foreign key relationship)
## index in 3-Gram hash is a lookup for matching unigram from the 1-Gram hash
xxDup <- xxc # create a backup just in case the join does not function as expected

rownamesTriples <- rownames(xxc)
xxc <- left_join(xxc,xxRef)
rownames(xxc) <- rownamesTriples

sum(is.na(xxc$index)) ## assert that this sum is zero

## Compute suffix reference index (index) in 2-Gram hash from 1-Gram hash rows (foreign key relationship)
## index in 2-Gram hash is a lookup for matching suffix token from the 1-Gram hash
xxDup <- xxb

rownamesDoubles <- rownames(xxb)
xxb <- left_join(xxb,xxRef)
rownames(xxb) <- rownamesDoubles

sum(is.na(xxb$index)) ## assert that this sum is zero

# Recompute the rownum in xxa to make sure the rows are computed
xxa$RowNum <- row(xxa)[,1]

ugset <- select(xxa,freq)
bgset <- select(xxb,freq,index1,index)
tgset <- select(xxc,freq,index2,index)
qgset <- select(xxd,freq,index3,index)

saveRDS(ugset,file="./ugset.rds")
saveRDS(bgset,file="./bgset.rds")
saveRDS(tgset,file="./tgset.rds")
saveRDS(qgset,file="./qgset.rds")



