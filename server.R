suppressWarnings(suppressMessages(library(shiny)))
suppressWarnings(suppressMessages(library(tm)))
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(stringr)))

library(shiny)
library(tm)
library(dplyr)
library(stringr)

stopWords <- stopwords("english")

## Read the profanities from the file
con <- file("profanities.txt", "r") ### read binary mode
profanities <- readLines(con, encoding = "UTF-8") ## Read the first line of text 
close(con) ## It's important to close the connection when you are done

ugset <- data.frame()
bgset <- data.frame()
tgset <- data.frame()
qgset <- data.frame()

f <- content_transformer(function(x, pattern, replacement) gsub(pattern, " ", x))

## Number of Top predictions
n5 <- 20

## global variable for data set initialization state
initUnigrams <- FALSE
initBigrams <- FALSE
initTrigrams <- FALSE
initQuadgrams <- FALSE

#predRanks <- data.frame()

### Retrieve test expression from the UI
# exp <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
# exp <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
# exp <- "Hey sunshine, can you follow me and make me the"
# exp <- "Very early observations on the Bills game: Offense still struggling but the"
# exp <- "Go on a romantic date at the"
# exp <- "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
# exp <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
# exp <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
# exp <- "Be grateful for the good times and keep the faith during the"
# exp <- "If this isn't the cutest thing you've ever seen, then you must be"
# exp <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
# exp <- "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
# exp <- "I'd give anything to see arctic monkeys this"
# exp <- "Talking to your mom has the same effect as a hug and helps reduce your"
# exp <- "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
# exp <- "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
# exp <- "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
# exp <- "Every inch of you is perfect from the bottom to the"
# exp <- "I’m thankful my childhood was filled with imagination and bruises from playing"
# exp <- "I like how the same people are in almost all of Adam Sandler's"
# exp <- "Why would he step into this cesspool when he can have his pick of"
# exp <- "It would die of old"
# exp <- "Though most people applauded plans for the new biomedical center, many deplored the potential loss of the"
# exp <- "He faces first-degree murder charge in woman's"
# exp <- "Let peace find a toe hold in this tumultuous"
#########################################

## Preprocess the test expression from UI with the same steps used for gathering the N-Grams

shinyServer(
  function(input, output) {
    output$text1 <- renderText({

      exp <- input$var
 
      # Create a Progress object
      progress <- shiny::Progress$new()
      progress$set(message = "Data Computation in progress", value = 0)
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())
      
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 10
        }
        progress$set(value = value, detail = detail)
      }
      
      ## Check for init state for data sets for unigrams, bigrams, trigrams, and quadgrams

      #if(length(grep("ugset", dsloads, fixed=TRUE)) == 0) {
      if(!initUnigrams) {
        
        # Set init to true
        initUnigrams <<- TRUE
        
        if (is.function(updateProgress)) {
          text <- "   Loading 1-Grams in Progress ..."
          updateProgress(detail = text)
        }
        
        ugset <<- readRDS(file="./ugset.rds")
        
        ## Add row numbers as additional column in all for easy crossreference lookups in the higher gram hash maps
        ## index refers to lookup for unigram embedded in the bigram or trigram or quadgram
        ## index2 refers to lookup for bigram embedded in the trigram
        ## index3 refers to lookup for trigram embedded in the quadgram
        
        ugset$rownum <<- row(ugset)[,1]
        ugset$Percent <<- ugset$freq / sum(ugset$freq,na.rm=TRUE)
      }
      
      if(!initBigrams) {
        
        initBigrams <<- TRUE
        
        if (is.function(updateProgress)) {
          text <- "   Loading 2-Grams in Progress ..."
          updateProgress(detail = text)
        }
        
        bgset <<- readRDS(file="./bgset.rds")
        
        ## Add row numbers as additional column in all for easy crossreference lookups in the higher gram hash maps
        ## index refers to lookup for unigram embedded in the bigram or trigram or quadgram
        ## index2 refers to lookup for bigram embedded in the trigram
        ## index3 refers to lookup for trigram embedded in the quadgram
        
        bgset$rownum <<- row(bgset)[,1]
      }
      
      #if(length(grep("tgset", dsloads, fixed=TRUE)) == 0) {
      if(!initTrigrams) {
        
        # Set init to true
        initTrigrams <<- TRUE
        
        if (is.function(updateProgress)) {
          text <- "   Loading 3-Grams in Progress ..."
          updateProgress(detail = text)
        }
        
        tgset <<- readRDS(file="./tgset.rds")
        
        ## Add row numbers as additional column in all for easy crossreference lookups in the higher gram hash maps
        ## index refers to lookup for unigram embedded in the bigram or trigram or quadgram
        ## index2 refers to lookup for bigram embedded in the trigram
        ## index3 refers to lookup for trigram embedded in the quadgram
        
        tgset$rownum <<- row(tgset)[,1]
      } 
      
      ##if(length(grep("qgset", dsloads, fixed=TRUE)) == 0) {
      if(!initQuadgrams) {
        
        # Set init to true
        initQuadgrams <<- TRUE
        
        if (is.function(updateProgress)) {
          text <- "   Loading 4-Grams in Progress ..."
          updateProgress(detail = text)
        }
        
        qgset <<- readRDS(file="./qgset.rds")
        
        ## Add row numbers as additional column in all for easy crossreference lookups in the higher gram hash maps
        ## index refers to lookup for unigram embedded in the bigram or trigram or quadgram
        ## index2 refers to lookup for bigram embedded in the trigram
        ## index3 refers to lookup for trigram embedded in the quadgram
        
        qgset$rownum <<- row(qgset)[,1]
      }          
      
      if (is.function(updateProgress)) {
        text <- "         Token Processing In Progress ..."
        updateProgress(detail = text)
      }
      
      exp <- str_trim(exp,side="both")
      
      refCorpus <- VCorpus(VectorSource(exp))
      
      refCorpus <- tm_map(refCorpus, f, "<.+>")
      
      ## Replacing “/”, “@” and “|” with space
      refCorpus <- tm_map(refCorpus, f, "“")
      refCorpus <- tm_map(refCorpus, f, "”")
      refCorpus <- tm_map(refCorpus, f, "’")
      refCorpus <- tm_map(refCorpus, f, "/")
      refCorpus <- tm_map(refCorpus, f, "@")
      refCorpus <- tm_map(refCorpus, f, "\\|")
      
      refCorpus <- tm_map(refCorpus, content_transformer(tolower))
      refCorpus <- tm_map(refCorpus, removeNumbers)
      #refCorpus <- tm_map(refCorpus, removeWords, stopWords)
      refCorpus <- tm_map(refCorpus, removeWords, profanities)
      refCorpus <- tm_map(refCorpus, removePunctuation)
      refCorpus <- tm_map(refCorpus, stripWhitespace)
      
      #inspect(refCorpus[1])
      
      exp_m <- content(refCorpus[[1]])
      
      exp_m <- str_trim(exp_m, side="both")
      
      if(nchar(exp_m) > 0 && !(nchar(exp_m) == 1 && exp_m != "i")) {
        
        ## As we are assuming markov chain model with N-Grams, where N=4,3,2, and 1. Extract partial bigram, trigram, and quadgra relative to the right end of the
        ## input string
        
        expTokens <- unlist(strsplit(exp_m, split=" ", fixed=TRUE))
        unigram <- ifelse(length(expTokens) > 0, expTokens[length(expTokens)], "")
        bigram <- ifelse(length(expTokens) > 1, paste(expTokens[length(expTokens)-1], unigram, sep=" "), "")
        trigram <- ifelse(length(expTokens) > 2, paste(expTokens[length(expTokens)-2], bigram, sep=" "), "")
        
        # Initialize the vector of candidate tokens for probability evaluation
        evalTokens <- vector()
        
        ## Step 1: Search the 3-Grams hash first to get relative counts for direct lookups for single index in 4-Gram (Quads) Hash
        
        found4gramMatches <- FALSE
        predWord <- NA
        candidates <- data.frame(nrow=0,ncol=0)
        if(nchar(trigram) > 0) {
          if(trigram %in% rownames(tgset)) {
            tglookups <- tgset[trigram,]$rownum
            candidates4gram <- qgset[!is.na(tglookups) & (qgset$index3 %in% tglookups),]
            if(nrow(candidates4gram) >= 1) {
              cat("Found trigram in 4-Gram\n")
              found4gramMatches = TRUE
              #predWord <- rownames(ugset[candidates[1,]$index,][1,])
            }
          } 
        }
        
        found3gramMatches <- FALSE
        if(nchar(bigram) > 0) {
          if(bigram %in% rownames(bgset)){
            ## Step 2: Search the 2-Grams hash first to get relative counts for direct lookups for single index in 3-Gram (Trigrams) Hash
            bglookups <- bgset[bigram,]$rownum
            candidates3gram <- tgset[!is.na(bglookups) & (tgset$index2 %in% bglookups),]
            if(nrow(candidates3gram) >= 1) {
              cat("Found bigram in 3-Gram\n")
              found3gramMatches <- TRUE    
              #predWord <- rownames(ugset[candidates[1,]$index,][1,])
            }
          }
        } 
        
        found2gramMatches <- FALSE
        if(nchar(unigram) > 0) {
          if(unigram %in% rownames(ugset)){
            ## Step 3: Search the 1-Grams hash first to get relative counts for direct lookups for single index in 2-Gram (Bigrams) Hash
            uglookups <- ugset[unigram,]$rownum
            candidates2gram <- bgset[!is.na(uglookups) & (bgset$index1 %in% uglookups),]
            if(nrow(candidates2gram) >= 1) {
              cat("Found unigram in 2-Gram\n")
              found2gramMatches <- TRUE
              #predWord <- rownames(ugset[candidates[1,]$index,][1,])
            }
          }
        }
        
        ## Compute the common candidates between Quads, Trigrams, Bigrams, and Unigrams for comparing weighted MLE probabilities
        
        # fn_splitLastToken <- function(x,n){
        #   t <- strsplit(x,split=" ")
        #   t[[1]][n]
        # }
        
        suffixQuadgram <- vector()
        suffixTrigram <- vector()
        suffixBigram <- vector()
        
        if(found4gramMatches) {
          test10 <- sapply(rownames(candidates4gram),FUN=function(x){
            t <- strsplit(x,split=" ")
            t[[1]][4]
          })
          suffixQuadgram <- sapply(1:length(test10),FUN=function(i)test10[[i]])
        }
        
        if(found3gramMatches) {
          test12 <- sapply(rownames(candidates3gram),FUN=function(x){
            t <- strsplit(x,split=" ")
            t[[1]][3]
          })
          suffixTrigram <- sapply(1:length(test12),FUN=function(i)test12[[i]])
        }
        
        if(found2gramMatches) {
          test14 <- sapply(rownames(candidates2gram),FUN=function(x){
            t <- strsplit(x,split=" ")
            t[[1]][2]
          })
          suffixBigram <- sapply(1:length(test14),FUN=function(i)test14[[i]])
        }
        
        if (is.function(updateProgress)) {
          text <- "         Token Processing In Progress ..."
          updateProgress(detail = text)
        }
        
        if(length(suffixQuadgram) > 0) {
          evalTokens <- suffixQuadgram
          if(length(suffixTrigram) > 0) {
            inta <-intersect(evalTokens,suffixTrigram)
            if(length(inta) > 0){
              evalTokens <- inta
            } 
            if(length(suffixBigram) > 0) {
              intb <- intersect(evalTokens,suffixBigram)
              if(length(intb) > 0) {
                evalTokens <- intb 
              }
            }
          }
        } else {
          if(length(suffixTrigram) > 0) {
            evalTokens <- suffixTrigram
            if(length(suffixBigram) > 0) {
              inta <- intersect(evalTokens,suffixBigram)
              if(length(inta) > 0) {
                evalTokens <- inta
              }
            } 
          } else {
            if(length(suffixBigram) > 0) {
              evalTokens <- suffixBigram
            }
          }
        }
        
        embeddedTrigrams <- vector()
        embeddeBigrams <- vector()
        embeddedSingles <- vector()
        
        includeQuadgrams <- FALSE
        includeTrigrams <- FALSE
        includeBigrams <- FALSE
        includeUnigrams <- FALSE
        
        lambda <- rep(as.numeric(NA), 4)
        
        if(found4gramMatches) {
          evalQuadgrams <- candidates4gram[candidates4gram$index %in% (ugset[evalTokens,]$rownum),]
          
          if(nrow(evalQuadgrams) > 0) {
            includeQuadgrams <- TRUE
            test10 <- sapply(rownames(evalQuadgrams),FUN=function(x){
              t <- strsplit(x,split=" ")
              paste(t[[1]][2],t[[1]][3],t[[1]][4],sep=" ")
            })
            
            ## initialize the conditional probabilities to 0.0. Then calibrate the probabilities based on the prefix probabilities (d1)
            evalQuadgrams$unweightedProbEst <- rep(0.0,nrow(evalQuadgrams)) ## initialize the conditional probabilities to 0.0
            embeddedTrigrams <- sapply(1:length(test10),FUN=function(i)test10[[i]])
            intx <- intersect(embeddedTrigrams,rownames(tgset))
            if(length(intx) > 0) {
              evalQuadgrams$triIndex <- tgset[intx,]$rownum
              
              d1 <- tgset[tgset$rownum == (evalQuadgrams$index3)[1],]$freq
              if(!is.na(d1)) {
                if(d1 > 0) {
                  evalQuadgrams$unweightedProbEst <- 1.0 * evalQuadgrams$freq/d1
                  evalQuadgrams$unweightedProbEst <- sapply(evalQuadgrams$unweightedProbEst,FUN=function(x){ifelse(x>=1,0.0,x)})
                }
              }
            }
          }
        }
        
#         if (is.function(updateProgress)) {
#           text <- "   Token Processing In Progress ..."
#           updateProgress(detail = text)
#         }
        
        if(found3gramMatches) {
          evalTrigrams <- candidates3gram[candidates3gram$index %in% (ugset[evalTokens,]$rownum),]
          
          if(nrow(evalTrigrams) > 0) {
            includeTrigrams <- TRUE
            test12 <- sapply(rownames(evalTrigrams),FUN=function(x){
              t <- strsplit(x,split=" ")
              paste(t[[1]][2],t[[1]][3],sep=" ")
            })
            
            ## initialize the conditional probabilities to 0.0
            evalTrigrams$unweightedProbEst <- rep(0.0,nrow(evalTrigrams))
            
            embeddedBigrams <- sapply(1:length(test12),FUN=function(i)test12[[i]])
            inty <- intersect(embeddedBigrams,rownames(bgset))
            if(length(inty) > 0) {
              evalTrigrams$ebgIdx <- bgset[inty,]$rownum
              
              d2 <- bgset[bgset$rownum == (evalTrigrams$index2)[1],]$freq
              if(!is.na(d2)) {
                if(d2 > 0) {
                  evalTrigrams$unweightedProbEst <- 1.0 * evalTrigrams$freq/d2
                  evalTrigrams$unweightedProbEst <- sapply(evalTrigrams$unweightedProbEst,FUN=function(x){ifelse(x>=1,0.0,x)})
                }
              }
            }
          }
        }
        
        if(found2gramMatches) {
          evalBigrams <- candidates2gram[candidates2gram$index %in% (ugset[evalTokens,]$rownum),]
          
          if(nrow(evalBigrams) > 0) {
            includeBigrams <- TRUE
            evalBigrams$unweightedProbEst <- rep(0.0,nrow(evalBigrams)) ## initialize the conditional probabilities to 0.0
            intz <- intersect(evalBigrams$index,ugset$rownum)
            if(length(intz) > 0) {
              d3 <- ugset[ugset$rownum == (evalBigrams$index1)[1],]$freq
              includeUnigrams <- TRUE
              if(!is.na(d3)) {
                if(d3 > 0) {
                  evalBigrams$unweightedProbEst <- 1.0 * evalBigrams$freq/d3
                  evalBigrams$unweightedProbEst <- sapply(evalBigrams$unweightedProbEst,FUN=function(x){ifelse(x >=1,0,x)})
                  evalUnigrams <- data.frame(ugset[intz,],stringsAsFactors=FALSE)
                  evalUnigrams$Percent <- sapply(evalUnigrams$Percent,FUN=function(x){ifelse(is.na(x),0.0,x)})
                  #rownames(evalUnigrams) <- rownames(ugset[intersect(evalBigrams$index,ugset$rownum),])
                }
              } else {
                evalUnigrams <- data.frame(ugset[intz,],stringsAsFactors=FALSE)
                evalUnigrams$Percent <- rep(0.0,nrow(evalUnigrams))
              }
            }
          }
        }
        
        
        if(includeQuadgrams & !includeTrigrams){
          predWord <- rownames(ugset[evalQuadgrams$index,][1,])
          predRanks <<- evalQuadgrams
        } else if(includeQuadgrams & includeTrigrams & 
                    includeBigrams & includeUnigrams) {
          
          ## Weighting coefficients for the interpolation of n-grams based on machine learning iterations
          lambda[1] <- 0.40
          lambda[2] <- 0.30
          lambda[3] <- 0.20
          lambda[4] <- 0.10
          
          T1 <- select(evalQuadgrams,rownum,unweightedProbEst,triIndex)
          names(T1) <-  c("rownum","Prob1","Trigram")
          T2 <- select(evalTrigrams,rownum,unweightedProbEst,ebgIdx)
          names(T2) <- c("Trigram","Prob2","Bigram")
          intResults <- inner_join(T1,T2)
          
          T3 <- select(evalBigrams,rownum,unweightedProbEst,index)
          names(T3) <- c("Bigram","Prob3","Single")
          intResults <- inner_join(intResults,T3)
          
          #names(evalUnigrams) <- c("freq", "rownum", "Prob4")
          T4 <- select(evalUnigrams,rownum,Percent)
          names(T4) <- c("Single","Prob4")
          results <- inner_join(intResults,T4)
          
          results$vote <- lambda[1]*results$Prob1 + lambda[2]*results$Prob2 + lambda[3]*results$Prob3 + lambda[4]*results$Prob4
          results$rownum <- row(results)[,1]
          results$ngram <- rownames(T3)
          results2 <- arrange(results,desc(vote))
          
          results3 <- select(results2,rownum)
          predRanks <<- inner_join(results3,results) %>% top_n(n5) %>% select(ngram,Prob1,Prob2,Prob3,Prob4,vote)
          
          finalResult <- results2[1,]$Single
          predWord <- rownames(ugset[finalResult,])
        } else if(includeTrigrams & 
                    includeBigrams & includeUnigrams) {
          
          ## Weighting coefficients for the interpolation of n-grams based on machine learning iterations
          lambda[2] <- 0.50
          lambda[3] <- 0.35
          lambda[4] <- 0.15
          
          T2 <- select(evalTrigrams,rownum,unweightedProbEst,ebgIdx)
          names(T2) <- c("rownum","Prob2","Bigram")
          T3 <- select(evalBigrams,rownum,unweightedProbEst,index)
          names(T3) <- c("Bigram","Prob3","Single")
          intResults <- inner_join(T2,T3)
          
          T4 <- select(evalUnigrams,rownum,Percent)
          names(T4) <- c("Single","Prob4")
          results <- inner_join(intResults,T4)
          results$vote <- lambda[2]*results$Prob2+lambda[3]*results$Prob3+lambda[4]*results$Prob4
          results$rownum <- row(results)[,1]
          results$ngram <- rownames(T3)
          results2 <- arrange(results,desc(vote))
          
          results3 <- select(results2,rownum)
          predRanks <<- inner_join(results3,results) %>% top_n(n5) %>% select(ngram,Prob2,Prob3,Prob4,vote)
          
          finalResult <- results2[1,]$Single
          predWord <- rownames(ugset[finalResult,])
        } else if(includeBigrams & includeUnigrams) {
 
          ## Weighting coefficients for the interpolation of n-grams based on machine learning iterations
          lambda[3] <- 0.60
          lambda[4] <- 0.40
          
          T3 <- select(evalBigrams,rownum,unweightedProbEst,index)
          names(T3) <- c("Bigram","Prob3","Single")
          T4 <- select(evalUnigrams,rownum,Percent)
          names(T4) <- c("Single","Prob4")
          
          results <- inner_join(T3,T4)
          results$vote <- lambda[3]*results$Prob3+lambda[4]*results$Prob4
          results$rownum <- row(results)[,1]
          results$ngram <- rownames(T3)
          results2 <- arrange(results,desc(vote))
          
          results3 <- select(results2,rownum)
          predRanks <<- inner_join(results3,results) %>% top_n(n5) %>% select(ngram,Prob3,Prob4,vote)
          
          finalResult <- results2[1,]$Single
          predWord <- rownames(ugset[finalResult,])
        }
               
        if(is.na(predWord)) {
          ## Highly improbable to rach this point. Just returning the most frequent token 
          ## occurring with the highest frequency
          predWord <- rownames(ugset[setdiff(rownames(ugset),stopWords),][1,])
          
          predRanks <<- ugset[1:n5,]
        }
        
        if (is.function(updateProgress)) {
          text <- "         Token Processing In Progress ..."
          updateProgress(detail = text)
        }
        
        paste(exp, predWord, sep=" ")
      } else {
        paste("Attention: the input phrase is either empty or is an invalid word !! Please input text with valid words to proceed")
      }
    })

#     output$rankPreds = renderDataTable({
#       predRanks
#     })
  }
)




