Capstone NLP Word Prediction
========================================================
author: Phanindra Reddigari
date: 1/17/2017

Objectives:
========================================================
Analyze the Swiftkey text files (blogs, twitter, and news) and develop a simple Shiny UI for next word prediction in a free form phrase. The UI is designed to accept user input in a text box and upon user submission, predicts the next word in the phrase. Essential components of the Shiny App design 
- Implementation of Dan Jurafsky's n-Gram NLP Interpolation model
- Building Hash Map indexes between 4-Grams, 3-Grams, 2-Grams, and 1-Grams for fast lookups (primary and foreign key relationships)
- Hash Map columns for computing probabilities of n-Gram based on conditional probabilities
- Tradeoff accuracy for speed and memory (reduce the size of n-Grams)
Implementation Principles
========================================================
Basic n-Gram Probability based on computation of conditional probabilities using:

P(w1 w2 w3 w4) = lambda1 * c(w1 w2 w3 w4) / c(w1 w2 w3) +
                 lambda2 * c(w2 w3 w4) / c(w2 w3) +
                 lambda3 * c(w3 w4) / c(w3) +
                 lambda4 * c(w4) / sum of frequencies of all 1-Grams,
                 
where w4 is the candidate word and the prefix (e.g., (w1 w2 w3) is a lower order n-gram,
c() represents the count of n-gram (frequency),
and lambda1 + lambda2 + lambda3 + lambda4 = 1.0,

Ranking the candidate words by aggregate probability of n-Gram (higher weights for higher order n-grams)

Compact n-Gram Data Structures with Primary and Foreign Key Indexes
========================================================
- Cross references to 4-Grams, 3-Grams, 2-Grams, and 1-Grams stored as integer indexes
- 1-Gram (ugset) stored on disk as RDS with named rows with frequency as the column
- 2-Gram (bgset) stored on disk as RDS with named rows with prefix and suffix unigram indexes
- 3-Gram (tgset) stored on disk as RDS with named rows with prefix bigram and suffix unigram indexes
- 4-Gram (qgset) stored on disk as RDS with named rows with prefix trigram and suffix unigram indexes
- (n-1)-Gram prefix in a n-Gram is indexed using rownum in (n-1)-Gram primary key

n-Gram Data Dimensions
========================================================
- Random sample size from blogs, news, and twitter: 30%
- nrow(ugset): 237894 (1-Grams)
- nrow(bgset): 347112 (2-Grams)
- nrow(tgset): 369445 (3-Grams)
- nrow(qgset): 253621 (4-Grams)
- Ranking of candidate words based on aggregated probabilities (see example below)
- ngram,Prob1,Prob2,Prob3,Prob4,Vote
- "the_world",0.7866667,0.2137592,0.006977663,0.0008197,0.3802719

Observations on Markov N-Gram model accuracy
========================================================
- Low accuracy of predictions. Natural languages have deep contextual relationships not covered by 4-Gram or even 6-Gram Markov models.
- Example: "Very early observations on the Bills game: Offense still struggling but the". This sentence has contextual relationship between the main clause and the subordinate clause. Certain word choices are obvious from the semantic context that n-Gram model tends to ignore.
- Better prediction accuracy is possible for training on specific application with more predicable range.
- Kneser-Nay Smoothing may yield higher accuracy of prediction

