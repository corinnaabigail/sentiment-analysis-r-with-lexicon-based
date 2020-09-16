setwd("C://Users/Asus pc/Documents/Tugas Kuliah/BAHAN PI H3H3/rstudio")

# Import Libraries
library(twitteR)
install.packages("tidyverse")
library(tidyverse)
install.packages("tidytext")
library(tidytext)
library(sentimentr)
library(tm)
library(stringr)
library(lexicon)
library(ggplot2)

download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile = "cacert.pem")

# Set Up Twitter API
setup_twitter_oauth("NHXIvY7bnZ13Epommi5gvAdFV",
                    "Ck9R0TGSsZl6QRrFSpMPL1yutilkHHcgoSG9l9EczuRkTvLCdS",
                    "558628526-xl43PsLh39T2fm6tOLeNcF9mSQuVvWjcjMPeUk2L",
                    "UckIJqaMCoUEFEdNdJGW83W92ONng0r8zS46biCQDNLLm")

# Crawl Twitter Data with API
Bipolar_Tweets <- searchTwitter("bipolar disorder", n = 1000, lang = "en")
str(Bipolar_Tweets)
View(Bipolar_Tweets)
df_Bipolar <- twListToDF(Bipolar_Tweets)
View(df_Bipolar)

# Write df_sentiment in csv with name "Bipolar_sayang.csv"
write.csv(df_Bipolar$text, file = "Bipolar_sayang.csv", row.names = Bipolar_Tweets)

# Load Positive and Negative Dictionary
pos <- scan('C://Users/Asus pc/Documents/Tugas Kuliah/BAHAN PI H3H3/rstudio/Positive-Words.txt', what = 'character', comment.char = ';')
neg <- scan('C://Users/Asus pc/Documents/Tugas Kuliah/BAHAN PI H3H3/rstudio/Negative-Words.txt', what = 'character', comment.char = ';')

# Order of Normalization
df_Bipolar2 <- tolower(df_Bipolar$text) # Case Folding
View(df_Bipolar2)
write.csv(df_Bipolar2, file = "Bipolar_sayang_tollower.csv", row.names = Bipolar_Tweets)
df_Bipolar2 <- gsub("https://t.co/\\w+", " ", df_Bipolar2) # Remove https
View(df_Bipolar2)
write.csv(df_Bipolar2, file = "Bipolar_sayang_http.csv", row.names = Bipolar_Tweets)
df_Bipolar2 <- gsub("@\\w+", " ", df_Bipolar2) # Remove "@" symbols
df_Bipolar2 <- gsub("#\\w+", " ", df_Bipolar2) # Remove "#" symbols
View(df_Bipolar2)
write.csv(df_Bipolar2, file = "Bipolar_sayang_@#.csv", row.names = Bipolar_Tweets)
df_Bipolar2 <- gsub("[[:punct:]]|.", " ", df_Bipolar2) # Remove Punctuations
View(df_Bipolar2)
write.csv(df_Bipolar2, file = "Bipolar_sayang_punct.csv", row.names = Bipolar_Tweets)
df_Bipolar2 <- gsub("[[:digit:]]", " ", df_Bipolar2) # Remove Digits
View(df_Bipolar2)
write.csv(df_Bipolar2, file = "Bipolar_sayang_digit.csv", row.names = Bipolar_Tweets)
df_Bipolar2 <- removeWords(df_Bipolar2, stopwords()) # Remove Stopwords
View(df_Bipolar2)
write.csv(df_Bipolar2, file = "Bipolar_sayang_stopwords.csv", row.names = Bipolar_Tweets)
df_Bipolar2 <- gsub(pattern = "\\b[A-z]\\b(1)", replacement=" ", df_Bipolar2) # Remove Single Characters
View(df_Bipolar2)
write.csv(df_Bipolar2, file = "Bipolar_sayang_karakter.csv", row.names = Bipolar_Tweets)
library(tm) #fungsi stripWhitespace ada di package tm, jadi harus manggil package ini dulu
df_Bipolar2 <- stripWhitespace(df_Bipolar2) # Remove some spaces and enter, because we're analizing sentiments, not spaces :)
View(df_Bipolar2)
write.csv(df_Bipolar2, file = "Bipolar_sayang_whitespace.csv", row.names = Bipolar_Tweets)
df_Bipolar2 <- unique(df_Bipolar2)
View(df_Bipolar2)
write.csv(df_Bipolar2, file = "Bipolar_sayang_unique.csv", row.names = TRUE)
df_Bipolar3 <- read.csv("C://Users/Asus pc/Documents/Tugas Kuliah/BAHAN PI H3H3/rstudio/Bipolar_unique.csv")
Bipolar_sentiment <- read.csv("C://Users/Asus pc/Documents/Tugas Kuliah/BAHAN PI H3H3/rstudio/Bipolar_unique.csv")
Bipolar_sentiment$x <- as.character(Bipolar_sentiment$x)
str(Bipolar_sentiment)
corpus <- Bipolar_sentiment$x
df_Bipolar3 <- twListToDF(Bipolar_sentiment)

# tokenize
BipolarTokens <- data_frame(text = Bipolar_sentiment) %>% unnest_tokens(word, text)
# use the unnest_tokens function to get the words from the "value" column of "child
BipolarTokens <- Bipolar_sentiment %>% unnest_tokens(word, value)
head(childsTokens)
# look at just the head of the sorted word frequencies
BipolarTokens %>% count(word, sort = T) %>% head
# anti_join removes any rows that are in the both dataframes, so I make a data_frame
# of 1 row that contins "chi" in the "word" column.
sortedTokensBipolar <- Bipolar_sentiment %>% unnest_tokens(word, value) %>% anti_join(data_frame(word = "chi")) %>% 
  count(word, sort = T)
head(sortedTokensBipolar)

#document term matrix
dtm_Bipolar <- DocumentTermMatrix(VCorpus(VectorSource(Bipolar_sentiment)))
dtm_Bipolar <- DocumentTermMatrix(VCorpus(VectorSource(Bipolar_sentiment$text)))
inspect(dtm_Bipolar)
write.csv(dtm_Bipolar, file = "Bipolar_dtm.csv", row.names = x)

#string split with ka Abitama
install.packages(stringr)
library(stringr)
Bipolar_split <- str_split(Bipolar_sentiment$x, pattern="\\s+")
lapply(Bipolar_split, function(x){sum(!is.na(match(x,pos)))}) # many words match the positive bundle in the positive dictionary
lapply(Bipolar_split, function(x){sum(!is.na(match(x,neg)))}) # many words match the negative bundle in the negative dictionary
skor <- lapply(Bipolar_split, function(x){sum(!is.na(match(x,pos))) - sum(!is.na(match(x,neg)))}) #score sentiment
Bipolar_sentiment$sentiment <- skor
#Bipolar_sentiment1$sentiment <- skor
View(Bipolar_sentiment)
Bipolar_sentiment$sentiment[Bipolar_sentiment$sentiment == 0] = "Neutral"
Bipolar_sentiment$sentiment[Bipolar_sentiment$sentiment < 0] = "Negative"
Bipolar_sentiment$sentiment[Bipolar_sentiment$sentiment > 0] = "Positive"
Bipolar_sentiment$sentiment <- unlist(Bipolar_sentiment$sentiment, use.names = T)
write.csv(Bipolar_sentiment, file = "Bipolar_skor_sentimen1.csv", row.names = F)

#string split
install.packages(stringr)
library(stringr)
Bipolar_splitstring <- str_split(Bipolar_sentiment$x, pattern="\\s+")
str(Bipolar_splitstring)
lapply(Bipolar_splitstring, function(x){sum(!is.na(match(x,pos)))}) # many words match the positive bundle in the positive dictionary
lapply(Bipolar_splitstring, function(x){sum(!is.na(match(x,neg)))}) # many words match the negative bundle in the negative dictionary
Bipolar_scoresentiment <- lapply(Bipolar_splitstring, function(x){sum(!is.na(match(x,pos))) - sum(!is.na(match(x,neg)))}) #score sentiment
Bipolar_sentiment$sentiment <- Bipolar_scoresentiment
View(Bipolar_sentiment)
write.csv(Bipolar_sentiment$sentiment, file = "Bipolar_score_digit.csv", row.names = F)
Bipolar_sentiment$sentiment[Bipolar_sentiment$sentiment == 0] = "Netral"
Bipolar_sentiment$sentiment[Bipolar_sentiment$sentiment < 0] = "Negative"
Bipolar_sentiment$sentiment[Bipolar_sentiment$sentiment > 0] = "Positive"
Bipolar_sentiment$sentiment <- unlist(Bipolar_sentiment$sentiment, use.names = T)
write.csv(Bipolar_sentiment, file = "Bipolar_skor_sentimen.csv", row.names = F)
str(Bipolar_sentiment)
table(Bipolar_sentiment)

Bipolar_score_sentiment <- read.csv("C://Users/Asus pc/Documents/score.csv")
Bipolar_skor_sentimen <- read.csv("C://Users/Asus pc/Documents/Bipolar_skor_sentimen.csv")
Bipolar_score_sentiment$x <- as.numeric(Bipolar_score_sentiment$x)
Bipolar_score_sentiment$sentiment <- as.numeric(Bipolar_score_sentiment$sentiment)
View(Bipolar_skor_sentimen)
str(Bipolar_score_sentiment)

#frequencies sentiment word in R (Positive, Negative, and Netral sentiment in R)
Bipolar_frequency <- data.frame(table(unlist(strsplit(tolower(Bipolar_sentiment$sentiment), " "))))
View(Bipolar_frequency)
str(Bipolar_frequency)
Bipolar_frequency$Var1 <- as.numeric(Bipolar_frequency$Var1)
Bipolar_frequency$Freq <- as.numeric(Bipolar_frequency$Freq)

#visualization
install.packages("wordcloud")
library(wordcloud)
piepercent <- round(100*Bipolar_frequency$Freq/sum(Bipolar_frequency$Freq), 1)

Bipolar_histogram <- hist(Bipolar_frequency$Freq, main = "Score of Bipolar Tweets", xlab = "Sentiment", ylab = "Count of Tweets", col = "Purple")
Bipolar_pie <- pie(Bipolar_frequency$Freq, labels = piepercent, radius = 1, clockwise = FALSE, col=rainbow(3),main="Pie Chart of Bipolar Analysis")
legend("topright", legend = Bipolar_frequency$Var1, fill = rainbow(3), cex = 1)