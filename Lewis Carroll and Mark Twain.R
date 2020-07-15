library(gutenbergr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(syuzhet)
library(magrittr)

summary(gutenberg_authors)
#############Analysing Lewis Carroll's two books######### 
#Frequency Analysis of Alice's Adventures in Wonderland
aliceref <- gutenberg_works(title == "Alice's Adventures in Wonderland")
names(aliceref)
#Downloding book by id number 
alice <- gutenberg_download(aliceref$gutenberg_id) %>% gutenberg_strip()
#Tidying and visualizing
tidytext <- data_frame(line = 1:nrow(alice), text = alice$text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
barplot(height=head(tidytext,10)$n, names.arg=head(tidytext,10)$word, xlab="Words", ylab="Frequency", col="#973232", main="Alice in Wonderland")
#Most used word is *of course* Alice.Then, time and queen comes. Actually, these words sum up the whole story.


#Frequency Analysis of The Game of Logic
gameref <- gutenberg_works(title == "The Game of Logic")
names(gameref)
#Downloding book by id number 
game <- gutenberg_download(gameref$gutenberg_id) %>% gutenberg_strip()
#Tidying and visualizing
tidytextgame <- data_frame(line = 1:nrow(game), text = game$text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
barplot(height=head(tidytextgame,10)$n, names.arg=head(tidytextgame,10)$word, xlab="Words", ylab="Frequency", col="#973232", main="The Game of Logic")
#The most used ones are zeros and ones. Also, appearently author made a lot of clarifications.

##########Sentiment Analysis of Those Books###########

#Cleaning the data, converting it to lower case, and splitting it into individual words
alicesent <- tolower(as.vector(unlist(strsplit(paste(gsub(" {2,}", " ", alice), sep = " "), " "))))
gamesent <- tolower(as.vector(unlist(strsplit(paste(gsub(" {2,}", " ", game), sep = " "), " "))))
#Extracting samples from each data set
alicesent <- sample(alicesent, 5000, replace = F)
gamesent <- sample(gamesent, 5000, replace = F)
#Performing the Sentiment Analysis with nrc
aliceemo <- get_nrc_sentiment(alicesent)
head(aliceemo)
gameemo <- get_nrc_sentiment(gamesent)
head(gameemo)


#Preparing the data for visualizations
aliceeemos <- colSums(aliceemo)/50
head(aliceeemos)
gameeemos <- colSums(gameemo)/50
head(gameeemos)

#Making a a single table
emolit <- data.frame(aliceeemos,gameeemos)
emo <- t(emolit)
#Cleaning row names
rownames(emo) <- gsub("emos", "", rownames(emo))
head(emo)
#Converting to a df 
emo <- as.data.frame(emo)
#Adding title column
emo$Title <- c("Alice's Adventures in Wonderland", "The Game of Logic")
#Converting data from wide to long
emol <- gather(emo, Emotion, Score, anger:positive, factor_key=TRUE)
#Extracting subset
emol2 <- emol %>%
  filter(Emotion != "positive") %>%
  filter(Emotion != "negative")
#Ploting
ggplot(emol2,                   
       aes(Emotion, Score, fill = Title)) +    
       geom_bar(stat="identity", position=position_dodge()) +  
       scale_fill_manual(values=c("goldenrod2", "indianred4")) +                
       theme_bw()                   
#wordcloud
library(wordcloud)
library(tm)
library(RColorBrewer)
alis <-wordcloud(alicesent, type ="text", lang = "english",
                 min.freq = 1,  max.words = 200,
                 colorPalette = "RdBu")

geym <- wordcloud(gamesent, type ="text", lang = "english",
                  min.freq = 1,  max.words = 200,
                  colorPalette = "RdBu")


#####################Analysing Lewis Carroll and Mark Twain###########
#Working with 2 books of Mark Twain 
books <- gutenberg_download(c(74, 76), meta_fields = "title")
#Converting the text column to be one-token-per-row 
tidy_books <- books %>%
  unnest_tokens(word, text)
tidy_books
#Removing stop words, finding the most common words in all the two books as a whole.
data("stop_words")
cleaned_books <- tidy_books %>%
  anti_join(stop_words)

cleaned_books %>%
  count(word, sort = TRUE)
#Sentiment Analysis
bing <- get_sentiments("bing")
bing_word_counts <- tidy_books %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

bing_word_counts %>%
  filter(n > 30) %>%
  mutate(n = ifelse(sentiment == 'negative', -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab('Contribution to sentiment') + ggtitle('Most common positive and negative words')

#Working with 2 books of Lewis Carroll
books1 <- gutenberg_download(c(4763, 11), meta_fields = "title")
tidy_books1 <- books1 %>%
  unnest_tokens(word, text)

tidy_books1

data("stop_words")
cleaned_books1 <- tidy_books1 %>%
  anti_join(stop_words)

cleaned_books1 %>%
  count(word, sort = TRUE)

bing1 <- get_sentiments("bing")
bing_word_counts1 <- tidy_books1 %>%
  inner_join(bing1) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts1

bing_word_counts1 %>%
  filter(n > 30) %>%
  mutate(n = ifelse(sentiment == 'negative', -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab('Contribution to sentiment') + ggtitle('Most common positive and negative words')
#Lewis Carroll does not have that much repated words(n>30).
#All in all Mark Twain used more negative words in his books that I selected. 
