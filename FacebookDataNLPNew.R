#install.packages("RCurl")
library(RCurl)

#install.packages("rjson")
library(rjson)

#install.packages("tm")
library(tm)
library(Rfacebook)

url ="https://graph.facebook.com/v3.0/2160365850647427/comments?access_token=EAACEdEose0cBAP10gl7Uc246vtZAfwZAXomcwWZC6U8rRkmrSba7MsrZCaEiZCxqMfV4gStaxbZCZACMS1fhJ8UZARwH9nZAPharqiLXXIN24UuV4iipoaGGTG76GLCkHU73QpCRq513VUiP3ivn7dEKmYVXUdeBbSl3vZAiXxLu2qNAeL6oGr1OjPDYLIvmCmnHcZD"
d = getURL(url)
d
j = fromJSON(d)
head(j,2)
#install.packages("sqldf")
#library(sqldf)
comments = sapply(j$data,function(j){list(comment=j$message)})
comments
comments1 = sapply(j$data,function(j){list(comment=j$message,id =j$id)})
comments1
#check the str
str(comments)
getwd()
comments.df =as.data.frame(comments1)
comments.dfnew= t(comments.df)
comments.dfnew[,1]
comments.dfnew[,2]
head(comments.dfnew,2)
colnames(comments.dfnew)
#influentialusers<- sqldf("select id,comment from comments.dfnew")
#head(influentialusers)
write.csv(comments.dfnew, file = "newFBCommentsWithID.csv")
#comments_new =load("newFBComments.csv")
comments_new = read.csv("newFBCommentsWithID.csv", header = TRUE)
comments_new



#Now cleaning the comments. making a corpus
cleanedcomments = sapply(comments, function(x) iconv(enc2utf8(x), sub ="byte"))
my_corpus = Corpus(VectorSource(cleanedcomments))
my_corpus
my_function = content_transformer(function(x, pattern)gsub(pattern,"",x))
my_cleaned_corpus = tm_map(my_corpus,my_function,"/")
my_cleaned_corpus = tm_map(my_cleaned_corpus,my_function,"@")
my_cleaned_corpus = tm_map(my_cleaned_corpus,content_transformer(tolower))
my_cleaned_corpus = tm_map(my_cleaned_corpus,removeWords, c(stopwords("english"),"like","so","honey"))
my_cleaned_corpus = tm_map(my_cleaned_corpus,removePunctuation)
my_cleaned_corpus = tm_map(my_cleaned_corpus,stripWhitespace)
my_cleaned_corpus

#cleaned dataframe
dataframe <- data.frame(text=sapply(my_cleaned_corpus, identity), 
                        stringsAsFactors=F)


dataframe
#count the no. of times a sentence has been used
library(plyr)
library(dplyr)
dataframe %>%count(text, sort = TRUE) 
write.csv(dataframe, file = "newcleanedFBComments2.csv")

#second document-stemming
#install.packages("SnowballC")
#library(SnowballC)
stemmingcorpus <- tm_map(my_cleaned_corpus, stemDocument, language = "english")
stemmingcorpus


stemmingdataframe <- data.frame(text=sapply(stemmingcorpus, identity), 
                                stringsAsFactors=F)
stemmingdataframe
write.csv(stemmingdataframe, file = "StemmingFBComments2.csv")

#Remove curse words
load_curse_words <- function(curse_words_url) {
  connection <- url(curse_words_url)
  lines <- readLines(connection)
  close(connection)
  lines
}

curse_words <- load_curse_words(
  "https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
)

corpus_without_curse_words <- tm_map(my_cleaned_corpus, removeWords, curse_words)
corpus_without_curse_words
withoutCursedataframe <- data.frame(text=sapply(corpus_without_curse_words, identity), 
                                    stringsAsFactors=F)
withoutCursedataframe

#bigrams&tri-grams
#install.packages("RWeka")
library(RWeka)
library(wordcloud)
#bigrams
minfreq_bigram=2
dataframe

as.character(my_cleaned_corpus[[1]])

token_delim = " \\t\\r\\n.!?,;\"()"
bitoken = NGramTokenizer(my_cleaned_corpus,Weka_control(min=2,max=2,delimiters = token_delim))
bitoken
two_word = data.frame(table(bitoken))
View(two_word)
sort_two = two_word[order(two_word$Freq,decreasing = TRUE),]
wordcloud(sort_two$bitoken,sort_two$Freq,random.order=FALSE,scale=c(2,0.35),colors = brewer.pal(8,"Dark2"),min.freq = minfreq_bigram,max.words=150)


# Trigrams 

minfreq_trigram <-5

token_delim <- " \\t\\r\\n.!?,;\"()"
tritoken <- NGramTokenizer(my_cleaned_corpus, Weka_control(min=3,max=3, delimiters = token_delim))
three_word <- data.frame(table(tritoken))
View(three_word)
sort_three <- three_word[order(three_word$Freq,decreasing=TRUE),]
wordcloud(sort_three$tritoken, sort_three$Freq, random.order=FALSE,min.freq = minfreq_trigram,scale = c(1.2,0.35),colors = brewer.pal(8,"Dark2"),max.words=150)

#POS tagging
#install.packages("openNLP")
#install.packages("NLP")
#install.packages("rJava")
#install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
#install.packages("stringi")
library(rJava)
library(openNLP)
library(NLP)
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()

POS <- NLP::annotate(my_cleaned_corpus, list(sent_token_annotator, word_token_annotator))
POS
POS_tg <- NLP::annotate(my_cleaned_corpus, pos_tag_annotator, POS)
POS_tg

POS_tg_subset <- subset(POS_tg, type=="word")
POS_tg_subset
tags <- sapply(POS_tg_subset$features, '[[', "POS")
# let's see the result
head(tags)

#Creating a document matrix-to capture everything we take original cleaned corpus
my_tdm = TermDocumentMatrix(my_cleaned_corpus)
m= as.matrix(my_tdm)
View(m)
words = sort(rowSums(m),decreasing = TRUE)
words
#make just one gram words
my_data = data.frame(ngram=names(words),freq=words)
View(my_data)
my_data



#rownames(my_data1) <- 1:length(freq)
#seeing it visually

library(ggplot2)
my_data %>%
  filter(freq > 1) %>%
  mutate(word = reorder(ngram, freq)) %>%
  ggplot(aes(word, freq,color="blue40")) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

library(RColorBrewer) 
#install.packages("wordcloud")
library(wordcloud)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(words=my_data$ngram,freq = my_data$freq,min.freq=1,max.words=Inf,random.order=FALSE,
          scale=c(1.5,0.8),rot.per=0.35, colors=pal2)

#POS tagging
#devtools::install_github("bnosac/RDRPOSTagger")
#devtools::install_github("ropensci/tokenizers")
library("RDRPOSTagger")
library("tokenizers")

#make 2 grams 
two_ngram_tokenizer <- function(x) 
  NGramTokenizer(x, Weka_control(min = 2, max = 2))

document_term_matrix <- DocumentTermMatrix(my_cleaned_corpus,
                                           control = list(tokenize = two_ngram_tokenizer,
                                                          wordLengths=c(1, Inf)))

two_ngrams <- as.matrix(document_term_matrix)
View(two_ngrams)
wordsCols = sort(colSums(document_term_matrix),decreasing = TRUE)
wordsCols
my_data1 = data.frame(ngram=names(wordsCols),freq=wordsCols, stringsAsFactors = FALSE)
my_data1
my_data1 <- arrange(my_data1, desc(freq))
my_data1

top_two_ngrams <- two_ngrams[1:50, ]
top_two_ngrams


#topic modelling
#LDA on our document matrix :my_tdm
#install.packages("topicmodels")
library(topicmodels)
#check which documents have zero topics and clean them
inspect(my_cleaned_corpus)
empty.rows <- dtm[rowTotals == 0, ]$dimnames[1][[1]]
my_cleaned_corpus1 <- my_cleaned_corpus[-as.numeric(empty.rows)]
my_cleaned_corpus1
inspect(my_cleaned_corpus1)
#corpus is cleaned

my_tdm1 = DocumentTermMatrix(my_cleaned_corpus1)
m1= as.matrix(my_tdm1)
inspect(my_tdm1)
View(m1)
#low frequency and high frequency
findFreqTerms(my_tdm1, 2,5)

#find associations
findAssocs(my_tdm1, "good", 0.5) 
findAssocs(my_tdm1, "animals", 0.5) 
findAssocs(my_tdm1, "life", 0.5) 

#controlling the seed and choosing 2 topics
data_lda <- LDA(my_tdm1, k = 2, control = list(seed = 1234))
data_lda



#word-Topic probabilities
#install.packages("tidytext")
library(tidytext)
ap_topics <- tidy(data_lda, matrix = "beta")
View(ap_topics)

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
ap_top_terms

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
#results show topic 1 shows the nature/sentiment for the picture from t
#topic 2 shows the picture describes the beauty of the animal/creature.
#however, picture exists in both topic 1 and topic 2. This will exist in LDA but can be removed by clustering


#check for sentiment analysis 
library(tidytext)
my_data_copy=my_data
colnames(my_data_copy)[1]="word"
colnames(my_data_copy)
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")
nrc_joy
#taking out all the joyous words
my_data_copy %>%
  filter(freq>1) %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

#most common positive and negative words

posNegWords <- my_data_copy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
posNegWords

#Visualizing
posNegWords %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#see where are the negative words existing--see for problem
#install.packages("data.table")
library(data.table)
commentsDataTable = data.table(comments_new)
commentsDataTable
#comments_new[comments_new$V1 %like% 'problem',]
commentsDataTable[V1 %like% "problem"]
commentsDataTable[V1 %like% "reject"]
commentsDataTable[V1 %like% "hug"]


#Now divide the data into 1-gram and translate the text
#use data.table-much faster than sapply and use textcat package
one_ngrams_dt <- data.table(my_data)
one_ngrams_dt

#install.packages("textcat")
library(textcat)
names(TC_byte_profiles)
names(ECIMCI_profiles)
#determine the language of our text
textcat(dataframe)

#can exclude languages if don't want
languages <- c("english","hindi")
sub_tc_byte_profiles <- TC_byte_profiles[names(TC_byte_profiles) %in% languages]
names(sub_tc_byte_profiles)

#detect language using textcat
language_dt <- one_ngrams_dt[, language := textcat(ngram, p = sub_tc_byte_profiles)]
#language_dt <- one_ngrams_dt[, language := textcat(ngram, p = TC_byte_profiles)]
language_dt[,]
language_dt <- data.table(language_dt)
language_aggregated_dt <- language_dt[, sum(freq), by = language]
names(language_aggregated_dt) <- c("language", "frequency")
language_aggregated_dt <- language_aggregated_dt[, percentage := round(frequency / sum(language_dt[, 2]) * 100, 2)]
as.data.frame(language_aggregated_dt)
language_dt[1:10,]
#see which words are taken as hindi
language_dt[,which(language_dt$language=="hindi")]
language_dt[language_dt[,which(language_dt$language=="hindi")],]





