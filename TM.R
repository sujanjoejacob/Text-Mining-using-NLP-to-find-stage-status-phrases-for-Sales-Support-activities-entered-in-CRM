

scripts <- read.csv("Act_clean.csv", header=T, na.strings=c("","","NA"), stringsAsFactors = FALSE)

scripts =  scripts %>% 
  rename(text = ActivityDescription)%>% 
  rename(Character = User)

fillColor = "#FFA07A"
fillColor2 = "#F1C40F"

#Drop missing values 
complete.cases(scripts)
scripts <- scripts[complete.cases(scripts), ]


glimpse(scripts)

View(scripts)

#remove number 
scripts <- 
  scripts %>% 
  mutate(text = gsub("[^A-Za-z ]","",text)) %>%
  filter(text != "")

#4 Who has Spoken more ?

Top10Characters = scripts %>%
  group_by(Character) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Character = reorder(Character,Count)) %>%
  head(15)

Top10Characters %>%
  
  ggplot(aes(x = Character,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = Character, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'User', 
       y = 'Count', 
       title = 'User and Count') +
  coord_flip() + 
  theme_bw()



#5 Which Character spoke long Sentences ?

scripts$len = str_count(scripts$text)

scriptsTopTenCharacters = scripts %>%
  filter(Character %in% Top10Characters$Character)

scriptsTopTenCharacters %>%
  group_by(Character) %>%
  summarise(CountMedian = median(len,na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Character = reorder(Character,CountMedian)) %>%
  
  ggplot(aes(x = Character,y = CountMedian)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = Character, y = 1, label = paste0("(",CountMedian,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Character', 
       y = 'Count', 
       title = 'Character and Count') +
  coord_flip() + 
  theme_bw()


#6 Tokenisation

scripts %>%
  unnest_tokens(word, text) %>%
  head(10)

#6.1 Removing the Stop words
scripts %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>% head(10)

stop_words %>% head(20)

#7 Top Twently most Common Words

createBarPlotCommonWords = function(train,title)
{
  train %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word) %>%
    count(word,sort = TRUE) %>%
    ungroup() %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>%
    head(20) %>%
    
    ggplot(aes(x = word,y = n)) +
    geom_bar(stat='identity',colour="white", fill =fillColor) +
    geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Word', y = 'Word Count', 
         title = title) +
    coord_flip() + 
    theme_bw()
  
}
createBarPlotCommonWords(scripts,'Top 20 most Common Words')


#6.1.2
# remove custom stopwords
custom_stop_words <- 
  bind_rows(data_frame(word = c("debt",
                                "gross",
                                "crude",
                                "well",
                                "maturity",
                                "work",
                                "marginally",
                                "leverage"), 
                       lexicon = c("custom")), 
            stop_words)


scripts <- 
  scripts %>%
  
  mutate(word = gsub("[^A-Za-z ]","",word)) %>%  # keep only letters (drop numbers and special symbols)
  filter(word != "") %>%
  count(report,word,sort=TRUE) %>%
  bind_tf_idf(word, report, n) %>%
  arrange(desc(tf_idf))

#7.1 WordCloud of the Common Words
createWordCloud = function(train)
{
  train %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word) %>%
    count(word,sort = TRUE) %>%
    ungroup()  %>%
    head(50) %>%
    
    with(wordcloud(word, n, max.words = 30,colors=brewer.pal(8, "Dark2"))) 
}

createWordCloud(scripts)

#8 Word Analysis of Characters

#8.1.1 Word cloud Sagar
createWordCloud(scripts %>%
                  filter(str_detect(Character,"Sagar Gharat")))

#8.1.2 Bar Plot
createBarPlotCommonWords(scripts %>%
                           filter(str_detect(Character,"Prachi Bidwe")),
                         'Top 10 most Common Words')

createBarPlotCommonWords(scripts %>%
                           filter(str_detect(Character,"Jyotirmay Parida")),
                         'Top 10 most Common Words')

#9 Parts of Speech analysis

WordCloudPOS <- function(scripts,partsOfSpeechName) {
  scripts %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word) %>%
    
    left_join(parts_of_speech) %>%
    filter(pos == partsOfSpeechName) %>%
    
    count(word,sort = TRUE) %>%
    ungroup()  %>%
    head(50) %>%
    
    with(wordcloud(word, n, max.words = 50,colors=brewer.pal(8, "Dark2")))
}

#9.1.1 WordCloud Adjectives
WordCloudPOS(scripts,"Adjective")

#9.1.2 Transitive Verb 
WordCloudPOS(scripts,"Verb (transitive)")

#9.1.3 In-Transitive Verb 
WordCloudPOS(scripts,"Verb (intransitive)")

#10 TF-IDF

#10.2 Twenty Most Important words for the Most Active Characters
trainWords <- scripts %>%
  unnest_tokens(word, text) %>%
  count(Character, word, sort = TRUE) %>%
  ungroup()

total_words <- trainWords %>% 
  group_by(Character) %>% 
  summarize(total = sum(n))

trainWords <- left_join(trainWords, total_words)

#Now we are ready to use the bind_tf_idf which computes the tf-idf for each term. 
trainWords <- trainWords %>%
  filter(!is.na(Character)) %>%
  bind_tf_idf(word, Character, n)


plot_trainWords <- trainWords %>% 
  filter( Character %in% Top10Characters$Character) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

plot_trainWords

plot_trainWords %>% 
  top_n(20) %>%
  ggplot(aes(word, tf_idf)) +
  geom_col(fill = fillColor) +
  labs(x = NULL, y = "tf-idf") +
  coord_flip() +
  theme_bw()

#11 Casting to a Term Document Matrix
plot_trainWords_dtm <- plot_trainWords %>%
  cast_dtm(Character, word, tf_idf)

inspect(plot_trainWords_dtm[1:10,1:5])


#11.1 
inspect(plot_trainWords_dtm["Sagar Gharat",1:5])
inspect(plot_trainWords_dtm["Jyotirmay Parida",1:5])


#12 Similiarities
getCosine <- function(x,y) 
{
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}

getCosine(plot_trainWords_dtm["Sagar Gharat",],plot_trainWords_dtm["Jyotirmay Parida",])
getCosine(plot_trainWords_dtm["Pooja Lasunkute",],plot_trainWords_dtm["Jyotirmay Parida",])
getCosine(plot_trainWords_dtm["Pooja Lasunkute",],plot_trainWords_dtm["Sunny Azad",])
getCosine(plot_trainWords_dtm["Sagar Gharat",],plot_trainWords_dtm["Sunny Azad",])

#13 Most Common Bigrams

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
  
}

visualize_bigrams_individual <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a,end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

scripts %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  unite(bigramWord, word1, word2, sep = " ") %>%
  group_by(bigramWord) %>%
  tally() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(bigramWord = reorder(bigramWord,n)) %>%
  head(30) %>%
  
  ggplot(aes(x = bigramWord,y = n)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = bigramWord, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Bigram', 
       y = 'Count', 
       title = 'Bigram and Count') +
  coord_flip() + 
  theme_bw()

#14 Most Common Trigrams
scripts %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2","word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  unite(trigramWord, word1, word2, word3,sep = " ") %>%
  group_by(trigramWord) %>%
  tally() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(trigramWord = reorder(trigramWord,n)) %>%
  head(25) %>%
  
  ggplot(aes(x = trigramWord,y = n)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  geom_text(aes(x = trigramWord, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Trigram', 
       y = 'Count', 
       title = 'Trigram and Count') +
  coord_flip() + 
  theme_bw()

#15 Relationship among words

trainWords <- scripts %>%
  count_bigrams()

trainWords %>%
  filter(n > 75) %>%
  visualize_bigrams()

#16 Relationship with the words
trainWords %>%
  filter(word1 == "Sagar Gharat" | word2 == "Sagar Gharat" | 
           word1 == "Jyotirmay Parida" | word2 == "Jyotirmay Parida")%>%
  filter(n > 5) %>%
  visualize_bigrams()


#17 Word Embeddings
#17.1 Unigram Probablities

unigram_probs <- scripts %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

unigram_probs

#17.2 Skipgram probabilities

#17.2.1 Step 1 : Create the skipgrams
skipgrams1 <-  scripts %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  unite(skipgramID,ngramID) %>%
  head(10)
datatable(skipgrams1, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

#17.2.2 Step 2 : Tidy the skipgrams

tidy_skipgrams <- scripts %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  unite(skipgramID, ngramID) %>%
  unnest_tokens(word, ngram)

datatable(head(tidy_skipgrams), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))



#17.2.3 Step 3: Cooccurring pairs
skipgram_probs <- tidy_skipgrams %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

datatable(head(skipgram_probs), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))


#17.3 Normalized skipgram probability
normalized_prob <- skipgram_probs %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

#17.4 Cast to a Matrix
pmi_matrix <- normalized_prob %>%
  mutate(pmi = log10(p_together)) %>%
  cast_sparse(word1, word2, pmi)

#17.5 Reduce the matrix dimensionality
dim(pmi_matrix)
pmi_svd <- irlba(pmi_matrix, 256, maxit = 1e3)



#19 Sentiment Analysis using AFINN sentiment lexicon
visualize_sentiments <- function(SCWords) {
  SCWords_sentiments <- SCWords %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(Character) %>%
    summarize(score = sum(score * n) / sum(n)) %>%
    arrange(desc(score))
  
  SCWords_sentiments %>%
    top_n(10) %>%
    mutate(Character = reorder(Character, score)) %>%
    ggplot(aes(Character, score, fill = score > 0)) +
    geom_col(show.legend = TRUE) +
    coord_flip() +
    ylab("Average sentiment score") + theme_bw()
}

trainWords <- scripts %>%
  unnest_tokens(word, text) %>%
  count(Character, word, sort = TRUE) %>%
  ungroup()

visualize_sentiments(trainWords)

#19.1 Postive and Not So Postive Words of Characters
positiveWordsBarGraph <- function(SC) {
  contributions <- SC %>%
    unnest_tokens(word, text) %>%
    count(Character, word, sort = TRUE) %>%
    ungroup() %>%
    
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(word) %>%
    summarize(occurences = n(),
              contribution = sum(score))
  
  contributions %>%
    top_n(30, abs(contribution)) %>%
    mutate(word = reorder(word, contribution)) %>%
    head(30) %>%
    ggplot(aes(word, contribution, fill = contribution > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() + theme_bw()
}

positiveWordsBarGraph(scripts)

#19.1.1 Positive and not so positive words by Sagar 
positiveWordsBarGraph(scripts %>%
                        filter(Character == "Sagar Gharat"))


#20 Sentiment Analysis using NRC Sentiment lexicon
#20.1 Sentiment Analysis Words - Fear for Jerry
getEmotionalWords = function(emotion,Character)
{
  nrcEmotions = get_sentiments("nrc") %>% 
    filter(sentiment == emotion) 
  
  emotionalWords = scripts %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word) %>%
    filter(Character == Character) %>%
    inner_join(nrcEmotions) %>%
    group_by(word) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count))
  
  
  return(emotionalWords)
  
}

WinWordsSagar = getEmotionalWords('trust','Sagar Gharat')

#For anger 
WinWordsSagar = getEmotionalWords('anger','Sagar Gharat')
wordcloud(WinWordsSagar$word, WinWordsSagar$Count, max.words = 30,colors=brewer.pal(8, "Dark2"))

#For joy 
WinWordsSagar = getEmotionalWords('joy','Sagar Gharat')

#For disgust 
WinWordsSagar = getEmotionalWords('disgust','Sagar Gharat')

datatable(head(WinWordsSagar,30), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

