# 1 The tidy text format -----------

# 1.1 The tidy text format -----------
# String: Text can, of course, be stored as strings
# Corpus: These types of objects typically contain raw strings annotated with 
# additional metadata and details.
# Document-term matrix: This is a sparse matrix describing a collection

# 1.2 The unnest_tokens function -----------
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text
library(dplyr)
text_df <- tibble(line = 1:4, text = text) #put it into a data frame.
text_df

library(tidytext)
text_df %>%
        unnest_tokens(word, text, to_lower = TRUE) # 拆词

#  1.3 Tidying the works of Jane Austen ----------------------
library(janeaustenr)
library(dplyr)
library(stringr)

original_books = austen_books() %>%
        group_by(book) %>%
        mutate(linenumber = row_number(),
               chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",ignore_case = TRUE)))) %>%
        ungroup()

tidy_books <- original_books %>%
        unnest_tokens(word, text)

data(stop_words)
tidy_books <- tidy_books %>%
        anti_join(stop_words)
tidy_books %>%
        count(word, sort = TRUE) 

library(ggplot2)

tidy_books %>%
        count(word, sort = TRUE) %>%
        filter(n > 600) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word)) +
        geom_col() +
        labs(y = NULL)

# 1.4 The gutenbergr package -------------------------
library(gutenbergr)
hgwells <- gutenberg_download(c(35, 36, 5230, 159))
tidy_hgwells <- hgwells %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words)
tidy_hgwells %>%
        count(word, sort = TRUE)

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bronte <- bronte %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words)
tidy_bronte %>%
        count(word, sort = TRUE)


# 1.5 Word frequencies -------------------------
library(tidyr)

frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
        mutate(word = str_extract(word, "[a-z']+")) %>%
        count(author, word) %>%
        group_by(author) %>%
        mutate(proportion = n / sum(n)) %>% 
        select(-n) %>% 
        spread(author, proportion) %>% 
        gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)


library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, 
                      color = abs(`Jane Austen` - proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), 
                             low = "darkslategray4", high = "gray75") +
        facet_wrap(~author, ncol = 2) +
        theme(legend.position="none") +
        labs(y = "Jane Austen", x = NULL)



cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)

cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)







# Sentiment Analysis ----------------------------------------
# One way to analyze the sentiment of a text is to consider the text as a combination 
# of its individual words and the sentiment content of the whole text as the sum of the
# sentiment content of the individual words. This isn’t the only way to approach 
# sentiment analysis, but it is an often-used approach, and an approach that naturally 
# takes advantage of the tidy tool ecosystem.

#  2.1 The sentiments datasets  -------------------------------

library(tidytext)
library(textdata)
afinn = get_sentiments("afinn")
bing = get_sentiments("bing")
nrc = get_sentiments("nrc")

# There are also some domain-specific sentiment lexicons available, constructed 
# to be used with text from a specific content area. Section 5.3.1 explores an 
# analysis using a sentiment lexicon specifically for finance.

#  2.2 Sentiment analysis with inner join  ---------------------------
library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
        group_by(book) %>%
        mutate(
                linenumber = row_number(),
                chapter = cumsum(str_detect(text, 
                                            regex("^chapter [\\divxlc]", 
                                                  ignore_case = TRUE)))) %>%
        ungroup() %>%
        unnest_tokens(word, text)

nrc_joy <- get_sentiments("nrc") %>% 
        filter(sentiment == "joy")

tidy_books %>%
        filter(book == "Emma") %>%
        inner_join(nrc_joy) %>%
        count(word, sort = TRUE)

library(tidyr)

jane_austen_sentiment <- tidy_books %>%
        inner_join(get_sentiments("bing")) %>%
        count(book, index = linenumber %/% 80, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative)
library(ggplot2)

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(.~book, ncol = 2, scales = "free_x")

#  2.3 Comparing the three sentiment dictionaries  ------------------
pride_prejudice <- tidy_books %>% 
        filter(book == "Pride & Prejudice")
pride_prejudice

afinn <- pride_prejudice %>% 
        inner_join(get_sentiments("afinn")) %>% 
        group_by(index = linenumber %/% 80) %>% 
        summarise(sentiment = sum(value)) %>% 
        mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
        pride_prejudice %>% 
                inner_join(get_sentiments("bing")) %>%
                mutate(method = "Bing et al."),
        pride_prejudice %>% 
                inner_join(get_sentiments("nrc") %>% 
                                   filter(sentiment %in% c("positive", 
                                                           "negative"))
                ) %>%
                mutate(method = "NRC")) %>%
        count(method, index = linenumber %/% 80, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative)

# geom_col
bind_rows(afinn, 
          bing_and_nrc) %>%
        ggplot(aes(index, sentiment, fill = method)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~method, ncol = 1, scales = "free_y")
# geom_line
bind_rows(afinn, 
          bing_and_nrc) %>%
        ggplot(aes(index, sentiment, color = method))+
        geom_line()
# detects similar relative changes in the text. 
# Why is, for example, the result for the NRC lexicon biased 
# so high in sentiment compared to the Bing et al. result? 
get_sentiments("nrc") %>% 
        filter(sentiment %in% c("positive", "negative")) %>% 
        count(sentiment)
get_sentiments("bing") %>% 
        count(sentiment)
# This is all important context to keep in mind when choosing a sentiment lexicon for analysis.


#  2.4 Most common positive and negative words --------------------
bing_word_counts <- tidy_books %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        ungroup()

bing_word_counts
bing_word_counts %>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(n, word, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(x = "Contribution to sentiment",
             y = NULL)+
        theme_bw()

# spot an anomaly in the sentiment analysis
custom_stop_words <- bind_rows(tibble(word = c("miss"),  
                                      lexicon = c("custom")), 
                               stop_words)
custom_stop_words

#  2.5 Wordclouds 
library(wordcloud)

tidy_books %>%
        anti_join(custom_stop_words) %>%
        count(word) %>%
        with(wordcloud(word, n, max.words = 100))

library(reshape2)

tidy_books %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("gray20", "gray80"),
                         max.words = 100)

#  2.6 Looking at units beyond just words ------
p_and_p_sentences <- tibble(text = prideprejudice) %>% 
        unnest_tokens(sentence, text, token = "sentences")
p_and_p_sentences

# The sentence tokenizing does seem to have a bit of trouble with UTF-8 encoded 
# text, especially with sections of dialogue; it does much better with punctuation 
# in ASCII. One possibility, if this is important, is to try using iconv(), 
# with something like iconv(text, to = 'latin1') in a mutate statement before 
# unnesting.
austen_chapters <- austen_books() %>%
        group_by(book) %>%
        unnest_tokens(chapter, text, token = "regex", 
                      pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
        ungroup()

austen_chapters %>% 
        group_by(book) %>% 
        summarise(chapters = n())

bingnegative <- get_sentiments("bing") %>% 
        filter(sentiment == "negative")

wordcounts <- tidy_books %>%
        group_by(book, chapter) %>%
        summarize(words = n())

tidy_books %>%
        semi_join(bingnegative) %>%
        group_by(book, chapter) %>%
        summarize(negativewords = n()) %>%
        left_join(wordcounts, by = c("book", "chapter")) %>%
        mutate(ratio = negativewords/words) %>%
        filter(chapter != 0) %>%
        top_n(1) %>%
        ungroup()




# 3 Analyzing word and document frequency: tf-idf -------------------------

## One measure of how important a word may be is its term frequency (tf), how frequently a
## word occurs in a document
## Another approach is to look at a term’s inverse document frequency (idf), which decreases 
## the weight for commonly used words and increases the weight for words that are not used very
## much in a collection of documents. 
## This can be combined with term frequency to calculate a term’s tf-idf (the two quantities 
## multiplied together), the frequency of a term adjusted for how rarely it is used.

## It is a rule-of-thumb or heuristic quantity; while it has proved useful in text mining,
## search engines, etc., its theoretical foundations are considered less than firm by information 
## theory experts. The inverse document frequency for any given term is defined as：

## idf(term) = ln(n_documents / n_documents_containing_term)

# 3.1 Term frequency in Jane Austen’s novels -----------------------------
library(dplyr)
library(janeaustenr)
library(tidytext)
book_words <- austen_books() %>%
        unnest_tokens(word, text) %>%
        count(book, word, sort = TRUE)

total_words <- book_words %>% 
        group_by(book) %>% 
        summarize(total = sum(n))

book_words <- left_join(book_words, total_words)
book_words

library(ggplot2)

ggplot(book_words, aes(n/total, fill = book)) +
        geom_histogram(show.legend = FALSE) +
        xlim(NA, 0.0009) +
        facet_wrap(~book, ncol = 2, scales = "free_y")
## There are very long tails to the right for these novels (those extremely rare words!) 
## that we have not shown in these plots. These plots exhibit similar distributions for all 
## the novels, with many words that occur rarely and fewer words that occur frequently.    


# 3.2 Zipf’s law-------------------------------
## Zipf’s law states that the frequency that a word appears is inversely proportional to its rank.
freq_by_rank <- book_words %>% 
        group_by(book) %>% 
        mutate(rank = row_number(), 
               `term frequency` = n/total) %>%
        ungroup()

freq_by_rank %>% 
        ggplot(aes(rank, `term frequency`, color = book)) + 
        geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
        scale_x_log10() +
        scale_y_log10() + theme_bw()

rank_subset <- freq_by_rank %>% 
        filter(rank < 500,
               rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

## Classic versions of Zipf’s law have
## frequency = 1/rank
freq_by_rank %>% 
        ggplot(aes(rank, `term frequency`, color = book)) + 
        geom_abline(intercept = -0.62, slope = -1.1, 
                    color = "gray50", linetype = 2) +
        geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
        scale_x_log10() +
        scale_y_log10()


# 3.3 The bind_tf_idf() function -------------------------------------
## The idea of tf-idf is to find the important words for the content of each document 
## by decreasing the weight for commonly used words and increasing the weight for words that 
## are not used very much in a collection

book_tf_idf <- book_words %>%
        bind_tf_idf(word, book, n)
book_tf_idf
## We calculated a total for each book for our explorations in previous sections, 
## but it is not necessary for 
## the bind_tf_idf() function; the table only needs to contain all the words in each document.
book_tf_idf %>%
        select(-total) %>%
        arrange(desc(tf_idf))

library(forcats)

book_tf_idf %>%
        group_by(book) %>%
        slice_max(tf_idf, n = 15) %>%
        ungroup() %>%
        ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~book, ncol = 2, scales = "free") +
        labs(x = "tf-idf", y = NULL)
## This is the point of tf-idf; it identifies words that are important to one document
## within a collection of documents.




# 3.4 A corpus of physics texts -----------
library(gutenbergr)
physics <- gutenberg_download(c(37729, 14725, 13476, 30155), 
                              meta_fields = "author")
physics_words <- physics %>%
        unnest_tokens(word, text) %>%
        count(author, word, sort = TRUE)
physics_words

plot_physics <- physics_words %>%
        bind_tf_idf(word, author, n) %>%
        mutate(author = factor(author, levels = c("Galilei, Galileo",
                                                  "Huygens, Christiaan", 
                                                  "Tesla, Nikola",
                                                  "Einstein, Albert")))
plot_physics %>% 
        group_by(author) %>% 
        slice_max(tf_idf, n = 15) %>% 
        ungroup() %>%
        mutate(word = reorder(word, tf_idf)) %>%
        ggplot(aes(tf_idf, word, fill = author)) +
        geom_col(show.legend = FALSE) +
        labs(x = "tf-idf", y = NULL) +
        facet_wrap(~author, ncol = 2, scales = "free")

library(stringr)

physics %>% 
        filter(str_detect(text, "_k_")) %>% 
        select(text)

physics %>% 
        filter(str_detect(text, "RC")) %>% 
        select(text)

## Let’s remove some of these less meaningful words to make a better, more meaningful plot. 
#3 Notice that we make a custom list of stop words and use anti_join() to remove them; this is a 
## flexible approach that can be used in many situations. We will need to go back a few steps
## since we are removing words from the tidy data frame.
mystopwords <- tibble(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                               "fig", "file", "cg", "cb", "cm",
                               "ab", "_k", "_k_", "_x"))
physics_words <- anti_join(physics_words, mystopwords, 
                           by = "word")
plot_physics <- physics_words %>%
        bind_tf_idf(word, author, n) %>%
        mutate(word = str_remove_all(word, "_")) %>%
        group_by(author) %>% 
        slice_max(tf_idf, n = 15) %>%
        ungroup() %>%
        mutate(word = reorder_within(word, tf_idf, author)) %>%
        mutate(author = factor(author, levels = c("Galilei, Galileo",
                                                  "Huygens, Christiaan",
                                                  "Tesla, Nikola",
                                                  "Einstein, Albert")))

ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
        geom_col(show.legend = FALSE) +
        labs(x = NULL, y = "tf-idf") +
        facet_wrap(~author, ncol = 2, scales = "free") +
        coord_flip() +
        scale_x_reordered()













# 4 Relationships between words: n-grams and correlations-----------
## So far we’ve considered words as individual units, and considered their relationships to 
## sentiments or to documents.
## many interesting text analyses are based on the relationships between words, whether 
## examining which words tend to follow others immediately, or that tend to co-occur within 
## the same documents.
## visualizing relationships between words in your text dataset. This includes the token = "ngrams" 
## argument, which tokenizes by pairs of adjacent words rather than by individual ones.
library(dplyr)
library(tidytext)
library(janeaustenr)
## When we set n to 2, we are examining pairs of two consecutive words, often called “bigrams”:
austen_bigrams <- austen_books() %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2)
austen_bigrams

# 4.1.1 Counting and filtering n-grams --------
austen_bigrams %>%
        count(bigram, sort = TRUE)
library(tidyr)

bigrams_separated <- austen_bigrams %>%
        separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word)
# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
        count(word1, word2, sort = TRUE)
bigrams_united <- bigrams_filtered %>%
        unite(bigram, word1, word2, sep = " ")

bigrams_united

austen_books() %>%
        unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
        separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
        filter(!word1 %in% stop_words$word,
               !word2 %in% stop_words$word,
               !word3 %in% stop_words$word) %>%
        count(word1, word2, word3, sort = TRUE)


# 4.1.2 Analyzing bigrams --------
bigrams_filtered %>%
        filter(word2 == "street") %>%
        count(book, word1, sort = TRUE)
bigram_tf_idf <- bigrams_united %>%
        count(book, bigram) %>%
        bind_tf_idf(bigram, book, n) %>%
        arrange(desc(tf_idf))
bigram_tf_idf


# 4.1.3 Using bigrams to provide context in sentiment analysis --------
## . One of the problems with this approach is that a word’s context can matter nearly as much as 
## its presence. For example, the words “happy” and “like” will be counted as positive, even in a 
## sentence like “I’m not happy and I don’t like it!”
bigrams_separated %>%
        filter(word1 == "not") %>%
        count(word1, word2, sort = TRUE)
AFINN <- get_sentiments("afinn")

AFINN
not_words <- bigrams_separated %>%
        filter(word1 == "not") %>%
        inner_join(AFINN, by = c(word2 = "word")) %>%
        count(word2, value, sort = TRUE)
not_words 

library(ggplot2)

not_words %>%
        mutate(contribution = n * value) %>%
        arrange(desc(abs(contribution))) %>%
        head(20) %>%
        mutate(word2 = reorder(word2, contribution)) %>%
        ggplot(aes(n * value, word2, fill = n * value > 0)) +
        geom_col(show.legend = FALSE) +
        labs(x = "Sentiment value * number of occurrences",
             y = "Words preceded by \"not\"")

negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
        filter(word1 %in% negation_words) %>%
        inner_join(AFINN, by = c(word2 = "word")) %>%
        count(word1, word2, value, sort = TRUE)

## 4.1.4 Visualizing a network of bigrams with ggraph -------

library(igraph)
bigram_counts
bigram_graph <- bigram_counts %>%
        filter(n > 20) %>%
        graph_from_data_frame()
bigram_graph


library(ggraph)

set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
        geom_edge_link(linetype = 2) +
        geom_node_point(pch=2) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
        theme_bw()

## A better look:
# We add the edge_alpha aesthetic to the link layer to make links transparent 
# based on how common or rare the bigram is
set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(0.1, "inches"))
ggraph(bigram_graph, layout = "fr") +
        geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                       arrow = a, end_cap = circle(.07, 'inches')) +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
        theme_void()

# Note that this is a visualization of a Markov chain, a common model in text processing. 
# In a Markov chain, each choice of word depends only on the previous word. In this case, 
# a random generator following this model might spit out “dear”, then “sir”, 
# then “william/walter/thomas/thomas’s”, by following each word to the most common words 
# that follow it. 

# 4.1.5 Visualizing bigrams in other texts ----------------------------
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)

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

# the King James version is book 10 on Project Gutenberg:
library(gutenbergr)
kjv <- gutenberg_download(10)

library(stringr)

kjv_bigrams <- kjv %>%
        count_bigrams()

# filter out rare combinations, as well as digits
kjv_bigrams %>%
        filter(n > 40,
               !str_detect(word1, "\\d"),
               !str_detect(word2, "\\d")) %>%
        visualize_bigrams()

# 4.2 Counting and correlating pairs of words with the widyr package ----------
# The widyr package makes operations such as computing counts and correlations easy, 
# by simplifying the pattern of “widen data, perform an operation, then re-tidy data” 

## 4.2.1 Counting and correlating among sections ----------
# Consider the book “Pride and Prejudice” divided into 10-line sections, 
# as we did (with larger sections) for sentiment analysis in Chapter 2. 
# We may be interested in what words tend to appear within the same section.

austen_section_words <- austen_books() %>%
        filter(book == "Pride & Prejudice") %>%
        mutate(section = row_number() %/% 10) %>%
        filter(section > 0) %>%
        unnest_tokens(word, text) %>%
        filter(!word %in% stop_words$word)

austen_section_words

## One useful function from widyr is the pairwise_count() function. 
## The prefix pairwise_ means it will result in one row for each pair of words 
## in the word variable. This lets us count common pairs of words co-appearing 
## within the same section:

library(widyr)
# count words co-occuring within sections
word_pairs <- austen_section_words %>%
        pairwise_count(word, section, sort = TRUE)
word_pairs


# 4.2.2 Pairwise correlation  ----------
# We may instead want to examine correlation among words, which indicates how 
# often they appear together relative to how often they appear separately.
# we need to filter for at least relatively common words first
word_cors <- austen_section_words %>%
        group_by(word) %>%
        filter(n() >= 20) %>%
        pairwise_cor(word, section, sort = TRUE)
word_cors
word_cors %>%
        filter(item1 == "pounds")

word_cors %>%
        filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
        group_by(item1) %>%
        slice_max(correlation, n = 6) %>%
        ungroup() %>%
        mutate(item2 = reorder(item2, correlation)) %>%
        ggplot(aes(item2, correlation)) +
        geom_bar(stat = "identity") +
        facet_wrap(~ item1, scales = "free") +
        coord_flip()

set.seed(2016)

word_cors %>%
        filter(correlation > .15) %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), repel = TRUE) +
        theme_void()
# Note that unlike the bigram analysis, the relationships here are symmetrical, 
# rather than directional (there are no arrows). 










# 5 Converting to and from non-tidy formats -----------
## This lets us use the popular suite of tidy tools such as dplyr, tidyr, and ggplot2 
## to explore and visualize text data. We’ve demonstrated that many informative text 
## analyses can be performed using these tools.
## However, most of the existing R tools for natural language processing,
## besides the tidytext package, aren’t compatible with this format. 
## The CRAN Task View for Natural Language Processing lists a large selection 
## of packages that take other structures of input and provide non-tidy outputs. 
## These packages are very useful in text mining applications, and many existing 
## text datasets are structured according to these formats.


## 5.1 Tidying a document-term matrix-----------
## One of the most common structures that text mining packages work with is the 
## document-term matrix (or DTM). This is a matrix where:
     
## each row represents one document (such as a book or article),
## each column represents one term, and
## each value (typically) contains the number of appearances of that term in that document.

## Since most pairings of document and term do not occur (they have the value zero),
## DTMs are usually implemented as sparse matrices. 

# tidy() turns a document-term matrix into a tidy data frame.
# This verb comes from the broom package (Robinson 2017), 
# which provides similar tidying functions for many statistical models and objects.
# cast() turns a tidy one-term-per-row data frame into a matrix. 
# tidytext provides three variations of this verb, each converting to a 
# different type of matrix: 
# cast_sparse() (converting to a sparse matrix from the Matrix package), 
# cast_dtm() (converting to a DocumentTermMatrix object from tm), and 
# cast_dfm() (converting to a dfm object from quanteda).


## 5.1.1 Tidying DocumentTermMatrix objects -------
library(tm)
data("AssociatedPress", package = "topicmodels")
AssociatedPress
class(AssociatedPress)
terms <- Terms(AssociatedPress)
head(terms)

## If we wanted to analyze this data with tidy tools, 
## we would first need to turn it into a data frame with one-token-per-document-per-row.
## The broom package introduced the tidy() verb, which takes a non-tidy object and
## turns it into a tidy data frame. 
## The tidytext package implements this method for DocumentTermMatrix objects.

library(dplyr)
library(tidytext)
ap_td <- tidy(AssociatedPress)
ap_td

## This tidying operation is similar to the melt() function 
# from the reshape2 package (Wickham 2007) for non-sparse matrices.


## 5.1.2 Tidying dfm objects
## Other text mining packages provide alternative implementations of 
## document-term matrices, such as the dfm (document-feature matrix) class 
## from the quanteda package (Benoit and Nulty 2016). 
## For example, the quanteda package comes with a corpus of presidential 
## inauguration speeches, which can be converted to a dfm using the appropriate function.
data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = FALSE)
inaug_dfm
inaug_td <- tidy(inaug_dfm)
inaug_td

inaug_tf_idf <- inaug_td %>%
        bind_tf_idf(term, document, count) %>%
        arrange(desc(tf_idf))



library(tidyr)

## complete operation to make it zero 

year_term_counts <- inaug_td %>%
        extract(document, "year", "(\\d+)", convert = TRUE) %>%
        complete(year, term, fill = list(count = 0)) %>%
        group_by(year) %>%
        mutate(year_total = sum(count))

year_term_counts %>%
        filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>%
        ggplot(aes(year, count / year_total)) +
        geom_point(pch = 2) +
        geom_smooth() +
        facet_wrap(~ term, scales = "free_y") +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(y = "% frequency of word in inaugural address")


# 5.2 Casting tidy text data into a matrix ------------
ap_td %>%
        cast_dtm(document, term, count)
ap_td %>%
        cast_dfm(document, term, count)

library(Matrix)

# cast into a Matrix object
m <- ap_td %>%
        cast_sparse(document, term, count)

class(m)
dim(m)


library(janeaustenr)

austen_dtm <- austen_books() %>%
        unnest_tokens(word, text) %>%
        count(book, word) %>%
        cast_dtm(book, word, n)

austen_dtm 

# 5.3 Tidying corpus objects with metadata --------
## Some data structures are designed to store document collections before tokenization,
## often called a “corpus”. One common example is Corpus objects from the tm package.
## These store text alongside metadata, which may include an ID, date/time, title, 
## or language for each document.
data("acq")
acq
acq[[1]]
acq_td <- tidy(acq)
acq_tokens <- acq_td %>%
        select(-places) %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words, by = "word")
acq_tokens %>%
        count(word, sort = TRUE)
acq_tokens %>%
        count(id, word) %>%
        bind_tf_idf(word, id, n) %>%
        arrange(desc(tf_idf))

# 5.3.1 Example: mining financial articles -----------
# Corpus objects are a common output format for data ingesting packages, 
# which means the tidy() function gives us access to a wide variety of text data. 
# One example is tm.plugin.webmining, which connects to online feeds to retrieve 
# news articles based on a keyword. 
# For instance, performing WebCorpus(GoogleFinanceSource("NASDAQ:MSFT")) allows us to 
# retrieve the 20 most recent articles related to the Microsoft (MSFT) stock.

# Here we’ll retrieve recent articles relevant to nine major technology stocks: 
# Microsoft, Apple, Google, Amazon, Facebook, Twitter, IBM, Yahoo, and Netflix.

library(tm.plugin.webmining)
library(purrr)

company <- c("Microsoft", "Apple", "Google", "Amazon", "Facebook", "IBM", "Yahoo", "Netflix")
symbol  <- c("MSFT", "AAPL", "GOOG", "AMZN", "FB", 
              "IBM", "YHOO", "NFLX")

download_articles <- function(symbol) {
        WebCorpus(GoogleFinanceSource(paste0("NASDAQ:", symbol)))
}

stock_articles <- tibble(company = company,
                         symbol = symbol) %>%
        mutate(corpus = map(symbol, download_articles))




