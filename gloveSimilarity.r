library(text2vec)
library(readr)
library(magrittr)
library(stringr)
library(jiebaR)
DFGDRaw <- read_csv("E:/pathology/DFGDRaw.csv")
conclusion=paste0(DFGDRaw$病理诊断,collapse = "") %>%
  str_replace_all(pattern="[:space:]|NA|:|=",replacement="") %>% 
  segment(worker(stop_word = "D:/cancerTumor/stopword.txt", user ='D:/cancerTumor/userDict.dict',symbol=T)) %>% 
  str_replace_all(pattern="[0-9]{1,}|\\.",replacement="") %>% 
  paste0(collapse=" ")

tokens=space_tokenizer(conclusion)
it = itoken(tokens, progressbar = T)
vocab <- create_vocabulary(it) %>% prune_vocabulary(term_count_min = 2L)

vectorizer <- vocab_vectorizer(vocab, grow_dtm = FALSE, skip_grams_window = 5L)

tcm <- create_tcm(it, vectorizer)
glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
glove$fit(tcm, n_iter = 50)

word_vectors <- glove$get_word_vectors()

query="血管"

cos_sim=sim2(x = word_vectors, y=word_vectors[query, , drop = FALSE], method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)
