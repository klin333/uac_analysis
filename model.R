
library(tidyverse)
library(text2vec)
library(plotly)
library(zeallot)

course_df <- read_csv('data/courses.csv') %>% mutate(course_code = str_pad(course_code, width = 6, pad = '0'))

load_glove <- function(vector_dim) {
  # downloaded from https://nlp.stanford.edu/projects/glove/
  stopifnot(vector_dim %in% c(50, 100, 200, 300))
  glove_dict <- read_lines(sprintf('data/glove.6B/glove.6B.%id.txt', vector_dim))
  
  mixed_m <- str_split_fixed(glove_dict, ' ', vector_dim+1)
  
  tokens <- mixed_m[, 1]
  embeddings <- mixed_m[, 2:ncol(mixed_m)]
  class(embeddings) <- 'numeric'
  
  embedding_list <- as.list(data.frame(t(embeddings)))
  names(embedding_list) <- tokens
  
  embedding_list
}

# embedding_m <- embeddings
# rownames(embedding_m) <- embedding_tokens

embedding_list <- load_glove(300)
embedding_tokens <- names(embedding_list)

c('physics', 'engineering', 'Engineering') %in% embedding_tokens

embedding_list[['king']] - embedding_list[['queen']]
embedding_list[['man']] - embedding_list[['woman']]

paragraph <- course_df$area_of_study[1]


get_tfidf_model <- function(documents) {
  it = itoken(documents, 
              preprocessor = tolower, 
              tokenizer = tokenizers::tokenize_words, 
              progressbar = FALSE)
  vocab = create_vocabulary(it)
  
  vectorizer = vocab_vectorizer(vocab)
  dtm = create_dtm(it, vectorizer)
  
  tfidf_model = TfIdf$new()
  dtm_tfidf = fit_transform(dtm, tfidf_model)
  list(
    vectorizer = vectorizer,
    tfidf_model = tfidf_model
  )
}


get_tfidf <- function(tfidf_model, vectorizer, document) {
  it <- itoken(document, preprocessor = tolower, tokenizer = tokenizers::tokenize_words)
  dtm <- create_dtm(it, vectorizer)
  x <- as.matrix(transform(dtm, tfidf_model))
  
  have <- as.vector(x != 0)
  tokens <- colnames(x)[have]
  values <- as.vector(x)[have]
  names(values) <- tokens
  
  values
}


embed_paragraph <- function(documents) {
  
  c(vectorizer, tfidf_model) %<-% get_tfidf_model(documents)
  
  map(documents, function(document) {
    
    # to do: make use of text2vec functions as opposed to hacking it together ourselves
    tokens <- tokenizers::tokenize_words(document)[[1]] 
    tokens <- unique(tokens)
    vectors <- embedding_list[tokens] 
    
    found_tokens <- names(vectors)
    found_tokens <- found_tokens[!is.na(found_tokens)]
    
    vectors_m <- do.call(rbind, vectors)  # drops tokens not in vocab
    
    tfidf <- get_tfidf(tfidf_model, vectorizer, document)
    tfidf <- tfidf[found_tokens]
    stopifnot(names(tfidf) == found_tokens)
    
    if (!is.null(vectors_m)) {
      # weights <- rep(1, nrow(vectors_m)) / nrow(vectors_m)  # simple average
      weights <- tfidf
      paragraph_vector <- t(weights) %*% vectors_m
    } else {
      paragraph_vector <- NULL
    }
    
    paragraph_vector
  })
  
}

enriched_course_df <- course_df %>%
  # head(10) %>%
  mutate(area_of_study_vector = embed_paragraph(area_of_study))

saveRDS(enriched_course_df, 'data/enriched_course_df_tfidf.rds')

df <- enriched_course_df %>% 
  filter(!map_lgl(area_of_study_vector, is.null)) %>%
  group_by(area_of_study_vector) %>%
  filter(row_number() == 1) %>%  # to do: maybe collapse course names
  ungroup()
  
m <- do.call(rbind, df$area_of_study_vector)
rownames(m) <- df$course_code

area_of_study_tsne <- Rtsne::Rtsne(m)

tsne_df <- area_of_study_tsne$Y %>% 
  as.data.frame() %>%
  as_tibble() %>%
  rename(tSNE1="V1", tSNE2="V2") %>%
  mutate(course_code = df$course_code) 

write_csv(tsne_df, 'data/area_of_study_tsne.csv')

ggplotly(
  tsne_df %>%
    inner_join(course_df %>% select(course_code, uni_code, course_name, course_field), by = 'course_code') %>%
    filter(!str_detect(course_name, '/')) %>% # ignore double degrees
    filter(uni_code %in% c('unsw', 'usyd', 'uts', 'mq', 'ws', 'acu')) %>%
    # filter(uni_code == 'unsw') %>%
    ggplot(aes(x = tSNE1, y = tSNE2, text = course_name, color = course_field)) +
    geom_point() +
    theme(legend.position="bottom")
)

