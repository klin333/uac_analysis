
library(tidyverse)
library(text2vec)
library(plotly)
library(zeallot)


load_glove <- function(vector_dim) {
  # downloaded from https://nlp.stanford.edu/projects/glove/
  stopifnot(vector_dim %in% c(50, 100, 200, 300))
  glove_dict <- read_lines(sprintf('data/glove.6B/glove.6B.%id.txt', vector_dim))

  mixed_m <- str_split_fixed(glove_dict, ' ', vector_dim+1)

  tokens <- mixed_m[, 1]
  embedding_m <- mixed_m[, 2:ncol(mixed_m)]
  class(embedding_m) <- 'numeric'
  rownames(embedding_m) <- tokens

  # create a fast hash map for faster indexing into embedding matrix
  vocab_map <- fastmap::fastmap()
  l <- as.list(seq.int(length(tokens)))
  names(l) <- tokens
  vocab_map$mset(.list = l)

  list(
    embedding_m = embedding_m,
    vocab_map = vocab_map
  )
}


if (FALSE) {

  embedding_tokens <- names(embedding_list)

  c('physics', 'engineering', 'Engineering') %in% embedding_tokens

  embedding_list[['king']] - embedding_list[['queen']]
  embedding_list[['man']] - embedding_list[['woman']]

  document <- course_df$area_of_study[1]

  # try different ways of indexing embedding
  embedding_m <- do.call(rbind, embedding_list)
  rownames(embedding_m) <- embedding_tokens

  vocba_map <- fastmap::fastmap()
  l <- as.list(seq.int(length(embedding_list)))
  names(l) <- names(embedding_list)
  vocba_map$mset(.list = l)

  microbenchmark(
    times = 10, unit = 'ms',
    embedding_list[tokens],
    embedding_m[tokens, ],
    embedding_m[unlist(vocba_map$mget(tokens)), ]
  )

  # Unit: milliseconds
  #                                          expr      min       lq      mean    median       uq      max neval
  #                        embedding_list[tokens] 110.9816 114.1969 117.20174 115.99775 121.5183 124.8416    10
  #                         embedding_m[tokens, ]  24.5218  25.0760  26.94321  26.46415  28.0538  31.4343    10
  # embedding_m[unlist(vocba_map$mget(tokens)), ]   0.1465   0.1891   0.22518   0.23440   0.2606   0.2724    10
}


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


embed_documents <- function(documents, embedding_m, vocab_map) {

  c(vectorizer, tfidf_model) %<-% get_tfidf_model(documents)

  map(documents, function(document) {

    # to do: make use of text2vec functions as opposed to hacking it together ourselves
    tokens <- tokenizers::tokenize_words(document)[[1]]
    tokens <- unique(tokens)
    tokens <- tokens[!is.na(tokens) & tokens != '']
    vectors_m <- embedding_m[unlist(vocab_map$mget(tokens)), , drop = FALSE]

    found_tokens <- rownames(vectors_m)

    tfidf <- get_tfidf(tfidf_model, vectorizer, document)
    tfidf <- tfidf[found_tokens]
    stopifnot(names(tfidf) == found_tokens)

    if (nrow(vectors_m > 0)) {
      # weights <- rep(1, nrow(vectors_m)) / nrow(vectors_m)  # simple average
      weights <- tfidf / sum(tfidf)
      document_vector <- t(weights) %*% vectors_m
    } else {
      document_vector <- NULL
    }

    document_vector
  })

}


main <- function() {

  course_df <- read_csv('data/courses.csv', col_types = cols(course_code = 'c'))

  c(embedding_m, vocab_map) %<-% load_glove(300)

  enriched_course_df <- course_df %>%
    # head(10) %>%
    mutate(area_of_study_vector = embed_documents(area_of_study, embedding_m, vocab_map))

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

}

