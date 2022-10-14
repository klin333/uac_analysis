
library(tidyverse)
library(text2vec)
library(plotly)
library(zeallot)
library(reticulate)
library(keras)


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


train_autoencoder <- function(m) {

  np <- import("numpy", convert=FALSE)

  # dim(np$array(c(1,2,3)))

  m_standardised <- scale(m, scale = FALSE)

  # array <- r_to_py(m)
  array <- r_to_py(m_standardised)

  latent_size = 2
  input_size = dim(m)[2]
  print(input_size)


  # encoder
  enc_input = layer_input(shape = input_size)
  enc_output = enc_input %>%
    # layer_dense(units=10, activation = "tanh") %>%
    # layer_dense(units=latent_size, activation = "tanh")
    # layer_activation_leaky_relu() %>%
    layer_dense(units=latent_size)
    # layer_activation_leaky_relu()

  encoder = keras_model(enc_input, enc_output)
  summary(encoder)

  # decoder
  dec_input = layer_input(shape = latent_size)
  dec_output = dec_input %>%
    # layer_dense(units=10, activation = "tanh") %>%
    # layer_dense(units = input_size, activation = "tanh")
    # layer_activation_leaky_relu() %>%
    layer_dense(units = input_size)
    # layer_activation_leaky_relu()

  decoder = keras_model(dec_input, dec_output)

  summary(decoder)

  # autoencoder
  aen_input = layer_input(shape = input_size)
  aen_output = aen_input %>%
    encoder() %>%
    decoder()

  aen = keras_model(aen_input, aen_output)
  summary(aen)

  # fit
  aen %>% compile(optimizer="rmsprop", loss="cosine_similarity")

  aen %>% fit(array, array, epochs=200, batch_size=128)


  # generating
  encoded_vectors = encoder %>% predict(array)

  list(
    encoder = encoder,
    encoded_vectors = encoded_vectors
  )

}


main <- function() {

  course_df <- read_csv('data/courses.csv', col_types = cols(course_code = 'c'))

  c(embedding_m, vocab_map) %<-% load_glove(300)

  enriched_course_df <- course_df %>%
    # head(10) %>%
    mutate(area_of_study_vector = embed_documents(area_of_study, embedding_m, vocab_map))

  saveRDS(enriched_course_df, 'data/enriched_course_df_tfidf.rds')

  # enriched_course_df <- readRDS('data/enriched_course_df_tfidf.rds')

  df <- enriched_course_df %>%
    filter(!map_lgl(area_of_study_vector, is.null)) %>%
    group_by(area_of_study_vector) %>%
    filter(row_number() == 1) %>%  # to do: maybe collapse course names
    ungroup()

  m <- do.call(rbind, df$area_of_study_vector)
  rownames(m) <- df$course_code

  area_of_study_tsne <- Rtsne::Rtsne(m)

  area_of_study_pca <- prcomp(m, center = T, scale = T)

  plot(area_of_study_pca$sdev)
  plot(cumsum(area_of_study_pca$sdev^2) / sum(area_of_study_pca$sdev^2))

  aos_autoencoded <- get_autoencoder()$encoded_vectors
  # aos_autoencoded <- encoded_vectors

  reduced_df <- area_of_study_tsne$Y %>%
    as.data.frame() %>%
    as_tibble() %>%
    rename(tSNE1="V1", tSNE2="V2") %>%
    mutate(course_code = df$course_code) %>%
    mutate(PC1 = area_of_study_pca$x[, 1], PC2 = area_of_study_pca$x[, 2], PC3 = area_of_study_pca$x[, 3]) %>%
    mutate(auto1 = aos_autoencoded[, 1], auto2 = aos_autoencoded[, 2])

  write_csv(reduced_df, 'data/area_of_study_tsne.csv')

  plot_df <- reduced_df %>%
    inner_join(course_df %>% select(course_code, uni_code, course_name, course_field), by = 'course_code') %>%
    filter(!str_detect(course_name, '/')) %>% # ignore double degrees
    filter(uni_code %in% c('unsw', 'usyd', 'uts', 'mq', 'ws', 'acu')) %>%
    # filter(uni_code == 'unsw') %>%
    select(everything())

  plot_df <- plot_df %>%
    group_by(course_field) %>%
    summarise_at(vars(PC1, PC2, PC3, tSNE1, tSNE2, auto1, auto2), mean) %>%
    ungroup() %>%
    mutate(course_name = course_field)

  ggplotly(
    plot_df %>%
      ggplot(aes(x = tSNE1, y = tSNE2, text = course_name, color = course_field)) +
      geom_point() +
      theme(legend.position="bottom")
  )

  ggplotly(
    plot_df %>%
      ggplot(aes(x = PC1, y = PC2, text = course_name, color = course_field)) +
      geom_point() +
      theme(legend.position="bottom")
  )

  ggplotly(
    plot_df %>%
      ggplot(aes(x = auto1, y = auto2, text = course_name, color = course_field)) +
      geom_point() +
      theme(legend.position="bottom")
  )

  # plots <- list(
  #   plot_df %>% ggplot(aes(x = PC1, y = PC2, text = course_name, color = course_field)),
  #   plot_df %>% ggplot(aes(x = PC1, y = PC3, text = course_name, color = course_field)),
  #   plot_df %>% ggplot(aes(x = PC2, y = PC3, text = course_name, color = course_field))
  # ) %>%
  #   map(function(pl) pl + geom_point() + theme(legend.position="bottom"))

  plots <- list(
    plot_ly(plot_df, x = ~PC1, y = ~PC2, color = ~course_field, legendgroup = ~course_field, text = ~course_name, colors = "Paired"),
    plot_ly(plot_df, x = ~PC1, y = ~PC3, color = ~course_field, legendgroup = ~course_field, showlegend = F, text = ~course_name, colors = "Paired"),
    plot_ly(plot_df, x = ~PC2, y = ~PC3, color = ~course_field, legendgroup = ~course_field, showlegend = F, text = ~course_name, colors = "Paired")
  ) %>%
    map(function(x) x %>% add_markers())

  plotly::subplot(plots, nrows = 1, shareX = FALSE, shareY = FALSE)


  plot_ly(plot_df, x = ~PC1, y = ~PC2, z = ~PC3, color = ~course_field, text = ~course_name, colors = "Paired") %>%
    add_markers()

}

