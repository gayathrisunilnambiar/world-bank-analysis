# World Bank Text Mining — Development Policy Documents
# TF-IDF analysis and word cloud on 15 thematic policy documents
# Uses base R string ops (no tm dependency)

library(wordcloud)
library(RColorBrewer)
library(ggplot2)

# Load documents from TMdocs.txt — strip surrounding quotes and trailing commas
raw_lines <- readLines("TMdocs.txt")
docs_text <- trimws(gsub('^[[:space:]]*"|"[[:space:]]*,?[[:space:]]*$', "", raw_lines))
docs_text <- docs_text[nchar(docs_text) > 0]

# English stopwords (base R, no tm)
stop_words <- c(
  "a","an","the","and","or","of","in","to","for","on","with","by","at","from",
  "is","are","was","were","be","been","being","have","has","had","do","does",
  "did","will","would","could","should","may","might","can","per","also","via",
  "as","it","its","that","this","these","those","which","who","what","how",
  "if","than","then","but","so","yet","both","each","more","other","such"
)

# Preprocess: lowercase → remove punctuation/numbers → tokenize → remove stopwords → stem
stem_word <- function(w) {
  w <- sub("ation$|ations$", "ate", w)
  w <- sub("ities$|ity$",    "ite", w)
  w <- sub("ments$|ment$",   "ment", w)
  w <- sub("ing$",           "",    w)
  w <- sub("tion$",          "te",  w)
  w <- sub("ers$|er$",       "",    w)
  w <- sub("ness$",          "",    w)
  w <- sub("ies$",           "y",   w)
  w <- sub("s$",             "",    w)
  w
}

preprocess <- function(text) {
  text  <- tolower(text)
  text  <- gsub("[^a-z ]", " ", text)
  words <- unlist(strsplit(text, "\\s+"))
  words <- words[nchar(words) > 2 & !words %in% stop_words]
  stem_word(words)
}

all_tokens <- lapply(docs_text, preprocess)
vocab      <- sort(table(unlist(all_tokens)), decreasing = TRUE)

# Build Document-Term Matrix (as list of term counts per doc)
dtm <- lapply(all_tokens, function(toks) table(toks))

# TF-IDF
n_docs <- length(docs_text)
idf <- sapply(names(vocab), function(term) {
  df <- sum(sapply(dtm, function(d) term %in% names(d)))
  log(n_docs / max(df, 1))
})
tfidf_scores <- sort(
  sapply(names(vocab), function(term) {
    tf  <- vocab[[term]] / sum(vocab)
    tf * idf[[term]]
  }),
  decreasing = TRUE
)

cat("Documents            :", n_docs, "\n")
cat("Unique terms         :", length(vocab), "\n\n")
cat("Top 15 terms by frequency:\n")
print(head(vocab, 15))
cat("\nTop 10 terms by TF-IDF:\n")
print(round(head(tfidf_scores, 10), 4))

# Word cloud
set.seed(42)
png("output/16_wordcloud.png", width = 900, height = 650, res = 120)
wordcloud(names(vocab), as.numeric(vocab),
          min.freq     = 2,
          max.words    = 80,
          random.order = FALSE,
          rot.per      = 0.25,
          colors       = brewer.pal(8, "Dark2"),
          scale        = c(3.5, 0.5))
title(main = "Word Cloud: World Development Policy Documents", cex.main = 1.2)
dev.off()

# Bar chart — top 20 terms
top20 <- data.frame(term  = names(head(vocab, 20)),
                    count = as.numeric(head(vocab, 20)))
p_bar <- ggplot(top20, aes(x = reorder(term, count), y = count, fill = count)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_c(option = "plasma", guide = "none") +
  theme_minimal() +
  labs(title = "Top 20 Terms — Development Policy Corpus",
       x = "Term (stemmed)", y = "Frequency")
ggsave("output/17_term_frequency_bar.png", p_bar, width = 8, height = 6, dpi = 150)

message("✓ Text mining complete")
message("  Corpus     : ", n_docs, " documents")
message("  Vocabulary : ", length(vocab), " unique terms after preprocessing")
message("  Output: output/16_wordcloud.png, output/17_term_frequency_bar.png")
