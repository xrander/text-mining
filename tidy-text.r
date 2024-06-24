library(tidyverse)
library(tidytext)
library(janeaustenr)
library(gutenbergr)

text <- c(
  "Because I could not stop for Death -",
  "He kindly stopped for me .",
  "The carriage held but just Ourselves -",
  "and Immortality"
)

text_df <- tibble(
  line = 1:4, 
  text = text
)
text_df |> 
  unnest_tokens(word, text)

# Tidying the works of Jane Austen ----------------------------------
original_books <- austen_books() |> 
  group_by(book) |> 
  mutate(
    line_number = row_number(),
    chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                            ignore_case = TRUE)))
  ) |> 
  ungroup()

original_books
tidy_books <- original_books |> 
  unnest_tokens(word, text)

# Removing stop-words -----------------------------------------------
data(stop_words)

tidy_books <- tidy_books |> 
  anti_join(stop_words)

tidy_books |> 
  count(word, sort = TRUE) |> 
  filter(n > 600) |> 
  ggplot(aes(n, fct_reorder(word, n))) +
  geom_col() +
  ylab(NULL)

# Word Frequencies --------------------------------------------------

hgwells <- gutenberg_download(c(35, 36, 5230))

tidy_hgwells <- hgwells |> 
  unnest_tokens(word, text) |> 
  anti_join(stop_words)

tidy_hgwells |> 
  count(word, sort = TRUE)

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- bronte |> 
  unnest_tokens(word, text) |> 
  anti_join(stop_words)

tidy_bronte |> 
  count(word, sort = TRUE)

frequency <- bind_rows(
  mutate(tidy_bronte, author = "Brontë Sisters"),
  mutate(tidy_hgwells, author = "H.G. Wells"),
  mutate(tidy_books, author = "Jane Austen")
) |> 
  select(-c(4:6)) |> 
  mutate(
    word = str_extract(word, "[a-z]+")
  ) |> 
  count(author, word) |> 
  mutate(
    .by = author,
    proportion = n/sum(n)
  ) |> 
  pivot_wider(
    id_cols = word,
    names_from = author,
    values_from = proportion
  ) |> 
  pivot_longer(
    cols = c(2, 3),
    names_to = "author",
    values_to = "proportion"
  ) |> 
  janitor::clean_names()

frequency |> 
  ggplot(aes(proportion, jane_austen, color = abs(jane_austen- proportion))) +
  geom_abline(
    color = "gray40",
    linetype = 2
  ) +
  geom_jitter(
    alpha = .1,
    size = 2.5, 
    width = .3,
    height = .3
  ) +
  geom_text(
    aes(label = word),
    check_overlap = TRUE,
    width = .3,
    height = .3,
    vjust = 1.5
  ) +
  scale_x_log10(
    labels = scales::percent_format()
  ) +
  scale_y_log10(
    labels = scales::percent_format()
  ) +
  scale_color_gradient(
    limits = c(0, .001),
    low = "darkslategray4",
    high = "gray75"
  ) +
  labs(
    y = "Jane Austen",
    x = NULL
  ) +
  facet_wrap(~author) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + jane_austen)

cor.test(data = frequency[frequency$author == "H.G. Wells",],
         ~ proportion + jane_austen)


# Sentiment Analysis -----------------------------------------------
tidy_books |> 
  filter(book == "Emma") |> 
  inner_join(
    get_sentiments("nrc") |> 
      filter(sentiment == "joy"),
    join_by(word)
  ) |> 
  count(word, sort = TRUE)

janeaustensentiment <- tidy_books |> 
  inner_join(
    get_sentiments("bing"),
    join_by(word)
  ) |> 
  count(book, index = line_number %/% 80, sentiment) |> 
  pivot_wider(
    names_from = sentiment,
    values_from = n,
    values_fill = 0
  ) |> 
  mutate(
    sentiment = positive - negative
  )

ggplot(
  janeaustensentiment,
  aes(index, sentiment, fill = book)
) +
  geom_col(
    show.legend = FALSE
  ) +
  facet_wrap(~book, scales = "free")

pride_prejudice <- tidy_books |> 
  filter(book == "Pride & Prejudice")

afinn <- pride_prejudice |> 
  inner_join(get_sentiments("afinn")) |> 
  group_by(index = line_number %/%  80) |> 
  summarize(sentiment = sum(value)) |> 
  mutate(
    method = "AFINN"
  )

bing_nrc <- bind_rows(
  pride_prejudice |> 
    inner_join(get_sentiments("bing")) |> 
    mutate(method = "Bing et al."),
  pride_prejudice |> 
    inner_join(get_sentiments("nrc")) |> 
    filter(sentiment %in% c("positive", "negative")) |> 
    mutate(method = "nrc")
)

bing_nrc <- bing_nrc |> 
  count(method, index = line_number %/% 80, sentiment) |> 
  pivot_wider(
    names_from = sentiment,
    values_from = n
  ) |> 
  mutate(sentiment = positive - negative)


afinn |> 
  bind_rows(
    bing_nrc |> 
      relocate(
        index, sentiment, method
      ) |> 
      select(1:3)
  ) |> 
  ggplot(
    aes(index, sentiment, fill = method)
  ) +
  geom_col(show.legend = FALSE) +
  facet_wrap(
    ~method,
    scales = "free_y",
    nrow = 3
  )

tidy_books |> 
  inner_join(get_sentiments("bing")) |>
  count(word, sentiment, sort = TRUE) |> 
  group_by(sentiment) |> 
  top_n(10) |> 
  mutate(word = reorder(word, n)) |> 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(
    y = "Contribution to sentiment",
    x = NULL
  ) +
  coord_flip()

custom_stop_words <- tibble(
  word = "miss",
  lexicon = "custom"
) |> 
  bind_rows(stop_words)


## Wordcloud ----------------------------------------------------------
library(wordcloud)

tidy_books |> 
  anti_join(stop_words) |> 
  count(word) |> 
  with(wordcloud(word, n, max.words = 100))
