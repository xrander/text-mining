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
  select(-n) |> 
  arrange(word) |> 
  relocate(word)

ggplot()


alice_wl <- gutenberg_download(c(28885, 38065))

alice_wl
