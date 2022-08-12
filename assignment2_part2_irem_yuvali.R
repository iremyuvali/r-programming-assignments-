# irem yuvali | 2017555071

library(tidyverse)
library(stringr)
library(htmlwidgets)

set.seed(2017555071)

sample_sent <- sample(sentences, size = 1000, replace = TRUE)

words <- unlist(str_split(sample_sent, " ", simplify = TRUE))

words <- unique(words)

new_data <- c(words)


# Find words which are starting with “a” and ending with “e”.

a_e_words <- new_data[startsWith(new_data, "a")]

a_e_words <- a_e_words[endsWith(a_e_words, "e")]

unique(a_e_words, incomparables = TRUE)

view(a_e_words)


# consisting MORE than three vowels 

three_vowel <- str_count(new_data, regex("[aeiou]")) > 3
three_vowel

sum(str_detect(three_vowel,"TRUE"))


# consisting some specific strings

pattern <- c("age","any","day","exp","her","pro","the")

sum(str_detect(new_data, pattern))


# the longest five words in the data.

longest_five <- new_data[order(nchar(new_data), new_data, decreasing = TRUE)]

head(longest_five,5)
