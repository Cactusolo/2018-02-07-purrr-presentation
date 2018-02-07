install.packages(c("tidyverse", "repurrrsive"))

library(tidyverse)
library(repurrrsive)

sw_people
length(sw_people)
sw_people[[1]]
sw_people[[1]]$name
sw_people[[1]][[1]]

sw_people[1]

## How many starships has Luke been in?
length(sw_people[[1]]$starships)

map(sw_people, ~ length(.$starships))
map(sw_people, function(foo) length(foo$starships))

## the other map functions
map_lgl()
map_int()
map_dbl()
map_chr()

walk()

## an example
map_dbl(sw_people, ~.[["mass"]])
map(sw_people, ~.[["mass"]])

map_dbl(sw_people, ~ as.numeric(.[["mass"]]))

map_chr(sw_people, ~ .[["mass"]]) %>% 
  readr::parse_number(na = "unknown")

## a little more about .f
map_chr(sw_people, "mass")

char_starships <- map(sw_people, "starships")
map_int(char_starships, length)

map(sw_people, "starships") %>% 
  map_int(length)

## create planet_lookup
planet_lookup <- map_chr(sw_planets, "name") %>% 
  set_names(map_chr(sw_planets, "url"))

## Which film (see sw_films) has the most
## characters?

map_int(sw_films, ~ length(.$characters)) %>% 
  set_names(map_chr(sw_films, "title")) %>% 
  sort(dec=TRUE)

## map2

map2(c("cat", "dog", "rat"),
     c(3, 2, 1),
     rep)

map2(c(100, 50, 75),
     c(4, 5, 3),
     rnorm)

## pmap

pmap(list(
  n = c(100, 50, 75),
  mean = c(4, 5, 3),
  sd = c(1, 5, .2)
), rnorm)

## an example

gap_split_small <- gap_split[1:10]
countries <- names(gap_split_small)

ggplot(gap_split_small[[1]]) +
  geom_line(aes(x = year, y = lifeExp)) +
  labs(title = countries[[1]])

plots <- map2(gap_split_small, countries,
              ~ ggplot(.x) +
  geom_line(aes(x = year, y = lifeExp)) +
  labs(title = .y))

walk(plots, print)

walk2(gap_split, names(gap_split),
      ~ write_csv(.x, file.path("data", 
            paste0("gapminder_", .y, ".csv")))
      )

regap <- list.files(path="data", pattern="csv$",
                    full.names = TRUE) %>% 
  map_dfr(read_csv)
regap

## create data frame from sw_people
people_tbl <- tibble( 
  name = sw_people %>% map_chr("name"),
  height = sw_people %>% map_chr("height") %>% 
    readr::parse_number(na="unknown"),
  species = sw_people %>% 
    map_chr("species", .null = NA_character_),
  films = sw_people %>% map("films")
  )

tidyr::unnest(people_tbl)

people_tbl %>% 
  mutate(
    n_films = map_int(films, length)
  ) %>% 
  select(name, n_films)



