library(data.table)
library(tidyverse)

titles = fread('https://datasets.imdbws.com/title.basics.tsv.gz')
ratings = fread('https://datasets.imdbws.com/title.ratings.tsv.gz')
episodes = fread('https://datasets.imdbws.com/title.episode.tsv.gz')

fwrite(titles, 'data/raw_titles.csv')
fwrite(ratings, 'data/raw_ratings.csv')
fwrite(episodes, 'data/raw_episodes.csv')

tv_viz_raw = episodes |>
  inner_join(titles |> select(tconst, primaryTitle, startYear), by = 'tconst') |>
  inner_join(ratings, by = 'tconst') |>
  inner_join(titles |>
               select(tconst, primaryTitle, startYear) |>
               rename(show_title = primaryTitle) |>
               rename(show_start = startYear),
             by = c('parentTconst' = 'tconst')
  ) |>
  mutate(across(c(seasonNumber, episodeNumber), as.integer)) |>
  group_by(parentTconst, seasonNumber) |>
  mutate(contains_0 = min(episodeNumber) == 0) |>
  mutate(seasonAverage = mean(averageRating, na.rm = TRUE)) |>
  ungroup()

tv_viz = tv_viz_raw |>
  filter(!contains_0) |>
  select(-contains_0) |>
  rbind(tv_viz_raw |>
          filter(contains_0) |>
          mutate(episodeNumber = episodeNumber + 1) |>
          select(-contains_0)
        ) |>
  ungroup() |>
  mutate(episodeNumber = str_pad(episodeNumber, width = 2, pad = '0')) |>
  mutate(seasonNumber = paste0('S', str_pad(seasonNumber, width = 2, pad = '0'))) |>
  mutate(episodeNumber = paste0('E', str_pad(episodeNumber, width = 2, pad = '0')))

show_select = tv_viz |>
  select(show_title, parentTconst) |>
  distinct() |>
  mutate(show_title = str_to_upper(show_title))

fwrite(tv_viz, 'data/imdb_tv.csv')
fwrite(show_select, 'data/imdb_tv_show_select.csv')
