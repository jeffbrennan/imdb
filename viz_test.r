# setup ---------------------------------------------------------------------------------------
library(data.table)
library(tidyverse)
library(RColorBrewer)

JB_COLORS = list(
  orange_teal_11 = c('#ff782a',
                     '#ff8d4d',
                     '#ffa26e',
                     '#ffb68e',
                     '#ffc9ae',
                     '#faddcf',
                     '#f1f1f1',
                     '#dae7e7',
                     '#c4dddd',
                     '#add4d3',
                     '#96caca',
                     '#7ec1c0',
                     '#63b7b7'
  ),
  orange =           '#ff782a',
  orange_dark =      '#FC581D',
  fuchsia =          '#C23180',
  teal =             '#63b7b7',
  green =            '#488f31',
  red =              '#de425b',
  purple =           '#923AB9',
  teal_white =       '#D9FBFB',
  teal_accent =      '#3DDBD9',
  teal_dark =        '#007D79',
  teal_med =         '#009C9D',
  teal_green =       '#006D5B',
  gray_dark =        '#333333',
  gray_light =       '#E5E5E5'
)


# load data -----------------------------------------------------------------------------------
tv_viz = fread('data/imdb_tv.csv')
show_select = fread('data/imdb_tv_show_select.csv')


# season + episode score breakdown by show ----------------------------------------------------

# TODO: add comparisons to comprable shows by genre, year?
Plot_Show = function(query) {
  show_tconst = show_select |>
    mutate(query_match = stringdist::stringdist(str_to_upper(query), show_title)) |>
    filter(query_match == min(query_match)) |>
    pull(parentTconst)

  tv_df = tv_viz |> filter(parentTconst == show_tconst)
  season_count = length(unique(tv_df$seasonNumber))

  season_epi = ggplot(tv_df,
                      aes(x = episodeNumber,
                          y = averageRating,
                          fill = averageRating)) +
    geom_hline(aes(yintercept = seasonAverage), linetype = 'dashed', color = 'red', size = 1) +
    geom_bar(stat = 'identity') +
    facet_grid(row = vars(seasonNumber),
               col = vars(episodeNumber),
               scales = 'free') +
    labs(title = tv_df$show_title[1], color = 'white') +
    geom_label(aes(label = averageRating),
               position=position_dodge(width=0.9),
               vjust=-1,
               size = 5,
               fontface = 'bold',
               fill = 'white') +
    geom_label(aes(label = primaryTitle),
               position=position_dodge(width=0.9),
               vjust=-0.1,
               size = 5,
               fill = 'white') +
    coord_cartesian(ylim = c(0, 10.5 + floor(season_count / 2))) +
    scale_fill_distiller(palette = 'RdBu', direction = 1) +
    ggpubr::theme_pubr() +
    theme(legend.position = 'none') +
    theme(axis.ticks.y = element_blank()) +
    theme(axis.text.y = element_blank()) +
    theme(axis.title.y = element_blank()) +
    theme(axis.ticks.x = element_blank()) +
    theme(axis.text.x = element_blank()) +
    theme(axis.title.x = element_blank()) +
    theme(strip.text.x = element_text(size = 15, color = "gray20", face = "bold.italic")) +
    theme(strip.text.y = element_text(size = 15, color = "gray20", face = "bold.italic")) +
    # theme(plot.background = element_rect(fill = 'gray20')) +
    theme(plot.title = element_text(color = 'gray10', size = 20, face = 'bold')) +
    theme(panel.background = element_rect(fill = 'gray20')) +
    theme(strip.background = element_rect(color="black", fill="white", size=1.5, linetype="solid"))


  # season version
  season = ggplot(tv_df |>
           group_by(seasonNumber) |>
           summarize(averageRating = mean(averageRating),
                     numVotes = mean(numVotes)
                     ) |>
           ungroup(),
         aes(x = seasonNumber,
             y = averageRating,
             fill = averageRating
             )
  ) +
    geom_bar(stat = 'identity') +
    labs(title = tv_df$show_title[1]) +
    geom_label(aes(label = round(averageRating, 1)),
               position=position_dodge(width=0.9),
               vjust=-0.3,
               size = 7,
               label.size = 1,
               fontface = 'bold',
               color = 'gray90',
               fill = '#333333') +
    coord_cartesian(ylim = c(0, 11)) +
    scale_fill_distiller(palette = 'RdBu', direction = 1) +
    ggpubr::theme_pubr(border = TRUE) +
    theme(legend.position = 'none') +
    theme(axis.ticks.y = element_blank()) +
    theme(axis.text.y = element_blank()) +
    theme(axis.title.y = element_blank()) +
    theme(axis.title.x = element_blank()) +
    theme(panel.background = element_rect(fill = 'gray20')) +
    theme(plot.background = element_rect(fill = 'gray20')) +
    theme(plot.title = element_text(color = 'gray90', size = 20, face = 'bold')) +
    theme(axis.text.x = element_text(color = 'gray90', size = 20, face = 'bold'))


  season_epi_simple = ggplot(tv_df,
                         aes(x = episodeNumber,
                             y = 1,
                             fill = averageRating)
  ) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = averageRating),
              position = position_stack(vjust = 0.5),
              fontface = 'bold',
              size = 6) +
    facet_grid(row = vars(seasonNumber)) +
    scale_fill_gradient2(low = JB_COLORS$teal_dark, mid = JB_COLORS$teal_white, high = JB_COLORS$teal_accent,
                         midpoint = median(tv_df$averageRating)
    ) +
    labs(title = tv_df$show_title[1]) +
    ggpubr::theme_pubr() +
    theme(legend.position = 'none') +
    theme(axis.ticks.y = element_blank()) +
    theme(axis.text.y = element_blank()) +
    theme(axis.title.y = element_blank()) +
    theme(axis.ticks.x = element_blank()) +
    theme(axis.text.x = element_blank()) +
    theme(axis.title.x = element_blank()) +
    theme(strip.text.x = element_text(size = 12, color = "gray20", face = "bold.italic")) +
    theme(strip.text.y = element_text(size = 12, color = "gray20", face = "bold.italic")) +
    theme(plot.title = element_text(color = 'gray10', size = 20, face = 'bold')) +
    theme(panel.background = element_rect(fill = 'gray20')) +
    theme(strip.background = element_rect(color = "black", fill = "white", size = 1.5, linetype = "solid"))


  return(list('season_epi' = season_epi,
              'season_epi_simple' = season_simple,
              'season' = season
              )
        )
}
#
# test = Plot_Show('breaking bad')
# test[[1]]
# test[[2]]


# SANDBOX -------------------------------------------------------------------------------------


# color grid test
query = 'breaking bad'

show_tconst = show_select |>
  mutate(query_match = stringdist::stringdist(str_to_upper(query), show_title)) |>
  filter(query_match == min(query_match)) |>
  pull(parentTconst)

tv_df = tv_viz |> filter(parentTconst == show_tconst)

ggplot(tv_df, aes(x = episodeNumber, y = 1, fill = averageRating)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = averageRating),
            position = position_stack(vjust = 0.5),
            fontface = 'bold',
            size = 7) +
  facet_wrap(~seasonNumber, strip.position = 'left', ncol = 1) +
  scale_fill_gradient2(low = 'gray40',
                       mid = JB_COLORS$teal_white,
                       high = JB_COLORS$teal_dark,
                       midpoint = median(tv_df$averageRating)
                      ) +
  theme_void() +
  theme(legend.position = 'none') +
  theme(plot.background = element_rect(fill = 'gray20', color = 'gray20')) +
  theme(strip.text.y = element_text(size = 24, color = JB_COLORS$gray_light, face = "bold.italic")) +
  theme(panel.spacing = unit(0.01, "lines"))

ggsave('viz/televiz_sample_season_epi.png',
       width = 10,
       height = 6,
       dpi = 600)


# ----

  season_df = tv_df |>
  group_by(show_title, seasonNumber) |>
  summarize(averageRating = mean(averageRating),
            numVotes = mean(numVotes)
  ) |>
  ungroup()


ggplot(season_df,
       aes(x = seasonNumber,
           y = averageRating,
           fill = averageRating)
) +
  geom_bar(stat = 'identity') +
  labs(title = season_df$show_title[1]) +
  ggtext::geom_richtext(aes(label = round(averageRating, 1)),
                        vjust = -0.1,
                        size = 7,
                        fontface = 'bold',
                        color = 'gray90',
                        fill = '#333333',
                        label.size = 1.1,
                        label.
                        = unit(5, 'pt'),
                        # label.r = unit(4, 'pt'),
                        label.margin = unit(6, "pt")) +
  coord_cartesian(ylim = c(0, 11)) +
  scale_fill_gradient2(low = 'gray40',
                       mid = JB_COLORS$teal_white,
                       high = JB_COLORS$teal_dark,
                       midpoint = median(tv_df$averageRating) * 1.02
  ) +
  ggpubr::theme_pubr() +
  theme(legend.position = 'none') +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(strip.background = element_blank()) +
  theme(panel.background = element_rect(fill = 'gray20', color = 'gray20')) +
  theme(plot.background = element_rect(fill = 'gray20', color = 'gray20')) +
  theme(plot.title = element_text(color = 'gray90', size = 20, face = 'bold')) +
  theme(axis.text.x = element_text(color = 'gray90', size = 20, face = 'bold'))


ggsave('viz/televiz_sample_season.png',
       width = 6,
       height = 6,
       dpi = 600)


ggplot(tv_df, aes(x = episodeNumber, y = 1, fill = averageRating)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = averageRating),
            position = position_stack(vjust = 0.5),
            fontface = 'bold',
            size = 7) +
  facet_wrap(~seasonNumber, strip.position = 'left', ncol = 1) +
  scale_fill_gradient2(low = 'gray40',
                       mid = 'gray95',
                       # mid = JB_COLORS$teal_white,
                       high = JB_COLORS$teal_green,
                       midpoint = median(tv_df$averageRating) * 0.95
  ) +
  # ggpubr::theme_pubr() +
  scale_x_discrete(position = "top") +
  theme_void() +
  theme(legend.position = 'none') +
  theme(plot.background = element_rect(fill = 'gray20', color = 'gray20')) +
  theme(strip.text.y = element_text(size = 24, color = JB_COLORS$gray_light, face = "bold")) +
  # theme(axis.text.x = element_text(size = 20, color = JB_COLORS$gray_light, face = "bold")) +
  theme(axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank()) +
  theme(panel.background = element_rect(fill = 'gray20')) +
  theme(strip.background.y = element_rect(fill = 'gray20', color = 'gray20')) +
  theme(panel.spacing = unit(0.1, 'lines'))
