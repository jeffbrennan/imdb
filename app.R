library(shiny)
library(shinybrowser)
library(data.table)
library(glue)
library(tidyverse)
library(ggrepel)
library(DT)
library(shinyWidgets)

options(dplyr.summarise.inform = FALSE)

# map colors
background_color = '#333333'     #gray
nonrec_color = '#FB836F'  #salmon

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

css = "
.navbar-default {
  background-color: inherit;
}
"

Label_Floats = function(x, round_sig=1) {
  raw_result = as.character(round(x, round_sig))

  result = map(raw_result,
               ~ifelse(str_length(.) == 1,
                       str_c(., '.0'),
                       .)
  ) |>
    unlist()

  return(result)
}


# ui ------------------------------------------------------------------------------------------
ui = fluidPage(
  shinybrowser::detect(),
  tags$head(
     tags$style(css)
    ),
  setBackgroundColor(color = c(JB_COLORS$gray_dark)),
  fluidRow(
    column(width = 4,
           offset = 4,
           align = 'center',
           style = glue('font-family: Cooper Black; font-size: 28px; color: {JB_COLORS$gray_light}; background-color:{JB_COLORS$gray_dark}'),
           strong("TeleViz™")
    ),
  ),
  fluidRow(
    column(12, offset = 0,
           align = 'center',
           style = glue('font-family: Arial; font-size: 18px; color: {JB_COLORS$gray_light}; background-color:{JB_COLORS$gray_dark}'),
           fluidRow(
             column(12,
                    align = 'center',
                    textInput("query",
                              label = NULL,
                              value = 'Betty Boop',
                              width = '40%'  # TODO: make this dynamic with renderui
                              )
                    )
             )
           )
    ),
# plots ---------------------------------------------------------------------------------------
  fluidRow(
    column(12, align = 'center',
          style = glue("background-color:{JB_COLORS$gray_dark}"),
          plotOutput('season')
          )
    ),
  hr(style = "border-top: 3px solid #FFFFFF;"),
  fluidRow(
    column(
          12, align = 'center',
          style = glue("background-color:{JB_COLORS$gray_dark}"),
          plotOutput('season_epi_simple')
        )
))
#   hr(style = "border-top: 3px solid #FFFFFF;"),
# fluidRow(
#   column(width = 2,
#        # offset = 0,
#        align = 'left',
#        style = 'color: black; padding: 0px; background-color:white',
#        # style = glue('font-family: Fira Code Medium;font-size: 10px; color: {JB_COLORS$gray_light}; background-color:{JB_COLORS$gray_dark}'),
#        textOutput('last_updated'))
# )
# )
server = function(input, output, session) {
  # TODO: add date modified helptext
  # TODO: add separate phone/desktop sizing - get active window size and make calculations based on that

# load data -----------------------------------------------------------------------------------
  last_updated = file.mtime('data/imdb_tv.csv')
  output$last_updated = renderText({format(as.Date(last_updated), '%Y-%m-%d')})
  output$title = renderText({'TeleViz™️'})

  tv_viz_raw = fread('data/imdb_tv.csv')
  show_select = fread('data/imdb_tv_show_select.csv')

# create df  ----------------------------------------------------------------------------------

  # output$dynamic_width = renderUI(ifelse(shinybrowser::getwidth() > 800, '35%', '50%'))

  tv_df = reactive({
    if(input$query != '') {
      show_tconst = show_select |>
        mutate(query_match = stringdist::stringdist(str_to_upper(input$query), show_title)) |>
        filter(query_match == min(query_match)) |>
        slice(1) |>
        pull(parentTconst)
      } else {
      show_tconst = show_select |> slice(1) |> pull(parentTconst)
      }
    tv_viz_raw |> filter(parentTconst == show_tconst)
    })


  num_seasons = reactive({tv_df() |> pull(seasonNumber) |> unique() |> length()})
  num_episodes =  reactive({tv_df() |> pull(episodeNumber) |> unique() |> length()})

  se_mod = reactive(case_when(
                              shinybrowser::get_width() > 800 ~ 1,
                              num_seasons() < 8 ~ 1,
                              num_seasons() >= 8 & num_seasons() < 15 ~ 1 - (num_seasons() - 8) * 0.1,
                              TRUE ~ 0.4
                              )
                    )
# season --------------------------------------------------------------------------------------
  output$season = renderPlot({
    season_df = tv_df() |>
      group_by(show_title, seasonNumber) |>
      summarize(averageRating = mean(averageRating),
                numVotes = mean(numVotes)
                ) |>
      ungroup()

    max_rating = max(season_df$averageRating)

    season = ggplot(season_df,
                    aes(x = seasonNumber,
                        y = averageRating,
                        fill = averageRating)
    ) +
      geom_bar(stat = 'identity') +
      labs(title = season_df$show_title[1]) +
      ggtext::geom_richtext(aes(label = Label_Floats(averageRating, 1)),
                            vjust = -0.1,
                            size = 5 * se_mod(),
                            fontface = 'bold',
                            color = 'gray90',
                            fill = '#333333',
                            label.size = 1.1,
                            label.padding = unit(4 * se_mod(), 'pt'),
                            label.margin = unit(3 * se_mod(), "pt")) +
      coord_cartesian(ylim = c(0, ceiling(max_rating + 1))) +
      scale_fill_gradient2(low = 'gray40',
                           mid = JB_COLORS$teal_white,
                           high = JB_COLORS$teal_green,
                           midpoint = median(season_df$averageRating)
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
      theme(plot.title = element_text(color = 'gray90', size = 25, face = 'bold.italic')) +
      theme(axis.text.x = element_text(color = 'gray90', size = 16 * se_mod(), face = 'bold.italic'))

    season
  }
  )

# season epi simple ---------------------------------------------------------------------------
  output$season_epi_simple = renderPlot({
    tv_epi_simple = tv_df()
    season_epi_simple = ggplot(tv_epi_simple, aes(x = episodeNumber, y = 1, fill = averageRating)) +
      geom_bar(stat = 'identity') +
      geom_text(aes(label = Label_Floats(averageRating)),
                position = position_stack(vjust = 0.5),
                fontface = 'bold',
                size = 6 * se_mod()) +
      facet_wrap(~seasonNumber, strip.position = 'left', ncol = 1) +
      scale_fill_gradient2(low = 'gray40',
                           mid = 'gray95',
                           high = JB_COLORS$teal_green,
                           midpoint = median(tv_epi_simple$averageRating) * 0.95
      ) +
      labs(x=NULL, y=NULL) +
      ggpubr::theme_pubr() +
      theme(axis.text.x = element_blank()) +
      theme(legend.position = 'none') +
      theme(plot.background = element_rect(fill = 'gray20', color = 'gray20')) +
      theme(strip.text.y = element_text(size = 12 * se_mod(),
                                        color = JB_COLORS$gray_light,
                                        face = "bold")) +
      theme(axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.line.y = element_blank(),
            axis.line.x = element_blank()) +
      theme(panel.background = element_rect(fill = 'gray20')) +
      theme(strip.background.y = element_rect(fill = 'gray20', color = 'gray20')) +
      theme(panel.spacing = unit(0.1, 'lines')) +
      theme(plot.margin = unit(c(0,0,0,0), 'pt'))

    season_epi_simple
  },
  height = function() {session$clientData$output_season_epi_simple_width * 0.75}
  )
}
shinyApp(ui = ui, server = server)
