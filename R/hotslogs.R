#' Hero Popularity Data
#'
#' This data includes, for each hero, their name, number of games in which the
#' hero was played, what percentage of total games included the hero, the
#' percentage of the hero's games won, the change in the win rate percentage,
#' their role and specialty.
#'
#' @details
#'
#' The raw data is pulled from \url{http://www.hotslogs.com/Default}.
#'
#' @export
hero_popularity <- function() {
  default_page <- xml2::read_html('http://www.hotslogs.com/Default')

  default_page %>%
    html_nodes(., xpath = '//div[@id="RadGridCharacterStatistics"]//tr[@id]/@id') %>%
    html_text() %>%
    map(
      ~ html_nodes(
        default_page,
        xpath = paste0('//tr[@id="', ., '"]//a/@title | //tr[@id="', ., '"]//td[position()>2]')
      )
    ) %>%
    map(
      ~ html_text(.) %>%
        as.list %>%
        setNames(
          c('Name', 'NumGames', 'Popularity', 'WinRate', 'WinChange', 'Role', 'Specialty')
        ) %>%
        as_data_frame
    ) %>%
    bind_rows %>%
    mutate_each(
      funs(as.numeric(sub('\\s+%', '', .))),
      2:5
    )
}

#' Hero Talent Build Data
#'
#' This data contains information about the current top 10 most popular builds
#' for a particular hero.
#'
#' @param name A character vector specifying the hero name. Caution: hotslogs
#'   will default to Abathur if a name is misspelled.
#'
#' @details
#'
#' The raw data is pulled from
#' \url{http://www.hotslogs.com/Sitewide/HeroDetails} with query string
#' ?Hero=\code{name}.
#'
#' @export
hero_builds <- function(name) {
  name <- as.character(name)

  build_page <- xml2::read_html(
    paste0('http://www.hotslogs.com/Sitewide/HeroDetails?Hero=', name)
  )

  build_page %>%
    html_nodes(
      xpath = '//table[@id="ctl00_MainContent_RadGridPopularTalentBuilds_ctl00"]/tbody/tr'
    ) %>%
    html_nodes(
      xpath = '//td[@align="center"]/img/@title'
    ) %>%
    html_text %>%
    data_frame(
      Talent = .,
      Group = rep(seq_len(10), each = 7),
      Tier = rep(c(seq(1, 16, 3), 20), times = 10)
    ) %>%
    mutate(
      Description = str_replace(Talent, '^.*:\\s+', ''),
      Talent = str_replace(Talent, ':\\s+.*$', '')
    ) %>%
    left_join(
      build_page %>%
        html_nodes(
          xpath = '//table[@id="ctl00_MainContent_RadGridPopularTalentBuilds_ctl00"]/tbody/tr/td[position()=1]'
        ) %>%
        html_text %>%
        data_frame(
          GamesPlayed = .,
          Group = seq_len(10)
        ),
      by = 'Group'
    ) %>%
    left_join(
      build_page %>%
        html_nodes(
          xpath = '//table[@id="ctl00_MainContent_RadGridPopularTalentBuilds_ctl00"]/tbody/tr/td[position()=2]'
        ) %>%
        html_text %>%
        data_frame(
          WinRate = .,
          Group = seq_len(10)
        ),
      by = 'Group'
    ) %>%
    mutate(
      WinRate = str_replace(WinRate, '\\s+%$', ''),
      Group = NULL,
      Hero = name
    ) %>%
    select(
      Hero, GamesPlayed, WinRate, Tier, Talent, Description
    )
}
