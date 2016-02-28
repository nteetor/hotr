#' Download and Parse Hero Data
#'
#' This function downloads and parses the heroesjson data into a mangable
#' data.frame.
#'
#' @details
#'
#' The raw data is pulled from \url{http://heroesjson.com/heroes.json}.
#'
#' @export
hero_data <- function() {
  GET('http://heroesjson.com/heroes.json') %>%
    content %>%
    lapply(
      function(hero) {
        data_frame(
          Id = hero$id,
          # Name = hero$name,
          Title = hero$title,
          Description = hero$description,
          Role = hero$role,
          Type = hero$type,
          Gender = hero$gender,
          Franchise = hero$franchise,
          Difficulty = hero$difficulty,
          DamageRating = hero$ratings$damage,
          UtilityRating = hero$ratings$utility,
          SurvivabilityRating = hero$ratings$survivability,
          ComplexityRating = hero$ratings$complexity,
          ReleaseDate = hero$releaseDate
        ) %>%
          left_join(
            parse_stats(hero$id, hero$stats),
            by = 'Id'
          ) %>%
          left_join(
            parse_abilities(hero$abilities),
            by = 'Name'
          ) %>%
          left_join(
            parse_talents(hero$id, hero$talents),
            by = 'Id'
          )
      }
    ) %>%
    bind_rows %>%
    select(Id, Name, everything())
}

parse_stats <- function(id, stats) {
  bind_rows(
    lapply(
      names(stats),
      function(nm) {
        s <- stats[[nm]]
        data_frame(
          Id = id,
          Name = `if`(nm == 'Uther', c('Uther', 'UtherSpirit'), nm),
          Hp = s$hp,
          HpPerLevel = s$hpPerLevel,
          HpRegen = s$hpRegen,
          HpRegenPerLevel = s$hpRegenPerLevel,
          Mana = s$mana,
          ManaPerLevel = s$manaPerLevel,
          ManaRegen = s$manaRegen,
          ManaRegenPerLevel = s$manaRegenPerLevel
        )
      }
    )
  )
}

parse_abilities <- function(abilities) {
  bind_rows(
    lapply(
      names(abilities),
      function(nm) {
        abilities_df <- bind_rows(
            lapply( # abilities
              abilities[[nm]],
              function(abl) {
                if (is.null(abl$trait)) {
                  data_frame(
                    Name = `if`(nm == 'LostVikings', c('HeroBaleog', 'HeroErik', 'HeroOlaf'), nm),
                    AbilityId = abl$id,
                    AbilityName = abl$name,
                    AbilityManaCost = abl$manaCost %||% NA,
                    AbilityHeroic = abl$heroic %||% FALSE,
                    AbilityDescription = abl$description,
                    AbilityCooldown = abl$cooldown %||% NA,
                    AbilityShortcut = abl$shortcut
                  )
                }
              }
            )
          )
        trait_df <- bind_rows(
          lapply( # traits
            abilities[[nm]],
            function(trt) {
              if (!is.null(trt$trait) && trt$trait) {
                data_frame(
                  Name = `if`(nm == 'LostVikings', c('HeroBaleog', 'HeroErik', 'HeroOlaf'), nm),
                  TraitId = trt$id,
                  TraitName = trt$name,
                  TraitDescription = trt$description,
                  TraitCooldown = trt$cooldown %||% NA
                )
              }
            }
          )
        )

        if (NROW(trait_df) == 0) {
          abilities_df
        } else {
          left_join(abilities_df, trait_df, by = 'Name')
        }
      }
    )
  )
}

parse_talents <- function(id, talents) {
  bind_rows(
    lapply(
      names(talents),
      function(nm) {
        bind_rows(
          lapply(
            talents[[nm]],
            function(t) {
              data_frame(
                Id = id,
                TalentTier = nm,
                TalentId = t$id,
                TalentName = t$name,
                TalentDescription = t$description,
                TalentCooldown = t$cooldown %||% NA
              )
            }
          )
        )
      }
    )
  )
}
