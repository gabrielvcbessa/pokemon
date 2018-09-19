library(dplyr)
library(textreadr)
library(rvest)
library(stringr)

pokemon_db_url <- 'https://pokemondb.net/evolution'

evolutions.node <- pokemon_db_url %>% 
  read_html(encoding = 'UTF-8') %>% 
  html_nodes('main > div.infocard-list-evo') 

evolutions <- evolutions.node %>% 
  map(function(evolutions) {
    html_nodes(evolutions, 'span.infocard-lg-data.text-muted') %>% 
      map(function(evolution_info) {
        html_text(evolution_info, 'small:first-child') %>% 
          str_match('#(\\d+)([A-Z]{1}[a-z0-9\\u00C0-\\u00FF\\-]+[\\.♀♂:]?(\\s[A-Z]?[a-z\\.]*)?)[A-Z]{1}.+') %>% 
          .[2:3] %>% 
          list(
            pokedex_number = as.integer(.[1]),
            name = .[2]
          )
      })
  })

pokemon = data.frame(numeric(0), character(0), character(0))

for (evolution in evolutions) {
  for (stage in evolution) {
    pokemon <- rbind(
      pokemon, 
      matrix(
        c(stage$pokedex_number, 
          evolution[[1]]$name,
          stage$name), ncol = 3 ))
    }
}

colnames(pokemon) <- c('pokedex_number', 'evolution_chain', 'name')
pokemon.unique <- pokemon[!duplicated(pokemon$pokedex_number), ]

write.csv(pokemon.unique, file = 'pokemon_evolution.csv', row.names = FALSE)
