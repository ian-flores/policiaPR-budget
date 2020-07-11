## code to prepare `budget` dataset goes here

library("pdftools")
library(tidyverse)

url <- 'http://www.presupuesto.pr.gov/PRESUPUESTOPROPUESTO2020-2021/Tablas%20estadisticas%202/2%20%20Presupuesto%20Consolidado%20por%20Agencia%20A%C3%B1os%20Fiscales%202018-2021.pdf'

out <- pdf_text(url)

table_budget <- function(x){
  x <- str_split(x, pattern = '\\n')[[1]]
  
  df <- as_tibble(x) %>%
    filter(!str_detect(value, 'PRESUPUESTO CONSOLIDADO POR AGENCIA'),
           !str_detect(value, 'AÑOS FISCALES'),
           !str_detect(value, 'PFP'),
           !str_detect(value, '\\d\\d\\-\\d\\d\\-\\d\\d\\d\\d'),
           !str_detect(value, '\\(Redondeado al Millar\\)'),
           !str_detect(value, 'Página'),
           !str_detect(value, 'Gastado'),
           !str_detect(value, 'Asignado'),
           !str_detect(value, '2018'),
           str_starts(value, 'Código', negate = T),
           str_starts(value, 'Agencia', negate = T)) %>%
    mutate(original_row = row_number())
  
  parsed_df <- df %>% 
    filter(str_detect(value, "(\\d)+")) %>%
    mutate(values = str_split_fixed(str_trim(value), '  ', n = 2),
           agency_code = values[, 1],
           values = str_split_fixed(str_trim(values[, 2]), '  ', n = 2),
           agency_name = values[, 1],
           values = str_split_fixed(str_trim(values[, 2]), ' ', n = 2),
           year_2018 = values[, 1],
           values = str_split_fixed(str_trim(values[, 2]), ' ', n = 2),
           year_2019 = values[, 1],
           values = str_split_fixed(str_trim(values[, 2]), ' ', n = 2),
           year_2020 = values[, 1],
           values = str_split_fixed(str_trim(values[, 2]), ' ', n = 2),
           year_2021_a = values[, 1],
           values = str_split_fixed(str_trim(values[, 2]), ' ', n = 2),
           year_2021_b = values[, 1],
           values = str_split_fixed(str_trim(values[, 2]), ' ', n = 2),
           abs_change = values[, 1],
           values = str_split_fixed(str_trim(values[, 2]), ' ', n = 2),
           pct_change = values[, 1]) %>%
    select(-value, -values)
  
  joined_df <- df %>%
    filter(!str_detect(value, "(\\d)+")) %>%
    mutate(new_row = original_row - 1,
           value = str_trim(value)) %>%
    right_join(parsed_df, by = c('new_row' = 'original_row')) %>%
    mutate(agency_name = if_else(is.na(value), agency_name, paste(agency_name, value))) %>%
    select(-value:-new_row)
  
  return(joined_df)
}

budget_df <- map_df(out, table_budget)

number_parser <- function(num){
  num <- as.character(num)
  num <- str_replace_all(num, ',', '')
  num <- str_replace_all(num, '\\(', '')
  num <- str_replace_all(num, '\\)', '')
  
  return(as.numeric(num))
}


budget <- budget_df %>%
  mutate_at(vars(year_2018:pct_change), list(number_parser)) %>%
  select(-contains('change'), -year_2021_a) %>%
  rename(year_2021 = year_2021_b) %>%
  gather(key = 'year', value = 'amount', -agency_code:-agency_name) %>%
  mutate(year = str_replace_all(year, 'year\\_', ''),
         year = as.numeric(year))

police_expenses <- budget %>%
  filter(str_detect(agency_name, 'Poli'))

program_expenses <- budget%>%
  filter(!str_detect(agency_name, 'Poli'))

usethis::use_data(police_expenses, overwrite = TRUE)
usethis::use_data(program_expenses, overwrite = TRUE)