
# install.packages(c("pxweb", "dplyr", "purrr", "tibble"))

library(pxweb)
library(dplyr)
library(purrr)
library(tibble)

ssb_url <- function(id) sprintf("https://data.ssb.no/api/v0/no/table/%s/", id)

# 1) Se hvilke variabelkoder en tabell har (slik at du ikke gjetter)
show_codes <- function(id){
  meta <- pxweb_get(ssb_url(id))
  tibble(
    table_id = id,
    code     = map_chr(meta$variables, "code"),
    text     = map_chr(meta$variables, "text"),
    n_values = map_int(meta$variables, ~length(.x$values))
  )
}

# Eksempel: inspiser tabeller
show_codes("13265")  # KI-bruk
show_codes("09174")  # produktivitet
show_codes("09181")  # kapital/investering
show_codes("13932")  # utslipp

# 2) Hent data (når du har valgt koder/verdier fra show_codes)
fetch_ssb <- function(id, query_list){
  px <- pxweb_get(ssb_url(id), query = query_list)
  as.data.frame(px, column.name.type = "text", variable.value.type = "text")
}

# MAL for query (bytt ut kode/verdier med faktiske fra show_codes):
# q_13265 <- list(
#   "NACE2007"    = c("B-S"),        # eksempel
#   "SYSS"        = c("0"),          # eksempel
#   "ContentsCode"= c("..."),        # eksempel
#   "Tid"         = c("2021","2022","2023","2024","2025")
# )
# ai_use <- fetch_ssb("13265", q_13265)


