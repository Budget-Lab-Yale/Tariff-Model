#' Build HS2 chapter code -> description lookup
#'
#' Pulls HS2 descriptions from the Census timeseries imports API and writes
#' a deduplicated CSV with one row per HS2 chapter.

suppressMessages({
  library(dplyr)
  library(readr)
  library(jsonlite)
})

key <- Sys.getenv('CENSUS_API_KEY')
if (key == '') stop('CENSUS_API_KEY not set.')

url <- sprintf(
  paste0('https://api.census.gov/data/timeseries/intltrade/imports/hsimport',
         '?get=I_COMMODITY,I_COMMODITY_LDESC,I_COMMODITY_SDESC',
         '&YEAR=2025&MONTH=12&COMM_LVL=HS2&for=world&key=%s'),
  key
)

raw <- fromJSON(url)
hdr <- raw[1, ]
df <- as_tibble(raw[-1, ], .name_repair = 'minimal')
names(df) <- hdr

hs2 <- df %>%
  transmute(
    hs2          = as.character(I_COMMODITY),
    description  = as.character(I_COMMODITY_LDESC),
    description_short = as.character(I_COMMODITY_SDESC)
  ) %>%
  distinct() %>%
  arrange(hs2)

out_dir <- 'data/trade_weights'
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_file <- file.path(out_dir, 'hs2_descriptions.csv')
write_csv(hs2, out_file)

cat(sprintf('Wrote %s (%d chapters)\n', out_file, nrow(hs2)))
