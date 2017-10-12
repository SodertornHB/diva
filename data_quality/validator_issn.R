#
# validator_issn
# 161123 JÖ /rev 171012 GL
# Enkelt verktyg för att validera SeriesISSN i DiVA-uttag.
#
#
#

library(tidyverse)
library(stringr)


# Hämta information från DiVA i format csvall2.
diva_all <- read_csv("/home/shub/assets/diva/diva_researchpubl_latest.csv")

# Selektera ut endast poster som har ett SeriesISSN
diva_issn <- diva_all %>%
  filter(!(is.na(SeriesISSN))) %>%
  select(PID, Title, Year, Series, SeriesISSN, SeriesEISSN)
  
# Separarera SeriesISSN till fler värden, om fler än ett. Detta skapar fler rader och duplicerar
# eventuella poster som har fler SeriesISSN
diva_issn <- diva_issn %>%
  mutate(ISSN = strsplit(as.character(SeriesISSN), ";")) %>%
  unnest(ISSN)

issn_format = "^\\d{4}-\\d{3}[\\dxX]$"
diva_malformed_issn <- diva_issn %>%
  filter(!(str_detect(ISSN, issn_format)))

# Det här sparar undar sessionen, för att kunna väva ihop med R Markdown
save.image("data_quality/validator_issn.RData")

