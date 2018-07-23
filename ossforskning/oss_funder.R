#
#
#oss_funder
#180720 GL
#Urval att jämföra med uttag från WoS och Scopus där Östersjöstiftelsen ansges som finansiär.
#Gjord för att komplettera DiVA.
#
#

library(tidyverse)
source('/home/shub/src/common/lib/sh_diva_bibliometrics_functions.R')

diva <- read_csv(file="/home/shub/assets/diva/diva_researchpubl_latest.csv")
diva <- diva %>% filter(between(Year, 2008, 2018))

ossforsk <- funder_oss(diva)

artiklar_isi <- ossforsk %>% 
  filter(PublicationType == "Artikel i tidskrift" |PublicationType == "Artikel, forskningsöversikt") %>%
  filter(!(is.na(ISI))) %>%
  filter(oss == FALSE) %>%
  select(PID, Title, ISI, Funder)

write_csv(artiklar, "ISI_ej_ÖSS")

artiklar_scopus <- ossforsk %>%
  filter(PublicationType == "Artikel i tidskrift" |PublicationType == "Artikel, forskningsöversikt") %>%
  filter(!(is.na(ScopusId))) %>%
  filter(oss == FALSE) %>%
  select(PID, Title, ScopusId, Funder)

write_csv(artiklar_scopus, "Scopus_ej_öss")
