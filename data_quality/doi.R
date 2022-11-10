#
# doi
# GL 211021
# Skapar en fil med DOI i format som går att kopiera till Scopus och WoS avancerade sökning.
# För att se om alla publikationer är knutna till Södertörn University i databaserna.
# Lista till Unpaywall dels samtliga, dels de som saknar länt till fri fulltext i DiVA.
#
#

library(tidyverse)

diva <- read_csv(file = "/home/shub/assets/diva/diva_researchpubl_sh_latest.csv")

publ_vet <- diva %>%
  filter(PublicationType=="Artikel i tidskrift"|PublicationType=="Artikel, forskningsöversikt" 
         |PublicationType=="Artikel, recension" |PublicationType=="Kapitel i bok, del av antologi"
         |PublicationType=="Bok"|PublicationType=="Konferensbidrag") %>%
  filter(ContentType!="Övrig (populärvetenskap, debatt, mm)")
  

doi_wos <- publ_vet %>%
  filter(between(Year, 2018, 2022)) %>%
  filter(!(is.na(DOI))) %>%
  transmute(wos =str_c("DO=", DOI, " OR "))
  
doi_scopus <- publ_vet %>%
  filter(between(Year, 2018, 2022)) %>%
  filter(!(is.na(DOI))) %>%
  transmute(scop = str_c("DOI(", DOI,") OR "))

doi_unpay_ej_diva <- publ_vet %>%
  filter(between(Year, 2018, 2022)) %>%
  filter(!(is.na(DOI))) %>%
  filter(is.na(FreeFulltext)) %>%
  select(DOI)

doi_unpay_alla <- publ_vet %>%
  filter(between(Year, 2018, 2022)) %>%
  filter(!(is.na(DOI))) %>%
  select(DOI)

urval <- publ_vet %>%
  filter(between(Year, 2018, 2022))%>%
  select(PID, DOI, ISI, ScopusId, FullTextLink, FreeFulltext)

write_csv(doi_wos, "wos.csv")
write_csv(doi_scopus, "scopus.csv")
write_csv(doi_unpay_ej_diva, "unpay_komplettera.csv")
write_csv(doi_unpay_alla, "unpay_alla.csv")
write_csv(urval, "urval.csv")
