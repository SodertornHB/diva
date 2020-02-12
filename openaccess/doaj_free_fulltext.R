#
#
#
#doaj_diva
#190813 GL
#Matchar lista från DOAJ med artiklar i DiVA. True/False att jämföra med om artikeln är markerad som FreeFulltext. 
#Ger en csv fil antingen med alla artiklar, eller med enbart de som finns i DOAJ med kolumnen add_doaj, som 
#visar från vilket år tidskriften är OA.
#
#
#

library(tidyverse)

source('/home/shub/src/common/lib/sh_parameters.R')
source('/home/shub/src/common/lib/sh_diva_bibliometrics_functions.R')

#Läs in data från DiVA. Vi använder en csvall2-fil.
diva <- read_csv(file = "/home/shub/assets/diva/diva_researchpubl_latest.csv")
#Tidskriftslista nerladdad från doaj.org
doaj_listan <- read_csv("/home/shub/assets/doaj.csv")

diva$JournalISSN[is.na(diva$JournalISSN)] <- 0L
diva$JournalEISSN[is.na(diva$JournalEISSN)] <- 0L
diva$FreeFulltext[diva$FreeFulltext == "true"] <- TRUE

diva_art_rec <- diva %>%
  filter(between(Year, 2015, 2019)) %>%
  filter(PublicationType %in% c("Artikel, forskningsöversikt", "Artikel i tidskrift", "Artikel, recension")) %>%
  mutate(doaj = ((JournalISSN %in% doaj_listan$`Journal ISSN (print version)`)|
                   (JournalEISSN %in% doaj_listan$`Journal EISSN (online version)`)))%>%
  select(PID, Name, Title, Journal,JournalISSN, JournalEISSN, Year, ContentType, PublicationSubtype, Status, FullTextLink,
         DOI, Urls, FreeFulltext, doaj)

doaj_kort <- doaj_listan %>%
  select(JournalISSN = `Journal ISSN (print version)`, JournalEISSN = `Journal EISSN (online version)`, add_doaj = `First calendar year journal provided online Open Access content`)


#Matcha för att få fram från vilket år tidkskriften är OA enligt DOAJ
diva_art_doaj <- diva_art_rec %>%
  #Matcha print issn och e issn:
  mutate(doaj_issn = match(JournalISSN, doaj_kort$JournalISSN, nomatch = 0)) %>%
  mutate(doaj_eissn = match(JournalEISSN, doaj_kort$JournalEISSN, nomatch = 0)) %>%
  #Välj den issn-källa som ger träff:
  mutate(doaj_row = pmax(doaj_issn, doaj_eissn))

diva_art_doaj <- diva_art_doaj %>%
  #Filtrera bort omatchade publikationer.
  filter(doaj_row > 0) %>%
  #rowwise behövs för att indexeringen i nästa rad ska fungera:
  rowwise() %>%
  mutate(add_doaj = doaj_kort[["add_doaj"]][[doaj_row]]) %>%
  #för att ta bort rowwise:
  ungroup() %>%
  select(-starts_with("doaj_"))

#diva_art_rec innehåller alla artiklar, diva_art_doaj enbert de med träff i DOAJ
write_csv(diva_art_doaj, "doaj_diva.csv")

