#
#
#publicering_sh
#GL 190604
#Tabeller över publiceringen för hela högskolan och för varje institution.
#Samma urval som verksahetsuppföljningen, vilket är något snävare än ÅR. 
#Publikationstyperna som i ÅR.
#

library(tidyverse)

source('/home/shub/src/common/lib/sh_parameters.R')
source('/home/shub/src/common/lib/sh_diva_bibliometrics_functions.R')

sh_archive_start("publicering_sh_2013_2018")

diva <- read_csv(file="/home/shub/assets/diva/diva_researchpubl_sh_latest.csv")
diva <- diva %>% filter(between(Year, 2013, 2018))


# Hela SH -----------------------------------------------------------------

publ_sh <- diva %>% 
  filter(PublicationType == "Artikel i tidskrift"|PublicationType == "Artikel, forskningsöversikt" |
    PublicationType == "Kapitel i bok, del av antologi" |PublicationType == "Bok" |PublicationType == "Konferensbidrag") %>%
  filter(ContentType!="Övrig (populärvetenskap, debatt, mm)") %>%
  filter((is.na(Status))|Status=="published") %>%
  filter(is.na(PublicationSubtype)|PublicationSubtype == "publishedPaper")

publ_sh$PublicationType <- recode(publ_sh$PublicationType,
                                 "Artikel i tidskrift" = "Artiklar i tidskrift",
                                 "Artikel, forskningsöversikt" = "Artiklar i tidskrift",
                                 "Kapitel i bok, del av antologi" = "Artiklar i antologi",
                                 "Bok" = "Monografier",
                                 "Konferensbidrag" = "Publicerade konferensbidrag")

publ_sh_tabell <- publ_sh %>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)


# Institutionerna ---------------------------------------------------------------------
#En tabell för varje institution. Lägg till institutionsnamn och slå ihop
sam <- publ_sh %>% 
  filter_orgs(sam)%>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)
  
sam["InstName"] <- "ISV"
sam <- sam %>% unite(PublicationType, PublicationType, InstName, sep=": ")

hs <- publ_sh %>% 
  filter_orgs(hs)%>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

hs["InstName"] <- "IHS"
hs <- hs %>% unite(PublicationType, PublicationType, InstName, sep=": ")

ikl <- publ_sh %>% 
  filter_orgs(ikl)%>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

ikl["InstName"] <- "IKL"
ikl <- ikl %>% unite(PublicationType, PublicationType, InstName, sep=": ")

nmt <- publ_sh %>% 
  filter_orgs(nmt)%>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

nmt["InstName"] <- "NMT"
nmt <- nmt %>% unite(PublicationType, PublicationType, InstName, sep=": ")

publ_inst_tabell <- bind_rows(publ_sh_tabell, hs, ikl, nmt, sam)

write_csv(publ_inst_tabell, "SH_publ_2013_2018.csv")
sh_archive_resource("SH_publ_2013_2018.csv")
sh_archive_df(publ_sh, "Medräknade_publikationer")
sh_archive_df(diva, "Diva_rådata")
sh_archive_end()
