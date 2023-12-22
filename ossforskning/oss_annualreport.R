#
# oss_annualreport
# 220121 GL / 231222 GL
# Återrapportering till ÖSS 
#
# 


library(tidyverse)
source('/home/shub/src/common/lib/sh_diva_bibliometrics_functions.R')
sh_archive_start("ÖSS")

#Välj år även rad 70, 92
y <- 2023

#Läs in data från DiVA. Vi använder en csvall2-fil
diva <- read_csv(file="/home/shub/assets/diva/diva_researchpubl_sh_latest.csv")
diva <- diva %>% filter(Year == y)

diva <- subject_baltic(diva)
diva <- funder_oss(diva)

#Filtrerar till Östersjöforskning
ossforsk <- diva %>% filter(baltic == TRUE)

#De publikationer som skall ingå i sammanställningen

ossforskAR <- ossforsk %>%
  filter(!(PublicationType == "Övrigt")) %>%
  filter(ContentType!="Övrig (populärvetenskap, debatt, mm)") %>%
  filter((is.na(Status))|Status=="published"|Status=="inPress") %>%
  filter(is.na(PublicationSubtype)|PublicationSubtype == "publishedPaper" |PublicationSubtype == "meetingAbstract" 
         |PublicationSubtype == "editorialMaterial")

#Slå ihop publikationstyper genom att byta namn på värden
ossforskAR$PublicationType <- recode(ossforskAR$PublicationType,
                                     "Artikel i tidskrift" = "Artikel",
                                     "Artikel, forskningsöversikt" = "Artikel",
                                     "Artikel, recension" = "Artikel, recension",
                                     "Kapitel i bok, del av antologi" = "Kapitel",
                                     "Bok" = "Monografi",
                                     "Doktorsavhandling, monografi" = "Doktorsavhandling",
                                     "Doktorsavhandling, sammanläggning" = "Doktorsavhandling",
                                     "Konferensbidrag" = "Publicerat konferensbidrag",
                                     "Licentiatavhandling, monografi" = "Licentiatavhandling",
                                     "Licentiatavhandling, sammanläggning" = "Licentiatavhandling",
                                     "Rapport" = "Rapport",
                                     "Samlingsverk (redaktörskap)" = "Samlingsverk/proceedings, redaktörskap",
                                     "Proceedings (redaktörskap)" = "Samlingsverk/proceedings, redaktörskap")

ossforskAR$PublicationType <- factor(ossforskAR$PublicationType, ordered = TRUE, 
                                     levels = c("Artikel", "Artikel, recension", "Monografi", "Kapitel", 
                                                "Publicerat konferensbidrag", "Doktorsavhandling", "Licentiatavhandling",
                                                "Rapport", "Samlingsverk/proceedings, redaktörskap"))
ossforskAR$ContentType <- factor(ossforskAR$ContentType, ordered = TRUE,
                                 levels = c("Refereegranskat", "Övrigt vetenskapligt"))

oss_table <- ossforskAR %>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

oss_ref <- ossforskAR %>%
  filter(ContentType == "Refereegranskat") %>%
  group_by(PublicationType, ContentType) %>%
  count(Year) %>%
  spread(Year, n)

oss_ref <- oss_ref %>%
  spread(ContentType, '2023')

oss_table <- left_join(oss_table, oss_ref, "PublicationType")

oss_table[is.na(oss_table)] <- 0L


# OSS funding -------------------------------------------------------------

oss_fund <- ossforskAR %>% filter(oss == TRUE)

oss_fund_table <- oss_fund %>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

oss_fund_ref <- oss_fund %>%
  filter(ContentType == "Refereegranskat") %>%
  group_by(PublicationType, ContentType) %>%
  count(Year) %>%
  spread(Year, n)

oss_fund_ref <- oss_fund_ref %>%
  spread(ContentType, '2023')

oss_fund_table <- left_join(oss_fund_table, oss_fund_ref, "PublicationType")

oss_fund_table[is.na(oss_fund_table)] <- 0L

# Epub ahead of print -----------------------------------------------------

ahead <- ossforsk %>%
  filter(Status == "aheadofprint")

ahead_table <- ahead %>%
  group_by(oss) %>% 
  count(Status) %>%
  spread(Status, n)

# DOI till citeringssökning -----------------------------------------------

doi_wos <- ossforskAR %>%
  filter(!(is.na(DOI))) %>%
  transmute(wos =str_c("DO=", DOI, " OR "))

doi_scopus <- ossforskAR %>%
  filter(!(is.na(DOI))) %>%
  transmute(scop = str_c("DOI(", DOI,") OR "))

write_csv(doi_wos, "wos.csv")
write_csv(doi_scopus, "scopus.csv")

# Dokumentation -----------------------------------------------------------

write_csv(oss_table, "Östersjöforskning.csv")
write_csv(oss_fund_table, "Östersjöstiftelsen.csv")
sh_archive_resource("Östersjöforskning.csv")
sh_archive_resource("Östersjöstiftelsen.csv")
sh_archive_resource("wos.csv")
sh_archive_resource("scopus.csv")
sh_archive_df(ahead_table, "Epub ahead of print")
sh_archive_df(ossforskAR, "Medräknade_publikationer")
sh_archive_df(diva, "Diva_rådata")
sh_archive_end()
