#
# Uppföljning SH och institutioner
# 210413 GL
# Uppföljning av publikationer med samma urval som till ÅR. 
# Skript på slutet för att filtrera på ämne.
#
#


library(tidyverse)
source('/home/shub/src/common/lib/sh_diva_bibliometrics_functions.R')
sh_archive_start("Uppföljning_2014_2020")

diva <- read_csv(file="/home/shub/assets/diva/diva_researchpubl_sh_latest.csv")
diva <- diva %>% filter(between(Year, 2014, 2020))

divaAR <- diva %>% 
  filter(!(PublicationType == "Samlingsverk (redaktörskap)"|PublicationType == "Proceedings (redaktörskap)" |PublicationType == "Övrigt")) %>%
  filter(ContentType!="Övrig (populärvetenskap, debatt, mm)") %>%
  filter((is.na(Status))|Status=="published"|Status=="inPress") %>%
  filter(is.na(PublicationSubtype)|PublicationSubtype == "publishedPaper" |PublicationSubtype == "meetingAbstract" 
         |PublicationSubtype == "editorialMaterial")

divaAR$PublicationType <- recode(divaAR$PublicationType,
                                 "Artikel i tidskrift" = "Artiklar i tidskrift",
                                 "Artikel, forskningsöversikt" = "Artiklar i tidskrift",
                                 "Artikel, recension" = "Artiklar i tidskrift",
                                 "Kapitel i bok, del av antologi" = "Artiklar i antologi",
                                 "Doktorsavhandling, monografi" = "Doktorsavhandlingar",
                                 "Doktorsavhandling, sammanläggning" = "Doktorsavhandlingar",
                                 "Licentiatavhandling, monografi" = "Licentiatavhandlingar",
                                 "Licentiatavhandling, sammanläggning" = "Licentiatavhandlingar",
                                 "Bok" = "Monografier",
                                 "Konferensbidrag" = "Publicerade konferensbidrag",
                                 "Rapport" = "Rapporter")

#Gör en tabell som skrivs till csv omsorterad och med innhållstyp för artiklar
divaAR$PublicationType <- factor(divaAR$PublicationType, ordered = TRUE, 
                                 levels = c("Artiklar i tidskrift", "Artiklar i antologi", "Monografier", "Publicerade konferensbidrag", 
                                            "Doktorsavhandlingar", "Licentiatavhandlingar", "Rapporter"))
divaAR$ContentType <- factor(divaAR$ContentType, ordered = TRUE,
                             levels = c("Refereegranskat", "Övrigt vetenskapligt"))

articles <- divaAR %>%
  subset(PublicationType=="Artiklar i tidskrift"|PublicationType=="Artiklar i antologi") %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")

other_publication <- divaAR %>%
  subset(!(PublicationType=="Artiklar i tidskrift"|PublicationType=="Artiklar i antologi")) %>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

ar_table <- bind_rows(articles, other_publication)
ar_table[is.na(ar_table)] <- 0L
write_csv(ar_table, "sh_tabell.csv")

# ISV ---------------------------------------------------------------------

inst_ar <- divaAR %>% filter_orgs(sam)
articles <- inst_ar %>%
  subset(PublicationType=="Artiklar i tidskrift"|PublicationType=="Artiklar i antologi") %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")

other_publication <- inst_ar %>%
  subset(!(PublicationType=="Artiklar i tidskrift"|PublicationType=="Artiklar i antologi")) %>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

inst_ar_table <- bind_rows(articles, other_publication)
inst_ar_table[is.na(inst_ar_table)] <- 0L

#Spara filer
write_csv(inst_ar_table, "isv_tabell.csv")
inst_ar <- select(inst_ar, -(list_of_orgs))
write_csv(inst_ar, "isv_medräknade_publikationer.csv")

# IKL ---------------------------------------------------------------------

inst_ar <- divaAR %>% filter_orgs(ikl)
articles <- inst_ar %>%
  subset(PublicationType=="Artiklar i tidskrift"|PublicationType=="Artiklar i antologi") %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")

other_publication <- inst_ar %>%
  subset(!(PublicationType=="Artiklar i tidskrift"|PublicationType=="Artiklar i antologi")) %>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

inst_ar_table <- bind_rows(articles, other_publication)
inst_ar_table[is.na(inst_ar_table)] <- 0L

#Spara filer
write_csv(inst_ar_table, "ikl_tabell.csv")
inst_ar <- select(inst_ar, -(list_of_orgs))
write_csv(inst_ar, "ikl_medräknade_publikationer.csv" )

# NMT ---------------------------------------------------------------------

inst_ar <- divaAR %>% filter_orgs(nmt)
articles <- inst_ar %>%
  subset(PublicationType=="Artiklar i tidskrift"|PublicationType=="Artiklar i antologi") %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")

other_publication <- inst_ar %>%
  subset(!(PublicationType=="Artiklar i tidskrift"|PublicationType=="Artiklar i antologi")) %>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

inst_ar_table <- bind_rows(articles, other_publication)
inst_ar_table[is.na(inst_ar_table)] <- 0L

#Spara filer
write_csv(inst_ar_table, "nmt_tabell.csv")
inst_ar <- select(inst_ar, -(list_of_orgs))
write_csv(inst_ar, "nmt_medräknade_publikationer.csv")

# IHS ---------------------------------------------------------------------

inst_ar <- divaAR %>% filter_orgs(hs)
articles <- inst_ar %>%
  subset(PublicationType=="Artiklar i tidskrift"|PublicationType=="Artiklar i antologi") %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")

other_publication <- inst_ar %>%
  subset(!(PublicationType=="Artiklar i tidskrift"|PublicationType=="Artiklar i antologi")) %>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

inst_ar_table <- bind_rows(articles, other_publication)
inst_ar_table[is.na(inst_ar_table)] <- 0L

#Spara filer
write_csv(inst_ar_table, "ihs_tabell.csv")
inst_ar <- select(inst_ar, -(list_of_orgs))
write_csv(inst_ar, "ihs_medräknade_publikationer.csv")

# Avslut ------------------------------------------------------------------
sh_archive_resource("sh_tabell.csv")
sh_archive_df(divaAR, "sh_medräknade_publikationer")
sh_archive_df(diva, "Diva_rådata")

sh_archive_resource("isv_tabell.csv")
sh_archive_resource("isv_medräknade_publikationer.csv")
sh_archive_resource("ikl_tabell.csv")
sh_archive_resource("ikl_medräknade_publikationer.csv")
sh_archive_resource("nmt_tabell.csv")
sh_archive_resource("nmt_medräknade_publikationer.csv")
sh_archive_resource("ihs_tabell.csv")
sh_archive_resource("ihs_medräknade_publikationer.csv")

sh_archive_end()


# Ämne ----------------------------------------------------------------

amne <- divaAR %>% filter_orgs(turismvetenskap)
articles <- amne %>%
  subset(PublicationType=="Artiklar i tidskrift"|PublicationType=="Artiklar i antologi") %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")

other_publication <- amne %>%
  subset(!(PublicationType=="Artiklar i tidskrift"|PublicationType=="Artiklar i antologi")) %>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

amne_table <- bind_rows(articles, other_publication)
amne_table[is.na(amne_table)] <- 0L

#Spara filer
write_csv(amne_table, "turismvetenskap_tabell.csv")
amne <- select(amne, -(list_of_orgs))
write_csv(amne, "turismvetenskap_publikationer.csv")

