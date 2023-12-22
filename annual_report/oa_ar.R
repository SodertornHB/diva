#
# oa_ar
# GL 231211
# 
# Statistik med DiVA som källa. Samma urval som till ÅR.
# Tabeller med antal publikationer och antal OA.
# Används till avsnittet Öppen vetenskap
# 
#

library(tidyverse)

source('/home/shub/src/common/lib/sh_diva_bibliometrics_functions.R')
diva <- read_csv(file="/home/shub/assets/diva/diva_researchpubl_sh_latest.csv")
diva <- diva %>% filter(between(Year, 2019, 2023))

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
ar_table <- ar_table[c(1,6,5,4,3,2)]
ar_table[is.na(ar_table)] <- 0L

write_csv(ar_table, "ar_alla_pub.csv")
write_csv(divaAR, "ar_pub.csv")

# OA antal till tabell ----------------------------------------------------
diva_oa <- divaAR %>%
  filter(FreeFulltext == TRUE | (!(is.na(FullTextLink))))

articles_oa <- diva_oa %>%
  subset(PublicationType=="Artiklar i tidskrift"|PublicationType=="Artiklar i antologi") %>%
  group_by(PublicationType,ContentType) %>% 
  count(Year) %>%
  spread(Year, n)%>%
  unite(PublicationType, PublicationType, ContentType, sep=": ")

other_publication_oa <- diva_oa %>%
  subset(!(PublicationType=="Artiklar i tidskrift"|PublicationType=="Artiklar i antologi")) %>%
  group_by(PublicationType) %>% 
  count(Year) %>%
  spread(Year, n)

oa_table <- bind_rows(articles_oa, other_publication_oa)
oa_table <- oa_table[c(1,6,5,4,3,2)] %>%
  rename("Öppet tillgänglig publicering" = PublicationType)
oa_table[is.na(oa_table)] <- 0L

write.csv(oa_table, "oa.csv")
