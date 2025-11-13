library(pxweb)
library(dplyr)
library(stringi)
library(stringr)
library(tidyverse)
library(tidyr)
library(broom)
library(dplyr)
library(zoo)
library(patchwork)
library(segmented)




####AINEISTON HAKEMINEN JA MUODOSTAMINEN

set.seed(211)


pxweb_query_list <-
  list("Vuosi" = c("2000", "2001", "2002", "2003", "2004", "2005", "2006",
                   "2007", "2008", "2009", "2010", "2011", "2012", "2013",
                   "2014", "2015", "2016", "2017", "2018", "2019", "2020",
                   "2021", "2022", "2023", "2024"),
       "Pääasiallinen toiminta" = c("SSS"),
       "Rikosryhmä ja teonkuvauksen tarkenne" = c("201_202_205", "203", "212"),
       "Epäillyn sukupuoli" = c("SSS"),
       "Syylliseksi epäillyn ikä" = c("15-17", "18-20", "21-24", "25-29"),
       "Tiedot" = c("ep_lkm", "ep_lkm_nimike", "ep_lkm_tork"))

# Download data
px_data <-
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/rpk/statfin_rpk_pxt_13h5.px",
            query = pxweb_query_list)

# Convert to data.frame
data <- as.data.frame(px_data, column.name.type = "text",
                      variable.value.type = "text")

#TÄSSÄ SIIVOTAAN DATAA JA JAETAAN ALUKSI 15-20-vuotiaisiin & 21-29-vuotiaisiin

data <- dplyr::select(data, -"Pääasiallinen toiminta", -"Epäillyn sukupuoli")
data <- data %>%
  rename(epaillyt = `Selvitettyihin rikoksiin syylliseksi epäillyt`,
         nimiklkm = `Selvitettyihin rikoksiin syylliseksi epäiltyjen rikosnimikkeiden määrä`,
         torkein = `Selvitettyihin rikoksiin syylliseksi epäillyt vuoden törkeimmän rikoksen mukaan`,
         ika = `Syylliseksi epäillyn ikä`,
         rikryhm = `Rikosryhmä ja teonkuvauksen tarkenne`,
         vuosi = Vuosi)

data <- data %>%
  mutate(rikryhm = ifelse(rikryhm == "1201 Henkirikokset  21:1-3,34a:1 yhteensä", "hr", rikryhm),
         rikryhm = ifelse(rikryhm == "1202 Tapon, murhan tai surman yritys yhteensä 21:1-3,34a:1", "hry", rikryhm),
         rikryhm = ifelse(rikryhm == "12042 Törkeä pahoinpitely 21:6,34a:1§1/6", "tpp", rikryhm))

nuorimmat <- data %>% filter(ika == "15 - 17" | ika == "18 - 20")

nuorimmat <- nuorimmat %>%
  group_by(vuosi, rikryhm) %>%
  summarize(epaillyt = sum(epaillyt),
            nimiklkm = sum(nimiklkm),
            torkein = sum(torkein))
nuorimmat$ika <- "15-20"

vanhemmat <- data %>% filter(ika == "21 - 24" | ika == "25 - 29")
vanhemmat <- vanhemmat %>%
  group_by(vuosi, rikryhm) %>%
  summarize(epaillyt = sum(epaillyt),
            nimiklkm = sum(nimiklkm),
            torkein = sum(torkein))
vanhemmat$ika <- "21-29"

#LADATAAN IKÄRYHMIEN KOOT TK:N RAJAPINNASTA

pxweb_query_list <-
  list("Vuosi" = c("2000", "2001", "2002", "2003", "2004", "2005", "2006",
                   "2007", "2008", "2009", "2010", "2011", "2012", "2013",
                   "2014", "2015", "2016", "2017", "2018", "2019", "2020",
                   "2021", "2022", "2023", "2024"),
       "Sukupuoli" = c("SSS"),
       "Ikä" = c("015", "016", "017", "018", "019", "020", "021", "022",
                 "023", "024", "025", "026", "027", "028", "029"),
       "Tiedot" = c("vaesto"))

# Download data
px_data <-
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/vaerak/statfin_vaerak_pxt_11rd.px",
            query = pxweb_query_list)

# Convert to data.frame
vaesto <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
vaesto <- vaesto %>%
  pivot_wider(names_from = Vuosi, values_from = `Väestö 31.12.`)
vaesto_n <- vaesto %>% filter(Ikä  < 21)
vaesto_n <- vaesto_n %>% summarize(across(starts_with("20"), sum, na.rm = TRUE))
vaesto_n <- vaesto_n %>% pivot_longer(cols = everything(),
                                      names_to = "vuosi")
vaesto_n$ika <- "15-20"

vaesto_v <- vaesto %>% filter(Ikä > 20)
vaesto_v <- vaesto_v %>% summarize(across(starts_with("20"), sum, na.rm = TRUE))
vaesto_v <- vaesto_v %>% pivot_longer(cols = everything(),
                                      names_to = "vuosi")
vaesto_v$ika <- "21-29"

vaesto <- rbind(vaesto_n, vaesto_v)

#TÄSSÄ YHDISTETÄÄN RIKOSTIEDOT JA TIEDOT IKÄRYHMÄN KOOSTA + LASKETAAN RIKOKSET 100K/KOHDEN

yhdistetty_v <- vanhemmat %>%
  left_join(vaesto, join_by("ika", "vuosi"))
yhdistetty_v <- yhdistetty_v %>%
  rowwise() %>%
  mutate(epaillyt21 = epaillyt / value * 100000,
         nimiklkm21 = nimiklkm / value * 100000,
         torkein21 = torkein / value * 100000)

yhdistetty_v <- yhdistetty_v %>%
  pivot_wider(names_from = rikryhm,
              values_from = c(epaillyt21, nimiklkm21, torkein21))

yhdistetty_v <- yhdistetty_v %>%
  mutate(across(everything(), as.numeric))

yhdistetty_v <- yhdistetty_v %>%
  group_by(vuosi) %>%
  summarize(across(everything(), max, na.rm = TRUE))
yhdistetty_v <- dplyr::select(yhdistetty_v, -"ika", -"epaillyt",
                              -"nimiklkm", -"torkein", -"value")

yhdistetty_n <- nuorimmat %>%
  left_join(vaesto, join_by("ika", "vuosi"))
yhdistetty_n <- yhdistetty_n %>%
  rowwise() %>%
  mutate(epaillyt15 = epaillyt / value * 100000,
         nimiklkm15 = nimiklkm / value * 100000,
         torkein15 = torkein / value * 100000)

yhdistetty_n <- yhdistetty_n %>%
  pivot_wider(names_from = rikryhm,
              values_from = c(epaillyt15, nimiklkm15, torkein15))
yhdistetty_n <- yhdistetty_n %>%

  mutate(across(everything(), as.numeric))

yhdistetty_n <- yhdistetty_n %>%
  group_by(vuosi) %>%
  summarize(across(everything(), max, na.rm = TRUE))
yhdistetty_n <- dplyr::select(yhdistetty_n, -"ika", -"epaillyt",
                              -"nimiklkm", -"torkein", -"value")

#TÄSSÄ VIIMEIN IKÄLUOKKIEN DATAT YHTEEN

data <- yhdistetty_n %>%
  left_join(yhdistetty_v, by = "vuosi")

rm(vaesto_n, vaesto_v, vanhemmat, nuorimmat, vaesto, yhdistetty_n, yhdistetty_v)

library(segmented)




##ANALYYSI

###TEKIJÄT 15-20
#Valitaan analysoitavat muuttujat

dep_vars <- c("epaillyt15_hr", "epaillyt15_hry", "epaillyt15_tpp",
              "nimiklkm15_hr", "nimiklkm15_hry", "nimiklkm15_tpp",
              "torkein15_hr", "torkein15_hry", "torkein15_tpp")


# Otsikot kuvioon
row_titles <- c("Rikokset",
                "Rikosnimikkeet",
                "Törkeimmän mukaan")

col_titles <- c("Henkirikokset", "Henkirikoksen yritykset", "Törkeät pahoinpitelyt")



# Kuvion pohja
num_vars <- length(dep_vars)
ncols <- 3  # Sarakkeita per rivi
nrows <- ceiling(num_vars / ncols)  # Rivien lukumäärä
par(mfrow = c(nrows, ncols), mar = c(4, 4, 2, 1), oma = c(2, 7, 4, 2))  # Marginaalit

# Looppi kaikille valituille muuttujille:
for (i in seq_along(dep_vars)) {
  dv <- dep_vars[i]


  formula_str <- as.formula(paste(dv, "~ vuosi"))

  # Lineaarinen regressiomalli
  lm_model <- lm(formula_str, data = data)

  # Segmentoitu malli (katkoskohtien valinta BIC:n perusteella; määritetään testattavien
  # katkoskohtien maksimiksi 5, joka riittää tässä tapauksessa)
  seg_model <- selgmented(lm_model, seg.Z = ~ vuosi, return.fit = TRUE, type = "bic", Kmax = 5)

  pred <- predict(seg_model, interval = "confidence")
  data$lwr <- pred[, "lwr"]
  data$upr <- pred[, "upr"]

  # Tulostetaan outputille mallin summary + luottamusvälit
  print(paste("Vastemuuttuja:", dv))
  print(summary(seg_model))
  print(confint.default(seg_model))

  # Tulostetaan outputille mallin mahdolliset slopet ja niiden luottamusvälit
  slope_test <- try(slope(seg_model), silent = TRUE)
  print(slope_test)

  # Katkoskohdat
  bp <- seg_model$psi[, "Est."]

  #CI

  # Luodaan hajontakuvio
  plot(data$vuosi, data[[dv]], pch = 16, col = "blue", ylab = "", xlab = "", main = "")

  # Lisätään (segmentoitu) regressiokäyrä (trend line)
  lines(data$vuosi, predict(seg_model), col = "red", lwd = 2)
  lines(data$vuosi, data$lwr, col = "gray", lty = 2)
  lines(data$vuosi, data$upr, col = "gray", lty = 2)

  # Korostetaan katkoskohdat pystyviivoilla
  if (length(bp) > 0) {
    abline(v = bp, col = "green", lwd = 2, lty = 2)
  }

  # Lisätään otsikot sarakkeisiin
  if (i <= ncols) {
    title(main = col_titles[i], line = 0.5, cex.main = 1.1)
  }

  # Lisätään otsikot riveihin
  if ((i - 1) %% ncols == 0) {
    row_index <- ceiling(i / ncols)
    mtext(row_titles[row_index], side = 2, line = 4, cex = 0.5, font = 2, las = 1)
  }
}


###TEKIJÄT 21-29
#Valitaan analysoitavat muuttujat

dep_vars <- c("epaillyt21_hr", "epaillyt21_hry", "epaillyt21_tpp",
              "nimiklkm21_hr", "nimiklkm21_hry", "nimiklkm21_tpp",
              "torkein21_hr", "torkein21_hry", "torkein21_tpp")

# Otsikot kuvioon
row_titles <- c("Rikokset",
                "Rikosnimikkeet",
                "Törkeimmän mukaan")

col_titles <- c("Henkirikokset", "Henkirikoksen yritykset", "Törkeät pahoinpitelyt")



# Kuvion pohja
num_vars <- length(dep_vars)
ncols <- 3  # Sarakkeita per rivi
nrows <- ceiling(num_vars / ncols)  # Rivien lukumäärä
par(mfrow = c(nrows, ncols), mar = c(4, 4, 2, 1), oma = c(2, 7, 4, 2))  # Marginaalit

# Looppi kaikille valituille muuttujille:
for (i in seq_along(dep_vars)) {
  dv <- dep_vars[i]


  formula_str <- as.formula(paste(dv, "~ vuosi"))

  # Lineaarinen regressiomalli
  lm_model <- lm(formula_str, data = data)

  # Segmentoitu malli (katkoskohtien valinta BIC:n perusteella; määritetään testattavien
  # katkoskohtien maksimiksi 5, joka riittää tässä tapauksessa)
  seg_model <- selgmented(lm_model, seg.Z = ~ vuosi, return.fit = TRUE, type = "bic", Kmax = 5)
  pred <- predict(seg_model, interval = "confidence")
  data$lwr <- pred[, "lwr"]
  data$upr <- pred[, "upr"]

  # Tulostetaan outputille mallin summary + luottamusvälit
  print(paste("Vastemuuttuja:", dv))
  print(summary(seg_model))
  print(confint.default(seg_model))

  # Tulostetaan outputille mallin mahdolliset slopet ja niiden luottamusvälit
  slope_test <- try(slope(seg_model), silent = TRUE)
  print(slope_test)

  # Katkoskohdat
  bp <- seg_model$psi[, "Est."]

  # Luodaan hajontakuvio
  plot(data$vuosi, data[[dv]], pch = 16, col = "blue", ylab = "", xlab = "", main = "")

  # Lisätään (segmentoitu) regressiokäyrä (trend line)
  lines(data$vuosi, predict(seg_model), col = "red", lwd = 2)
  lines(data$vuosi, data$lwr, col = "gray", lty = 2)
  lines(data$vuosi, data$upr, col = "gray", lty = 2)


  # Korostetaan katkoskohdat pystyviivoilla
  if (length(bp) > 0) {
    abline(v = bp, col = "green", lwd = 2, lty = 2)
  }

  # Lisätään otsikot sarakkeisiin
  if (i <= ncols) {
    title(main = col_titles[i], line = 0.5, cex.main = 1.1)
  }

  # Lisätään otsikot riveihin
  if ((i - 1) %% ncols == 0) {
    row_index <- ceiling(i / ncols)
    mtext(row_titles[row_index], side = 2, line = 4, cex = 0.5, font = 2, las = 1)
  }
}