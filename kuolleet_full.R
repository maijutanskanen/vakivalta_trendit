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
  list("Tapaturmat ja väkivalta (ulkoisten syiden luokitus)" = c("098-109"),
       "Ikä" = c("15-19", "20-24", "25-29"),
       "Vuosi" = c("2000", "2001", "2002", "2003", "2004", "2005", "2006",
                   "2007", "2008", "2009", "2010", "2011", "2012", "2013",
                   "2014", "2015", "2016", "2017", "2018", "2019", "2020",
                   "2021", "2022", "2023",  "2024"),
       "Sukupuoli" = c("SSS"),
       "Tiedot" = c("ksyylkm3"))

# Download data
px_data <-
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/StatFin/ksyyt/statfin_ksyyt_pxt_11b2.px",
            query = pxweb_query_list)

# Convert to data.frame
data <- as.data.frame(px_data, column.name.type = "text",
                      variable.value.type = "text")

#TÄSSÄ SIIVOTAAN DATAA JA JAETAAN ALUKSI 15-20-vuotiaisiin & 21-29-vuotiaisiin
data <- dplyr::select(data, -`Sukupuoli`)
data <- data %>%
  rename(kuolleet = `Tapaturmiin ja väkivaltaan kuolleet yhteensä`,
         ika = `Ikä`,
         vuosi = Vuosi)

nuorimmat <- data %>% filter(ika == "15 - 19")

nuorimmat$ika <- "15-19"

vanhemmat <- data %>% filter(ika == "20 - 24" | ika == "25 - 29")
vanhemmat <- vanhemmat %>%
  group_by(vuosi) %>%
  summarize(kuolleet_yht = sum(kuolleet))

vanhemmat$ika <- "20-29"

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
vaesto <- as.data.frame(px_data, column.name.type = "text",
                        variable.value.type = "text")
vaesto <- vaesto %>%
  pivot_wider(names_from = Vuosi, values_from = `Väestö 31.12.`)
vaesto_n <- vaesto %>% filter(Ikä  < 20)
vaesto_n <- vaesto_n %>% summarize(across(starts_with("20"), sum, na.rm = TRUE))
vaesto_n <- vaesto_n %>% pivot_longer(cols = everything(),
                                      names_to = "vuosi")
vaesto_n$ika <- "15-19"

vaesto_v <- vaesto %>% filter(Ikä > 19)
vaesto_v <- vaesto_v %>% summarize(across(starts_with("20"), sum, na.rm = TRUE))
vaesto_v <- vaesto_v %>% pivot_longer(cols = everything(),
                                      names_to = "vuosi")
vaesto_v$ika <- "20-29"

vaesto <- rbind(vaesto_n, vaesto_v)

#YHDISTETÄÄN RIKOSTIEDOT JA TIEDOT IKÄRYHMÄN KOOSTA + LASKETAAN RIKOKSET 100K/KOHDEN

yhdistetty_v <- vanhemmat %>%
  left_join(vaesto, join_by("ika", "vuosi"))
yhdistetty_v <- yhdistetty_v %>%
  rowwise() %>%
  mutate(kuolleet20 = kuolleet_yht / value * 100000)

yhdistetty_v <- yhdistetty_v %>%
  group_by(vuosi) %>%
  summarize(across(everything(), max, na.rm = TRUE))

yhdistetty_v <- dplyr::select(yhdistetty_v, -`kuolleet_yht`, -`ika`, -`value`)


yhdistetty_n <- nuorimmat %>%
  left_join(vaesto, join_by("ika", "vuosi"))
yhdistetty_n <- yhdistetty_n %>%
  rowwise() %>%
  mutate(kuolleet15 = kuolleet / value * 100000)
yhdistetty_n <- dplyr::select(yhdistetty_n, -`Tapaturmat ja väkivalta (ulkoisten syiden luokitus)`,
                              -`ika`, -`kuolleet`, -`value`)


#TÄSSÄ VIIMEIN IKÄLUOKKIEN DATAT YHTEEN
data <- yhdistetty_n %>%
  left_join(yhdistetty_v, by = "vuosi")

data <- data %>%
  mutate(across(everything(), as.numeric))

rm(vaesto_n, vaesto_v, vanhemmat, nuorimmat, vaesto, yhdistetty_n, yhdistetty_v)


##ANALYYSI

#Valitaan analysoitavat muuttujat

dep_vars <- c("kuolleet15", "kuolleet20")

# Otsikot kuvioon


col_titles <- c("15–19-vuotiaat", "20–29-vuotiaat")



# Kuvion pohja
num_vars <- length(dep_vars)
ncols <- 2  # Sarakkeita per rivi
nrows <- 1  # Rivien lukumäärä
par(mfrow = c(nrows, ncols), mar = c(7, 1, 7, 1), oma = c(2, 2, 2, 2))  # Marginaalit

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


}