# SUOMEKSI:



# R-koodi artikkeliin "Nuorten vakavan väkivallan kehitystrendit eri osoittimien valossa. Toistettava menetelmä avoimien tilastotietokantojen analyysiin" liittyen

**Artikkelin kirjoittajat** Maiju Tanskanen, Aaro Beuker, Karoliina Suonpää, Kimmo Haapakangas & Janne Kivivuori

**Koodin laatineet** Maiju Tanskanen (maiju.tanskanen@helsinki.fi) & Aaro Beuker (aaro.beuker@helsinki.fi)

***Koodiin voi viitata näin:*** Tanskanen, M., & Beuker, A. (2025). Nuorten vakavan väkivallan kehitys: Tilastokeskuksen avoimien tilastotietokantojen analyysi / Trends in serious youth violence: Analyzing Statistics Finland's open statistical databases (v.1.0.0). Zenodo. https://doi.org/10.5281/zenodo.17727855 


## Tiedostot
- `tekijat_full.R` – koodi hakee aineiston rikoksista epäillyistä nuorista henkilöistä ja tekee segementoituun regressioanalyysin perustuvan tarkastelun
- `uhrit_full.R` – koodi hakee aineiston rikoksen uhreina olleista nuorista henkilöistä ja tekee segementoituun regressioanalyysin perustuvan tarkastelun
- `kuolleet_full.R` – koodi hakee aineiston väkivallan uhreina kuolleista nuorista henkilöistä ja tekee segementoituun regressioanalyysin perustuvan tarkastelun
- `output.txt` – R-tuloste, jossa koodit ajettu em. järjestyksessä (tulokset, joihin artikkeli perustuu)
- `session_info.txt` – tiedot R-istunnosta (käyttöjärjestelmä, R-versio, pakettien versiot), josta em. tuloste peräisin
- `renv.lock` – lockfile (versiotiedot)


## Huomioita
Analyysissa tarkastellaan nuorten vakavan väkivallan kehitystrendejä eri osoittimien valossa.
Tarkastelu perustuu Tilastokeskuksen avoimiin tilastotietokantoihin (aineistot haetaan R:n pxweb-paketilla https://cran.r-project.org/web/packages/pxweb/index.html).



# IN ENGLISH:

# R Code for "Trends in Serious Youth Violence in Light of Different Indicators: A Reproducible Method for Analyzing Open Statistical Databases"

**Authors of the article** Maiju Tanskanen, Aaro Beuker, Karoliina Suonpää, Kimmo Haapakangas & Janne Kivivuori
**Authors of the code** Maiju Tanskanen (maiju.tanskanen@helsinki.fi) & Aaro Beuker (aaro.beuker@helsinki.fi)
***You can cite this code as follows:*** Tanskanen, M., & Beuker, A. (2025). Nuorten vakavan väkivallan kehitys: Tilastokeskuksen avoimien tilastotietokantojen analyysi / Trends in serious youth violence: Analyzing Statistics Finland's open statistical databases (v.1.0.0). Zenodo. https://doi.org/10.5281/zenodo.17727855 

## Files
- `tekijat_full.R` – script retrieves data on young persons suspected of crimes and performs an analysis based on segmented regression  
- `uhrit_full.R` – script retrieves data on young persons who have been victims of crime and performs an analysis based on segmented regression  
- `kuolleet_full.R` – script retrieves data on young persons who died as victims of violence and performs an analysis based on segmented regression  
- `output.txt` – R output file showing the results obtained by running the scripts above in this order (the results on which the article is based)  
- `session_info.txt` – information on the R session (operating system, R version, package versions) from which the above output was generated
- `renv.lock` – lockfile (version records)

## Notes
The analysis examines trends in serious youth violence using several indicators.
The analysis is based on open statistical databases provided by Statistics Finland (data are retrieved using the R package pxweb: https://cran.r-project.org/web/packages/pxweb/index.html).
