library(tidyverse)
library(readxl)
library(leaflet)

source("_common.R")

parameters <- read_csv("data/parameters.csv")

parameters <- parameters |> mutate(key = make_key(year(date), week(date), location)) |>
  rename(site = location) |> relocate(key, date, site)

parameters |> write_csv("data/processed/parameters.csv")

hidrotecnia_canteras <- read_xlsx("data/hidrotecnia-canteras.xlsx")

hidrotecnia_canteras <- hidrotecnia_canteras |> rename(site = Denominación, date = Fecha) |>
  filter(site %in% locations) |> mutate(key = make_key(year(date), week(date), site)) |> 
  select(key, date, site, ecoli = Ecoli, ecocci = `Enter. Intestinales`) |> 
  mutate(ecoli = parse_number(ecoli, na = c("*"))) |> 
  mutate(ecocci = parse_number(ecocci))

hidrotecnia_confital <- read_xlsx("data/hidrotecnia-confital.xlsx")

hidrotecnia_confital <- hidrotecnia_confital |> rename(site = `Nombre completo`, date = Fecha) |> 
  mutate(key = make_key(year(date), week(date), site)) |>
  select(key, date, site, ecoli = Ecoli, ecocci = `Enter. Intestinales`) |> 
  mutate(ecoli = parse_number(ecoli, na = c("NA", "*")))

hidrotecnia <- hidrotecnia_canteras |> bind_rows(hidrotecnia_confital)

hidrotecnia |> write_csv("data/processed/hidrotecnia.csv")

nayade_confital <- read_xlsx("data/nayade-confital.xlsx")

nayade_confital <- nayade_confital |> rename(date = `Fecha Muestreo`, site = `Denominación PM`) |> 
  mutate(date = dmy(date) |> as_datetime()) |> 
  pivot_wider(names_from = Parámetro, values_from = `Valor Cuantificado`)

nayade_confital <- nayade_confital |> mutate(key = make_key(year(date), week(date), site)) |>
  select(key, date, site, ecoli = `Escherichia coli`, ecocci = `Enterococo intestinal`) |> 
  mutate(ecoli = parse_number(ecoli), ecocci = parse_number(ecocci))

nayade_confital

nayade_canteras <- read_xlsx("data/nayade-canteras.xlsx")

nayade_canteras <- nayade_canteras |> rename(site = `Denominación PM`, date = `Fecha Muestreo`) |> 
  filter(site %in% locations) |> 
  mutate(date = dmy(date) |> as_datetime()) |> 
  pivot_wider(names_from = Parámetro, values_from = `Valor Cuantificado`)

nayade_canteras <- nayade_canteras |> mutate(key = make_key(year(date), week(date), site)) |>
  rename(ecoli = `Escherichia coli`, ecocci = `Enterococo intestinal`) |> 
  group_by(key, date, site) |> summarize(ecoli = max(ecoli), ecocci = max(ecocci))

nayade_canteras

nayade <- nayade_canteras |> bind_rows(nayade_confital)

nayade |> write_csv("data/processed/nayade.csv")

samples <- 
  read_csv("data/samples.csv", col_types = cols(location = col_factor(levels = locations))) |>
  mutate(date = make_datetime(year, month, day)) |> 
  mutate(season = make_season(date))

microbiome <- 
  read_csv("data/prokaryotes.csv") |> 
  left_join(samples, by = join_by(sample)) |> 
  mutate_asv_rate() |> 
  mutate(key = make_key(year, week(date), location))

microbiome <- microbiome |> 
  mutate(pi = reads / sum(reads), .by = sample) |> 
  select(key, date, site = location, reads, pi, domain, phylum, class, order, family, genus, species, asv)

microbiome |> write_csv("data/processed/microbiome.csv")