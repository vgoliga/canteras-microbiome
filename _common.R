locations <- 
  c(
    "Confital_2", "Confital_1",
    "La_Puntilla", "Hotel_Cristina", "Reina_Isabel", "Playa_Chica",
    "Pena_la_Vieja", "Cicer"
  )

make_key <- function(year, week, site){
  site_key <- 
    site |> case_match(
      "Confital_1"      ~ "1-CFP",
      "Confital_2"      ~ "2-CFS",
      "La_Puntilla"     ~ "3-PNT",
      "Hotel_Cristina"  ~ "4-HTC",
      "Reina_Isabel"    ~ "5-HTI",
      "Playa_Chica"     ~ "6-PCH",
      "Pena_la_Vieja"   ~ "7-PVJ",
      "Cicer"           ~ "9-CCR"
    )
  paste0(substr(year, 3, 4), "w", str_pad(week, 2, pad = 0), "-", site_key)
}
  

filter_analysis <- function(data){
  data |>
    filter(location %in% c("Confital_1", "Hotel_Cristina", "Pena_la_Vieja")) |> 
    filter(date |> between(start, end))
}

filter_organic <- function(data){
  data |> filter(formula %in% c("toc", "ton"))
}

filter_inorganic <- function(data){
  data |>
    filter(formula %in% c("silicate", "phosphate", "ammonium", "nitrates_and_nitrites"))
}

filter_eukaryotes <- function(data){
  data |> filter(celltype %in% c("nanoeuka", "picoeuka"))
}

filter_cyanobacteria <- function(data){
  data |> filter(celltype %in% c("prochloro", "synecho"))
}

filter_dna <- function(data){
  data |> filter(celltype %in% c("hna", "lna"))
}

start <- ymd("2021-07-01")
end <- ymd("2023-07-01")

seasons_limits <- seq(start, end, by = "3 month") |> as_datetime()

make_season <- function(date){
  seasons <- 
    c("2021-summer", "2021-autumn", "2021-winter", "2022-spring", "2022-summer", "2022-autumn", "2022-winter", "2023-spring")
  
  match_season <- function(x) {
    idx = which(x >= seasons_limits) |> last()
    if_else(is.na(idx), NA, seasons[idx])
  }
  
  date |> map_chr(match_season) |> fct(levels = seasons)
  
}

mutate_asv_rate <- function(data){
  data |> 
    mutate(encounters = n(), .after = asv, .by = asv) |> 
    mutate(rate_pct = encounters / max(encounters) * 100, .after = encounters) |> 
    mutate(
      rate = 
        case_when(
          rate_pct < 5                ~ "< 5",
          between(rate_pct, 5, 25)    ~ "5-25",
          between(rate_pct, 25, 50)   ~ "25-50",
          between(rate_pct, 50, 75)   ~ "50-75",
          between(rate_pct, 75, 95)   ~ "75-95",
          rate_pct > 95               ~ "> 95",
          .default = NA
        ) |> 
        fct(levels = c("< 5", "5-25", "25-50", "50-75", "75-95", "> 95"))
    )
  # mutate(rarity = probability_pct |> cut_width(5, center = 5/2), .after = probability_pct)
}

# Labels -----

labels_location <- 
  c(
    "Confital_2"      = "El Confital 2",
    "Confital_1"      = "El Confital 1",
    "Hotel_Cristina"  = "Hotel Cristina",
    "Playa_Chica"     = "Playa Chica",
    "Pena_la_Vieja"   = "Peña la Vieja",
    "Reina_Isabel"    = "Reina Isabel",
    "La_Puntilla"     = "La Puntilla"
  )

labels_season <- 
  c(
    "2021-summer" = "Sum 2021",
    "2021-autumn" = "Aut 2021",
    "2021-winter" = "Win 2021",
    "2022-spring" = "Spr 2022",
    "2022-summer" = "Sum 2022",
    "2022-autumn" = "Aut 2022",
    "2022-winter" = "Win 2022",
    "2023-spring" = "Spr 2023"
  )

labels_formula <- 
  c(
    "toc"                   = "TOC",
    "ton"                   = "TON",
    "silicate"              = "Si(OH)₄",
    "phosphate"             = "PO4³⁻",
    "ammonium"              = "NH₄⁺",
    "nitrates_and_nitrites" = "NO₃⁻ + NO₂⁻" 
  )

labels_celltype <- 
  c(
    "nanoeuka"  = "Nanoeukaryotes",
    "picoeuka"  = "Picoeukaryotes",
    "prochloro" = "Prochlorococcus",
    "synecho"   = "Synechococcus",
    "hna"       = "HNA Bacteria",
    "lna"       = "LNA Bacteria"
  )

labels_ratio <- 
  c(
    "ag"     = "α:γ",
    "as"     = "A:S",
    "aos"    = "AO:S",
    "bngb"   = "BNγ:β",
    "bbca"   = "BBC:α",
    "bfga"   = "BFγ:α"
  )