# Builds the example templates in data/ from random numbers.
#
# Everything here is SYNTHETIC: a fictional 3-day survey ("SYN2026") with
# made-up stations, catches, fish and NASC. No real survey data is used, so
# the templates can be shared freely. Re-run from the project root:
#   Rscript data-raw/make_synthetic_templates.R

set.seed(20260310)
out_dir <- "data"

# --- Species: plausible lengths (cm) and length-weight parameters (g, cm) --
species <- data.frame(
  espece  = c("Sardina pilchardus", "Engraulis encrasicolus", "Sardinella aurita",
              "Trachurus trachurus", "Scomber colias", "Merluccius merluccius",
              "Boops boops", "Sepia officinalis"),
  len_mu  = c(17, 12, 22, 20, 25, 30, 18, NA),
  len_sd  = c(2, 1.5, 3, 4, 4, 7, 2.5, NA),
  a       = c(0.0075, 0.0050, 0.0080, 0.0095, 0.0070, 0.0055, 0.0100, NA),
  b       = c(3.10, 3.15, 3.10, 3.00, 3.15, 3.10, 3.00, NA),
  weight  = c(5, 4, 4, 3, 3, 2, 2, 1),   # relative chance of being caught
  stringsAsFactors = FALSE
)

fmt_hm  <- function(m) sprintf("%02d:%02d", m %/% 60, m %% 60)
fmt_hms <- function(s) sprintf("%02d:%02d:%05.2f", s %/% 3600, (s %% 3600) %/% 60, s %% 60)
move <- function(lat, lon, dist_nmi, heading_deg) {
  h <- heading_deg * pi / 180
  c(lat + dist_nmi * cos(h) / 60, lon + dist_nmi * sin(h) / (60 * cos(lat * pi / 180)))
}

# --- Stations ---------------------------------------------------------------
n_st <- 12
days <- as.Date("2026-03-10") + rep(0:2, each = 4)
st <- data.frame(Station = sprintf("%02d", seq_len(n_st)), stringsAsFactors = FALSE)
st$Date <- format(days, "%d/%m/%Y")
st$Durée_chalutage <- sample(c(20, 25, 30, 30, 30, 35, 40), n_st, replace = TRUE)
st$Cap_chalutage <- sample(seq(0, 350, 10), n_st, replace = TRUE)
st$Vitesse_chalutage <- round(runif(n_st, 3.4, 4.0), 1)
st$Longueur_funes <- sample(c(150, 200, 250, 300), n_st, replace = TRUE)
start_min <- sample(seq(7 * 60, 17 * 60, 5), n_st)
st$Heure_filage <- fmt_hm(start_min)
lat0 <- runif(n_st, 20.2, 21.4)
lon0 <- runif(n_st, -18.0, -17.4)
dist <- st$Vitesse_chalutage * st$Durée_chalutage / 60
end <- t(mapply(move, lat0, lon0, dist, st$Cap_chalutage))
st$Latitude_filage <- round(lat0, 5)
st$Longitude_filage <- round(lon0, 5)
bottom <- round(runif(n_st, 40, 220))
st$Prof_fond_filage <- bottom
st$Prof_filet_filage <- pmax(10, round(bottom * runif(n_st, 0.3, 0.9)))
st$Heure_virage <- fmt_hm(start_min + st$Durée_chalutage)
st$Latitude_virage <- round(end[, 1], 5)
st$Longitude_virage <- round(end[, 2], 5)
st$Prof_fond_virage <- bottom + sample(-5:5, n_st, replace = TRUE)
st$Prof_filet_virage <- pmax(10, st$Prof_filet_filage + sample(-3:3, n_st, replace = TRUE))
st$Note <- NA_character_

# --- Catches ----------------------------------------------------------------
catch_rows <- list()
for (s in st$Station) {
  k <- sample(3:6, 1)
  sp <- sample(species$espece, k, prob = species$weight)
  for (e in sp) {
    tot_p <- round(exp(rnorm(1, log(15), 1.2)), 2)
    ech_p <- if (tot_p > 30) round(runif(1, 10, 25), 2) else tot_p
    row <- species[species$espece == e, ]
    mean_w <- if (is.na(row$a)) 150 else row$a * row$len_mu^row$b
    tot_n <- max(1, round(tot_p * 1000 / mean_w))
    catch_rows[[length(catch_rows) + 1]] <- data.frame(
      Station = s, Espece = e, Ech_P = ech_p, Tot_P = tot_p, Tot_N = tot_n,
      stringsAsFactors = FALSE
    )
  }
}
catches <- do.call(rbind, catch_rows)

# --- Length frequency and biology (fish only) -------------------------------
lf_rows <- list(); bio_rows <- list()
for (i in seq_len(nrow(catches))) {
  cr <- catches[i, ]
  row <- species[species$espece == cr$Espece, ]
  if (is.na(row$a)) next
  n_sample <- round(cr$Tot_N * cr$Ech_P / cr$Tot_P)
  n_meas <- max(1, min(n_sample, sample(30:80, 1)))
  len <- round(pmax(3, rnorm(n_meas, row$len_mu, row$len_sd)), 1)
  station_num <- as.integer(cr$Station)
  lf_rows[[length(lf_rows) + 1]] <- data.frame(
    station = station_num, espece = cr$Espece, Poids_Mens = cr$Ech_P,
    taille_cm = len, nombre = seq_len(n_meas), stringsAsFactors = FALSE
  )
  n_bio <- min(n_meas, 10)
  lb <- len[seq_len(n_bio)]
  stn <- st[st$Station == cr$Station, ]
  bio_rows[[length(bio_rows) + 1]] <- data.frame(
    id_campagne = "SYN2026", mois = 3, annee = 2026, saison = "Spring",
    navire = "Synthetic vessel", chef_mission = "N/A", stock = NA_character_,
    type_mission = "Acoustic", espece = cr$Espece, station = station_num,
    latitude = stn$Latitude_filage, longitude = stn$Longitude_filage,
    nombre = seq_len(n_bio),
    poids_g = round(row$a * lb^row$b * exp(rnorm(n_bio, 0, 0.08)), 2),
    taille_cm = lb,
    sexe = ifelse(lb < row$len_mu - 1.5 * row$len_sd, "I", sample(c("F", "M"), n_bio, replace = TRUE)),
    sms = pmin(5, pmax(1, round(1 + 4 * (lb - min(lb)) / max(1, diff(range(lb))) + rnorm(n_bio, 0, 0.6)))),
    graisse = NA_character_, aliment = NA_character_, couleur = NA_character_,
    stringsAsFactors = FALSE
  )
}
lengths <- do.call(rbind, lf_rows)
biology <- do.call(rbind, bio_rows)

# --- NASC (Echoview region-by-cell export) ----------------------------------
acoustic_species <- c("Sardina pilchardus", "Engraulis encrasicolus", "Sardinella aurita",
                      "Trachurus trachurus", "Scomber colias")
nasc_rows <- list(); region_id <- 0
for (d in 0:2) {
  date <- format(as.Date("2026-03-10") + d, "%Y%m%d")
  t_sec <- 7 * 3600 + 15 * 60
  interval <- 0
  lat <- 20.3 + 0.4 * d
  lon <- -17.4
  for (transect in 1:2) {
    # Zig-zag: offshore (west) then back inshore (east), 0.2 degrees further north
    heading <- if (transect == 1) 270 else 90
    if (transect == 2) lat <- lat + 0.2
    for (j in 1:25) {
      interval <- interval + 1
      t_end <- t_sec + 360 + runif(1, -20, 20)           # about 1 nmi at 10 kn
      p_end <- move(lat, lon, 1, heading)
      offshore <- if (transect == 1) j else 26 - j
      bottom <- round(60 + 6 * offshore + rnorm(1, 0, 5))
      cell <- data.frame(
        Interval = interval, Date_S = as.numeric(date), Time_S = sub(".", ",", fmt_hms(t_sec), fixed = TRUE),
        Date_E = as.numeric(date), Time_E = sub(".", ",", fmt_hms(t_end), fixed = TRUE),
        Lat_S = round(lat, 5), Lon_S = round(lon, 5), Lat_E = round(p_end[1], 5), Lon_E = round(p_end[2], 5),
        Exclude_above_line_depth_mean = 10,
        Exclude_below_line_range_mean = bottom - 7, Exclude_below_line_range_max = bottom - 4,
        Exclude_below_line_depth_min = bottom - 3, stringsAsFactors = FALSE
      )
      for (layer in 1:2) {
        n_reg <- sample(0:2, 1, prob = c(0.35, 0.45, 0.2))
        if (n_reg == 0) {
          region_id <- region_id + 1
          nasc_rows[[length(nasc_rows) + 1]] <- cbind(cell, Layer = layer, Region_ID = region_id,
            PRC_NASC = 0, Species_name = "Autres", Species_percent = 100)
          next
        }
        for (r in seq_len(n_reg)) {
          region_id <- region_id + 1
          k <- sample(1:3, 1)
          sp <- c(sample(acoustic_species, k), if (runif(1) < 0.3) sample(c("Autres", "Plancton"), 1))
          pct <- round(100 * prop.table(runif(length(sp))), 2)
          pct[1] <- round(100 - sum(pct[-1]), 2)
          nasc_rows[[length(nasc_rows) + 1]] <- cbind(cell, Layer = layer, Region_ID = region_id,
            PRC_NASC = round(exp(rnorm(1, log(150), 1.3)), 4), Species_name = sp, Species_percent = pct)
        }
      }
      t_sec <- t_end + 1
      lat <- p_end[1]; lon <- p_end[2]
    }
    t_sec <- t_sec + 75 * 60                               # run between transects
  }
}
nasc <- do.call(rbind, nasc_rows)
nasc <- nasc[, c("Interval", "Layer", "Region_ID", "PRC_NASC", "Date_S", "Time_S", "Date_E", "Time_E",
                 "Lat_S", "Lon_S", "Lat_E", "Lon_E", "Exclude_above_line_depth_mean", "Species_name",
                 "Species_percent", "Exclude_below_line_range_mean", "Exclude_below_line_range_max",
                 "Exclude_below_line_depth_min")]

# --- Write ------------------------------------------------------------------
writexl::write_xlsx(list(Stations = st),            file.path(out_dir, "Stations_model.xlsx"))
writexl::write_xlsx(list(Catches = catches),        file.path(out_dir, "Catches_model.xlsx"))
writexl::write_xlsx(list(Length_frequency = lengths), file.path(out_dir, "Length-frequency_model.xlsx"))
writexl::write_xlsx(list(Biology = biology),        file.path(out_dir, "Biology_model.xlsx"))
writexl::write_xlsx(list(NASC = nasc),              file.path(out_dir, "NASC_model.xlsx"))
cat(sprintf("Synthetic templates written: %d stations, %d catch rows, %d lengths, %d biology rows, %d NASC rows\n",
            nrow(st), nrow(catches), nrow(lengths), nrow(biology), nrow(nasc)))
