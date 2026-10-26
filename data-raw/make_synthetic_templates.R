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

# --- Landing-site sheet (stock-assessment form: one row per measured fish) ---
# Fictional sites and fish on the lake coast; written after every other draw
# so the templates above do not change.
ls_hdr <- c("SN\u00b0", "Village Nam/ Landing site ", "FISHING UNIT ID", "Sampling Date (dd-mm-yy)",
  "Landing site Latitude (S)(DD) 4.8873\u00b0", "Landing site Longtude (S)(DD) 29.8873\u00b0", "Fishing Unit type",
  "Number of Auxiliary Boats/Auxiliary Lamps attached to the fishing unit", "SOURCE OF LIGHT", "Number of lamps in a Unit",
  "Total Watts of all Lamps(W)", "Mesh size of the net (mm)", "Fishing Net Width (m)", "Fishing Net Length (m)",
  "Number of Hooks used in a Unit", "HOOK SIZE", "# of Hauls per Day", "Sailing hours to the Fishing Ground (Hr)",
  "Time spent in fishing (hr)", "Number of days Fished in 7 days using same fishing unit type",
  "Fishing Location Latitude (S)(DD) 4.8873\u00b0", "Fishing Location Longtude (S)(DD) 29.8873\u00b0",
  "Sailing Direction (S,W,E,WS etc)", "Fishing ground", "Estimated Sailing Speed ( Engine powered 10 or 15 Km/h) (HL =5 Km/h)",
  "Is Catch sorted? (YES/NO)", "Unsorted Total Catch of the day (Kg) ", "Total weight of adult  Lates stap.(Kg)",
  "Total weight of juvenile  Lates stap.(Kg)", "Total weight of Stolothrissa tanganicae (Kg)",
  "Total weight of Limnothrissa miodon (Kg)", "Total weight of OTHER SPECIES (Kg)",
  "Total weight of Shrimps (Kg) ( caught in the same fishing net)",
  "Species Measured (Lates stappersii, Stolothrissa tanganica, Limnothrissa miodon )", "Total Length (mm)", "Weight (g)",
  "Sex", "MS", "Gonad Weight (gm)", "Gall Blader Length (mm)", "Basin", "Remarks")
ls_sites <- data.frame(site = c("Site Alpha", "SITE BRAVO", "Site Charlie"), lat = c(5.21, 6.38, 7.92), lon = c(29.62, 29.84, 30.78),
                       stringsAsFactors = FALSE)
ls_sp <- data.frame(name = c("Lates stappersii", "Stolothrissa tanganicae", "Limnothrissa miodon"),
                    len_mu = c(220, 80, 85), len_sd = c(60, 12, 14), a = c(0.0083, 0.0060, 0.0057), b = c(2.91, 3.04, 3.10),
                    stringsAsFactors = FALSE)
ls_units <- data.frame(site = rep(1:3, each = 3), unit = rep(1:3, 3), day = rep(c(1, 1, 2), 3),
                       gear = c("LN", "HL", "RN", "LN", "RN", "HL", "LN", "GN", "HL"), stringsAsFactors = FALSE)
ls_rows <- list()
for (i in seq_len(nrow(ls_units))) {
  u <- ls_units[i, ]; site <- ls_sites[u$site, ]
  hl <- u$gear == "HL"
  p_sp <- if (hl) c(0.9, 0.05, 0.05) else c(0.15, 0.55, 0.30)
  n <- sample(15:25, 1)
  sp <- sample(ls_sp$name, n, replace = TRUE, prob = p_sp)
  k <- match(sp, ls_sp$name)
  len <- round(pmax(25, rnorm(n, ls_sp$len_mu[k], ls_sp$len_sd[k])))
  w <- round(ls_sp$a[k] * (len / 10)^ls_sp$b[k] * exp(rnorm(n, 0, 0.08)), 2)
  w[sample(n, max(1, round(0.1 * n)))] <- NA                          # measured but not weighed
  date <- as.Date("2026-03-31") + u$day
  catch <- unname(sapply(ls_sp$name, function(nm) round(sum(w[sp == nm], na.rm = TRUE) / 1000 * runif(1, 2, 8), 1)))
  fish_ok <- i != 5                                                    # one unit with no fishing location
  df <- data.frame(
    `SN\u00b0` = 1000 + i * 100 + seq_len(n), site = site$site, unit = u$unit,
    date = if (i == 6) "2April2026" else as.character(as.numeric(date - as.Date("1899-12-30"))),
    land_lat = site$lat, land_lon = site$lon, gear = u$gear, aux = 1,
    light = if (u$gear == "LN") "LED" else NA, nlamps = if (u$gear == "LN") 12 else NA, watts = if (u$gear == "LN") 240 else NA,
    mesh = if (hl) NA else 5, netw = if (hl) NA else 27, netl = if (hl) NA else 27, nhooks = if (hl) 300 else NA, hook = NA,
    hauls = 1, sail = 1, fish_h = sample(c(4, 6, 10, 11), 1), days7 = 6,
    fish_lat = if (fish_ok) round(site$lat + runif(1, -0.05, 0.05), 5) else NA,
    fish_lon = if (fish_ok) round(site$lon - runif(1, 0.03, 0.15), 5) else NA,
    dir = NA, ground = paste("Ground", u$site), speed = 10, sorted = "NO", unsorted = sum(catch),
    lates_ad = if (i == 2) 0.5 else round(catch[1] * 0.8, 1), lates_juv = round(catch[1] * 0.2, 1),
    stolo = catch[2], limno = catch[3], other = 0, shrimp = 0,
    species = sp, tl = len, w = w, sex = sample(c("F", "M", "f", "-"), n, replace = TRUE, prob = c(0.4, 0.4, 0.1, 0.1)),
    ms = sample(1:4, n, replace = TRUE), gonad = round(runif(n, 0.02, 0.5), 2), gb = NA, basin = NA,
    remarks = if (i == 8) "Test fishing" else NA, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- ls_hdr
  ls_rows[[i]] <- df
}
landings <- do.call(rbind, ls_rows)

# --- Write ------------------------------------------------------------------
writexl::write_xlsx(list(Stations = st),            file.path(out_dir, "Stations_model.xlsx"))
writexl::write_xlsx(list(Catches = catches),        file.path(out_dir, "Catches_model.xlsx"))
writexl::write_xlsx(list(Length_frequency = lengths), file.path(out_dir, "Length-frequency_model.xlsx"))
writexl::write_xlsx(list(Biology = biology),        file.path(out_dir, "Biology_model.xlsx"))
writexl::write_xlsx(list(NASC = nasc),              file.path(out_dir, "NASC_model.xlsx"))
writexl::write_xlsx(list(Landings = landings),      file.path(out_dir, "Landings_model.xlsx"))
cat(sprintf("Synthetic templates written: %d stations, %d catch rows, %d lengths, %d biology rows, %d NASC rows, %d landing-site rows\n",
            nrow(st), nrow(catches), nrow(lengths), nrow(biology), nrow(nasc), nrow(landings)))
