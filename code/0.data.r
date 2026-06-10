# ==============================================================================
# Data section - descriptive exhibits
# Builds the Data-section tables (per-hurricane summary statistics; the
# data-denial sample) and the storm-track map (Figure 1).
# Inputs:  data/clean/final_dataset.rds; data/clean/track_actual.rds
# Outputs: output/{summary_dmg,data_denial}.tex; output/denial_tracks_map.png
# Run:     Rscript --vanilla code/0.data.r
# ==============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyverse, here, kableExtra,
               sf, ggplot2, ggrepel, rnaturalearth)
source(here("code/functions_table.r"))

# ------------------------------------------------------------------------------
# Table: per-hurricane summary statistics (tab:summary_dmg)
# ------------------------------------------------------------------------------
voi_full <- readRDS(here("data/clean/final_dataset.rds")) |>
  as_tibble() |> drop_na(lat, lon, total_cost_pc, total_cost_per_gdp)

data_summary <- voi_full |>
  group_by(hurricane, county) |>
  mutate(wind_obs = max(wind_obs, na.rm = TRUE), precip_obs = max(precip_obs, na.rm = TRUE)) |>
  ungroup() |>
  distinct(year, hurricane, state, county, wind_obs, wind_fc, wind_error,
           precip_obs, precip_fc, precip_error, damages, deaths, ex_post_spending, ex_ante_spending) |>
  group_by(hurricane, year) |>
  summarise(max_wind_speed = sprintf("%.2f", mean(wind_obs)),
            max_wind_error = sprintf("%.2f", mean(abs(wind_error))),
            max_prec       = sprintf("%.2f", mean(precip_obs)),
            max_prec_error = sprintf("%.2f", mean(abs(precip_error))),
            sum_damage     = sprintf("%.2f", sum(damages) / 1e9),
            ex_post_spending = sprintf("%.2f", sum(ex_post_spending) / 1e9), .groups = "drop") |>
  arrange(year) |>
  mutate(hurricane = str_to_title(hurricane)) |>
  rename(Hurricane = hurricane, Year = year, `Wind Speed` = max_wind_speed,
         `Wind Speed Error` = max_wind_error, `Precipitation` = max_prec,
         `Precipitation Error` = max_prec_error, `Total Damage` = sum_damage,
         `Recovery Expenditures` = ex_post_spending)

summary_units <- c("", "", "(m/s)", "abs(m/s)", "(mm)", "abs(mm)", "(Billion \\$)", "(Billion \\$)")
kbl(data_summary, format = "latex", caption = "Summary Statistics by Hurricane.",
    label = "summary_dmg", booktabs = TRUE, escape = FALSE, linesep = "",
    col.names = summary_units, align = c("l", rep("c", ncol(data_summary) - 1))) |>
  add_header_above(setNames(rep(1, length(names(data_summary))), names(data_summary)), escape = FALSE) |>
  footnote(general = paste(
    "The table includes all category 1 and greater hurricanes (maximum wind speeds $\\\\geq$ 33 m/s)",
    "that made landfall in the continental US. Wind speed, precipitation, and their errors are averaged",
    "across counties to the hurricane level, and damages and expenditures are summed across counties. Wind",
    "speed is maximum sustained wind in m/s and precipitation is total precipitation in mm. Errors are averages",
    "of the absolute county-level errors. Damages and expenditures are in billions of (2019) US\\\\$."),
    threeparttable = TRUE, fixed_small_size = TRUE, escape = FALSE) |>
  as.character() |> writeLines(here("output/summary_dmg.tex"))
add_adjust_box(here("output/summary_dmg.tex"))

# ------------------------------------------------------------------------------
# Table: the data-denial sample (tab:data_denial)
# ------------------------------------------------------------------------------
denial_summary <- tribble(
  ~Year, ~Storm,     ~Intensity,   ~Landfall,
  2017,  "Harvey",   "Category 4", "26 Aug 0000",
  2017,  "Irma",     "Category 4", "10 Sep 1200",
  2017,  "Maria",    "Category 4", "20 Sep 0600",
  2018,  "Florence", "Category 1", "14 Sep 0600",
  2018,  "Michael",  "Category 5", "10 Oct 1200",
  2019,  "Dorian",   "Category 1", " 6 Sep 1200",
  2020,  "Hanna",    "Category 1", "25 Jul 1800",
  2020,  "Isaias",   "Category 1", " 4 Aug 0000",
  2020,  "Laura",    "Category 4", "27 Aug 0600",
  2020,  "Sally",    "Category 2", "16 Sep 0600")
writeLines(c(
  "\\begin{table}", "    \\centering", "    \\begin{threeparttable}",
  "      \\caption{Landfalling hurricanes in the data-denial sample, 2017--2020.}",
  "      \\label{tab:data_denial}", "      \\small", "      \\setlength{\\tabcolsep}{22pt}",
  "      \\renewcommand{\\arraystretch}{1.25}", "      \\begin{tabular}{c c c c}", "        \\hline",
  "        Year & Storm Name & Landfall Intensity & Landfall Time\\\\", "        \\hline",
  paste0("        ", denial_summary$Year, " & ", denial_summary$Storm, " & ",
         denial_summary$Intensity, " & ", denial_summary$Landfall, "\\\\"),
  "        \\hline", "      \\end{tabular}", "\\begin{tablenotes}", "\\small",
  "\\item \\textit{Note:} Landfall intensity follows the Saffir-Simpson Hurricane Wind Scale. Landfall time is the nearest synoptic hour (UTC). Maria made landfall in Puerto Rico; the remaining storms in the continental United States.",
  "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}"
), here("output/data_denial.tex"))

# ------------------------------------------------------------------------------
# Figure 1: observed tracks of the ten denial-sample storms
# ------------------------------------------------------------------------------
sf_use_s2(FALSE)
yr  <- c(harvey = 2017, irma = 2017, maria = 2017, florence = 2018, michael = 2018,
         dorian = 2019, laura = 2020, sally = 2020, isaias = 2020, hanna = 2020)
trk <- readRDS(here("data/clean/track_actual.rds")) |>
  filter(type == "BEST", hurricane %in% names(yr))
if (is.na(st_crs(trk))) st_crs(trk) <- 4326
trk$np   <- vapply(st_geometry(trk), function(g) nrow(st_coordinates(g)), integer(1))
trk      <- trk |> group_by(hurricane) |> slice_max(np, n = 1, with_ties = FALSE) |> ungroup()
trk$Name <- tools::toTitleCase(trk$hurricane)
trk$Year <- factor(yr[trk$hurricane])

land   <- ne_countries(scale = "medium", returnclass = "sf")
states <- ne_states(country = "United States of America", returnclass = "sf")

# label anchor = landfall: Maria at Puerto Rico, the rest at the nearest US land
us    <- st_union(st_geometry(states))
pr_pt <- st_sfc(st_point(c(-66.5, 18.2)), crs = 4326)
anchor <- do.call(rbind, lapply(seq_len(nrow(trk)), function(i) {
  pts <- st_cast(st_geometry(trk)[i], "POINT")
  p   <- pts[which.min(st_distance(pts, if (trk$hurricane[i] == "maria") pr_pt else us))]
  data.frame(Name = trk$Name[i], Year = trk$Year[i],
             lon = st_coordinates(p)[1], lat = st_coordinates(p)[2])
}))
# ghost points along each track so labels repel off the lines
gv <- as.data.frame(st_coordinates(trk)); gv <- gv[seq(1, nrow(gv), by = 6), c("X", "Y")]
repel_df <- rbind(anchor, data.frame(Name = "", Year = anchor$Year[1], lon = gv$X, lat = gv$Y))
# manual directional nudges for a few crowded labels
ndx <- setNames(rep(0, nrow(anchor)), anchor$Name); ndy <- ndx
ndy["Laura"] <- 2.5; ndy["Irma"] <- -1.8; ndy["Isaias"] <- 3.0
ndx["Dorian"] <- 2.0; ndy["Dorian"] <- -2.0
ndx["Florence"] <- 2.5; ndy["Florence"] <- -3.0; ndx["Hanna"] <- -5.0
nx <- c(unname(ndx), rep(0, nrow(gv))); ny <- c(unname(ndy), rep(0, nrow(gv)))

p <- ggplot() +
  geom_sf(data = land, fill = "grey95", color = "grey80", linewidth = 0.2) +
  geom_sf(data = states, fill = NA, color = "grey85", linewidth = 0.15) +
  geom_sf(data = trk, aes(color = Year), linewidth = 0.7) +
  geom_point(data = anchor, aes(lon, lat, color = Year), size = 1.6) +
  ggrepel::geom_text_repel(data = repel_df, aes(lon, lat, label = Name, color = Year),
                           nudge_x = nx, nudge_y = ny, size = 3, fontface = "bold", seed = 1,
                           box.padding = 0.7, point.padding = 0.25, force = 2, force_pull = 0.6,
                           segment.size = 0.3, segment.color = "grey55",
                           min.segment.length = 0, max.overlaps = Inf, show.legend = FALSE) +
  coord_sf(xlim = c(-104, -58), ylim = c(12, 47), expand = FALSE) +
  scale_color_brewer(palette = "Dark2") + labs(color = NULL, x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_line(color = "grey92", linewidth = 0.2), legend.position = "right")
ggsave(here("output/denial_tracks_map.png"), p, width = 8.5, height = 5.4, dpi = 300)
cat("Wrote output/{summary_dmg,data_denial}.tex + denial_tracks_map.png\n")
