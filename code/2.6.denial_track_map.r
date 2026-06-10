# ==============================================================================
# 2.6 Figure 1: tracks of the ten 2017-2020 denial-sample storms (observed BEST
# tracks from data/clean/track_actual.rds) over the US, Gulf, and Caribbean.
# Writes output/denial_tracks_map.png (promote to the manuscript when ready).
# ==============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf, ggplot2, ggrepel, rnaturalearth, dplyr, here)
sf_use_s2(FALSE)

den <- c("harvey","irma","maria","florence","michael","dorian","laura","sally","isaias","hanna")
yr  <- c(harvey=2017, irma=2017, maria=2017, florence=2018, michael=2018,
         dorian=2019, laura=2020, sally=2020, isaias=2020, hanna=2020)

trk <- readRDS(here("data/clean/track_actual.rds")) |>
  filter(type == "BEST", hurricane %in% den)
if (is.na(sf::st_crs(trk))) sf::st_crs(trk) <- 4326

# keep the most complete BEST linestring per storm
trk$np <- vapply(st_geometry(trk), function(g) nrow(st_coordinates(g)), integer(1))
trk <- trk |> group_by(hurricane) |> slice_max(np, n = 1, with_ties = FALSE) |> ungroup()
trk$Name <- tools::toTitleCase(trk$hurricane)
trk$Year <- factor(yr[trk$hurricane])

land   <- ne_countries(scale = "medium", returnclass = "sf")
states <- ne_states(country = "United States of America", returnclass = "sf")

# label/dot anchor = each track's landfall point: Maria at its Puerto Rico
# landfall, the rest at the vertex nearest the continental US.
us    <- st_union(st_geometry(states))
pr_pt <- st_sfc(st_point(c(-66.5, 18.2)), crs = 4326)
anchor <- do.call(rbind, lapply(seq_len(nrow(trk)), function(i) {
  pts <- st_cast(st_geometry(trk)[i], "POINT")
  tgt <- if (trk$hurricane[i] == "maria") pr_pt else us
  p   <- pts[which.min(st_distance(pts, tgt))]
  data.frame(Name = trk$Name[i], Year = trk$Year[i],
             lon = st_coordinates(p)[1], lat = st_coordinates(p)[2])
}))

# ghost points sampled along every track so labels also repel off the lines
gv <- as.data.frame(st_coordinates(trk))
gv <- gv[seq(1, nrow(gv), by = 6), c("X", "Y")]
repel_df <- rbind(anchor,
                  data.frame(Name = "", Year = anchor$Year[1], lon = gv$X, lat = gv$Y))

# manual directional nudges (degrees) for a few crowded labels
ndx <- setNames(rep(0, nrow(anchor)), anchor$Name); ndy <- ndx
ndy["Laura"]    <-  2.5
ndy["Irma"]     <- -1.8
ndy["Isaias"]   <-  3.0
ndx["Dorian"]   <-  2.0; ndy["Dorian"]   <- -2.0
ndx["Florence"] <-  2.5; ndy["Florence"] <- -3.0
ndx["Hanna"]    <- -5.0
nx <- c(unname(ndx), rep(0, nrow(gv)))
ny <- c(unname(ndy), rep(0, nrow(gv)))

p <- ggplot() +
  geom_sf(data = land,   fill = "grey95", color = "grey80", linewidth = 0.2) +
  geom_sf(data = states, fill = NA, color = "grey85", linewidth = 0.15) +
  geom_sf(data = trk, aes(color = Year), linewidth = 0.7) +
  geom_point(data = anchor, aes(lon, lat, color = Year), size = 1.6) +
  ggrepel::geom_text_repel(data = repel_df, aes(lon, lat, label = Name, color = Year),
                           nudge_x = nx, nudge_y = ny,
                           size = 3, fontface = "bold", seed = 1,
                           box.padding = 0.7, point.padding = 0.25,
                           force = 2, force_pull = 0.6,
                           segment.size = 0.3, segment.color = "grey55",
                           min.segment.length = 0, max.overlaps = Inf,
                           show.legend = FALSE) +
  coord_sf(xlim = c(-104, -58), ylim = c(12, 47), expand = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  labs(color = NULL, x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_line(color = "grey92", linewidth = 0.2),
        legend.position = "right")

dir.create(here("output"), showWarnings = FALSE)
ggsave(here("output/denial_tracks_map.png"), p, width = 8.5, height = 5.4, dpi = 300)
cat("Wrote output/denial_tracks_map.png\n")
