library(tidyverse); library(sf); library(tigris); library(here)
options(tigris_use_cache = TRUE, tigris_class = "sf")
options(theme_set(theme_minimal()))
# 1. Identify MSA geography
url <- "https://www.nber.org/cbsa-msa-fips-ssa-county-crosswalk/cbsatocountycrosswalk.csv"
temp <- tempfile()
download.file(url, temp)
xwalk <- read_csv(temp) %>%
  select(fipscounty, state, msa, msaname, y2017) %>%
  drop_na(y2017) %>%
  filter(msaname == "WASHINGTON, DC-MD-VA-WV")
# 2. Download DC and region ODs (if not already downloaded)
if(!file.exists(here("process_data", "regional_commutes.csv"))){
  # OD in same state
  state <- c("dc", "md", "va", "wv")
  full_in <- NULL
  for (y in 2010:2015){
    for(s in state){
      url <- paste0("https://lehd.ces.census.gov/data/lodes/LODES7/",
                    s, "/od/", s, "_od_main_JT00_", y, ".csv.gz")
      temp <- tempfile()
      download.file(url, temp)
      in_state_jobs <- read_csv(gzfile(temp)) %>%
        mutate(w_cty = str_sub(w_geocode, 1, 5),
               h_cty = str_sub(h_geocode, 1, 5),
               w_geoid = str_sub(w_geocode, 1, 11),
               h_geoid = str_sub(h_geocode, 1, 11)) %>%
        mutate_at(vars("w_geocode"), as.character) %>%
        mutate_at(vars("h_geocode"), as.character) %>%
        filter(w_cty %in% xwalk$fipscounty & h_cty %in% xwalk$fipscounty)
      in_state_jobs$year <- y
      full_in <- rbind(full_in, in_state_jobs)
    }
  }
  # OD cross state boundaries
  full_out <- NULL
  for (y in 2010:2015){
    for(s in state){
      url <- paste0("https://lehd.ces.census.gov/data/lodes/LODES7/",
                    s, "/od/", s, "_od_aux_JT00_", y, ".csv.gz")
      temp <- tempfile()
      download.file(url, temp)
      out_of_state_jobs <- read_csv(gzfile(temp)) %>%
        mutate(w_cty = str_sub(w_geocode, 1, 5),
               h_cty = str_sub(h_geocode, 1, 5),
               w_geoid = str_sub(w_geocode, 1, 11),
               h_geoid = str_sub(h_geocode, 1, 11)) %>%
        mutate_at(vars("w_geocode"), as.character) %>%
        mutate_at(vars("h_geocode"), as.character) %>%
        filter(w_cty %in% xwalk$fipscounty & h_cty %in% xwalk$fipscounty)
      out_of_state_jobs$year <- y
      full_out <- rbind(full_out, out_of_state_jobs)
    }
  }
  # Merge into a single file and save so we don't have to DL again
  full <- bind_rows(full_in, full_out)
  write_csv(full, here("process_data", "regional_commutes.csv"))
} else {
  full <- read_csv(here("process_data", "regional_commutes.csv")) %>%
    mutate_at(vars(14:17), as.character)
}
# 3. Download DC and region tract shapefiles (if not already downloaded)
if(!file.exists(here("process_data", "region.shp"))){
  st <- str_sub(xwalk$fipscounty, 1, 2)
  cty <- str_sub(xwalk$fipscounty, 3, 5)
  region <- map2(st, cty, ~{tracts(state = .x,
                                      county = .y)}) %>%
    rbind_tigris() %>%
    st_transform(., 26918) %>%
    select(GEOID)
  st_write(region, here("process_data", "region.shp"))
} else {
  region <- st_read(here("process_data", "region.shp")) %>%
    mutate_if(is.factor, as.character)
}
# 4. Prep tract OD
# Collapse block OD into tracts
# `od_total` shows total no. tract-to-tract OD obs / year
# `od_mean` averages these out 2010-2015
od_total <- full %>%
  group_by(w_geoid, h_geoid, year) %>%
  summarize(y_tot = sum(S000),
            y_1_2 = sum(SE01) + sum(SE02),
            y_3 = sum(SE03))
od_mean <- od_total %>%
  group_by(w_geoid, h_geoid) %>%
  summarize(m_tot = mean(y_tot),
            m_1_2 = mean(y_1_2),
            m_3 = mean(y_3))
# Append commuting distance to OD
trct_cent <- st_centroid(region)
trct_dist <- st_distance(trct_cent)
trct_dist_df <- NULL
for(i in 1:nrow(region)){
  h_geoid <- trct_cent$GEOID
  w_geoid <- rep(trct_cent$GEOID[i], nrow(region))
  dist <- trct_dist[,i]
  trct_dist_df <- rbind(trct_dist_df, cbind(h_geoid, w_geoid, dist))
}
# Drop duplicates; convert distance to miles
trct_dist_df <- as_tibble(trct_dist_df) %>%
  distinct(.) %>%
  mutate_at(vars(dist), as.numeric) %>%
  mutate_at(vars(dist), funs(. * 0.000621371))
od_mean <- inner_join(od_mean, trct_dist_df,
                      by = c("w_geoid" = "w_geoid", "h_geoid" = "h_geoid"))
# 5. Regionwide weighted mean commuting distance
wm_overall <- sum(od_mean$m_tot * od_mean$dist) / sum(od_mean$m_tot)
wm_1_2 <- sum(od_mean$m_1_2 * od_mean$dist) / sum(od_mean$m_1_2)
wm_3 <- sum(od_mean$m_3 * od_mean$dist) / sum(od_mean$m_3)
# 6. Statewide mean commuting distance and median tract no. obs.
state_mean <- od_mean %>%
  mutate(st = str_sub(h_geoid, 1, 2)) %>%
  group_by(st) %>%
  summarize(wm_overall = sum(m_tot * dist) / sum(m_tot),
            wm_1_2 = sum(m_1_2 * dist) / sum(m_1_2),
            wm_3 = sum(m_3 * dist) / sum(m_3))
state_median_obs <- od_mean %>%
  mutate(st = str_sub(h_geoid, 1, 2)) %>%
  group_by(h_geoid) %>%
  summarize(st = min(st),
            tot = sum(m_tot),
            o_1_2 = sum(m_1_2),
            o_3 = sum(m_3)) %>%
  group_by(st) %>%
  summarize(m_obs_overall = median(tot),
            m_obs_1_2 = median(o_1_2),
            m_obs_3 = median(o_3))
state_mean <- left_join(state_mean, state_median_obs) %>%
  mutate_if(is.numeric, round, 3)
write_csv(state_mean, here("output_data", "commuters_st.csv"))
# 7. DC weighted mean commuting distance by ward
# Note crude assignment of tracts to wards:
# tracts assigned by max land area within ward
ward <- st_read("https://opendata.arcgis.com/datasets/0ef47379cbae44e88267c01eaec2ff6e_31.geojson") %>%
  select(WARD) %>%
  rename_all(tolower) %>%
  st_transform(., st_crs(region))
ward_trct <- ward %>%
  st_intersection(region %>% filter(str_sub(GEOID, 1, 2) == "11"), .) %>%
  mutate(land = st_area(.)) %>%
  group_by(GEOID) %>%
  arrange(-land) %>%
  slice(1) %>%
  st_set_geometry(NULL) %>%
  select(-land)
dc_mean <- od_mean %>%
  mutate(st = str_sub(h_geoid, 1, 2)) %>%
  filter(st == "11") %>%
  inner_join(., ward_trct, by = c("h_geoid" = "GEOID")) %>%
  group_by(ward) %>%
  summarize(wm_overall = sum(m_tot * dist) / sum(m_tot),
            wm_1_2 = sum(m_1_2 * dist) / sum(m_1_2),
            wm_3 = sum(m_3 * dist) / sum(m_3))
dc_median_obs <- od_mean %>%
  mutate(st = str_sub(h_geoid, 1, 2)) %>%
  filter(st == "11") %>%
  inner_join(., ward_trct, by = c("h_geoid" = "GEOID")) %>%
  group_by(h_geoid) %>%
  summarize(ward = min(ward),
            tot = sum(m_tot),
            o_1_2 = sum(m_1_2),
            o_3 = sum(m_3)) %>%
  group_by(ward) %>%
  summarize(m_obs_overall = median(tot),
            m_obs_1_2 = median(o_1_2),
            m_obs_3 = median(o_3))
dc_mean <- left_join(dc_mean, dc_median_obs) %>%
  mutate_if(is.numeric, round, 3)
write_csv(dc_mean, here("output_data", "commuters_ward.csv"))
# 8. Weighted mean commuting distance by tract
tract_mean <- od_mean %>%
  group_by(h_geoid) %>%
  summarize(wm_overall = sum(m_tot * dist) / sum(m_tot),
            wm_1_2 = sum(m_1_2 * dist) / sum(m_1_2),
            wm_3 = sum(m_3 * dist) / sum(m_3))
tract_tot_obs <- od_mean %>%
  group_by(h_geoid) %>%
  summarize(obs_overall = sum(m_tot),
            obs_1_2 = sum(m_1_2),
            obs_3 = sum(m_3))
tract_mean <- left_join(tract_mean, tract_tot_obs) %>%
  mutate_if(is.numeric, round, 3)
write_csv(tract_mean, here("output_data", "commuters_trct.csv"))
# 9. Map DC wards
ward <- left_join(ward, dc_mean)
ggplot(ward) +
  geom_sf(aes(fill = wm_overall), color = NA) +
  coord_sf(datum = NA) +
  ggtitle("Weighted mean commuting distance by ward") +
  labs(fill = "Miles")
ggsave(here("figures", "ward_overall.png"), width = 5, height = 7, units = "in")
ggplot(ward) +
  geom_sf(aes(fill = wm_1_2), color = NA) +
  coord_sf(datum = NA) +
  ggtitle("Weighted mean commuting distance by ward\nLow-income residents") +
  labs(fill = "Miles")
ggsave(here("figures", "ward_li.png"), width = 5, height = 7, units = "in")
ggplot(ward) +
  geom_sf(aes(fill = wm_3), color = NA) +
  coord_sf(datum = NA) +
  ggtitle("Weighted mean commuting distance by ward\nHigher-income residents") +
  labs(fill = "Miles")
ggsave(here("figures", "ward_hi.png"), width = 5, height = 7, units = "in")
# 10. Map region tracts
region <- left_join(region, tract_mean, by = c("GEOID" = "h_geoid"))
ggplot(region) +
  geom_sf(aes(fill = wm_overall), color = NA) +
  coord_sf(datum = NA) +
  ggtitle("Weighted mean commuting distance by census tract") +
  labs(fill = "Miles")
ggsave(here("figures", "region_overall.png"), width = 7, height = 7, units = "in")
ggplot(region) +
  geom_sf(aes(fill = wm_1_2), color = NA) +
  coord_sf(datum = NA) +
  ggtitle("Weighted mean commuting distance by census tract\nLow-income residents") +
  labs(fill = "Miles")
ggsave(here("figures", "region_li.png"), width = 7, height = 7, units = "in")
ggplot(region) +
  geom_sf(aes(fill = wm_3), color = NA) +
  coord_sf(datum = NA) +
  ggtitle("Weighted mean commuting distance by census tract\nHigher-income residents") +
  labs(fill = "Miles")
ggsave(here("figures", "region_hi.png"), width = 7, height = 7, units = "in")
