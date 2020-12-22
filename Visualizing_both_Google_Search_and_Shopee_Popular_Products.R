# IMPORT PACKAGES
library(gtrendsR) # getting data from Google Trends
library(tidyverse) # necessary R package
library(maps) # for maps
library(gridExtra) # for arranging the graphs in grids

# IMPORT DATA----
milo_search <- gtrends("milo", time = "2020-01-05 2020-12-13")
bbt_search <- gtrends(c("bubble tea", "bbt", "koi", "liho", "gong cha"), time = "2020-01-05 2020-12-13")
body_lotion_search <- gtrends("body lotion", time = "2020-01-05 2020-12-13")
air_fryer_search <- gtrends(c("philips airfryer", "ninja airfryer", "cuisinart airfryer", "nuwave airfryer", "tefal airfryer"), time = "2020-01-05 2020-12-13")
bluetooth_earphones_search <- gtrends(c("bluetooth earphones", "airpods", "galaxy buds", "wireless earbuds", "bluetooth headset"), time = "2020-01-05 2020-12-13")
powdered_milk_search <- gtrends(c("powdered milk", "nespray", "nestle milk powder", "promex milk powder"), time = "2020-01-05 2020-12-13")
nintendo_switch_search <- gtrends(c("nintendo switch", "animal crossing", "mario kart 8 deluxe", "dragon quest builders 2"), time = "2020-01-05 2020-12-13")

# TIDY AND TRANSFORM----
# handle the map of showing Southeast Asia's countries and Taiwan (based on the Shopee's infographic)
asia <- map_data("world")
asia <- asia %>%
  filter(region %in% c("Singapore", "Malaysia", "Philippines", "Taiwan", "Indonesia", "Vietnam", "Thailand"))
singapore <- map_data("world") # Singapore is too small to show on the Asia map, hence there is a need to create a standalone Singapore map (even though Singapore is included in the Asia map)
singapore <- singapore %>%
  filter(region == "Singapore")

# create the dataframes of the searches
milo_search_df <- milo_search$interest_by_country %>%
  filter(location %in% asia$region, hits > 0) %>%
  mutate(region = location, hits = as.numeric(hits)) %>%
  select(region, hits)

bbt_search_df <- bbt_search$interest_by_country %>%
  filter(location %in% asia$region, hits > 0) %>%
  mutate(region = location, hits = as.numeric(hits)) %>%
  select(region, hits)

body_lotion_search_df <- body_lotion_search$interest_by_country %>%
  filter(location %in% asia$region, hits > 0) %>%
  mutate(region = location, hits = as.numeric(hits)) %>%
  select(region, hits)

air_fryer_search_df <- air_fryer_search$interest_by_country %>%
  filter(location %in% asia$region, hits > 0) %>%
  mutate(region = location, hits = as.numeric(hits)) %>%
  select(region, hits)

bluetooth_earphones_search_df <- bluetooth_earphones_search$interest_by_country %>%
  filter(location %in% asia$region, hits > 0) %>%
  mutate(region = location, hits = as.numeric(hits)) %>%
  select(region, hits)

powdered_milk_search_df <- powdered_milk_search$interest_by_country %>%
  filter(location %in% asia$region, hits > 0) %>%
  mutate(region = location, hits = as.numeric(hits)) %>%
  select(region, hits)

nintendo_switch_search_df <- nintendo_switch_search$interest_by_country %>%
  filter(location %in% asia$region, hits > 0) %>%
  mutate(region = location, hits = as.numeric(hits)) %>%
  select(region, hits)

# VISUALIZATION----
milo_asia_map <- ggplot() +
  geom_map(data = asia,
           map = asia,
           aes(x = long, y = lat, map_id = region),
           fill="white", color="white", size=0.15) +
  geom_map(data = milo_search_df,
           map = asia,
           aes(fill = hits, map_id = region),
           color="white", size=0.15) +
  scale_fill_continuous(low = "grey", high = "green") +
  labs(title = "Google search interest for Milo in Southeast Asia and Taiwan regions",
       caption = "Source: Google Trends") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

milo_singapore_map <- ggplot() +
  geom_map(data = singapore,
           map = singapore,
           aes(x = long, y = lat, map_id = region),
           fill="white", color="white", size=0.15) +
  geom_map(data = milo_search_df,
           map = singapore,
           aes(fill = hits, map_id = region),
           color="white", size=0.15) +
  scale_fill_continuous(low = "grey", high = "green") +
  labs(title = "Google search interest for Milo in Singapore",
       caption = "Source: Google Trends") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

bbt_asia_map <- ggplot() +
  geom_map(data = asia,
           map = asia,
           aes(x = long, y = lat, map_id = region),
           fill="white", color="white", size=0.15) +
  geom_map(data = bbt_search_df,
           map = asia,
           aes(fill = hits, map_id = region),
           color="white", size=0.15) +
  scale_fill_continuous(low = "grey", high = "saddlebrown") +
  labs(title = "Google search interest for Bubble Tea in Southeast Asia and Taiwan regions",
       caption = "Source: Google Trends") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

bbt_singapore_map <- ggplot() +
  geom_map(data = singapore,
           map = singapore,
           aes(x = long, y = lat, map_id = region),
           fill="white", color="white", size=0.15) +
  geom_map(data = bbt_search_df,
           map = singapore,
           aes(fill = hits, map_id = region),
           color="white", size=0.15) +
  scale_fill_continuous(low = "grey", high = "saddlebrown") +
  labs(title = "Google search interest for Bubble Tea in Singapore",
       caption = "Source: Google Trends") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

body_lotion_asia_map <- ggplot() +
  geom_map(data = asia,
           map = asia,
           aes(x = long, y = lat, map_id = region),
           fill="white", color="white", size=0.15) +
  geom_map(data = body_lotion_search_df,
           map = asia,
           aes(fill = hits, map_id = region),
           color="white", size=0.15) +
  scale_fill_continuous(low = "grey", high = "orange") +
  labs(title = "Google search interest for Body Lotion in Southeast Asia and Taiwan regions",
       caption = "Source: Google Trends") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

body_lotion_singapore_map <- ggplot() +
  geom_map(data = singapore,
           map = singapore,
           aes(x = long, y = lat, map_id = region),
           fill="white", color="white", size=0.15) +
  geom_map(data = body_lotion_search_df,
           map = singapore,
           aes(fill = hits, map_id = region),
           color="white", size=0.15) +
  scale_fill_continuous(low = "grey", high = "orange") +
  labs(title = "Google search interest for Body Lotion in Singapore",
       caption = "Source: Google Trends") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

air_fryer_asia_map <- ggplot() +
  geom_map(data = asia,
           map = asia,
           aes(x = long, y = lat, map_id = region),
           fill="white", color="white", size=0.15) +
  geom_map(data = air_fryer_search_df,
           map = asia,
           aes(fill = hits, map_id = region),
           color="white", size=0.15) +
  scale_fill_continuous(low = "grey", high = "firebrick") +
  labs(title = "Google search interest for Air Fryer in Southeast Asia and Taiwan regions",
       caption = "Source: Google Trends") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

air_fryer_singapore_map <- ggplot() +
  geom_map(data = singapore,
           map = singapore,
           aes(x = long, y = lat, map_id = region),
           fill="white", color="white", size=0.15) +
  geom_map(data = air_fryer_search_df,
           map = singapore,
           aes(fill = hits, map_id = region),
           color="white", size=0.15) +
  scale_fill_continuous(low = "grey", high = "firebrick") +
  labs(title = "Google search interest for Air Fryer in Singapore",
       caption = "Source: Google Trends") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

bluetooth_earphones_asia_map <- ggplot() +
  geom_map(data = asia,
           map = asia,
           aes(x = long, y = lat, map_id = region),
           fill="white", color="white", size=0.15) +
  geom_map(data = bluetooth_earphones_search_df,
           map = asia,
           aes(fill = hits, map_id = region),
           color="white", size=0.15) +
  scale_fill_continuous(low = "grey", high = "midnightblue") +
  labs(title = "Google search interest for Bluetooth Earphones\nin Southeast Asia and Taiwan regions",
       caption = "Source: Google Trends") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

bluetooth_earphones_singapore_map <- ggplot() +
  geom_map(data = singapore,
           map = singapore,
           aes(x = long, y = lat, map_id = region),
           fill="white", color="white", size=0.15) +
  geom_map(data = bluetooth_earphones_search_df,
           map = singapore,
           aes(fill = hits, map_id = region),
           color="white", size=0.15) +
  scale_fill_continuous(low = "grey", high = "midnightblue") +
  labs(title = "Google search interest for Bluetooth Earphones\nin Singapore",
       caption = "Source: Google Trends") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

powdered_milk_asia_map <- ggplot() +
  geom_map(data = asia,
           map = asia,
           aes(x = long, y = lat, map_id = region),
           fill="white", color="white", size=0.15) +
  geom_map(data = powdered_milk_search_df,
           map = asia,
           aes(fill = hits, map_id = region),
           color="white", size=0.15) +
  scale_fill_continuous(low = "grey", high = "darkgoldenrod1") +
  labs(title = "Google search interest for Powdered Milk in Southeast Asia and Taiwan regions",
       caption = "Source: Google Trends") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

powdered_milk_singapore_map <- ggplot() +
  geom_map(data = singapore,
           map = singapore,
           aes(x = long, y = lat, map_id = region),
           fill="white", color="white", size=0.15) +
  geom_map(data = powdered_milk_search_df,
           map = singapore,
           aes(fill = hits, map_id = region),
           color="white", size=0.15) +
  scale_fill_continuous(low = "grey", high = "darkgoldenrod1") +
  labs(title = "Google search interest for Powdered Milk in Singapore",
       caption = "Source: Google Trends") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

nintendo_switch_asia_map <- ggplot() +
  geom_map(data = asia,
           map = asia,
           aes(x = long, y = lat, map_id = region),
           fill="white", color="white", size=0.15) +
  geom_map(data = nintendo_switch_search_df,
           map = asia,
           aes(fill = hits, map_id = region),
           color="white", size=0.15) +
  scale_fill_continuous(low = "grey", high = "cyan") +
  labs(title = "Google search interest for Nintendo Switch\nin Southeast Asia and Taiwan regions",
       caption = "Source: Google Trends") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

nintendo_switch_singapore_map <- ggplot() +
  geom_map(data = singapore,
           map = singapore,
           aes(x = long, y = lat, map_id = region),
           fill="white", color="white", size=0.15) +
  geom_map(data = nintendo_switch_search_df,
           map = singapore,
           aes(fill = hits, map_id = region),
           color="white", size=0.15) +
  scale_fill_continuous(low = "grey", high = "cyan") +
  labs(title = "Google search interest for Nintendo Switch\nin Singapore",
       caption = "Source: Google Trends") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

# arrange the maps in Asia and Singapore map in a comparison view
grid.arrange(milo_asia_map, milo_singapore_map, ncol = 2)
grid.arrange(bbt_asia_map, bbt_singapore_map, ncol = 2)
grid.arrange(body_lotion_asia_map, body_lotion_singapore_map, ncol = 2)
grid.arrange(powdered_milk_asia_map, powdered_milk_singapore_map, ncol = 2)
grid.arrange(bluetooth_earphones_asia_map, bluetooth_earphones_singapore_map, ncol = 2)
grid.arrange(nintendo_switch_asia_map, nintendo_switch_singapore_map, ncol = 2)
grid.arrange(air_fryer_asia_map, air_fryer_singapore_map, ncol = 2)