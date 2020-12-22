# IMPORT PACKAGES
library(gtrendsR) # getting data from Google Trends
library(tidyverse) # necessary R package
library(lubridate) # for data manipulation of dates
library(plotly) # for interactive chart

# IMPORT DATA----
sg_bbt <- gtrends(keyword = c("koi", "liho", "gong cha", "chi cha", "r&b tea"), # insert the key search terms (max = 5 keywords)
                         geo = "SG", # based on which country for the Google's search terms, i.e. Singapore
                         gprop = "web", # based on Google Search Engine
                         time = "2020-01-05 2020-12-13") # select the date range: the start date and end date can only start from 5 Jan to 13 Dec

# TIDY----
sg_bbt_df <- sg_bbt[[1]] #sg_bbt is a list not a df, we will be taking the interest_over_time as our scope of interest for the df
glimpse(sg_bbt_df)
sg_bbt_df$hits <- as.numeric(sg_bbt_df$hits) # make sure the hits is in numeric instead of character

# TRANSFORM----
sg_bbt_df <- sg_bbt_df %>%
              mutate(year = year(date),
                     month = month(date),
                     new_date = date(date))
glimpse(sg_bbt_df) # create new columns to retrieve the year, month, and date (instead of datetime) formats for visualizations

# VISUALIZATION----
bbt_5_sparklines <- sg_bbt_df %>% # data
  ggplot(aes(x = new_date, y = hits) ) +  # aesthetics
  facet_grid(rows = vars(keyword)) + # facets
  geom_area(aes(fill=keyword), alpha=0.4) + # geometrics
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + # ticks
  labs(title = "Singapore's Google Year in Search 2020 for the 5 Bubble Tea Brands", # labels
       caption = "Source: Google Trends") + # labels
  ggthemes::theme_fivethirtyeight() + # themes
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position="none")

bbt_5_sparklines_plotly <- ggplotly(bbt_5_sparklines) # show the chart in non-static way using plotly

bbt_5_viz <- sg_bbt_df %>% # data
                ggplot(aes(x = new_date, y = hits, group = keyword)) + # aesthetics
                geom_point(aes(color = keyword, shape = keyword)) + # geometrics
                geom_smooth(method = "loess", formula = y ~ x, level = 0.95, aes(color=keyword)) + # regression
                scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + # ticks
                geom_smooth(method = "loess", formula = y ~ x, level = 0.99, aes(color=keyword)) +
                labs(x = "Month", y = "Search Volumes",
                     title = "Top 5 Bubble Tea Brands on Google Search Volume (SG) in 2020",
                     subtitle = "The Comparison of the Bubble Tea Brands by Month",
                     caption = "Source: Google Trends") + # labels
                theme_linedraw() # themes