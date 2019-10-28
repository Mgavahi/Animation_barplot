## VNL 2019
# Import required library
library(tidyverse)
library(gganimate)
library(magick)
library(ggridges)
library(ggplot2)
library(grid)
library(png)
theme_set(theme_void()) ## set theme

# Import dataset
vnl <- read_csv("~path/vnl2019.csv")

# Preparing data    
vnl <- vnl %>% group_by(game) %>%
    mutate(
        rank = min_rank(-win),
        Value_rel = win / win[rank == 1],
        Value_lbl = paste0(" ", win)
    ) %>%
    ungroup() %>%
    group_by(team) %>%
    arrange(game) %>%
    mutate(prev.rank = lag(rank)) %>%
    ungroup() %>%
    group_by(game) %>%
    arrange(rank, prev.rank) %>%
    mutate(x = seq(1, n())) %>%
    ungroup()

# read picture's for adding to plot
img <- png::readPNG("~path/lgo_fivb_small.png")
g_pic <- rasterGrob(img,
                    interpolate = TRUE)
img2 <- png::readPNG("~path/vnl.png")
vnl_pic <- rasterGrob(img2,
                      interpolate = TRUE)

# visualizing and animate the data 
p_vnl <- ggplot(data = vnl,
                aes(
                    x = x,
                    y = win,
                    fill = team,
                    color = team
                )) +
    geom_col() +
    geom_text(aes(y = 0,
                  label = team),
              hjust = 1.1) +
    geom_text(aes(label = Value_lbl),
              hjust = 0) +
    coord_flip(clip = "off",
               expand = FALSE) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_reverse() +
    guides(color = FALSE,
           fill = FALSE) +
    labs(
        title = paste0(
            "_______________________________",
            "\n",
            "GAME : {closest_state}"
        ),
        x = "",
        y = "Win Game's"
    ) +
    theme(
        plot.title = element_text(
            hjust = 0,
            size = 20,
            color = "red",
            face = "bold"
        ),
        plot.subtitle = element_text(
            hjust = 0,
            size = 14,
            color = "blue",
            face = "italic"
        ),
        plot.caption = element_text(hjust = 1,
                                    size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(),
        plot.margin = margin(1, 1, 1, 4, "cm")
    ) +
    labs(
        subtitle = paste0(
            "FIVB Volleyball Men's Nations League 2019",
            "\n",
            "___________________________________________"
        ),
        caption = paste0(
            "________________________________________________",
            "\n",
            "رده بندی بر اساس تعداد برد تیم ها می باشد."
        )
    ) +
    transition_states(game,
                      transition_length = 4,
                      state_length = 1) +
    ease_aes('cubic-in-out') +
    annotation_custom(
        g_pic,
        xmin = 0.2,
        xmax = 1.2,
        ymin = 12,
        ymax = 14
    ) +
    annotation_custom(
        vnl_pic,
        xmin = -8,
        xmax = -18,
        ymin = 7,
        ymax = 16
    )

# animation plot
anim <- animate(p_vnl,
                nframes = 100,
                fps = 5,
                end_pause = 10)

    