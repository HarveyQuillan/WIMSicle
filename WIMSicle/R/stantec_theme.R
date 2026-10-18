library(ggplot2)

# Stantec Palette ---------------------------------------------------------

stantec_cols <- c(
  "orange" = "#ED6631",
  "black" = "#000000",
  "white" = "#FFFFFF",
  "fog" = "#F2EFEC",
  "pebble" = "#E2DDDB",
  "fossil" = "#C9C4BD",
  "fern" = "#1A845C",
  "marine" = "#2A73D9",
  "turmeric" = "#F3BE0A",
  "salt" = "#F4B5B5"
)

# Theme -------------------------------------------------------------------

theme_stantec <- function(base_size = 12,
                          base_family = "sans") {
  
  theme_minimal(
    base_size = base_size,
    base_family = base_family
  ) +
    
    theme(
      
      # Backgrounds
      plot.background = element_rect(
        fill = stantec_cols["fog"],
        colour = NA
      ),
      
      panel.background = element_rect(
        fill = "white",
        colour = NA
      ),
      
      # Grid
      panel.grid.major = element_line(
        colour = stantec_cols["pebble"],
        linewidth = 0.3
      ),
      
      panel.grid.minor = element_blank(),
      
      # Axes
      axis.line = element_line(
        colour = stantec_cols["black"],
        linewidth = 0.4
      ),
      
      axis.text = element_text(
        colour = stantec_cols["black"]
      ),
      
      axis.title = element_text(
        colour = stantec_cols["black"],
        face = "bold"
      ),
      
      # Titles
      plot.title = element_text(
        face = "bold",
        colour = stantec_cols["black"],
        size = rel(1.4),
        margin = margin(b = 10)
      ),
      
      plot.subtitle = element_text(
        colour = stantec_cols["black"],
        margin = margin(b = 10)
      ),
      
      plot.caption = element_text(
        colour = stantec_cols["fossil"],
        size = rel(0.85)
      ),
      
      # Legend
      legend.background = element_blank(),
      legend.key = element_blank(),
      
      legend.title = element_text(
        face = "bold",
        colour = stantec_cols["black"]
      ),
      
      legend.text = element_text(
        colour = stantec_cols["black"]
      ),
      
      # Facets
      strip.background = element_rect(
        fill = stantec_cols["orange"],
        colour = NA
      ),
      
      strip.text = element_text(
        colour = "white",
        face = "bold"
      ),
      
      # Margins
      plot.margin = margin(
        t = 15,
        r = 15,
        b = 15,
        l = 15
      )
    )
}

# Colour scales -----------------------------------------------------------

scale_colour_stantec <- function(...) {
  scale_colour_manual(
    values = c(
      "#ED6631", # orange
      "#1A845C", # fern
      "#2A73D9", # marine
      "#F3BE0A", # turmeric
      "#F4B5B5"  # salt
    ),
    ...
  )
}

scale_fill_stantec <- function(...) {
  scale_fill_manual(
    values = c(
      "#ED6631", # orange
      "#1A845C", # fern
      "#2A73D9", # marine
      "#F3BE0A", # turmeric
      "#F4B5B5"  # salt
    ),
    ...
  )
}

ggplot(mtcars,
       aes(wt, mpg, colour = factor(cyl))) +
  geom_point(size = 3) +
  scale_colour_stantec() +
  labs(
    title = "Vehicle Fuel Efficiency",
    subtitle = "Example Stantec Theme",
    x = "Weight",
    y = "Miles per Gallon"
  ) +
  theme_stantec()
