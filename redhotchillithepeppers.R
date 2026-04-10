library(tidyverse)

# -------------------------
# Members data
# -------------------------

members <- tribble(
  ~member, ~instrument, ~start, ~end,
  
  "Anthony Kiedis", "Vocal", 1982, 2008.5,
  "Anthony Kiedis", "Vocal", 2009.8, 2026,
  "Flea", "Bass", 1982, 2008.5,
  "Flea", "Bass", 2009.8, 2026,
  
  "Cliff Martinez", "Drums", 1983.10, 1986,
  "Jack Irons", "Drums", 1983.1, 1983.10,
  "Jack Irons", "Drums", 1986, 1988.6,
  "D. H. Peligro", "Drums", 1988.6, 1988.9,
  "Chad Smith", "Drums", 1988.9, 2008.5,
  "Chad Smith", "Drums", 2009.8, 2026,
  
  "Jack Sherman", "Guitar", 1983.9, 1985.1,
  "Hillel Slovak", "Guitar", 1983.1, 1983.9,
  "Hillel Slovak", "Guitar", 1985.1, 1988.6,
  
  "John Frusciante", "Guitar", 1988.6, 1992,
  "John Frusciante", "Guitar", 1998.4, 2008.5,
  "John Frusciante", "Guitar", 2019.9, 2026,
  
  "Dave Navarro", "Guitar", 1993.7, 1998.4,
  "Josh Klinghoffer", "Guitar", 2009.8, 2019.9
)

# -------------------------
# Highlight core lineup
# -------------------------

core_lineup <- c(
  "Anthony Kiedis",
  "Flea",
  "John Frusciante",
  "Chad Smith",
  "Hillel Slovak"
)

members <- members %>%
  mutate(
    core_member = member %in% core_lineup
  )

# -------------------------
# Manual ordering of members
# -------------------------

instrument_order <- c(
  "Vocal",
  "Bass",
  "Drums",
  "Guitar"
)

order_members <- c(
  
  "Anthony Kiedis",
  "Flea",
  
  "Cliff Martinez",
  "Jack Irons",
  "D. H. Peligro",
  "Chad Smith",
  
  "Jack Sherman",
  "Hillel Slovak",
  
  "John Frusciante",
  "Dave Navarro",
  "Josh Klinghoffer"
)

members <- members %>%
  mutate(
    instrument = factor(
      instrument,
      levels = instrument_order
    ),
    member = factor(
      member,
      levels = order_members
    )
  )

# -------------------------
# Band hiatus
# -------------------------

hiatus <- tribble(
  ~start, ~end,
  2008.5, 2009.8
)

# -------------------------
# Álbums
# -------------------------

albums <- tribble(
  ~album, ~year,
  
  "The Red Hot Chili Peppers", 1984.1,
  "Freaky Styley", 1985.9,
  "The Uplift Mofo Party Plan", 1987.9,
  "Mother's Milk", 1989.9,
  "Blood Sugar Sex Magik", 1991.9,
  "One Hot Minute", 1995,
  "Californication", 1999,
  "By the Way", 2002,
  "Stadium Arcadium", 2006,
  "I'm with You", 2011,
  "The Getaway", 2016,
  "Unlimited Love", 2022,
  "Return of the Dream Canteen", 2022.9
)

# -------------------------
# Historical events
# -------------------------

events <- tribble(
  ~event, ~year,
  
  "Death of Hillel Slovak", 1988.6
)

# -------------------------
# Row reserved for albums
# -------------------------

album_y <- "Albums"

members_levels <- c(
  order_members,
  album_y
)

members$member <- factor(
  members$member,
  levels = members_levels
)

# -------------------------
# Plot
# -------------------------

ggplot() +
  
  geom_rect(
    data = hiatus,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "grey70",
    alpha = 0.4
  ) +
  
  geom_vline(
    data = events,
    aes(
      xintercept = year
    ),
    linetype = "dashed",
    linewidth = 0.6,
    alpha = 0.8
  ) +
  
  geom_text(
    data = events,
    aes(
      x = year,
      y = length(members_levels) - 6.5,
      label = event
    ),
    angle = 90,
    vjust = 1.2,
    size = 2.5,
    check_overlap = TRUE
  ) +
  
  geom_segment(
    data = members,
    aes(
      x = start,
      xend = end,
      y = member,
      yend = member,
      color = instrument,
      linewidth = core_member,
      alpha = core_member
    )
  ) +
  
  scale_linewidth_manual(
    values = c(
      "TRUE" = 3,
      "FALSE" = 1.2
    ),
    guide = "none"
  ) +
  
  scale_alpha_manual(
    values = c(
      "TRUE" = 1,
      "FALSE" = 0.5
    ),
    guide = "none"
  ) +
  
  geom_point(
    data = albums,
    aes(
      x = year,
      y = album_y
      
    ),
    size = 3,
    color = "darkred"
  ) +
  
  geom_label(
    data = albums,
    aes(
      x = year,
      y = album_y,
      label = album
    ),
    fill = "transparent",
    color = "darkred",
    label.size = 0,
    angle = 25,
    vjust = -0.5,
    hjust = 0,
    size = 2.2,
    label.padding = unit(0.25, "lines"),
    label.r = unit(0.2, "lines")
  
  ) +
  
  scale_y_discrete(
    limits = rev(members_levels),
    labels = function(x) ifelse(x == "Albums", "", x)
  ) +
  
  labs(
    title = "Timeline of Red Hot Chili Peppers Members, Albums, and Historical Events",
    subtitle = "Core lineup highlighted; grey area indicates band hiatus (May 2008 – October 2009)",
    x = "Year",
    y = NULL,
    color = "Instrument"
  ) +
  
  guides(
    color = guide_legend(
      override.aes = list(
        linewidth = 3
      )
    )
  ) +
  
  coord_cartesian(clip = "off")+
  theme_minimal(
    base_size = 12
  ) +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.line.x = element_line(
      color = "gray70",
      linewidth = 0.6
    ),
    axis.line.y = element_line(
      color = "gray70",
      linewidth = 0.6
    ),
    
    axis.ticks = element_line(
      color = "gray70"
    ),
    
    legend.key.width  = unit(1, "cm"),
    legend.key.height = unit(.6, "cm")
  )

ggsave(
  "rhcp_timeline.png",
  width = 12,
  height = 6,
  dpi = 300,
  bg = "white"
)

library(magick)

# Call back the plot
plot <- image_read("rhcp_timeline.png")

plot2 <- image_annotate(
  plot,
  "",
  color = "black",
  size = 25,
  location = "10+50",
  gravity = "north"
)
plot3 <- image_annotate(
  plot2,
  "Data: Wikipedia
Visualization by @fblpalmeira",
  color = "gray",
  size = 50,
  location = "0+20",
  gravity = "southeast"
)

# -------------------------
# Band logo — top-right corner
# -------------------------

logo <- image_read("red-hot.jpg")

plot4 <- image_composite(
  plot3,
  image_scale(logo, "x260"),
  gravity = "northeast",
  offset = "+40+40"
)

# -------------------------
# Anthony photo
# -------------------------

logo2 <- image_read("Anthony_Kiedis.png")

plot5 <- image_composite(
  plot4,
  image_scale(logo2, "x130"),
  gravity = "northwest",
  offset = "+350+160"
)

# -------------------------
# Flea photo
# -------------------------

logo3 <- image_read("Flea.png")

plot6 <- image_composite(
  plot5,
  image_scale(logo3, "x120"),
  gravity = "northwest",
  offset = "+350+300"
)

# -------------------------
# Cliff photo
# -------------------------

logo4 <- image_read("Cliff_Martinez.png")

plot7 <- image_composite(
  plot6,
  image_scale(logo4, "x130"),
  gravity = "northwest",
  offset = "+350+420"
)

# -------------------------
# Jack Irons photo
# -------------------------

logo5 <- image_read("Jack_Irons.png")

plot8 <- image_composite(
  plot7,
  image_scale(logo5, "x120"),
  gravity = "northwest",
  offset = "+350+550"
)

# -------------------------
# D. H. Peligro photo
# -------------------------

logo6 <- image_read("DH_Peligro.png")

plot9 <- image_composite(
  plot8,
  image_scale(logo6, "x120"),
  gravity = "northwest",
  offset = "+360+670"
)

# -------------------------
# Chad Smith photo
# -------------------------

logo7 <- image_read("Chad_Smith.png")

plot10 <- image_composite(
  plot9,
  image_scale(logo7, "x110"),
  gravity = "northwest",
  offset = "+350+800"
)

# -------------------------
# Jack Sherman photo
# -------------------------

logo8 <- image_read("Jack_Sherman.png")

plot11 <- image_composite(
  plot10,
  image_scale(logo8, "x120"),
  gravity = "northwest",
  offset = "+350+910"
)

# -------------------------
# Hillel Slovak photo
# -------------------------

logo9 <- image_read("Hillel_Slovak.png")

plot12 <- image_composite(
  plot11,
  image_scale(logo9, "x130"),
  gravity = "northwest",
  offset = "+350+1030"
)

# -------------------------
# John Frusciante photo
# -------------------------

logo10 <- image_read("John_Frusciante.png")

plot13 <- image_composite(
  plot12,
  image_scale(logo10, "x120"),
  gravity = "northwest",
  offset = "+350+1160"
)

# -------------------------
# Dave Navarro photo
# -------------------------

logo11 <- image_read("Dave_Navarro.png")

plot14 <- image_composite(
  plot13,
  image_scale(logo11, "x120"),
  gravity = "northwest",
  offset = "+350+1280"
)

# -------------------------
# Josh Klinghoffer photo
# -------------------------

logo12 <- image_read("Josh_Klinghoffer.png")

plot15 <- image_composite(
  plot14,
  image_scale(logo12, "x120"),
  gravity = "northwest",
  offset = "+350+1400"
)

image_browse(plot15)

# Save final figure
image_write(
  plot15,
  "rhcp_timeline2.png"
)