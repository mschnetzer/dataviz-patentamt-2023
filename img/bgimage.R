librarian::shelf(tidyverse, janitor, ggstream, ggtext, colorspace, MetBrewer, msthemes)

raw <- readxl::read_xlsx("energymix.xlsx",
                         col_types = c("numeric","text","numeric"))

translate <- tribble(~Art, ~Type,
                     "Kohle", "Coal",
                     "Erdöl", "Oil",
                     "Erdgas", "Natural gas",
                     "Biogene Brennstoffe", "Biogenic fuels",
                     "Sonstige Brennstoffe", "Other fuels",
                     "Speicherkraftwerke", "Storage power plant",
                     "Laufkraftwerke", "Flow power plant",
                     "Wind", "Wind",
                     "Photovoltaik", "Photovoltaics",
                     "Geothermie", "Geothermics"
)


plotdat <- raw |>
  rename(Art = `Group: Laufkraftwerke - Speicherkraft...`) |> 
  left_join(translate) |>
  mutate(Jahr = ymd(Jahr, truncated = 2L),
         Type = factor(Type, levels = rev(translate$Type)))

pal <- met.brewer("Lakota")[c(2,1,5,3)]
cols <- c(rbind(lighten(pal, 0.1), darken(pal, 0.2)))

plotdat |> filter(!Art %in% c("Sonstige Brennstoffe","Geothermie")) |> 
  ggplot(aes(x = Jahr, y = Value, group = Art, fill = Art)) +
  geom_stream(type = "mirror") +
  geom_hline(yintercept = 0, color = "white", linewidth = 0.1) +
  scale_fill_manual(values = cols, name = "", 
                    guide = guide_legend(keywidth = 0.5, keyheight = 0.3, 
                    label.theme = element_text(size = 5, family = "Raleway"))) +
  scale_x_date(position = "bottom") +
  labs(x=NULL, y=NULL) +
  coord_cartesian(expand = F) +
  theme_ms() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 6),
        legend.text = element_text(size = 7),
        legend.position = c(0.1, 0.2),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill='transparent', color = NA),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent',color = NA))

ggsave("bgimage2.png", dpi = 320, width = 8, height = 2, bg = "transparent")
