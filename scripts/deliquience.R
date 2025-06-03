pacman::p_load(tidyverse, readxl, sysfonts, showtext, ggh4x, scales)
font_add_google("Inter", "font")
showtext_auto()
showtext_opts(dpi = 400)

list.files("scripts/pks/", pattern = "^\\d{4}\\.xlsx$", full.names = TRUE) |>
  lapply(read_excel, skip = 7) |>
  lapply(function(x) x |> mutate(across(everything(), as.character))) |>
  setNames(2015:2018) |>
  bind_rows(.id = "year") |>
  select(-22) -> d1

nms <- c("year", "id", "name", "n_total", "pct_total", "n_versuch", "pct_versuch", "einw_20k", "einw_100k", "einw_500k", "einwohn_more_500k", "einwohn_unkown", "n_weapon_threat", "n_weapon_used", "n_solved", "pct_solved", "n_suspect_total", "n_suspect_men", "n_suspect_woman", "n_nongerman", "pct_nongerman")

colnames(d1) <- nms
  
d2 <- list.files("scripts/pks/", pattern = "BU-F-01-T01", full.names = TRUE) |>
  lapply(read_excel, skip = 7) |>
  lapply(function(x) x |> mutate(across(everything(), as.character))) |>
  setNames(2019:2024) |>
  bind_rows(.id = "year")

colnames(d2) <- nms

d <- bind_rows(d1, d2)

# Plot

d |>
 filter(id == "100000") |>
 select(year, n_total, pct_total, pct_solved, n_suspect_total, n_suspect_men, n_suspect_woman) |>
 mutate(across(everything(), as.numeric)) |>
 mutate(pct_men = 100 * n_suspect_men / n_suspect_total, pct_women = 100 * (1 - pct_men/100)) |>
 select(year, n_total, pct_total, pct_solved, pct_men) |>
 pivot_longer(-year, names_to = "var") |>
 mutate(var = factor(var, levels = c("n_total", "pct_total", "pct_solved", "pct_men"), labels = c("Anzahl erfasste Fälle", "Anteil an Gesamtfallzahl", "Aufklärungsquote", "Anteil männlicher Täter"))) |>
 ggplot(aes(x = year, y = value)) +
 facet_wrap(~var, scales = "free", ncol = 2) +
 geom_step(linewidth = 1, color = "#01364C") +
 ggh4x::facetted_pos_scales(
    y = list(
      "Anzahl erfasste Fälle" = scale_y_continuous(limits = c(0, 150000), breaks = breaks_pretty(n = 8),labels = label_number(scale_cut = cut_short_scale()), expand = expansion()),
      "Anteil an Gesamtfallzahl" = scale_y_continuous(limits = c(0, 3), labels = label_percent(scale = 1), expand = expansion()),
      "Aufklärungsquote" = scale_y_continuous(limits = c(50, 100), breaks = seq(0, 100, 10), labels = label_percent(scale = 1), expand = expansion()),
      "Anteil männlicher Täter" = scale_y_continuous(limits = c(50, 100), breaks = seq(0, 100, 10), labels = label_percent(scale = 1), expand = expansion())
    )
 ) +
 scale_x_continuous(
   name = "Jahr", 
   breaks = seq(2015, 2024, 2)
   ) +
 ylab(NULL) +
 ggdist::theme_ggdist() +
 theme(
  text = element_text(family = "font", size = 12),
  panel.grid.minor = element_line(color = "grey90", linewidth = 0.25,),
  strip.text = element_text(size = 13, face = "bold", margin = margin(b = 15, t = 15)),
  strip.background = element_rect(fill = "grey95", color = "grey80"),
  strip.placement = "outside",
  strip.clip = "off",
  panel.spacing.x = unit(1, "cm"),
  panel.spacing.y = unit(1, "cm")
  ) -> p

ggsave("img/prevalence_deliquence.png", plot = p, width = 10, height = 10, dpi = 300, bg = "white")


set <- c(19, 20, 32, 33, 44, 46)
set <- c(19, 33, 44, 46, 55)
d |>
  filter(id %in% d$id[set]) |>
  select(year, id, name,  n_total, pct_total, pct_solved, n_suspect_total, n_suspect_men, n_suspect_woman) |>
  mutate(across(-c(id, name), as.numeric)) |>
  mutate(pct_men = 100 * n_suspect_men / n_suspect_total, pct_women = 100 * (1 - pct_men/100)) |>
  select(year, id, name, n_total, pct_total, pct_solved, pct_men) |>
  pivot_longer(-c(year, id, name), names_to = "var") |>
  mutate(var = factor(var, levels = c("n_total", "pct_total", "pct_solved", "pct_men"), labels = c("Anzahl erfasste Fälle", "Anteil an Gesamtfallzahl", "Aufklärungsquote", "Anteil männlicher Täter"))) |>
  mutate(id = factor(id, levels = d$id[set], labels = d$name[set])) |>
  ggplot(aes(x = year, y = value, color = id)) +
  facet_wrap(~var, scales = "free") +
  geom_step(linewidth = 0.9) +
  ggh4x::facetted_pos_scales(
    y = list(
      "Anzahl erfasste Fälle" = scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())),
      "Anteil an Gesamtfallzahl" = scale_y_continuous(labels = label_percent(scale = 1)),
      "Aufklärungsquote" = scale_y_continuous(labels = label_percent(scale = 1)),
      "Anteil männlicher Täter" = scale_y_continuous(labels = label_percent(scale = 1))
    )
 ) +
  scale_x_continuous(
    name = "Jahr", 
    breaks = seq(2015, 2024, 1)
  ) +
  ylab(NULL) +
  #sjPlot::scale_color_sjplot(palette = "social", name = NULL) +
  scale_color_manual(name = NULL, values = c("#00496FFF", "#0F85A0FF", "#EDD746FF", "#ED8B00FF", "#DD4124FF")) +
  #scale_color_manual(name = NULL, values = c("#010101FF", "#FFB81CFF", "#DA291CFF", "#004C97FF", "#418FDEFF")) +
  sjPlot::theme_sjplot() +
  theme(
    text = element_text(family = "font", size = 15),
    legend.spacing.y = unit(2, "cm"),
    legend.byrow = TRUE,
    legend.position = "bottom",
    legend.direction = "vertical",
    #panel.spacing.x = unit(0.5, "cm"),
    panel.spacing.y = unit(1, "cm")
    ) -> p2 



ggsave("img/prevalence_deliquence_by_type.png", plot = p2, width = 14, height = 10, dpi = 400, bg = "white")


d |>
  filter(id %in% d$id[set]) |>
  filter(year == 2024) |>
  select(name, n_total, pct_total, n_weapon_threat:pct_nongerman) |>
  mutate(across(-name, as.numeric)) |>
  mutate(pct_men = 100 * n_suspect_men / n_suspect_total, pct_women = 100 * (1 - pct_men/100)) |>
  mutate(across(where(is.numeric), ~round(.x, 1))) |>
  select(name, n_total, pct_men, pct_solved) |>
  write_csv("data/deliquence_2024.csv")
