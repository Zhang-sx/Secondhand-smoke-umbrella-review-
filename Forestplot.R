# forestplot
library(ggplot2)
library(patchwork)
library(readxl)
library(dplyr)
library(readxl)
library(ggplot2)
library(scales)
library(janitor)

df <- read_excel("C:/patchwork", sheet = 1)

df <- df %>%
  rename(
    Outcome             = `Outcome`,
    ExposureSource      = `Exposure Source`,
    ExposurePopulation  = `Exposure Population`,
    Doses               = `Doses`,
    es                  = `es`,
    lci                 = `lci`,
    uci                 = `uci`,
    EffectSize          = `Effect Size`,
    AMSTAR2             = `AMSTAR2`,
    EvidenceClass       = `Evidence Class`,
    GRADE               = `GRADE`
  )

df <- df %>%
  mutate(RowID = row_number())

# left side
p_left <- ggplot(df, aes(y = RowID)) +
  geom_text(aes(x = 0,  label = Outcome), hjust = 0, size = 3) +
  geom_text(aes(x = 26, label = ExposureSource), hjust = 0, size = 3) +
  geom_text(aes(x = 52, label = ExposurePopulation), hjust = 0, size = 3) +
  geom_text(aes(x = 70, label = Doses), hjust = 0, size = 3) +
  coord_cartesian(xlim = c(0, 82)) + 
  theme_void()

# forestplot in the middle
p_forest <- ggplot(df, aes(y = RowID, x = es)) +
  geom_errorbarh(aes(xmin = lci, xmax = uci), 
                 height = 0, color = "black", size = 0.5) +
  geom_point(shape = 23, size = 1.2, fill = '#D61B52', color = "black") +
  geom_vline(xintercept = 1, linetype = "solid") +
  scale_x_continuous(
    breaks = c(0, 1, 2, 3, 4),
    limits = c(0, 4)
  ) +
  xlab("Effect Size (95% CI)") +
  ylab(NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.margin = margin(l = 0, r = 0, unit = "pt"),
    axis.text.y = element_blank(),
    panel.grid  = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm")
  )

# right side
p_right <- ggplot(df, aes(y = RowID)) +
  geom_text(aes(x = 0,  label = EffectSize), hjust = 0, size = 3) +
  geom_text(aes(x = 18, label = AMSTAR2),    hjust = 0, size = 3) +
  geom_text(aes(x = 30, label = EvidenceClass), hjust = 0, size = 3) +
  geom_text(aes(x = 40, label = GRADE),      hjust = 0, size = 3) +
  coord_cartesian(xlim = c(0, 50)) +
  theme_void() +
  theme(plot.margin = margin(l = 0, unit = "pt"))

final_plot <- p_left + p_forest + p_right +
  plot_layout(ncol = 3, widths = c(60, 35, 30))

final_plot

