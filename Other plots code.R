
# Fig 1
library(ggplot2)
library(dplyr)
library(grid)

data <- data.frame(
  ExposedPopulation = c(),
  Percentage = c())
data <- data %>%
  arrange(desc(Percentage)) %>%
  mutate(
    LabelText = paste0(ExposedPopulation, " (", Percentage, "%)"),
    ymax = cumsum(Percentage),
    ymin = lag(ymax, default = 0),
    labelPosition = (ymax + ymin) / 2)
base_colors <- c("#97B48B", "#bed7b3","#D8E8DD","#EBF1E7")
custom_colors <- colorRampPalette(base_colors)(nrow(data))
data$FillColor <- custom_colors
data$LabelText <- factor(data$LabelText, levels = data$LabelText)

ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 6.5, xmin = 1.5, fill = FillColor)) +
  geom_rect(color = "white") +
  coord_polar(theta = "y") +
  xlim(c(0, 7.2)) +  
  theme_void() +
  geom_text(
    data = filter(data, Percentage >= 5),
    aes(x = 4.2, y = labelPosition, label = paste0(Percentage, "%")),
    size = 1.4
  ) +
  scale_fill_identity(guide = "legend", labels = data$LabelText, breaks = data$FillColor) +
  guides(fill = guide_legend(
    override.aes = list(color = NA),
    ncol = 1
  )) +
  ggtitle("Exposed Populations (Gradient, % ≥5 Shown)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    legend.title = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 10.5, lineheight = 0.55),
    legend.key.size = unit(0.25, "cm"),
    legend.spacing.y = unit(0.2, "cm")
  )

# Fig 2
library(ggplot2)
library(dplyr)
library(forcats)
data <- data.frame( Outcome = c(), Count = c( ))
data <- data %>% mutate(Outcome = fct_reorder(Outcome, Count))
ggplot(data, aes(x = Count, y = Outcome)) +
  geom_bar(stat = "identity", fill = ) +
  geom_text(aes(label = Count), hjust = -0.2, size = 3.5) +
  theme_minimal(base_size = 13) +
  labs(x = "Number of Studies", y = "Outcome") +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 10)  
  ) +
  coord_cartesian(xlim = c(0, max(data$Count) + 5))



# Fig S2
library(ggplot2)
library(tidyr)
library(dplyr)

data <- data.frame(
  Criterion = c("PICO", "Protocol", "Study selection", "Search strategy", "Study selection in duplicate",
                "Data extraction in duplicate", "List of excluded studies", "Description of included studies",
                "Satisfactory RoB assessment", "Sources of funding", "Statistical combination of results",
                "Effect of RoB on meta-analysis", "Account for RoB in interpretation",
                "Explanation for heterogeneity", "Publication bias", "Conflict of interest"),
  Yes = c(           81, 32, 81, 81, 79, 80, 43,  76,  81, 5,   81,   63, 77, 76, 77, 67),
  `Partial yes` = c(0, 43,   0,  0,  2,  1,  37,  5,    0, 69,  0,   15,  3,  3,  3, 11),
  No = c(           0, 6,    0,  0,  0,  0,  1,   0,    0, 7,   0,   3,   1,  2,  1, 3),
  check.names = FALSE  
)

data_long <- pivot_longer(
  data,
  cols = c("Yes", "Partial yes", "No"),
  names_to = "Response",
  values_to = "Count"
)

response_colors <- c(
  "Yes" = "#00C1B2",         
  "Partial yes" = "#F9B233", 
  "No" = "#E94B4B"           
)

ggplot(data_long, aes(x = Count, 
                      y = factor(Criterion, levels = rev(c("PICO", "Protocol", "Study selection", "Search strategy", 
                                                           "Study selection in duplicate", "Data extraction in duplicate", 
                                                           "List of excluded studies", "Description of included studies", 
                                                           "Satisfactory RoB assessment", "Sources of funding", 
                                                           "Statistical combination of results", "Effect of RoB on meta-analysis", 
                                                           "Account for RoB in interpretation", "Explanation for heterogeneity", 
                                                           "Publication bias", "Conflict of interest"))),
                      fill = Response)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(Count == 0, NA, Count)),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = response_colors) +
  labs(
    title = "AMSTAR-2 assessment",
    x = NULL, y = NULL,
    caption = "Note: The chart shows how many studies met each AMSTAR-2 criterion ('Yes', 'Partial yes', 'No')."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    plot.caption = element_text(hjust = 0, size = 9, face = "italic", margin = margin(t = 10))
  )


# Fig S3
install.packages("janitor")  
library(janitor)            
data <- read_excel("C:/patchwork", sheet = "Sheet1")
data <- clean_names(data)
data$percentage <- as.numeric(data$percentage)
data$label <- paste0(data$no, " (", round(data$percentage, 1), "%)")
ggplot(data, aes(x = reorder(exposure_type, -no), y = no, fill = no)) +
  geom_bar(stat = "identity", color = "black", width = 0.8) +
  geom_text(aes(label = label), vjust = -0.4, size = 3) +
  scale_fill_gradient(low = "#C9FDFF", high = "#D4FEDC") +
  labs(title = "Distribution of Exposure Types",
       x = "Exposure Type",
       y = "Number of Cases (n)") +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 12),
    legend.position = "none"
  )
scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
