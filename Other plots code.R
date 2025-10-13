
# Fig 2
library(ggplot2)
library(ggforce)
library(dplyr)
library(stringr)

library(readxl)

keggresult <- read_excel("C:/Users/张圣欣/Desktop/棒棒糖图.xlsx")

dat <- keggresult %>%
  arrange(Confidence) %>%
  mutate(Description = factor(Description, levels = rev(unique(Description)))) 

dat$log_Confidence <- log10(dat$Confidence)

# Wrap description for better readability
dat$Wrapped_Description <- str_wrap(dat$Description, width = 40)


num_categories <- length(unique(dat$category))
base_cols <- c( "#B6E3E3", "#F7E3A1", "#CBE6A3",
                "#BFD7EA", "#FAD4C0", "#D7C7FF")

custom_colors <- c(
  "#E65100",  
  "#FF9800",  
  "#FFB74D", 
  "#FFCC80",  
  "#FFF3E0"   
)


dat$Wrapped_Description <- str_wrap(dat$Description, width = 80)  

dat <- dat %>%
  arrange(desc(Count)) %>%
  mutate(Wrapped_Description = factor(Wrapped_Description, levels = unique(Wrapped_Description)))  


ggplot(dat) +
  ggforce::geom_link(
    aes(
      x = 0, 
      y = Wrapped_Description,
      xend = Count,  
      yend = Wrapped_Description,
      color = category,  
      size = 2 
    ),
    n = 500,
    show.legend = c(color = TRUE, size = FALSE)  
  ) +
  geom_point(
    aes(
      x = Count,  
      y = Wrapped_Description,
      fill = log_Confidence  
    ),
    color = "black", 
    size = 5,
    shape = 21,
    show.legend = TRUE  
  ) +
 
  geom_text(
    aes(
      x = Count + 2,  
      y = Wrapped_Description,
      label = Count  
    ),
    size = 4,  
    color = "black",  
    hjust = 0  
  ) +
  scale_color_manual(
    values = levelcolor,
    name = "Category"
  ) +
  scale_fill_gradientn(
    colors = custom_colors,  
    values = scales::rescale(seq(0, 6, length.out = 100)),
    name = "Log10(Confidence)"
  ) +
  guides(
    alpha = "none",
    size = "none"
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.75),
    axis.text.y = element_text(
      color = "black", 
      size = 12,
      lineheight = 0.8
    ),
    axis.text.x = element_text(color = "black", size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    plot.margin = margin(1, 1, 1, 2, "cm")
  ) +
  ylab("") + 
  xlab("No of studies") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.04))) +
  scale_y_discrete(limits = rev)  



# Fig S1
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


