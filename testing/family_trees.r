library(BGmisc)
library(ggpedigree)
library(tidyverse)
library(patchwork) # for combining plots
data("potter") # load the potter pedigree data
library(plotly)
library(viridis)

df_potter <- potter %>%
  mutate(
    name = case_when(
      personID == 1 ~ "Vernon",
      personID == 2 ~ "Marjorie",
      personID == 3 ~ "Petunia",
      personID == 4 ~ "Lily",
      personID == 5 ~ "James",
      personID == 6 ~ "Dudley",
      personID == 7 ~ "Harry",
      personID == 8 ~ "Ginny",
      personID == 9 ~ "Arthur",
      personID == 10 ~ "Molly",
      personID == 11 ~ "Ron",
      personID == 12 ~ "Fred",
      personID == 13 ~ "George",
      personID == 14 ~ "Percy",
      personID == 15 ~ "Charlie",
      personID == 16 ~ "Bill",
      personID == 17 ~ "Hermione",
      personID == 18 ~ "Fleur",
      personID == 19 ~ "Gabrielle",
      personID == 20 ~ "Audrey",
      personID == 21 ~ "James",
      personID == 22 ~ "Albus",
      personID == 23 ~ "Lily",
      personID == 24 ~ "Rose",
      personID == 25 ~ "Hugo",
      personID == 26 ~ "Victoire",
      personID == 27 ~ "Dominique",
      personID == 28 ~ "Louis",
      personID == 29 ~ "Molly",
      personID == 30 ~ "Lucy",
      personID == 101 ~ "Mother",
      personID == 102 ~ "Father",
      personID == 103 ~ "Mother",
      personID == 104 ~ "Father",
      personID == 105 ~ "Mother",
      personID == 106 ~ "Father"
    )
  )

#m1 <- 
ggPedigree(df_potter %>% filter(personID %in% c(1:7, 101:104)),
                 famID = "famID",
                 personID = "personID",
                 config = list(
                   label_include = TRUE,
                   label_column = "name",
                   point_size = 8,
                   # outline_include = TRUE,
                   #focal_fill_personID = 8,
                   outline_multiplier = 1.5,
                   segment_linewidth = 0.5,
                   label_text_size = 4,
                   focal_fill_include = TRUE,
                   label_nudge_y = 0.30,
                   #   focal_fill_method = "viridis_d",
                   #   focal_fill_viridis_option = "inferno",
                   focal_fill_force_zero = TRUE,
                   label_method = "geom_text",
                   focal_fill_na_value = "grey10",
                   focal_fill_scale_midpoint = 0.40,
                      #focal_fill_n_breaks = 15,
                   focal_fill_component = "matID",
                   focal_fill_method = "manual",
                   focal_fill_color_values = c( # okabe and ito
                     #   "#052f60",
                     "#e69f00", # "#56b4e9",
                     "#009e73",
                     "#f0e442", "#0072b2",
                     # "#d55e00",
                     "#cc79a7"
                   ),
                   focal_fill_labels = NULL,
                   focal_fill_legend_title = "Additive\nGenetic\nRelatives \nof Harry",
                   sex_legend_show = FALSE,
                   # "additive",\
                   # label_text_angle = -35,
                   sex_color_include = FALSE
                 ) # highlight Harry Potter
                 # config  = list(segment_mz_color = NA) # color for monozygotic twins
) + guides(shape = "none") + theme(
  plot.title = element_blank(),
  plot.title.position = "plot"
) + coord_cartesian(ylim = c(3.25, 1), clip = "off")


# Add status column
df_potter$status 

#m2 <-
ggPedigree(df_potter,
                 famID = "famID",
                 personID = "personID",
                 config = list(
                   label_include = TRUE,
                   label_column = "name",
                   point_size = 8,
                   # outline_include = TRUE,
                   focal_fill_personID = 7,
                   #outline_multiplier = 1.5,
                   segment_linewidth = 0.5,
                   label_text_size = 4.5,
                   label_nudge_y = 0.25,
                   label_nudge_x = .1,
                   focal_fill_include = TRUE,
                   focal_fill_high_color = "#d55e00",
                   focal_fill_mid_color = "#d55e00",
                   focal_fill_low_color = "blue",
                   focal_fill_scale_midpoint = 0.85,
                   focal_fill_component = "additive",
                    focal_fill_method = "gradient",
                   focal_fill_force_zero = TRUE,
                   label_method = "geom_text",
                   focal_fill_na_value = "grey10",
                   #   focal_fill_n_breaks = 15,
                   label_text_angle = -30,
                   # focal_fill_legend_title = "Additive\nGenetic\nRelatives \nof Harry",
                   #sex_legend_show = FALSE,
                   # "additive",
                   sex_color_include = FALSE,
                   segment_mz_fill = NA # colour of line connecting twins
                 ) # highlight Harry Potter
                 # config  = list(segment_mz_color = NA) # color for monozygotic twins
) + theme(
  legend.position = "none",
  plot.title = element_blank(),
  plot.title.position = "plot"
) + coord_cartesian(ylim = c(4.25, 1), clip = "off")

# use twins to show cluthes? Can only really indicate 2 individuals
# use overaly_column to show clutches?



# status column
data("hazard")

ggPedigree(
  hazard,
  famID = "famID",
  personID = "ID",
  status_column = "affected",
  config = list(
    code_male = 0,
    sex_color_include = TRUE,
    status_code_affected = TRUE,
    status_code_unaffected = FALSE,
    status_shape_affected = 10
  )
)



# Add clutch id to try and colour by clutch
df_potter <- df_potter %>%
  mutate(
    clutchID = case_when(
      personID %in% c(8, 11, 12, 13) ~ "Clutch1",   # Ginny, Ron, Fred, George
      personID %in% c(14, 15, 16)    ~ "Clutch2",   # Percy, Charlie, Bill
      personID %in% c(21, 22)        ~ "Clutch3",   # James, Albus
      personID == 23                 ~ "Clutch4",   # Lily
      personID %in% c(26, 27)        ~ "Clutch5",   # Victoire, Dominique
      personID == 28                 ~ "Clutch6",   # Louis
      TRUE                           ~ "Other"
    )
  )

# focal
focal_id <- 7 

# Tooltip text
# Add tooltip
df_potter <- df_potter %>%
  mutate(
    tooltip_text = paste0(
      "Name: ", name, "\n",
      "Sex: ", ifelse(sex == 0, "Female", ifelse(sex == 1, "Male", "Unknown")), "\n",
      "Clutch: ", clutchID
    )
  )


# Base pedigree
p <- ggPedigreeInteractive(
  df_potter,
  famID = "famID",
  personID = "personID",
  dadID = "dadID",
  momID = "momID",
  sex_color_include = FALSE,
  config = list(
    label_include = TRUE,
    label_column = "name",
    point_size = 6,
    segment_linewidth = 0.5,
    label_text_size = 3,
    label_nudge_y = 0.25,
    label_nudge_x = .1,
    label_text_angle = -30,
    return_static = TRUE
  ),
  tooltip_columns = c("name", "clutchID")
)


p
head(p$data)
nodes <- p$data %>%
  left_join(df_potter %>% select(personID, clutchID), by = "personID")
head(nodes)


# keeping shape to depict sex
p +
  geom_point(
    data = nodes,
    aes(
      x = x_pos,
      y = y_pos,
      fill = clutchID.y,
      shape = factor(sex)   # map sex to shape
    ),
    size = 6, colour = "black"
  ) +
  scale_shape_manual(values = c(21, 22, 23)) +  # 21 = circle, 22 = square, 23 = diamond (unknown sex)
  scale_fill_viridis_d(option = "C", na.value = "grey80")

p
ggplotly(p)


# Separate nodes
clutch_nodes <- nodes %>% filter(clutchID.y != "Other")
other_nodes   <- nodes %>% filter(clutchID.y == "Other")



a <- p +
  # Step 1: all nodes in beige
  geom_point(
    data = other_nodes,
    aes(x = x_pos, y = y_pos, shape = factor(sex), text = tooltip_text),
    size = 6, fill = "#F0E1C6", colour = "black"
  ) +
  # Step 2: clutch nodes coloured automatically with viridis
  geom_point(
    data = clutch_nodes,
    aes(x = x_pos, y = y_pos, fill = clutchID.y, shape = factor(sex), text = tooltip_text),
    size = 6, colour = "black",
    show.legend = FALSE,
  ) +
  #scale_shape_manual(values = c(21, 22, 23)) +  # circle, square, diamond (unknown)
  scale_shape_manual(
    name = "Sex",
    values = c("0" = 21, "1" = 22, "NA" = 23),  # circle, square, diamond
    labels = c("Female", "Male", "Unknown")
  ) +
  scale_fill_viridis_d(option = "C", end = 0.85)     +
  geom_point(
    data = nodes %>% filter(personID == focal_id),
    aes(x = x_pos, y = y_pos),
    shape = 22,               # keep shape consistent with sex
    size = 10,                 # slightly bigger         # highlight colour
    colour = "black",          # border colour
    stroke = 1             # thicker border
  )

a

# Fixing legend issue and tooltips
# And tooltip - should be a way to retain it
ggplotly(a, tooltip = "text") %>%
  style(showlegend = FALSE, traces = c(1:10, 13:19))  # remove legend from specific traces - have to figure out which they are


# Changing between static and plotly interactive: https://r-computing-lab.github.io/ggpedigree/articles/v11_extendedinteractiveplots.html


# Using overlay instead
df_potter <- df_potter %>%
  mutate(clutchID_overlay = clutchID)  






# Use ggplot to draw box around the focal?


# Or otherwise convert back to interactive/plotly object
ggplotly(a)
