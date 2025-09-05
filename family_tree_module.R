
# move to R folder when finished
# ggPedigree ---------------------------------------------------------------------------
# https://cran.r-project.org/web/packages/ggpedigree/vignettes/v10_interactiveplots.html
# https://r-computing-lab.github.io/ggpedigree/
# https://github.com/R-Computing-Lab/ggpedigree/

library(ggpedigree)
library(ggplot2) # ggplot2 for plotting
library(viridis) # viridis for color palettes
library(tidyverse) # for data wrangling
library(plotly)

vlieland.data <- read.csv("data/IndividualsData.csv", row.names = NULL)
location.data <- read.csv("data/NestLocationData.csv", row.names = NULL)

ped.data <- vlieland.data 


# notes
# ggepedigree is interactive (plotly) but not very customisable (e.g. can't colour in the focal)
# kinship2/pedtools - notinteractive but more customisable so could be controlled using shiyn tickboxes etc. to change data and redraw tree
# pedtool has these:
# Add/remove/extract individuals
#The functions below are used to modify an existing ped object by adding/removing individuals, or extracting a sub-pedigree. For details, see ?ped_modify.
#addChildren(), with special cases addSon(), addDaughter(), addChild()
#addParents()
#removeIndividuals()
#branch()
#subset()


# Function to get focal individual family tree data ---------------
get_family_subset <- function(ped.data, focal_id) {
  # Parents
  parents <- ped.data[ped.data$RingNumber == focal_id, c("Parent1", "Parent2")]
  p1 <- parents$Parent1
  p2 <- parents$Parent2
  
  # Children
  children <- ped.data$RingNumber[ped.data$Parent1 %in% focal_id | ped.data$Parent2 %in% focal_id]
  
  # Siblings (share at least one parent)
  siblings <- ped.data$RingNumber[ped.data$Parent1 %in% c(p1, p2) | ped.data$Parent2 %in% c(p1, p2)]
  
  # Combine all IDs
  ids <- unique(c(focal_id, p1, p2, children, siblings))
  
  # Subset the pedigree
  fam.data <- ped.data[ped.data$RingNumber %in% ids, ]
  
  # Add placeholder rows for missing parents
  all_parents <- unique(c(fam.data$Parent1, fam.data$Parent2))
  missing_parents <- setdiff(all_parents, fam.data$RingNumber)
  missing_parents <- missing_parents[!is.na(missing_parents)]
  
  if (length(missing_parents) > 0) {
    get_parent_sex <- function(id, df) {
      in_dad <- id %in% df$Parent1
      in_mom <- id %in% df$Parent2
      
      if (in_dad && !in_mom) return(1)   # male
      if (in_mom && !in_dad) return(2)   # female
      return(NA)                         # ambiguous or both
    }
    
    sex_vals <- vapply(missing_parents, get_parent_sex, numeric(1), df = fam.data)
    
    missing_rows <- tibble(
      RingNumber      = missing_parents,
      Parent1         = NA_character_,
      Parent2         = NA_character_,
      BirthYear       = NA_integer_,
      DeathYear       = NA_integer_,
      Sex             = NA,
      sex             = sex_vals,
      ColourRingCombo = NA,
      ColourRingLeft1 = NA,
      ColourRingLeft2 = NA,
      ColourRingRight1= NA,
      ColourRingRight2= NA,
      Species         = NA
    )
    
    fam.data <- bind_rows(fam.data, missing_rows)
  }
  
  fam.data$focal <- ifelse(fam.data$RingNumber == focal_id, TRUE, NA)
  
  fam.data
}


fam.data <- get_family_subset(ped.data, "RN00402")


# trying ggpedigreeInteractive -------

ggPedigreeInteractive(
  fam.data,
  personID = "RingNumber",
  momID    = "Parent2",
  dadID    = "Parent1",
  tooltip  = c("RingNumber", "BirthYear", "DeathYear"),
  config = list(
    #focal_fill_column = "focal",
    #focal_fill_include = TRUE,
    #focal_fill_high_color = "yellow",
    sex_color_palette = c("#440154", "#5ec962")#,
    #sex_colour_include = T
    )
    ) %>%
  config(
    displaylogo = FALSE,                 # remove plotly logo and other controls
    modeBarButtonsToRemove = c(
      "lasso2d", "select2d",
      "hoverClosestCartesian", "hoverCompareCartesian",
      "toggleSpikelines",
      "sendDataToCloud", "toImage"
    )
  )


# Plot with ggpedigree and then wrap in ploty?
# Create the plot
p <- ggPedigree(
  fam.data,
  personID = "RingNumber",
  momID    = "Parent2",
  dadID    = "Parent1",
  tooltip  = c("RingNumber", "BirthYear", "DeathYear"),
  config = list(segment_linewidth = .25,
                point_size = 2,
  sex_color_palette = c("#440154", "#5ec962"),
  label_include = FALSE))

# Convert to plotly and remove extra buttons
p <- p %>%
  config(
    displaylogo = FALSE,
    modeBarButtonsToRemove = c(
      "lasso2d","select2d",
      "hoverClosestCartesian","hoverCompareCartesian",
      "toggleSpikelines",
      "sendDataToCloud","toImage"
    )
  )

p



# Using fam.id to colour cluthes
fam.data <- fam.data %>%
  # Combine parent IDs and birth date to define a clutch
  mutate(
    clutch_key = paste(Parent1, Parent2, BirthYear, BirthMonth, sep = "_")
  ) %>%
  # Assign a numeric clutch ID
  group_by(clutch_key) %>%
  mutate(
    clutch_id = cur_group_id()
  ) %>%
  ungroup() %>%
  select(-clutch_key)  # optional: remove intermediate column

#here
# Render as static
fam.data$clutch_id <- as.factor(fam.data$clutch_id)
fam.data$id <- fam.data$RingNumber
#


#staticPed <-
ggPedigreeInteractive(
  fam.data,
  #famID = "clutch_id",
  personID = "id",
  momID    = "Parent2",
  dadID    = "Parent1",
  tooltip  = c("RingNumber", "BirthYear", "DeathYear"),
  config = list(segment_linewidth = .25,
                point_size = 2,
                #overlay_column = "clutch_id",
                #overlay_include = TRUE,
                sex_color_palette = c("#440154", "#5ec962"),
                label_include = FALSE#,
                #return_static = TRUE
                )
                )


staticPed

# Add static customisaion using ggplot

staticPed + scale_color_viridis(
  discrete = TRUE,
  labels = c("Female", "Male", "Unknown"))



# Return to interactive
plotly::ggplotly(staticPed,
                         tooltip = "text")





# using kinship ----------------
library(kinship2)
library(visNetwork)
head(fam.data)
# Create pedigree object
ped <- pedigree(id = fam.data$RingNumber,
                dadid = fam.data$Parent1,
                momid = fam.data$Parent2,
                sex = fam.data$sex,
                affected = fam.data$focal)  # or use a column if you want to highlight certain individuals


plot(ped, cex = 1,
     )  

# Create color palette for clutch_id
clutch_palette <- rainbow(length(unique(fam.data$clutch_id)))
names(clutch_palette) <- unique(fam.data$clutch_id)

# Map node colors
node_colors <- clutch_palette[fam.data$clutch_id]

# Create pedigree
ped <- pedigree(
  id    = fam.data$RingNumber,
  dadid = fam.data$Parent1,
  momid = fam.data$Parent2,
  sex   = fam.data$sex,
  affected = fam.data$focal
)

# Create color palette for clutch_id
clutch_palette <- rainbow(length(unique(fam.data$clutch_id)))
names(clutch_palette) <- unique(fam.data$clutch_id)

# Map node colors in the same order as ped$id
node_colors <- clutch_palette[match(ped$id, fam.data$RingNumber) %>% sapply(function(i) fam.data$clutch_id[i])]

# Better way: directly match RingNumber to clutch_id
node_colors <- clutch_palette[fam.data$clutch_id[match(ped$id, fam.data$RingNumber)]]

# Plot pedigree
plot(
  ped,
  cex = 0.8,
  col = node_colors,       # color by clutch_id
  symbolsize = 1.5,
  affected = fam.data$focal  # highlight focal birds
)
 # struggling with colouring


# Using pedtools -----------
library(pedtools)
head(fam.data)
head(fam.data[ ,c("sex", "Sex")])

# get clutches
fam.data <- fam.data %>%
  # Combine parent IDs and birth date to define a clutch
  mutate(
    clutch_key = paste(Parent1, Parent2, BirthYear, BirthMonth, sep = "_")
  ) %>%
  # Assign a numeric clutch ID
  group_by(clutch_key) %>%
  mutate(
    clutch_id = cur_group_id()
  ) %>%
  ungroup() %>%
  select(-clutch_key)  # optional: remove intermediate column


ped <- ped(id = fam.data$RingNumber, fid =fam.data$Parent1, mid = fam.data$Parent2, sex = fam.data$sex)


clutch_palette <- rainbow(length(unique(fam.data$clutch_id)))
names(clutch_palette) <- unique(fam.data$clutch_id)
fill_colors <- clutch_palette[fam.data$clutch_id]
names(fill_colors) <- fam.data$RingNumber
# Need to remove NAs
# And make it viridis

as.data.frame(ped)

plot(ped, hatched = "RN00402")
plot(ped, hatched = "RN00402",
     fill = fill_colors,
     label = FALSE)




# can build up:
singleton(id = fam.data[which(fam.data$RingNumber == "RN00402", "RingNumber")], 
          fid = ,
          mid = ,
          sex = )



# use subseting
# branch -  just gets descendents of focal individual from full dataset

ped.full <- pedtools::ped(
  id  = ped.data$RingNumber,
  fid = ped.data$Parent1,
  mid = ped.data$Parent2,
  sex = ped.data$sex
)
# Suppose your ID of interest is:
focus_id <- "RN00131"
fam_index <- sapply(ped.full, function(x) focus_id %in% x$ID)
fam_index

ped.single <- ped.full[[which(fam_index)]]


branch(ped.single, "RN00131")
       

ancestors(ped.single, "RN00131")



# collapsibleTree ---------------------------------------------------------------------

library(collapsibleTree)
# can only show one parent

# Prepare a long-format for hierarchy (Parent -> Child)
# Combine Parent1 and Parent2 into one column
ped_long <- fam.data %>%
  select(RingNumber, Parent1, Parent2) %>%
  pivot_longer(cols = c(Parent1, Parent2), names_to = "ParentType", values_to = "Parent") %>%
  filter(!is.na(Parent))  # remove missing parents

# Plot top-down tree
collapsibleTree(
  ped_long,
  hierarchy = c("Parent", "RingNumber"),  # Parent -> Child
  #root = "Root",                          # virtual root for unconnected parents
  direction = "tb",                        # top-down
  width = 800,
  height = 600,
  nodeSize = "leafCount",
  fontSize = 12
)


# Trying again
# Prepare data in long format: child -> parents
fam.long <- fam.data %>%
  select(child = RingNumber, Parent1, Parent2) %>%
  tidyr::pivot_longer(cols = c("Parent1", "Parent2"), names_to = "parent_type", values_to = "parent") %>%
  filter(!is.na(parent) & parent != "")

# You need a hierarchical structure: root -> generation -> child
# One simple way is to treat Parent1 as main lineage for tree
tree.data <- fam.data %>%
  select(Parent1, RingNumber) %>%
  rename(parent = Parent1, child = RingNumber) %>%
  filter(!is.na(parent) & parent != "")

# Plot interactive collapsible tree
collapsibleTree(
  tree.data,
  hierarchy = c("parent", "child"),
  root = "Root",
  fill = "child"#,
  #nodeSize = "leafCount"
)



