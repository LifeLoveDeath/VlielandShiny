
# Creating coloured icons to represent colour rings

# This might be better for colour icons: https://stackoverflow.com/questions/30486412/r-shiny-custom-icon-image-in-selectinput
# For making the icons? https://www.datanovia.com/en/blog/how-to-create-icon-in-r/
# Or grid?


library(grid)

grid.newpage()

# Draw three solid color boxes
grid.rect(x = 0.2, y = 0.5, width = 0.2, height = 0.02, gp = gpar(fill = "#2986cc", col = "#2986cc"))
grid.rect(x = 0.2, y = 0.48, width = 0.2, height = 0.02, gp = gpar(fill = "white", col = "white"))
grid.rect(x = 0.2, y = 0.46, width = 0.2, height = 0.02, gp = gpar(fill = "#2986cc", col = "#2986cc"))


grid.rect(x = 0.5, y = 0.4, width = 0.2, height = 0.3, gp = gpar(fill = "#2986cc"))


grid.rect(x = 0.8, y = 0.5, width = 0.2, height = 0.1, gp = gpar(fill = "white"))
grid.rect(x = 0.8, y = 0.4, width = 0.2, height = 0.1, gp = gpar(fill = "#2986cc"))
grid.rect(x = 0.8, y = 0.3, width = 0.2, height = 0.1, gp = gpar(fill = "white"))


grid.newpage()

grid.rect(x = 0.2, y = 0.5, width = 0.2, height = 0.1, gp = gpar(fill = "red"))
grid.rect(x = 0.2, y = 0.4, width = 0.2, height = 0.1, gp = gpar(fill = "white"))
grid.rect(x = 0.2, y = 0.3, width = 0.2, height = 0.1, gp = gpar(fill = "red"))


# Save icon as a png?
library(gridSVG)
grid.newpage()
# Open PNG device
png("striped_box.png", width = 400, height = 300)
#Change area
pushViewport(viewport(xscale = c(0.1, 0.3), yscale = c(0.2, 0.6)))
# Draw boxes
grid.rect(x = 0.2, y = 0.5, width = 0.2, height = 0.1, gp = gpar(fill = "#2986cc"))
grid.rect(x = 0.2, y = 0.4, width = 0.2, height = 0.1, gp = gpar(fill = "white"))
grid.rect(x = 0.2, y = 0.3, width = 0.2, height = 0.1, gp = gpar(fill = "#2986cc"))

# Close device and save
dev.off()


# Trying to crop it
library(gridSVG)
grid.newpage()
# Draw your boxes
grid.rect(x = 0.2, y = 0.5, width = 0.2, height = 0.1, gp = gpar(fill = "#2986cc"))
grid.rect(x = 0.2, y = 0.4, width = 0.2, height = 0.1, gp = gpar(fill = "white"))
grid.rect(x = 0.2, y = 0.3, width = 0.2, height = 0.1, gp = gpar(fill = "#2986cc"))
grid.export("striped_box.svg")

library(rsvg)
library(magick)

# Read and convert
svg_file <- "striped_box.svg"
png_file <- "striped_box.png"

image <- rsvg::rsvg_png(svg_file, file = png_file)
