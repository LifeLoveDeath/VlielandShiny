
# Creating coloured icons to represent colour rings

# This might be better for colour icons: https://stackoverflow.com/questions/30486412/r-shiny-custom-icon-image-in-selectinput
# For making the icons? https://www.datanovia.com/en/blog/how-to-create-icon-in-r/
# Or grid?


library(grid)

grid.newpage()

# Draw three solid color boxes
grid.rect(x = 0.2, y = 0.5, width = 0.2, height = 0.1, gp = gpar(fill = "#2986cc"))
grid.rect(x = 0.2, y = 0.4, width = 0.2, height = 0.1, gp = gpar(fill = "white"))
grid.rect(x = 0.2, y = 0.3, width = 0.2, height = 0.1, gp = gpar(fill = "#2986cc"))


grid.rect(x = 0.5, y = 0.4, width = 0.2, height = 0.3, gp = gpar(fill = "#2986cc"))


grid.rect(x = 0.8, y = 0.5, width = 0.2, height = 0.1, gp = gpar(fill = "white"))
grid.rect(x = 0.8, y = 0.4, width = 0.2, height = 0.1, gp = gpar(fill = "#2986cc"))
grid.rect(x = 0.8, y = 0.3, width = 0.2, height = 0.1, gp = gpar(fill = "white"))


grid.newpage()

grid.rect(x = 0.2, y = 0.5, width = 0.2, height = 0.1, gp = gpar(fill = "red"))
grid.rect(x = 0.2, y = 0.4, width = 0.2, height = 0.1, gp = gpar(fill = "white"))
grid.rect(x = 0.2, y = 0.3, width = 0.2, height = 0.1, gp = gpar(fill = "red"))


