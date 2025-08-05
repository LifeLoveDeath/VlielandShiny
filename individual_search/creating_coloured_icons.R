
# Creating coloured icons to represent colour rings

# This might be better for colour icons: https://stackoverflow.com/questions/30486412/r-shiny-custom-icon-image-in-selectinput
# For making the icons? https://www.datanovia.com/en/blog/how-to-create-icon-in-r/
# Or grid?

library(imager)
library(grid)

grid.newpage()

# Draw three solid color boxes
grid.rect(x = 0.2, y = 0.5, width = 0.2, height = 0.02, gp = gpar(fill = "#2986cc", col = "#2986cc"))
grid.rect(x = 0.2, y = 0.48, width = 0.2, height = 0.02, gp = gpar(fill = "white", col = "white"))
grid.rect(x = 0.2, y = 0.46, width = 0.2, height = 0.02, gp = gpar(fill = "#2986cc", col = "#2986cc"))


# blue/white icon ------------------------------------------------
# Drawing with grid, saving image, cropping with imagr and saving
png("boxes.png", width = 400, height = 400, res = 100)
grid.newpage()
grid.rect(x = 0.5, y = 0.5, width = 0.6, height = 0.1, gp = gpar(fill = "white"))
grid.rect(x = 0.5, y = 0.4, width = 0.6, height = 0.1, gp = gpar(fill = "#2986cc"))
grid.rect(x = 0.5, y = 0.3, width = 0.6, height = 0.1, gp = gpar(fill = "white"))
dev.off()
# Laod in imager
img <- load.image("boxes.png")
plot(img)
# crop
plot(img)
cropped_rect1 <- imsub(img, x %in% 80:320, y %in% 300:180)
plot(cropped_rect1)
# save
save.image(cropped_rect1,"blue_white.png")



# Function to specify the three colours and create and save the icon ------------------------------------------------


