
# Creating coloured icons to represent colour rings

# This might be better for colour icons: https://stackoverflow.com/questions/30486412/r-shiny-custom-icon-image-in-selectinput
# For making the icons? https://www.datanovia.com/en/blog/how-to-create-icon-in-r/
# Or grid?

library(imager)
library(grid)

# Function to specify the three colours and create and save the icon ------------------------------------------------
# Could update it to accept one colour as well
create_ring_icon <- function(cols, name, 
                             filename = paste0("www/", gsub("/", "_", name), ".png"),
                             width = 400, height = 400, res = 100,
                             crop_x_3 = 78:322, crop_y_3 = 302:178,
                             crop_x_2 = 78:322, crop_y_2 = 312:188) {
  
  #if (!(length(cols) %in% c(2, 3))) {
  #  stop("cols must be a character vector of 2 or 3 colours.")
  #}
  
  temp_file <- tempfile(fileext = ".png")
  
  png(temp_file, width = width, height = height, res = res)
  grid.newpage()
  
  if (length(cols) == 3) {
    grid.rect(x = 0.5, y = 0.5, width = 0.6, height = 0.1, gp = gpar(fill = cols[1], col = cols[1]))
    grid.rect(x = 0.5, y = 0.4, width = 0.6, height = 0.1, gp = gpar(fill = cols[2], col = cols[2]))
    grid.rect(x = 0.5, y = 0.3, width = 0.6, height = 0.1, gp = gpar(fill = cols[3], col = cols[3]))
    grid.rect(
      x = 0.5, y = 0.4,
      width = 0.6, height = 0.3,
      gp = gpar(fill = NA, col = "black", lwd = 2)
    )
    
  } else {
    grid.rect(x = 0.5, y = 0.45, width = 0.6, height = 0.15, gp = gpar(fill = cols[1], col = cols[1]))
    grid.rect(x = 0.5, y = 0.3, width = 0.6, height = 0.15, gp = gpar(fill = cols[2], col = cols[2]))
    grid.rect(
      x = 0.5, y = 0.375,  # vertical midpoint of both stripes
      width = 0.6, height = 0.3,
      gp = gpar(fill = NA, col = "black", lwd = 2)
    )
  }
  
  dev.off()
  
  img <- load.image(temp_file)
  if (length(cols) == 3) {
    cropped <- imsub(img, x %in% crop_x_3, y %in% crop_y_3)
  } else {
    cropped <- imsub(img, x %in% crop_x_2, y %in% crop_y_2)
  }
  
  save.image(cropped, filename)
  
  message("Saved: ", filename)
}



# Create icons -------------------------------
# The rings: c("blue", "blue/white", "green", "metal", "orange", "pink/blue", "pink/green", "red", "red/white", "white", "white/blue", "yellow", "yellow/black")

# Blue
create_ring_icon(c("#2986cc", "#2986cc", "#2986cc"), "blue")

# Green
create_ring_icon(c("#3C8558", "#3C8558", "#3C8558"), "green")

# Metal
create_ring_icon(c("#818385", "#818385", "#818385"), "metal")

# Orange
create_ring_icon(c("#D98B23", "#D98B23", "#D98B23"), "orange")

# Pink/blue
create_ring_icon(c("#2986cc", "#E0539B", "#2986cc"), "pink_blue")

# Pink/green
create_ring_icon(c("#3C8558", "#E0539B", "#3C8558"), "pink_green")

# Red
create_ring_icon(c("#C73232", "#C73232", "#C73232"), "red")

# Red/white
create_ring_icon(c("#C73232", "white", "#C73232"), "red_white")

# White
create_ring_icon(c("white", "white", "white"), "white")

# White/blue
create_ring_icon(c("white", "#2986cc"), "white_blue")

# Yellow
create_ring_icon(c("Yellow", "Yellow", "Yellow"), "yellow")

# Yellow black
create_ring_icon(c("yellow", "black"), "yellow_black")









# Old: figuring out code for drawing/saving icons ------------------------------
grid.newpage()

# Draw three solid color boxes
grid.rect(x = 0.2, y = 0.5, width = 0.2, height = 0.02, gp = gpar(fill = "#2986cc", col = "#2986cc"))
grid.rect(x = 0.2, y = 0.48, width = 0.2, height = 0.02, gp = gpar(fill = "white", col = "white"))
grid.rect(x = 0.2, y = 0.46, width = 0.2, height = 0.02, gp = gpar(fill = "#2986cc", col = "#2986cc"))
grid.rect(x = 0.2, y = 0.48,  # vertical midpoint of stripes
          width = 0.2, height = 0.06,
          gp = gpar(fill = NA, col = "black", lwd = 1)
)

# blue/white icon:
# Drawing with grid, saving image, cropping with imagr and saving
png("boxes.png", width = 400, height = 400, res = 100)
grid.newpage()
grid.rect(x = 0.5, y = 0.5, width = 0.6, height = 0.1, gp = gpar(fill = "white"))
grid.rect(x = 0.5, y = 0.4, width = 0.6, height = 0.1, gp = gpar(fill = "#2986cc"))
grid.rect(x = 0.5, y = 0.3, width = 0.6, height = 0.1, gp = gpar(fill = "white"))
grid.rect(
  x = 0.5, y = 0.4,
  width = 0.6, height = 0.3,
  gp = gpar(fill = NA, col = "black", lwd = 2)
)
dev.off()
# Laod in imager
img <- load.image("boxes.png")
plot(img)
# crop
plot(img)
cropped_rect1 <- imsub(img, x %in% 78:322, y %in% 302:178)
plot(cropped_rect1)
# save
save.image(cropped_rect1,"www/blue_white.png")

# If it has two colours (half and half) rather than one or three:
png("boxes.png", width = 400, height = 400, res = 100)
grid.newpage()
grid.rect(x = 0.5, y = 0.45, width = 0.6, height = 0.15, gp = gpar(fill = "white"))
grid.rect(x = 0.5, y = 0.3, width = 0.6, height = 0.15, gp = gpar(fill = "#2986cc"))
grid.rect(
  x = 0.5, y = 0.375,  # vertical midpoint of both stripes
  width = 0.6, height = 0.3,
  gp = gpar(fill = NA, col = "black", lwd = 2)
)

dev.off()
# Laod in imager
img <- load.image("boxes.png")
plot(img)
# crop
plot(img)
cropped_rect1 <- imsub(img, x %in% 78:322, y %in% 312:188)
plot(cropped_rect1)
# save
save.image(cropped_rect1,"test.png")


