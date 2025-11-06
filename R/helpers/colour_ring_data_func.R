
# colour_ring_data_func


get_colour_rings <- function() {
  val <- c("blue", "blue/white", "green", "metal", "orange",
           "pink/blue", "pink/green", "red", "red/white",
           "white", "white/blue", "yellow", "yellow/black")
  
  code <- c("bl", "bw", "gr", "al", "or",
            "pb", "pg", "re", "rw",
            "wh", "wb", "ye", "yb")
  
  img <- sprintf(
    "<div class='picker-item'>
       <span class='text'>%s</span>
       <img src='%s.png' class='icon'>
     </div>",
    val,
    gsub("/", "_", val)
  )
  
  data.frame(val = val, img = img, stringsAsFactors = FALSE)
}
