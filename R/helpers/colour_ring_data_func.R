
# colour_ring_data_func


get_colour_rings <- function() {
  val <- c("blue", "blue/white", "green", "metal", "orange",
           "pink/blue", "pink/green", "red", "red/white",
           "white", "white/blue", "yellow", "yellow/black", "pink",
           "green/white")
  
  code <- c("bl", "bw", "gr", "al", "or",
            "pb", "pg", "re", "rw",
            "wh", "wb", "ye", "yb", "pi",
            "gw")
  
  # Ensure path works in Shiny modules and app subdirs
  # We use the "www" folder correctly by prepending it with "./" so Shiny serves it
  img <- sprintf(
    "<div class='picker-item'>
       <span class='text'>%s</span>
       <img src='./%s.png' class='icon'>
     </div>",
    val,
    gsub("/", "_", val)
  )
  
  # Optional: check which images exist and warn if missing
  img_files <- file.path("www", paste0(gsub("/", "_", val), ".png"))
  missing <- val[!file.exists(img_files)]
  if(length(missing) > 0) {
    warning("Missing icon files in www/: ", paste(missing, collapse = ", "))
  }
  
  data.frame(val = val, code = code, img = img, stringsAsFactors = FALSE)
}

