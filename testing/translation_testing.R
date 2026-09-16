library(rnames)

# ENGLISH ----
en <- list(
  
  ## General terms ----
  general = list(
    
    Bt = "Blue tit",
    Gt = "Great tit",
    Title = "Vlieland Nestboxes"
    
  ),
  
  ## Bird search module terms ----
  bird_search = list(
    
    SearchRing = "Search using ring number",
    SearchCol = "Search using colour rings",
    Egs = "Example birds"
    
  )
  
)



# BLANK ----
bl <- list(
  
  ## General terms ----
  general = list(
    
    Bt = "",
    Gt = ""
    
  ),
  
  ## Bird search module terms ----
  bird_search = list(
    
    SearchRing = "",
    SearchCol = "",
    Egs = ""
    
  )
  
)



# OPERATION ----

## Text swap ----
tx <- en
tx <- bl

## Trial paste ----
paste("this is a", tx$general$Bt, "text test")

## List comparisons ----
bl_names <- capture.output(rnames(bl))
en_names <- capture.output(rnames(en))

length(bl_names) == length(en_names)

en_names[!en_names %in% bl_names]
