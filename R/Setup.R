# wipes previous workspace
rm(list=ls(all=TRUE)) 

# Install Packages and Load
packages<-c("tidyverse","zoo","openxlsx","cansim","fredr")
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,type="binary")
  sapply(pkg, require, character.only = TRUE)
}
check.packages(packages)

# Load the FRED API Key
fredr_set_key(Sys.getenv("FRED_KEY"))

# Your preferred color scheme (https://www.color-hex.com/color-palette/33490)
col<-c("#CC2529","#396ab1","#3E9651","#DA7C30","#535154","#6B4C9A","#922428","#948B3D")
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = col)
}
scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = col)
}

# Set figure theme
theme_set(theme_minimal())
theme_update(panel.grid.minor = element_blank(),
             panel.grid.major.x = element_blank(),
             plot.subtitle = element_text(size = 9, color = "gray40"),
             panel.background = element_rect(fill = "white",colour = "white"),
             plot.background = element_rect(fill = "white",colour = "white"),
             axis.title.y = element_text(size=9),
             axis.title.x = element_text(size=9),
             legend.title=element_blank(),
             strip.background = element_rect(fill="gray90",color="transparent"))
