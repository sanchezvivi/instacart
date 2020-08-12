
instacart_colors <- c("green"= "#43b02a",
                      "orange"= "#ff8200",
                      "dark-green" = "#0e491f", 
                      "blue" = "#007bff", 
                      "indigo"= "#6610f2",
                      "purple"= "#6f42c1", 
                      "pink"= "#e83e8c",
                      "red"= "#dc3545",
                      "yellow"= "#ffc107", 
                      "teal" = "#20c997", 
                      "cyan"= "#17a2b8", 
                      "white" = "#fff", 
                      "gray"= "#6c757d", 
                      "gray-dark"= "#343a40",
                      "light-blue"= "#eff9ff", 
                      "dark"= "#343a40")

ic_cols <- function(...){
  cols <- c(...)
  if(is.null(cols))
    return (instacart_colors)
  instacart_colors[cols]
}

instacart_palettes <- list(
  `main`  = ic_cols("green", "orange"),
  
  `cool`  = ic_cols("light-blue", "cyan", "blue", "indigo"),
  
  `hot`   = ic_cols("yellow","orange","red"),
  
  `greens` = ic_cols("dark-green", "green", "light-blue"),
  
  `mixed` = ic_cols("green", "orange", "cyan", "yellow", "red", "purple"),
  
  `contrast`  = ic_cols("orange","light-blue","dark-green")
)

instacart_pal <- function(palette = "main", reverse = FALSE, ...) {
  
  pal <- instacart_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

scale_color_instacart <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- instacart_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("instacart_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_instacart <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  
  pal <- instacart_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("instacart_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

# fonte https://brandguide.brandfolder.com/instacart/color