#' Create a map for respondent's state based on an appendix file
#'
#' Function that creates a map from an appendix file.
#' @param apendix_file: .xlsx file with appendix data
#' @keywords Pollfish
#' @export
#' @examples
#' create_map("appendix_file.xlsx")

create_map <- function(appendix_file){
  library(tidyverse);library(readxl);library(urbnmapr)
  x <- read_excel(appendix_file, sheet = "states_for_map")
  
  k <- urbnmapr::states %>%
    inner_join(x, by = c("state_name" = "state")) %>%
    ggplot(aes(long, lat, group = group, fill = region)) + 
    geom_polygon(alpha =0.75, show.legend = FALSE, color = "gray30") + 
    theme_minimal() + 
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.text = element_text(size = 8),
          text = element_text(face = "bold"),
          legend.title = element_text(size =10)) + 
    scale_fill_manual(values = c("royalblue3", "darkorange2", "firebrick2", "darkslategray3","forestgreen", "darkorchid3")) + 
    guides(fill = guide_legend(title = "Region\n", title.position = "top", 
                               title.hjust = 0.5)) + 
    geom_text(data = get_urbn_labels(map = "states") %>% inner_join(x, by = c("state_name" = "state")) %>%
                mutate(for_label = ifelse(state_abbv %in% c("MD", "DE","NJ","CT","RI","MA","ME","NH","VT", "DC","HI","AK"),state_abbv, "")) %>%
                mutate(label = ifelse(for_label == "",n, paste0(for_label,"-",n))), aes(x = long, lat, label = label), 
              size = 3.9, inherit.aes = FALSE, fontface = "bold", color = "black")
  
  return(k)
}
