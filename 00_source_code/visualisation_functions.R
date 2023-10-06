

# Personal variation of the black and white theme ------------------------------

theme_bw2 <- function (base_size = 12, base_family = '',
                       base_line_size = base_size/22,
                       base_rect_size = base_size/22)
{
  theme_grey(base_size = base_size,
             base_family = base_family,
             base_line_size = 0,
             base_rect_size = base_rect_size) %+replace%
    theme(panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(fill = NA, colour = "grey20"),
          panel.grid.minor = element_line(size = rel(0.5)),
          strip.background = element_rect(color = "black", fill = "grey90"),
          # strip.background = element_blank(),
          legend.key = element_rect(fill = "white", colour = NA), complete = TRUE,
          legend.title.align = 0,
          legend.position = "top",
          legend.justification = "right",
          legend.direction = "horizontal") +
    theme(axis.text.x = element_text(color = "black", size = rel(1.1)),
          axis.text.y = element_text(color = "black", size = rel(1.1)),
          strip.text.x = element_text(face="bold", size = rel(1.1)),
          strip.text.y = element_text(face="bold", size = rel(1.1)),
          axis.title = element_text(size = rel(1.1)),
          legend.text = element_text(size = rel(1.0)),
          legend.title =  element_text(face = "bold", size = rel(1.1)),
          plot.title = element_text(hjust = 0,
                                    vjust=0,
                                    size = 19, face = "bold",
                                    color = "#222222"), # for fonts
          plot.subtitle = element_text(hjust = 0,
                                       size = 16,
                                       color = "#222222"),
          plot.title.position = 'plot',
          strip.placement = "outside",
          axis.ticks = ggplot2::element_blank(),
          #plot.caption = element_text(vjust = -3, face = "italic", size = rel(1.2)),
          # plot.caption.position =  "plot",
          strip.switch.pad.grid = unit(.7, "cm"), # for shifting y label away from levels
          axis.text.x.bottom = element_text(margin = margin(b=12)),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 8, l = 0)))
  # plot.margin=unit(c(r=1,l=1,b=1.0,t=-.5),"cm"))
  
}

# Function to produce interaction plot -----------------------------------------

interaction_plot <- function(
    data, 
    x_label = "Difference in mean grip strength (kg)", 
    y_label = 'Highest qualification', save_plot = F) {
  
  p <- data |> 
    ggplot(aes(x = estimate, y = fct_rev(term))) +
    geom_path(aes(group = fct_rev(sex))) +
    geom_effect(
      ggplot2::aes(
        xmin = conf.low,
        xmax = conf.high,
        colour = fct_rev(sex)
      ),
      position = ggstance::position_dodgev(height = 0.7),
      fatten = 3.5
    ) + 
    facet_grid(sex ~ age_group_0) +
    geom_stripes(odd = "#22222222", even = "#00000000") +
    geom_vline(xintercept = 0, linetype = 6, size = 0.4, colour = "black") +
    labs(x = x_label, y = y_label) +
    theme_bw2() +
    theme(
      axis.text.x = element_text(angle = 90, size = rel(1.0), hjust = 1),
      legend.title = element_blank()
    ) +
    guides(
      colour = "none",
      fill = guide_legend(
        reverse = F,
        override.aes = list(size = 4)
      )
    ) +
    scale_colour_brewer(palette = "Set1")
  
  if (save_plot & y_label == "Highest qualification") {
    
    ggsave("04_visualisations/4b_visualisation_outputs/edu_age_sex_int.pdf", 
           width = 9.4, height = 6.07, dpi = 320, scale = 1.0,
           useDingbats = TRUE)
    
  } else if (save_plot & y_label != "Highest qualification") {
    
    ggsave("04_visualisations/4b_visualisation_outputs/imd_age_sex_int.pdf", 
           width = 9.4, height = 6.07, dpi = 320, scale = 1.0,
           useDingbats = TRUE)
    
  }
  
  return(p)
}