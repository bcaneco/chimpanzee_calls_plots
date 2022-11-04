
# ------------------------ #
# ---     Preamble    ------
# ------------------------ #

library(tidyverse)
library(MetBrewer)
library(patchwork)
library(sf)
library(sfheaders)
library(colortools)
library(rcartocolor)

dt <- read_csv("data/calls_data.csv", show_col_types = FALSE) %>%
  mutate(session = as.factor(session))

sapply(list.files("R/", full.names = TRUE), source)

# --------------------------------- #
# ---    Global variables      -----
# --------------------------------- #

pnt_col_key <- c("Riet" = "firebrick3", "Alex" = "royalblue4")


# ------------------------ #
# ---    Options      -----
# ------------------------ #

p1 <- dt |>
  pizza_plot(
    x = delta_duration, 
    #y = delta_voice_inflexion,
    y = delta_voice_freq,
    n_slices = 8,
    offset = pi/8,
    fill_slices = TRUE,
    plot_points = FALSE,
    add_nr_points = TRUE,
    #cheking_plot = TRUE,
    #title = "Riet", 
    xlab = expression(Delta ~ "Duration (secs)"),
    ylab = expression(Delta ~ "Max Frequency (Hz)")
  )


p2 <- dt |>
  pizza_plot(
    x = delta_duration, 
    #y = delta_voice_inflexion,
    y = delta_voice_freq,
    n_slices = 8,
    offset = pi/8,
    fill_slices = TRUE,
    plot_points = TRUE,
    add_nr_points = TRUE,
    #cheking_plot = TRUE,
    #title = "Riet", 
    xlab = expression(Delta ~ "Duration (secs)"),
    ylab = expression(Delta ~ "Max Frequency (Hz)")
  )



p3 <- dt |>
  pizza_plot(
    x = delta_duration, 
    #y = delta_voice_inflexion,
    y = delta_voice_freq,
    n_slices = 8,
    offset = pi/8,
    fill_slices = FALSE,
    plot_points = TRUE,
    add_nr_points = TRUE,
    #cheking_plot = TRUE,
    #title = "Riet", 
    xlab = expression(Delta ~ "Duration (secs)"),
    ylab = expression(Delta ~ "Max Frequency (Hz)")
  )


options_panel <- p1 + p2 + p3


ggsave("outputs/pizza_plots_options.png", options_panel,
       device = "png", width = 14, height = 4.5, units = "in", scale = 1)




# ----------------------------- #
# ---    Colour Palette     -----
# ----------------------------- #

p1 <- dt |>
  pizza_plot(
    x = delta_duration, 
    #y = delta_voice_inflexion,
    y = delta_voice_freq,
    n_slices = 8,
    offset = pi/8,
    fill_slices = TRUE,
    plot_points = TRUE,
    add_nr_points = FALSE,
    #cheking_plot = TRUE,
    #title = "Riet", 
    xlab = expression(Delta ~ "Duration (secs)"),
    ylab = expression(Delta ~ "Max Frequency (Hz)")
  )


p2 <- dt |>
  pizza_plot(
    x = delta_duration, 
    #y = delta_voice_inflexion,
    y = delta_voice_freq,
    n_slices = 8,
    offset = pi/8,
    fill_slices = TRUE,
    plot_points = TRUE,
    add_nr_points = FALSE,
    #cheking_plot = TRUE,
    #title = "Riet", 
    xlab = expression(Delta ~ "Duration (secs)"),
    ylab = expression(Delta ~ "Max Frequency (Hz)"),
    fill_pal = rcartocolor::carto_pal(name = "Teal")
  )



p3 <- dt |>
  pizza_plot(
    x = delta_duration, 
    y = delta_voice_freq,
    n_slices = 8,
    offset = pi/8,
    fill_slices = TRUE,
    plot_points = TRUE,
    add_nr_points = FALSE,
    #cheking_plot = TRUE,
    #title = "Riet", 
    xlab = expression(Delta ~ "Duration (secs)"),
    ylab = expression(Delta ~ "Max Frequency (Hz)"), 
    fill_pal = rcartocolor::carto_pal(name = "Mint")
  )

p4 <- dt |>
  pizza_plot(
    x = delta_duration, 
    y = delta_voice_freq,
    n_slices = 8,
    offset = pi/8,
    fill_slices = TRUE,
    plot_points = TRUE,
    add_nr_points = FALSE,
    #cheking_plot = TRUE,
    #title = "Riet", 
    xlab = expression(Delta ~ "Duration (secs)"),
    ylab = expression(Delta ~ "Max Frequency (Hz)"), 
    fill_pal = rcartocolor::carto_pal(name = "DarkMint")
  )

p1 + p3 + p4




# ----------------------------- #
# ---    Panel Building     -----
# ----------------------------- #

build_pizza_panel <- function(data, x, y, n_slices, offset = pi/n_slices,   
                              fill_slices = TRUE, plot_points = TRUE,
                              pnt_col_id = NULL, pnt_col_key = NULL, lolli = FALSE,
                              title = NA, fixed_lims = FALSE,
                              xlab = NA, ylab = NA, fill_pal = MetBrewer::met.brewer("Hokusai2")
                              ){
  # overall plot
  p_all <- data |>
    pizza_plot(x = {{x}}, y = {{y}}, n_slices = n_slices, offset = pi/n_slices,
               fill_slices = fill_slices, plot_points = plot_points, 
               pnt_col_id = {{pnt_col_id}}, pnt_col_key = pnt_col_key, lolli = lolli,
               add_nr_points = TRUE, 
               title = "All Sessions", 
               xlab = "", ylab = ylab, fill_pal = fill_pal
    )
  
  # limits management
  if(fixed_lims){
    # Make limits of all plots the same as the overall plot
    xlim <- ggplot_build(p_all)$layout$panel_scales_x[[1]]$range$range
    ylim <- ggplot_build(p_all)$layout$panel_scales_y[[1]]$range$range    
  }else{
    # Limits of each plot defined internally based on theit inherent data
    xlim <- NULL
    ylim <- NULL
  }
  
  
  # Riet - all sessions
  p_riet <- data |>
    filter(subject == "Riet") |>
    pizza_plot(x = {{x}}, y = {{y}}, n_slices = n_slices, offset = pi/n_slices,
               fill_slices = fill_slices, plot_points = plot_points, 
               pnt_col_id = {{pnt_col_id}}, pnt_col_key = pnt_col_key, lolli = lolli,
               add_nr_points = TRUE, xlab = "", ylab = "", fill_pal = fill_pal, 
               ylim = ylim, xlim = xlim,
               title = "Riet Sessions")
  
  # Alex - All sessions
  p_alex <- data |>
    filter(subject == "Alex") |>
    pizza_plot(x = {{x}}, y = {{y}}, n_slices = n_slices, offset = pi/n_slices,
               fill_slices = fill_slices, plot_points = plot_points, 
               pnt_col_id = {{pnt_col_id}}, pnt_col_key = pnt_col_key, lolli = lolli,
               add_nr_points = TRUE, fill_pal = fill_pal, xlab = "", ylab = "", 
               ylim = ylim, xlim = xlim,
               title = "Alex Sessions")
  
  # Riot - session 1
  p_riet_s1 <- data |>
    filter(subject == "Riet", session == 1) |>
    pizza_plot(x = {{x}}, y = {{y}}, n_slices = n_slices, offset = pi/n_slices,
               fill_slices = fill_slices, plot_points = plot_points, pnt_size = 0.8,
               pnt_col_id = {{pnt_col_id}}, pnt_col_key = pnt_col_key, lolli = lolli,
               add_nr_points = FALSE, fill_pal = fill_pal, xlab = "", ylab = "", 
               ylim = ylim, xlim = xlim,
               title = "RS 1")
  
  # Riot - session 2
  p_riet_s2 <- data |>
    filter(subject == "Riet", session == 2) |>
    pizza_plot(x = {{x}}, y = {{y}}, n_slices = n_slices, offset = pi/n_slices,
               fill_slices = fill_slices, plot_points = plot_points, pnt_size = 0.8,
               pnt_col_id = {{pnt_col_id}}, pnt_col_key = pnt_col_key, lolli = lolli,
               add_nr_points = FALSE, fill_pal = fill_pal, xlab = "", ylab = "", 
               ylim = ylim, xlim = xlim,
               title = "RS 2")
  
  
  # Alex - session 1
  p_alex_s1 <- data |>
    filter(subject == "Alex", session == 1) |>
    pizza_plot(x = {{x}}, y = {{y}}, n_slices = n_slices, offset = pi/n_slices,
               fill_slices = fill_slices, plot_points = plot_points, pnt_size = 0.8,
               pnt_col_id = {{pnt_col_id}}, pnt_col_key = pnt_col_key, lolli = lolli,
               add_nr_points = FALSE, fill_pal = fill_pal, xlab = "", ylab = "",
               ylim = ylim, xlim = xlim,
               title = "AS 1")
  
  # Alex - session 2
  p_alex_s2 <- data |>
    filter(subject == "Alex", session == 2) |>
    pizza_plot(x = {{x}}, y = {{y}}, n_slices = n_slices, offset = pi/n_slices,
               fill_slices = fill_slices, plot_points = plot_points, pnt_size = 0.8,
               pnt_col_id = {{pnt_col_id}}, pnt_col_key = pnt_col_key, lolli = lolli,
               add_nr_points = FALSE, fill_pal = fill_pal, xlab = "", ylab = "", 
               ylim = ylim, xlim = xlim,
               title = "AS 2")
  

  # patchwork 
  
  # session patches
  riet_session_patch <- p_riet_s1/p_riet_s2
  alex_session_patch <- p_alex_s1/p_alex_s2 
  
  # subject patches
  riet_patch <- p_riet + riet_session_patch +
    plot_layout(widths = c(1.8, 0.8))
  alex_patch <- p_alex + alex_session_patch +
    plot_layout(widths = c(1.8, 0.8))
  
  # main patch
  main_patch <- p_all + 
    (riet_patch/alex_patch) +
    plot_layout(widths = c(1.15, 0.85))
  
  # final panel
  main_patch/grid::textGrob(xlab) + plot_layout(heights = c(50,1))
  
}




# --------------------------------------------- #
# --- Delta Duration Vs. Delta Frequency   ------
# --------------------------------------------- #

# with points
build_pizza_panel(
  data = dt, 
  x = delta_duration, 
  y = delta_voice_freq,
  n_slices = 8,  
  fill_slices = TRUE, 
  plot_points = TRUE, 
  pnt_col_id = subject, 
  pnt_col_key = pnt_col_key,
  fixed_lims = FALSE,
  xlab = expression(Delta ~ "Duration (secs)"),
  ylab = expression(Delta ~ "Max Frequency (Hz)"),
  #fill_pal = rcartocolor::carto_pal(name = "DarkMint")
  fill_pal = rcartocolor::carto_pal(name = "Teal")
) 

ggsave("outputs/delta_duration_vs_delta_maxfreq_pizza.png", 
       device = "png", width = 17, height = 10, units = "in", scale = 1)



# origin-lollipop style
build_pizza_panel(
  data = dt, 
  x = delta_duration, 
  y = delta_voice_freq,
  n_slices = 8,  
  fill_slices = TRUE, 
  plot_points = TRUE, 
  pnt_col_id = subject, 
  pnt_col_key = pnt_col_key, 
  lolli = TRUE,
  fixed_lims = FALSE,
  xlab = expression(Delta ~ "Duration (secs)"),
  ylab = expression(Delta ~ "Max Frequency (Hz)"),
  fill_pal = rcartocolor::carto_pal(name = "Teal")
) 

ggsave("outputs/delta_duration_vs_delta_maxfreq_pizza_lolli.png", 
       device = "png", width = 17, height = 10, units = "in", scale = 1)




# ---------------------------------------------------- #
# ---     Delta PC Slope Vs. Delta Entropy        ------
# ---------------------------------------------------- #

build_pizza_panel(
  data = dt, 
  x = delta_voice_inflexion, 
  y = delta_voice_entropy,
  n_slices = 8,  
  fill_slices = TRUE, 
  plot_points = TRUE, 
  pnt_col_id = subject, 
  pnt_col_key = pnt_col_key,
  fixed_lims = FALSE,
  xlab = expression(Delta ~ "Pitch Counter Slope (Hz)"),
  ylab = expression(Delta ~ "Entropy (Hz)"),
  fill_pal = rcartocolor::carto_pal(name = "Teal")
) 

ggsave("outputs/delta_pcslope_vs_delta_entropy_pizza.png", 
       device = "png", width = 17, height = 10, units = "in", scale = 1)


# origin-lollipop style
build_pizza_panel(
  data = dt, 
  x = delta_voice_inflexion, 
  y = delta_voice_entropy,
  n_slices = 8,  
  fill_slices = TRUE, 
  plot_points = TRUE, 
  pnt_col_id = subject, 
  pnt_col_key = pnt_col_key, 
  lolli = TRUE,
  fixed_lims = FALSE,
  xlab = expression(Delta ~ "Pitch Counter Slope (Hz)"),
  ylab = expression(Delta ~ "Entropy (Hz)"),
  fill_pal = rcartocolor::carto_pal(name = "Teal")
) 

ggsave("outputs/delta_pcslope_vs_delta_entropy_pizza_lolli.png", 
       device = "png", width = 17, height = 10, units = "in", scale = 1)





# -------------------------------------------------------- #
# ---  Pct Change Duration Vs. Pct Change Frequency   ------
# --------------------------------------------------------- #

build_pizza_panel(
  data = dt, 
  x = pc_duration, 
  y = pc_voice_freq,
  n_slices = 8,  
  fill_slices = TRUE, 
  plot_points = TRUE, 
  pnt_col_id = subject, 
  pnt_col_key = pnt_col_key,
  fixed_lims = FALSE,
  xlab = "Change in Duration (%)",
  ylab = "Change in Max Frequency (%)",
  fill_pal = rcartocolor::carto_pal(name = "Teal")
) 

ggsave("outputs/pctchange_duration_vs_pctchange_maxfreq_pizza.png", 
       device = "png", width = 17, height = 10, units = "in", scale = 1)


# origin-lollipop style
build_pizza_panel(
  data = dt, 
  x = pc_duration, 
  y = pc_voice_freq,
  n_slices = 8,  
  fill_slices = TRUE, 
  plot_points = TRUE, 
  pnt_col_id = subject, 
  pnt_col_key = pnt_col_key,
  lolli = TRUE,
  fixed_lims = FALSE,
  xlab = "Change in Duration (%)",
  ylab = "Change in Max Frequency (%)",
  fill_pal = rcartocolor::carto_pal(name = "Teal")
) 

ggsave("outputs/pctchange_duration_vs_pctchange_maxfreq_pizza_lolli.png", 
       device = "png", width = 17, height = 10, units = "in", scale = 1)




# --------------------------------------------------------------- #
# ---      Pct Change PC Slope Vs. Pct Change Entropy           --
# --------------------------------------------------------------- #

build_pizza_panel(
  data = dt, 
  x = pc_voice_inflexion, 
  y = pc_voice_entropy,
  n_slices = 8,
  fill_slices = TRUE, 
  plot_points = TRUE, 
  pnt_col_id = subject, 
  pnt_col_key = pnt_col_key,
  fixed_lims = FALSE,
  xlab = "Change in Pitch Contour Slope (%)",
  ylab = "Change in Entropy (%)",
  fill_pal = rcartocolor::carto_pal(name = "Teal")
) 

ggsave("outputs/pctchange_pcslope_vs_pctchange_entropy_pizza.png", 
       device = "png", width = 17, height = 10, units = "in", scale = 1)



build_pizza_panel(
  data = dt, 
  x = pc_voice_inflexion, 
  y = pc_voice_entropy,
  n_slices = 8,
  fill_slices = TRUE, 
  plot_points = TRUE, 
  pnt_col_id = subject, 
  pnt_col_key = pnt_col_key, 
  lolli = TRUE,
  fixed_lims = FALSE,
  xlab = "Change in Pitch Contour Slope (%)",
  ylab = "Change in Entropy (%)",
  fill_pal = rcartocolor::carto_pal(name = "Teal")
) 

ggsave("outputs/pctchange_pcslope_vs_pctchange_entropy_pizza_lolli.png", 
       device = "png", width = 17, height = 10, units = "in", scale = 1)



