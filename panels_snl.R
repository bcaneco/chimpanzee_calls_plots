
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



# ------------------------------------------ #
# ---   Local function building panels  ------
# ------------------------------------------ #

build_snl_panel <- function(data, x, y, time, recording_id = NULL, ncx = 20, ncy = ncx,
                            #fixed_limits = TRUE, 
                            add_freq_col = FALSE,
                            type = "spokes", xlab = NULL, ylab = NULL){
  
  #browser()
  
  # overall plot
  p_all <- data |>
    snl_plot(x = {{x}}, y = {{y}}, time = {{time}}, recording_id = {{recording_id}}, 
             ncx = ncx, ncy = ncy, type = type, xlab = "", ylab = ylab, 
             add_freq_col = add_freq_col, title = "All Sessions") 
  
  xlim_overall <- ggplot_build(p_all)$layout$panel_scales_x[[1]]$range$range
  ylim_overall <- ggplot_build(p_all)$layout$panel_scales_y[[1]]$range$range  
  
  # Riet - all sessions
  p_riet <- data |>
    filter(subject == "Riet") |>
    snl_plot(x = {{x}}, y = {{y}}, time = {{time}}, recording_id = {{recording_id}}, 
             ncx = ncx, ncy = ncy, type = type, xlab = "", ylab = "",
             xlim = xlim_overall, ylim = ylim_overall, arrow_fctr = 1.8, cellcent_fct = 0.8,
             add_freq_col = add_freq_col, title = "Riet Sessions") 
  
  # Alex - All sessions
  p_alex <- data |>
    filter(subject == "Alex") |>
    snl_plot(x = {{x}}, y = {{y}}, time = {{time}}, recording_id = {{recording_id}}, 
             ncx = ncx, ncy = ncy, type = type, xlab = "", ylab = "", 
             xlim = xlim_overall, ylim = ylim_overall, arrow_fctr = 1.8, cellcent_fct = 0.8,
             add_freq_col = add_freq_col, title = "Alex Sessions") 
  
  # Riot - session 1
  p_riet_s1 <- data |>
    filter(subject == "Riet", session == 1) |>
    snl_plot(x = {{x}}, y = {{y}}, time = {{time}}, recording_id = {{recording_id}}, 
             ncx = ceiling(ncx*0.8), ncy = ceiling(ncx*0.8), type = type, xlab = "", ylab = "", 
             xlim = NULL, ylim = NULL, arrow_fctr = 3, cellcent_fct = 0.5, 
             add_freq_col = add_freq_col, axis_nbreaks_fct = 0.7, title = "RS 1") 
  
  # Riot - session 2
  p_riet_s2 <- data |>
    filter(subject == "Riet", session == 2) |>
    snl_plot(x = {{x}}, y = {{y}}, time = {{time}}, recording_id = {{recording_id}}, 
             ncx = ceiling(ncx*0.8), ncy = ceiling(ncx*0.8), type = type, xlab = "", ylab = "",
             xlim = NULL, ylim = NULL, arrow_fctr = 3, cellcent_fct = 0.5,
             add_freq_col = add_freq_col, axis_nbreaks_fct = 0.7, title = "RS 2") 
  
  
  # Alex - session 1
  p_alex_s1 <- data |>
    filter(subject == "Alex", session == 1) |>
    snl_plot(x = {{x}}, y = {{y}}, time = {{time}}, recording_id = {{recording_id}}, 
             ncx = ceiling(ncx*0.8), ncy = ceiling(ncx*0.8), type = type, 
             xlab = "", ylab = "", xlim = NULL, ylim = NULL, 
             arrow_fctr = 3, cellcent_fct = 0.5,
             add_freq_col = add_freq_col, axis_nbreaks_fct = 0.7, title = "AS 1") 
  
  # Alex - session 2
  p_alex_s2 <- data |>
    filter(subject == "Alex", session == 2) |>
    snl_plot(x = {{x}}, y = {{y}}, time = {{time}}, recording_id = {{recording_id}}, 
             ncx = ceiling(ncx*0.8), ncy = ceiling(ncx*0.8), type = type, xlab = "", ylab = "", 
             xlim = NULL, ylim = NULL, arrow_fctr = 3, cellcent_fct = 0.5,
             add_freq_col = add_freq_col, axis_nbreaks_fct = 0.7, title = "AS 2") 
  
  
  # patchwork 
  
  # session patches
  riet_session_patch <- p_riet_s1/p_riet_s2
  alex_session_patch <- p_alex_s1/p_alex_s2 

  # subject patches
  riet_patch <- p_riet + riet_session_patch +
    plot_layout(widths = c(2.1, 0.9))
  alex_patch <- p_alex + alex_session_patch +
    plot_layout(widths = c(2.1, 0.9))

  # main patch
  main_patch <- p_all + 
    (riet_patch/alex_patch) + 
    plot_layout(widths = c(1.2, 0.8), guides = "collect")

  # final panel
  main_patch/grid::textGrob(xlab) + plot_layout(heights = c(50,1))
  
}





# ------------------------------------------ #
# ---   Duration Vs. Max Frequency   ------
# ------------------------------------------ #

build_snl_panel(
  data = dt,
  x = duration,
  y = max_freq, 
  time = begin_time,  
  ncx = 10, 
  ncy = 10,
  recording_id = file,
  add_freq_col = TRUE,
  type = "spokes",
  xlab = 'Call Duration (secs)', 
  ylab = "Maximum Frequency (Hz)"
)

ggsave("outputs/duration_vs_maxfreq_snl_spokes.png", 
       device = "png", width = 18, height = 10, units = "in", scale = 1)


build_snl_panel(
  data = dt,
  x = duration,
  y = max_freq, 
  time = begin_time,  
  ncx = 10, 
  ncy = 10,
  recording_id = file, 
  type = "tracks",
  xlab = 'Call Duration (secs)', 
  ylab = "Maximum Frequency (Hz)"
)

ggsave("outputs/duration_vs_maxfreq_snl_tracks.png",
       device = "png", width = 18, height = 10, units = "in", scale = 1)



# ------------------------------------------ #
# ---       PFC Slope Vs. Entropy     ------
# ------------------------------------------ #

build_snl_panel(
  data = dt,
  x = pfc_avg_slope,
  y = avg_entropy, 
  time = begin_time,  
  ncx = 10, 
  ncy = 10,
  recording_id = file, 
  type = "spokes",
  add_freq_col = TRUE,
  xlab = 'Pitch Contour Slope (Hz)', 
  ylab = "Entropy (Hz)"
)


ggsave("outputs/pcslope_vs_entropy_snl_spokes.png", 
       device = "png", width = 18, height = 10, units = "in", scale = 1)



build_snl_panel(
  data = dt,
  x = pfc_avg_slope,
  y = avg_entropy, 
  time = begin_time,  
  ncx = 10, 
  ncy = 10,
  recording_id = file, 
  type = "tracks",
  xlab = 'Pitch Contour Slope (Hz)', 
  ylab = "Entropy (Hz)"
)

ggsave("outputs/pcslope_vs_entropy_snl_tracks.png",
       device = "png", width = 18, height = 10, units = "in", scale = 1)








