
# ------------------------ #
# ---     Preamble    ------
# ------------------------ #

library(tidyverse)
library(MetBrewer)
library(patchwork)
library(sf)
library(sfheaders)
library(colortools)

dt <- read_csv("data/calls_data.csv", show_col_types = FALSE) %>%
  mutate(session = as.factor(session))

sapply(list.files("R/", full.names = TRUE), source)



# ------------------------------------------ #
# ---   Local function building panels  ------
# ------------------------------------------ #

build_snl_panel <- function(data, x, y, time, recording_id = NULL, ncx = 20, ncy = ncx,
                            #fixed_limits = TRUE,
                            type = "spokes", xlab = NULL, ylab = NULL){
  
  # overall plot
  p_all <- dt |>
    snl_plot(x = {{x}}, y = {{y}}, time = {{time}}, recording_id = {{recording_id}}, 
             ncx = ncx, ncy = ncy, type = type, xlab = "", ylab = ylab, 
             title = "Overall") 
  
  xlim_overall <- ggplot_build(p_all)$layout$panel_scales_x[[1]]$range$range
  ylim_overall <- ggplot_build(p_all)$layout$panel_scales_y[[1]]$range$range  
  
  # # define axes limits of remaining plots
  # if(free_xlim){
  #   # axes limits fixed to those of overall plot
  #   xlim <- ggplot_build(p_all)$layout$panel_scales_x[[1]]$range$range
  #   ylim <- ggplot_build(p_all)$layout$panel_scales_y[[1]]$range$range  
  # }else{
  #   # axes limits will be defined internally by data under use
  #   xlim <- NULL
  #   ylim <- NULL
  # }
  
  # Riet - all sessions
  p_riet <- dt |>
    filter(subject == "Riet") |>
    snl_plot(x = {{x}}, y = {{y}}, time = {{time}}, recording_id = {{recording_id}}, 
             ncx = ncx, ncy = ncy, type = type, xlab = "", ylab = "", 
             xlim = xlim_overall, ylim = ylim_overall, arrow_fctr = 1.8, cellcent_fct = 0.8,
             title = "Riet") 
  
  # Alex - All sessions
  p_alex <- dt |>
    filter(subject == "Alex") |>
    snl_plot(x = {{x}}, y = {{y}}, time = {{time}}, recording_id = {{recording_id}}, 
             ncx = ncx, ncy = ncy, type = type, xlab = "", ylab = "", 
             xlim = xlim_overall, ylim = ylim_overall, arrow_fctr = 1.8, cellcent_fct = 0.8,
             title = "Alex") 
  
  # Riot - session 1
  p_riet_s1 <- dt |>
    filter(subject == "Riet", session == 1) |>
    snl_plot(x = {{x}}, y = {{y}}, time = {{time}}, recording_id = {{recording_id}}, 
             ncx = ncx, ncy = ncy, type = type, xlab = "", ylab = "", 
             xlim = NULL, ylim = NULL, arrow_fctr = 3, cellcent_fct = 0.5,
             title = "Session 1") 
  
  # Riot - session 2
  p_riet_s2 <- dt |>
    filter(subject == "Riet", session == 2) |>
    snl_plot(x = {{x}}, y = {{y}}, time = {{time}}, recording_id = {{recording_id}}, 
             ncx = ncx, ncy = ncy, type = type, xlab = "", ylab = "", 
             xlim = NULL, ylim = NULL, arrow_fctr = 3, cellcent_fct = 0.5,
             title = "Session 2") 
  
  
  # Alex - session 1
  p_alex_s1 <- dt |>
    filter(subject == "Alex", session == 1) |>
    snl_plot(x = {{x}}, y = {{y}}, time = {{time}}, recording_id = {{recording_id}}, 
             ncx = ncx, ncy = ncy, type = type, xlab = "", ylab = "", 
             xlim = NULL, ylim = NULL, arrow_fctr = 3, cellcent_fct = 0.5,
             title = "Session 1") 
  
  # Alex - session 2
  p_alex_s2 <- dt |>
    filter(subject == "Alex", session == 2) |>
    snl_plot(x = {{x}}, y = {{y}}, time = {{time}}, recording_id = {{recording_id}}, 
             ncx = ncx, ncy = ncy, type = type, xlab = "", ylab = "", 
             xlim = NULL, ylim = NULL, arrow_fctr = 3, cellcent_fct = 0.5,
             title = "Session 2") 
  
  
  # patchwork 
  
  # session patches
  riet_session_patch <- p_riet_s1/p_riet_s2
  alex_session_patch <- p_alex_s1/p_alex_s2 

  # subject patches
  riet_patch <- p_riet + riet_session_patch +
    plot_layout(widths = c(2,1))
  alex_patch <- p_alex + alex_session_patch +
    plot_layout(widths = c(2,1))

  # main patch
  main_patch <- p_all + 
    (riet_patch/alex_patch) + 
    plot_layout(widths = c(1.2, 0.8), guides = "collect")

  # final panel
  main_patch/grid::textGrob(xlab) + plot_layout(heights = c(50,1))
  
}





# ------------------------------------------ #
# ---   Duration Vs. Average Entropy   ------
# ------------------------------------------ #

build_snl_panel(
  data = dt,
  x = duration,
  y = max_freq, 
  time = begin_time,  
  ncx = 10, 
  ncy = 10,
  recording_id = file, 
  type = "spokes",
  xlab = 'Call Duration (secs)', 
  ylab = "Maximum Frequency (Hz)"
)

ggsave("outputs/duration_vs_maxfreq_snl_spokes.png", 
       device = "png", width = 17, height = 10, units = "in", scale = 1)


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
       device = "png", width = 17, height = 10, units = "in", scale = 1)



# ------------------------------------------ #
# ---   Duration Vs. pfc_avg_slope  ------
# ------------------------------------------ #

build_snl_panel(
  data = dt,
  x = duration,
  y = pfc_avg_slope, 
  time = begin_time,  
  ncx = 10, 
  ncy = 10,
  recording_id = file, 
  type = "spokes",
  xlab = 'Duration (secs)', 
  ylab = "Voice Inflection (Hz)"
)


ggsave("outputs/duration_vs_pfcslope_snl_spokes.png", 
       device = "png", width = 17, height = 10, units = "in", scale = 1)



build_snl_panel(
  data = dt,
  x = duration,
  y = pfc_avg_slope, 
  time = begin_time,  
  ncx = 10, 
  ncy = 10,
  recording_id = file, 
  type = "tracks",
  xlab = 'Call Duration (secs)', 
  ylab = "Voice Inflection (Hz)"
)

ggsave("outputs/duration_vs_pfcslope_snl_tracks.png",
       device = "png", width = 17, height = 10, units = "in", scale = 1)




# ------------------------------------------ #
# ---   Duration Vs. Average Entropy  ------
# ------------------------------------------ #

build_snl_panel(
  data = dt,
  x = duration,
  y = avg_entropy, 
  time = begin_time,  
  ncx = 10, 
  ncy = 10,
  recording_id = file, 
  type = "spokes",
  xlab = 'Call Duration (secs)', 
  ylab = "Voice Entropy (Hz)"
)

ggsave("outputs/duration_vs_entropy_snl_spokes.png",
       device = "png", width = 17, height = 10, units = "in", scale = 1)



build_snl_panel(
  data = dt,
  x = duration,
  y = avg_entropy, 
  time = begin_time,  
  ncx = 10, 
  ncy = 10,
  recording_id = file, 
  type = "tracks",
  xlab = 'Call Duration (secs)', 
  ylab = "Voice Entropy (Hz)"
)

ggsave("outputs/duration_vs_entropy_snl_tracks.png",
       device = "png", width = 17, height = 10, units = "in", scale = 1)






