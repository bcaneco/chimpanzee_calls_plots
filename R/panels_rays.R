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


# ----------------------------- #
# ---    Panel Building     -----
# ----------------------------- #

build_rays_panel <- function(data, x, y, n_slices, offset = pi/n_slices, col_lolli = FALSE,
                              xlab = NULL, ylab = NULL, title = NULL){
  
  # overall plot
  p_all <- data |>
    rays_plot(x = {{x}}, y = {{y}}, n_slices = n_slices, show_slices = TRUE, 
      col_lolli = col_lolli,xlab = "", ylab = ylab, title = "All Sessions")
  

  # Riet - all sessions
  p_riet <- data |>
    filter(subject == "Riet") |>
    rays_plot(x = {{x}}, y = {{y}}, n_slices = n_slices, show_slices = TRUE, 
               col_lolli = col_lolli, xlab = "", ylab = "", title =  "Riet Sessions")
  
  
  # Alex - All sessions
  p_alex <- data |>
    filter(subject == "Alex") |>
    rays_plot(x = {{x}}, y = {{y}}, n_slices = n_slices, show_slices = TRUE, 
               col_lolli = col_lolli, xlab = "", ylab = "", title =  "Alex Sessions")
  
  
  # Riot - session 1
  p_riet_s1 <- data |>
    filter(subject == "Riet", session == 1) |>
    rays_plot(x = {{x}}, y = {{y}}, n_slices = n_slices, show_slices = TRUE, 
               col_lolli = col_lolli, point_size = 1,  xlab = "", ylab = "", title =  "RS 1")
  
  
  # Riot - session 2
  p_riet_s2 <- data |>
    filter(subject == "Riet", session == 2) |>
    rays_plot(x = {{x}}, y = {{y}}, n_slices = n_slices, show_slices = TRUE, 
               col_lolli = col_lolli, point_size = 1, xlab = "", ylab = "", title =  "RS 2")
  
  
  # Alex - session 1
  p_alex_s1 <- data |>
    filter(subject == "Alex", session == 1) |>
    rays_plot(x = {{x}}, y = {{y}}, n_slices = n_slices, show_slices = TRUE, 
               col_lolli = col_lolli, point_size = 1, xlab = "", ylab = "", title =  "AS 1")
  
  
  # Alex - session 2
  p_alex_s2 <- data |>
    filter(subject == "Alex", session == 2) |>
    rays_plot(x = {{x}}, y = {{y}}, n_slices = n_slices, show_slices = TRUE, 
               col_lolli = col_lolli, point_size = 1, xlab = "", ylab = "", title =  "AS 2")
  
  
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
    plot_layout(widths = c(1.15, 0.85), guides = "collect")
  
  # final panel
  main_patch/grid::textGrob(xlab) + plot_layout(heights = c(50,1))
  
}



# --------------------------------------------- #
# --- Delta Duration Vs. Delta Frequency   ------
# --------------------------------------------- #

build_rays_panel(
  data = dt, 
  x = delta_duration, 
  y = delta_voice_freq,
  n_slices = 8,  
  xlab = expression(Delta ~ "Duration (secs)"),
  ylab = expression(Delta ~ "Max Frequency (Hz)")
) 

ggsave("outputs/delta_duration_vs_delta_maxfreq_rays.png", 
       device = "png", width = 17, height = 10, units = "in", scale = 1)



# ---------------------------------------------------- #
# ---     Delta PC Slope Vs. Delta Entropy        ------
# ---------------------------------------------------- #

build_rays_panel(
  data = dt,
  x = delta_voice_inflexion, 
  y = delta_voice_entropy,
  n_slices = 8,  
  xlab = expression(Delta ~ "Pitch Counter Slope (Hz)"),
  ylab = expression(Delta ~ "Entropy (Hz)"),
) 


ggsave("outputs/delta_pcslope_vs_delta_entropy_rays.png", 
       device = "png", width = 17, height = 10, units = "in", scale = 1)




# -------------------------------------------------------- #
# ---  Pct Change Duration Vs. Pct Change Frequency   ------
# --------------------------------------------------------- #

build_rays_panel(
  data = dt, 
  x = pc_duration, 
  y = pc_voice_freq,
  n_slices = 8,  
  xlab = "Change in Duration (%)",
  ylab = "Change in Max Frequency (%)"
) 

ggsave("outputs/pctchange_duration_vs_pctchange_maxfreq_rays.png", 
       device = "png", width = 17, height = 10, units = "in", scale = 1)






# ------------------------------------------------------------- #
# ---   Pct Change PC Slope Vs. Pct Change Voice Entropy   ------
# ------------------------------------------------------------- #

build_rays_panel(
  data = dt, 
  x = pc_voice_inflexion, 
  y = pc_voice_entropy,
  n_slices = 8,  
  xlab = "Change in Pitch Contour Slope (%)",
  ylab = "Change in Entropy (%)"
) 

ggsave("outputs/pctchange_pcslope_vs_pctchange_entropy_rays.png", 
       device = "png", width = 17, height = 10, units = "in", scale = 1)

