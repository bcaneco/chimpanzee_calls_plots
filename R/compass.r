compass <- function(n_slices, offset = pi/n_slices){
  
  tibble::tibble(
    slice_id = factor(1:(n_slices+1)),
    angle_start = seq(offset, 2*pi+offset, by = 2*pi/n_slices),
    angle_end = lead(angle_start)
  ) |>
    dplyr::slice(-n()) |>
    mutate(
      color = colortools::setColors("firebrick3", n_slices)
    )
  
}