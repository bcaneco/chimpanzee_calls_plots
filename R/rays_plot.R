#' @param data
#' @param x
#' @param y
#' @param xlab
#' @param ylab
#' @param title

rays_plot <- function(data, x, y, n_slices = 8, offset = pi/n_slices, 
                       x_orig = 0, y_orig = 0, show_slices = TRUE, col_lolli = TRUE, 
                       point_size = 1.5,
                       xlab = NULL, ylab = NULL, title = NULL){
  
  # --- Pre-processing ---
  
  # drop rows with NAs in variable of interest
  data <- tidyr::drop_na(data, {{x}}, {{y}})
  
  # Isolate point coords
  pts <- dplyr::select(data, {{x}}, {{y}} )
  
  # get range on each axis
  xrange <- range(pts[[1]], na.rm = TRUE)
  yrange <- range(pts[[2]], na.rm = TRUE)
  
  
  # Rescale coordinates to range [-1, 1] using min-max normalization
  pts <- pts |>
    dplyr::mutate(
      x_scaled = rescale({{x}}, min = xrange[1], max = xrange[2]),
      y_scaled = rescale({{y}}, min = yrange[1], max = yrange[2])
    )
  
  
  # --- Generate rays ---
  # Generate angles for rays delimiting slices
  rays <- tibble::tibble(
    slice_id = factor(1:(n_slices+1)),
    angle = seq(offset, 2*pi+offset, by = 2*pi/n_slices)
  )
  
  r <- 5 # max(c(xrange, yrange))
  
  # So, setting up the origin of rays to the min-max rescaled value of 0
  x0 <- rescale(x_orig, min = xrange[1], max = xrange[2])
  y0 <- rescale(y_orig, min = yrange[1], max = yrange[2])
  
  # generate data with slices' polygons
  slices <- rays |>
    dplyr::mutate(
      angle_end = lead(angle)
    ) |>
    dplyr::slice(-n()) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      slice_poly = list(
        data.frame(
          x = c(x0, x0 + r * cos(c(angle, angle_end)), x0),
          y = c(y0, y0 + r * sin(c(angle, angle_end)), y0)
        )
      )
    )
  
  # --- Snap points to slices  ----
  
  # convert polygons to {sf} object
  slices_polys_sf <- slices |>
    dplyr::select(slice_id, slice_poly) |>
    tidyr::unnest(slice_poly) |>
    sfheaders::sf_polygon(polygon_id = "slice_id")
  
  # convert points {sf} object
  pnts_sf <- pts |>
    dplyr::select(x_scaled, y_scaled) |>
    as.matrix() |>
    sfheaders::sfc_point()
  
  # Get points inside each slice
  pts_in_slices <- sf::st_contains(slices_polys_sf, pnts_sf) |>
    purrr::map(\(x){
      sfheaders::sfc_to_df(pnts_sf[x]) |>
        dplyr::select(x, y)
    }) |>
    map(function(x){
      drop_na(x)
    })
  
  # Update slice data and drop empty slices 
  slices <- slices |>
    dplyr::ungroup() |>
    dplyr::mutate(
      pts_in_slice = pts_in_slices,
      n_points = purrr::map_int(pts_in_slice, nrow)
    )
  
  
  # --- Back-calculate to original scale and isolate for plotting  ----
  
  slice_polys <- slices |>
    select(slice_id, n_points, slice_poly) |>
    unnest(slice_poly) |>
    mutate(
      x = unrescale(x, xmin = xrange[1], xmax = xrange[2]),
      y = unrescale(y, xmin = yrange[1], xmax = yrange[2])
      #x = x*diff(xrange) + xrange[1],
      #y = y*diff(yrange) + yrange[1]
    )
  
  
  slice_pnts <- slices |> 
    select(slice_id, pts_in_slice) |> 
    unnest(pts_in_slice) |>
    mutate(
      x = unrescale(x, xmin = xrange[1], xmax = xrange[2]),
      y = unrescale(y, xmin = yrange[1], xmax = yrange[2])
    )
  
  
  # --- Build Plot  ----
  if(is.null(xlab)) xlab <- rlang::as_label(enquo(x))
  if(is.null(ylab)) ylab <- rlang::as_label(enquo(y))
  
  # Base layers
  p <- data |>
    tidyr::drop_na({{x}}) |>
    ggplot2::ggplot(aes(x = {{x}}, y = {{y}})) +
    
    # axis definitions
    ggplot2::labs(x = xlab, y = ylab, title = title) +
    coord_cartesian(xlim = xrange, ylim = yrange) +
    
    #ggplot2::coord_fixed(ratio = diff(xrange)/diff(yrange), xlim = xrange, ylim = yrange) +
    
    # theme specification
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = element_blank())
    
  
  if(show_slices){
    
    # Add slices
    p <- p +
      ggplot2::geom_polygon(
        data = slice_polys,
        aes(x = x, y = y, group = slice_id, fill = slice_id), 
        show.legend = FALSE, 
        alpha = 0.25,
        colour = "white"
      ) +
      # ggplot2::scale_fill_manual(
      #   values = colortools::setColors("firebrick2", num = n_slices)
      # )
    ggplot2::scale_fill_manual(
      values = MetBrewer::met.brewer("Johnson", n = n_slices)
      #values = MetBrewer::met.brewer("Signac", n = n_slices)
      #values = MetBrewer::met.brewer("VanGogh", n = n_slices)
      #values = MetBrewer::met.brewer("Redon", n = n_slices)
    )
  }
  
  
  
  # Add lollipops
  if(col_lolli){
    
    p <- p +
      ggplot2::geom_segment(
        data = slice_pnts, 
        aes(x = x_orig, y = x_orig, xend = x, yend = y, colour = slice_id),
        size = 0.5,
        show.legend = FALSE
      ) +
      ggplot2::geom_point(
        data = slice_pnts, 
        aes(x = x, y = y, col = slice_id),
        size = point_size,
        show.legend = FALSE) +
      ggplot2::scale_color_manual(
        values = colortools::setColors("firebrick2", num = n_slices)
        #values = MetBrewer::met.brewer("Signac", n = n_slices)
      )
    
  }else{
    p <- p +
      ggplot2::geom_segment(
        aes(x = x_orig, y = x_orig, xend = {{x}}, yend = {{y}}),
        color = "grey55",
        size = 0.2
      ) +
      ggplot2::geom_point(
        aes(x = {{x}}, y = {{y}}),
        col = "black", 
        size = point_size)  
  }
  
  p
  
}







