#' Plot of data binned over radial polygons
#' 
#' @param data
#' @param x
#' @param y
#' @param n_slices 
#' @param xlab
#' @param ylab
#' @param title
#' @param checking_plot logical, whether to plot auxiliary plot to vizualise
#'   points in slices, for checking purposes
#' @param offset numeric, the offset angle (in radians) to start the slices, anti-clockwise

pizza_plot <- function(data, x, y, n_slices = NULL, xlab = NULL, ylab = NULL, title = NULL,
                       cheking_plot = FALSE, fill_slices = TRUE,
                       plot_points = FALSE,  pnt_col_id = NULL, pnt_col_key = ggplot2::waiver(), pnt_size = 1,
                       lolli = FALSE,
                       add_nr_points = TRUE, fill_pal = MetBrewer::met.brewer("Hokusai2"),
                       xlim = NULL, ylim = NULL,
                       offset = 0){
  
  # Note: In cases where x and y span over very different scales,
  # working with radial geometry is tricky. To ease things up, all
  # derivations are performed on scaled coordinates, which are subsequently
  # back-calculated to the original scales for outputting.
  
  # --- 1. Re-scale points ----
  
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
  
  # --- 2. Generate slices ---
  # Generate angles (in radians) for rays delimiting slices
  rays <- tibble::tibble(
    slice_id = factor(1:(n_slices+1)),
    angle = seq(offset, 2*pi+offset, by = 2*pi/n_slices)
  )
  
  # Add coordinates of points on a circle at given angles.
  #
  # Scaled Coords within [-1, 1], so max possible distance between two points is 
  # sqrt(2^2+2^2) = 2.83. Thus setting radius = 3 so that circle encloses all points. 
  r <- 3
  
  # Rays/circle origin - we want pizza in final plot to be centered at (0,0). 
  # So, setting up the origin of rays to the min-max rescaled value of 0
  x0 <- rescale(0, min = xrange[1], max = xrange[2])
  y0 <- rescale(0, min = yrange[1], max = yrange[2])
  
  # Derive slices polygons
  slices <- rays |>
    dplyr::mutate(
      angle_end = lead(angle)
    ) |>
    dplyr::slice(-n()) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      slice_arc = list(arc(x0, y0, r, angle, angle_end)),
      # connecting at circle centre to create polygon
      slice_poly = list(
        slice_arc |>
          add_row(x = x0, y = y0) |>
          add_row(x = x0, y = y0, .before = 1)
      )
    )
  
  
  # --- 3. Snap points to slices  ----
  
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
      ) |>
    filter(n_points > 0)
    
  
  
  # --- 4. Compute slice-specific summaries  ----
  
  slices <- slices |>
    dplyr::mutate(
      dist_pts_orig = purrr::map(pts_in_slice, ~ sqrt((.$x - x0)^2 + (.$y - y0)^2)),
      max_dist_orig = purrr::map_dbl(dist_pts_orig, max, na.rm = TRUE),
      med_dist_orig = purrr::map_dbl(dist_pts_orig, median, na.rm = TRUE),
      lpct_dist_orig = purrr::map_dbl(dist_pts_orig, quantile, probs = 0.025, na.rm = TRUE),
      upct_dist_orig = purrr::map_dbl(dist_pts_orig, quantile, probs = 0.975, na.rm = TRUE)
    )
  

  
  # --- 5. Generate arcs/polygons for slice summaries  ----
  
  slices <- slices |>
    dplyr::rowwise() |>
    dplyr::mutate(
      max_dist_poly = list(
        arc(x0, y0, max_dist_orig , angle, angle_end) |>
          add_row(x = x0, y = y0) |>
          add_row(x = x0, y = y0, .before = 1)
      ),
      med_dist_arc = list(arc(x0, y0, med_dist_orig, angle, angle_end)),
      lpct_dist_arc = list(arc(x0, y0, lpct_dist_orig, angle, angle_end)),
      upct_dist_arc = list(arc(x0, y0, upct_dist_orig, angle, angle_end)),
      label_pos = list(
        data.frame(
          x =  x0 + max_dist_orig*1.15 * cos((angle_end + angle)/2),
          y =  y0 + max_dist_orig*1.15 * sin((angle_end + angle)/2)
        )
      )
    ) |>
    ungroup()
    


  
  # --- 6. Back-calculate to original scale and isolate for plotting  ----
  
  slice_polys <- slices |>
    select(slice_id, n_points, max_dist_poly) |>
    unnest(max_dist_poly) |>
    mutate(
      x = unrescale(x, xmin = xrange[1], xmax = xrange[2]),
      y = unrescale(y, xmin = yrange[1], xmax = yrange[2])
    )
  
  
  slice_arcs <- slices |>
    select(slice_id, med_dist_arc, lpct_dist_arc, upct_dist_arc ) |>
    unnest(c(med_dist_arc, lpct_dist_arc, upct_dist_arc), names_sep = "_") |>
    mutate(
      across(contains("x"), ~ unrescale(., xmin = xrange[1], xmax = xrange[2])),
      across(contains("y"), ~ unrescale(., xmin = yrange[1], xmax = yrange[2]))
    )
    
  
  slice_pnts <- slices |> 
    select(slice_id, pts_in_slice) |> 
    unnest(pts_in_slice) |>
    mutate(
      x = unrescale(x, xmin = xrange[1], xmax = xrange[2]),
      y = unrescale(y, xmin = yrange[1], xmax = yrange[2])
    )
  
  
  slice_labels <- slices |>
    select(slice_id, n_points, label_pos) |>
    unnest(label_pos) |>
    mutate(
      x = unrescale(x, xmin = xrange[1], xmax = xrange[2]),
      y = unrescale(y, xmin = yrange[1], xmax = yrange[2]),
      label = glue::glue("n = {n_points}")
    )
    
  
  # --- 7. Build Plot  ----
  
  if(is.null(xlab)) xlab <- rlang::as_label(enquo(x))
  if(is.null(ylab)) ylab <- rlang::as_label(enquo(y))
  
  # Base layers
  p <- slice_polys |>
    ggplot2::ggplot() +
    
    # colour scale
    #scale_fill_gradientn(colours = met.brewer("Tam"), name = "Nr. Points") +
    # scale_fill_gradientn(
    #   colours = met.brewer("Homer2", direction = -1),
    #   guide = "none") +
    
    # axis definitions
    ggplot2::scale_x_continuous(breaks = scales::extended_breaks(n = 5)) +
    ggplot2::scale_y_continuous(breaks = scales::extended_breaks(n = 5)) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    ggplot2::labs(x = xlab, y = ylab, title = title) +
    
    #coord_fixed(ratio = diff(xrange)/diff(yrange)) +
    #coord_fixed() +
    
    
    # theme specification
    ggplot2::theme_bw()  #+
    #ggplot2::theme(panel.grid = element_blank())
  
  
  # option to fill slices with colour encoding the number of points 
  if(fill_slices){
    p <- p +
      ggplot2::geom_polygon(
        aes(x = x, y = y, group = slice_id, fill = n_points),
        alpha = 0.9,
        col = "white")+
      scale_fill_stepsn(
        colours = fill_pal,
        n.breaks = 10,
        limits = c(0, 70),
        #breaks = seq(0, 100, by = 10),
        guide = "none"
      ) 
    
  }else{
    p <- p +
      ggplot2::geom_polygon(
        aes(x = x, y = y, group = slice_id), fill = NA,
        col = "black")  
  }
  
  
  # option to add original points 
  if(plot_points){
    # p <- p +
    #   ggplot2::geom_point(
    #   data =  slice_pnts,
    #   aes(x = x, y = y), 
    #   alpha = 0.5
    # )
    
    p <- p +
      ggplot2::geom_point(
      data =  data,
      aes(x = {{x}}, y = {{y}}, col = {{pnt_col_id}}),
      alpha = 0.5,
      size = pnt_size,
      show.legend = FALSE
    ) +
      scale_color_manual(values = pnt_col_key)
    
    if(lolli){
      p <- p +
        ggplot2::geom_segment(
          data =  data,
          aes(x = 0, y = 0, xend = {{x}}, yend = {{y}}, 
              col = {{pnt_col_id}}
              ),
          size = 0.5,
          show.legend = FALSE,
          alpha = 0.2
        )
    }
  }
  
  # option to add labels with nr of points in each sclice
  if(add_nr_points){
   p <- p +
     geom_text(
       data = slice_labels,
       aes(x = x, y = y, label = label),
       size = 9/.pt
     )
  }
  
  # Add slice summary lines and label
  p <- p + 
    ggplot2::geom_path(
      data = slice_arcs,
      aes(x = med_dist_arc_x, y = med_dist_arc_y, group = slice_id),
      size = 1
    ) +
    ggplot2::geom_path(
      data = slice_arcs,
      aes(x = lpct_dist_arc_x, y = lpct_dist_arc_y, group = slice_id),
      linetype = "dashed"
    ) +
    ggplot2::geom_path(
      data = slice_arcs,
      aes(x = upct_dist_arc_x, y = upct_dist_arc_y, group = slice_id),
      linetype = "dashed"
    ) 
  
  
  
  if(cheking_plot){
    
    # base layers
    p_check <- slice_polys |>
      ggplot2::ggplot() +
      ggplot2::geom_polygon(
        aes(x = x, y = y, fill = slice_id),
        col = "black",
        alpha = 0.2
      ) +
      
      coord_fixed(ratio = diff(xrange)/diff(yrange)) +
      ggplot2::labs(x = xlab, y = ylab, title = title) +
      
      # colour scales
      scale_fill_manual(values = met.brewer("Lakota", n = n_slices)) +
      scale_color_manual(values = met.brewer("Lakota", n = n_slices)) +
      
      # theme specification
      ggplot2::theme_bw() 
      
    p_check <- p_check + 
      ggplot2::geom_point(
        data =  slice_pnts,
        aes(x = x, y = y, col = slice_id)
      )
    
    p_check <- p_check + 
      ggplot2::geom_path(
        data = slice_arcs,
        aes(x = med_dist_arc_x, y = med_dist_arc_y, group = slice_id),
        size = 1
      ) +
      ggplot2::geom_path(
        data = slice_arcs,
        aes(x = lpct_dist_arc_x, y = lpct_dist_arc_y, group = slice_id),
        linetype = "dashed"
      ) +
      ggplot2::geom_path(
        data = slice_arcs,
        aes(x = upct_dist_arc_x, y =  upct_dist_arc_y, group = slice_id),
        linetype = "dashed"
      ) 
    
    p_check + p
    
  }else{
    p
  }
  
  
}


# generate points in an arc 
arc <- function(xc = 0, yc = 0, r, theta_start, theta_end, n = 100){
  v <- seq(theta_start, theta_end, length.out = n)
  data.frame(
    x = xc + r * cos(v),
    y = yc + r * sin(v)
  )
}


# # square must be between -1 and 1
# rays_on_sqr <- function(angles){
#   
#   out <- sapply(angles, function(theta){
#     if(0 <= theta && theta < pi/4){
#       c(1, tan(theta))
#     }else if(pi/4 <= theta && theta < pi/2){
#       c(tan(pi/2 - theta), 1)
#     }else if(pi/2 <= theta && theta < 3*pi/4){
#       c(-tan(theta - pi/2), 1)
#     }else if(3*pi/4 <= theta && theta < pi){
#       c(-1, tan(pi-theta))
#     }else if(pi <= theta && theta < 5*pi/4){
#       c(-1, -tan(theta - pi))
#     }else if(5*pi/4 <= theta && theta < 3*pi/2){
#       c(-tan(3*pi/2 - theta), -1)
#     }else if(3*pi/2 <= theta && theta < 7*pi/4){
#       c(tan(theta - 3*pi/2), -1)
#     }else if(7*pi/4 <= theta && theta <= 2*pi){
#       c(1, -tan(2*pi - theta))
#     }
#     else{
#       stop("theta must be within [0, 2*pi]")
#     }
#   })
#   
#   out <- data.frame(t(out))matrix(c(0,0,0,1,1,1,0,0),,2,byrow=TRUE))
#   colnames(out) <- c("x", "y")
#   out
# }
# 
# 
# plot(runif(100, -1, 1), runif(100, -1, 1))
# abline(v = c(-1, 1), col = "blue")
# abline(h = c(-1, 1), col = "blue")
# rays <- seq(0, 2*pi, by = 2*pi/12)
# sapply(tan(rays), abline, a = 0, col = "red")
# pts(rays_on_sqr(rays), col ="red", pch = 19)







