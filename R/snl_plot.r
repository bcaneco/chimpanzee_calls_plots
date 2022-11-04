#' 'Snakes and Ladders' plot for acoustic data from orangutan calls
#'
#' @param data
#' @param x
#' @param y
#' @param ncx,ncy
#' @param recording_id <data-masking> Name of variable identifying the recorded file.
#' @param time <data-masking> Name of variable defining call time in recording.
#'   Used to sort data chronologically within each recorded file

snl_plot <- function(data, x, y, time, recording_id = NULL,
                     ncx = 20, ncy = ncx, xlim = NULL, ylim = NULL, 
                     arrow_fctr = 1, cellcent_fct = 1, axis_nbreaks_fct = 1,
                     type = "spokes", add_freq_col = FALSE, xlab = NULL, 
                     ylab = NULL, title = NULL){
  
  # browser()
  
  # group by recording file and sort by recording time
  data <- data |>
    dplyr::group_by( {{recording_id}} ) |>
    dplyr::arrange( {{time}}, .by_group = TRUE) |>
    dplyr::ungroup()
  
  # isolate point coords
  xy <- dplyr::select(data, {{x}}, {{y}} )
  
  # set range of grid based on 2% extension of ranges in data on all margins
  if(is.null(xlim)) xlim <- range(xy[[1]]) * c(0.98, 1.02)  
  if(is.null(ylim)) ylim <- range(xy[[2]]) * c(0.98, 1.02)
  
  # get grid resolution, based on number of cells across each axis
  res <- c(diff(xlim)/ncx, diff(ylim)/ncy)
  
  # generate grid cells data
  gridcells <- make_grid(xlim, ylim, res)

  # snap points to cells, returning the index of cells
  cell_idx <- apply(
    gridcells[, 3:6], 
    1, 
    \(c){ pts_in_cell(xy, c)}
  ) |>
    apply(
      1, 
      \(c){ which(c)[1]}
    )
  
  # 1. append cells data to original dataset 
  # 2. compute distance and angle to subsequent cell
  data <- data |>
    tibble::add_column(
      cell_idx,
      gridcells[cell_idx, ]
      ) |>
    dplyr::group_by( {{recording_id}} ) |>
    mutate(
      sbsq_cell_x = lead(cell_x),
      sbsq_cell_y = lead(cell_y),
      sbsq_cell_dist = sqrt((cell_x - sbsq_cell_x)^2 + (cell_y - sbsq_cell_y)^2),
      sbsq_cell_angle = atan2(y = sbsq_cell_y - cell_y, x = sbsq_cell_x - cell_x),
      sbsq_cell_angle = if_else(sbsq_cell_angle < 0, sbsq_cell_angle + 2*pi, sbsq_cell_angle)
    ) |>
    # drop last point, as there is no subsequent cell to go to
    slice(-n())
  
  # if spokes plot-type, calculate radius distances
  # This is computed as the distance between the intersection of a line linking
  # two cells and the margins of the first cell
  if(type == "spokes"){
    data <- data |>
      filter(sbsq_cell_dist > 1e-6) |> # need to drop rows with static movement
      rowwise() |>
      mutate(
        spoke_radius = get_radius(
          sbsq_cell_x, sbsq_cell_y, cell_xmin, cell_ymin, cell_xmax, cell_ymax
        )
      )
    
    if(add_freq_col){
      data <- data |>
        group_by(cell_x, cell_y, sbsq_cell_angle, spoke_radius) |>
        summarise(n = n(), .groups = "keep")
    }
  }
  
  
  # build plot
  
  if(is.null(xlab)) xlab <- rlang::as_label(enquo(x))
  if(is.null(ylab)) ylab <- rlang::as_label(enquo(y))
  
  p <- gridcells |>
    # base layers
    ggplot2::ggplot(ggplot2::aes(x = cell_x, y = cell_y)) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = cell_xmin, xmax = cell_xmax, ymin = cell_ymin, ymax = cell_ymax),
      col = "white", 
      #linetype = "dashed",
      fill = "grey95"
    ) +
    geom_point(col = "white", size = 1.5 * cellcent_fct) +
    
    #geom_point(data = data, aes(x = {{x}}, y = {{y}}), col = "blue", alpha = 0.3) +
    
    # axis definition
    ggplot2::labs(x = xlab, y = ylab, title = title) +
    ggplot2::scale_x_continuous(
      expand = expansion(mult = c(0, 0)),
      breaks = scales::extended_breaks(min(ncx * axis_nbreaks_fct, 20))
    ) +
    ggplot2::scale_y_continuous(
      expand = expansion(mult = c(0, 0)),
      breaks = scales::extended_breaks(min(ncy * axis_nbreaks_fct, 20))
    ) +
    
    #ggplot2::coord_fixed(ratio = res[1]/res[2]) +
    
    # theme specification
    ggplot2::theme_bw() + 
    ggplot2::theme(panel.grid = element_blank())
    

  # Spoke plot
  if(type == "spokes"){
    
    if(!add_freq_col){
      p <- p +
        ggplot2::geom_spoke(
          data = data,
          ggplot2::aes(angle = sbsq_cell_angle, radius = spoke_radius*0.9),
          size = 0.7,
          arrow = arrow(length = unit(0.008 * arrow_fctr, "npc"), type = "closed", angle = 20),
          alpha = 0.25
        ) #+
        # ggplot2::geom_point(
        #   data = data,
        #   ggplot2::aes(x = cell_x, y = cell_y), col = "#575cf7", size = 1
        # )
    }else{
      p <- p +
        ggplot2::geom_spoke(
          data = data,
          ggplot2::aes(angle = sbsq_cell_angle, radius = spoke_radius*0.9, col = n), 
          size = 0.7,
          arrow = arrow(length = unit(0.008 * arrow_fctr, "npc"), type = "closed", angle = 20)
        ) +
        # scale_colour_gradientn(
        #   name = "Freq.",
        #   colours = c(rcartocolor::carto_pal(name = "DarkMint")[-c(1, 2)], "#000501"),
        #   limits = c(1, 20)
        # )
        scale_colour_stepsn(
          name = "Freq.",
          #colours = c("grey85", "grey42", "grey1"),
          #colours = c(MetBrewer::met.brewer("Hokusai2")[-1], "#000501"),
          colours = c(rcartocolor::carto_pal(name = "DarkMint")[-c(1, 2)], "#000501"),
          breaks = seq(5, 20, 5), 
          limits = c(1, 30))
    }
  }
  
  # segment plot
  if(type == "tracks"){
    
    p <- p +
      ggplot2::geom_curve(
        data = filter(data, sbsq_cell_dist > 1e-6), # need to drop rows with static movement
        ggplot2::aes(
          x = cell_x, 
          y = cell_y, 
          xend = sbsq_cell_x, 
          yend = sbsq_cell_y, 
          col = sbsq_cell_angle*180/pi
          ),
        arrow = arrow(length = unit(0.01 * arrow_fctr, "npc"), type = "closed", angle = 20),
        curvature = 0.1,
        #col = "#575cf7",
        #size = 0.8
      ) +
      scale_colour_stepsn(
        colors = MetBrewer::met.brewer("Johnson"), #, type = "continuous"),
        name = "Direction\n(deg)",
        limits = c(0, 360),
        #n.breaks = 6,
        breaks = seq(0, 360, by = 45),
        show.limits = TRUE
      )
    
    
    # p <- p +
    #   geom_segment(
    #     data = data,
    #     aes(x = cell_x, y = cell_y, xend = sbsq_cell_x, yend = sbsq_cell_y),
    #     arrow = arrow(length = unit(0.005, "npc"), type = "closed")
    #   )
    
    # p <- p +
    #   ggforce::geom_link(
    #     data = data,
    #     aes(xend = sbsq_cell_x,
    #         yend = sbsq_cell_y,
    #         alpha = stat(index))
    #     #col = "#575cf7",
    #     #arrow = arrow(length = unit(0.005, "npc"), type = "closed")
    #   )
    
    # p <- p +
    #   ggforce::geom_diagonal(
    #     data = data,
    #     aes(xend = sbsq_cell_x, yend = sbsq_cell_y, alpha = after_stat(index)),
    #     arrow = arrow(length = unit(0.005, "npc"), type = "closed")
    #     )

  }
  p 
}




# Function stolen and converted to R from
# https://stackoverflow.com/questions/1585525/how-to-find-the-intersection-point-between-a-line-and-a-rectangle/31254199#31254199
#
# Finds the intersection point between a rectangle with parallel sides to the x
# and y axes  and the half-line pointing towards (x,y) originating from the middle of the rectangle
#
# Note: the function works given min[XY] <= max[XY] even though minY may not be
# the "top" of the rectangle because the coordinate system is flipped.
#
# Note: if the input is inside the rectangle, the line segment wouldn't have an
# intersection with the rectangle, but the projected half-line does.
#
# Warning: passing in the middle of the rectangle will return the midpoint itself
# there are infinitely many half-lines projected in all directions,  so let's just shortcut to midpoint (GIGO).
#
# @param x Number x coordinate of point to build the half-line from
# @param y Number y coordinate of point to build the half-line from
# @param minX Number the "left" side of the rectangle
# @param minY Number the "top" side of the rectangle
# @param maxX Number the "right" side of the rectangle
# @param maxY Number the "bottom" side of the rectangle
# @param validate: boolean (optional) whether to treat point inside the rect as error
#
# @return an object with x and y members for the intersection

# @author TWiStErRob
# @licence Dual CC0/WTFPL/Unlicence, whatever floats your boat
# @see <a href="http://stackoverflow.com/a/31254199/253468">source</a>
  # @see <a href="http://stackoverflow.com/a/18292964/253468">based on</a>
    #/


pointOnRect <- function(x, y, minX, minY, maxX, maxY, validate = TRUE) {
  # assert minX <= maxX
  # assert minY <= maxY 
  if (validate && (minX < x && x < maxX) && (minY < y && y < maxY))
    stop(glue::glue("Point [{x}, {y}] cannot be inside the rectangle: [{minX}, {minY}] - [{maxX}, {maxY}]"))
  
  midX <- (minX + maxX) / 2
  midY <- (minY + maxY) / 2
  
  #if (midX - x == 0) -> m == ±Inf -> minYx/maxYx == x (because value / ±Inf = ±0)
  m <- (midY - y) / (midX - x)
  
  if (x <= midX) { # check "left" side
    minXy <- m * (minX - x) + y
    if (minY <= minXy && minXy <= maxY)
      return(c(minX, minXy))
  }
  
  if (x >= midX) { # check "right" side
    maxXy <- m * (maxX - x) + y
    if (minY <= maxXy && maxXy <= maxY)
      return(c(maxX, maxXy))
  }
  
  if (y <= midY) { # check "top" side
    minYx <- (minY - y) / m + x
    if (minX <= minYx && minYx <= maxX)
      return(c(minYx, minY))
  }
  
  if (y >= midY) { # check "bottom" side
    maxYx <- (maxY - y) / m + x
    if (minX <= maxYx && maxYx <= maxX)
      return(c(maxYx, maxY))
  }
  
  # edge case when finding midpoint intersection: m = 0/0 = NaN
  if (x == midX && y == midY) return(c(x, y))
  
  # Should never happen :) If it does, please tell me!
  stop(glue::glue("Cannot find intersection for [{x},{y}] inside rectangle [{minX}, {minY}] - [{maxX}, {maxY}]"))
}



# Wraps around `pointOnRect` to calculate distance between intersection point
# and cell centre
get_radius <- function(x, y, xmin, ymin, xmax, ymax){
  
  # intersection point between cell and line
  intrsct <- pointOnRect(x, y, xmin, ymin, xmax, ymax)
  
  # centre coordinates
  xy <- c((xmin + xmax)/2, (ymin + ymax)/2)
  
  # distance between intersection and cell centre
  sqrt(sum((xy - intrsct)^2))
  
}




# --------------------------------------------------------------------------------
make_grid <- function(xlim, ylim, res){
  
  # some obvious checks
  if(res[1] > diff(xlim)){
    stop("Cell width must be smaller than the range of x")
  }
  
  if(res[2] > diff(ylim)){
    stop("Cell height must be smaller than the range y")
  }
  
  # isolate cell width and height
  w <- res[1]
  h <- res[2]
  
  # generate grid centres 
  expand.grid(
    cell_x = seq(xlim[1] + w/2, xlim[2] - w/2, w), 
    cell_y = seq(ylim[1] + h/2, ylim[2] - h/2, h) 
  ) |>
    # add cells' borders
    dplyr::mutate(
      cell_xmin = cell_x - w/2,
      cell_xmax = cell_x + w/2,
      cell_ymin = cell_y - h/2,
      cell_ymax = cell_y + h/2   
    )
}


#make_grid(xlim = c(0, 10), ylim = c(0,10), res = c(0.5, 0.5))


## ================================================
# dplyr::between is very efficient due to its implementation in c++ 

pts_in_cell <- function(xy, cell_lims){
  
  x <- xy[[1]]
  y <- xy[[2]]
  xmin <- cell_lims[[1]]
  xmax <- cell_lims[[2]]
  ymin <- cell_lims[[3]]
  ymax <- cell_lims[[4]]
  
  dplyr::between(x, xmin, xmax) & 
    dplyr::between(y, ymin, ymax)
}








