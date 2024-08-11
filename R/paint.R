# library(tidyverse)

deg2rad <- function(deg) {
  2 * pi * deg / 360
}

rad2deg <- function(rad) {
  rad * 360 / (2 * pi)
}

side_length <- function(x, xend, y, yend) {
  sqrt((x-xend)^2 + (y-yend)^2)
}


make_poly <- function(sides, r = 1, mean = 0.3, sd = 0.1) {

  points <- seq(from = 0,
                to = 2 * pi,
                length.out = sides + 1)

  data <- tibble::tibble(
    iteration = 1,
    point = c(1:(length(points)-1), 1),
    angle = points,
    side_variability = abs(rnorm(n = sides + 1, mean = mean, sd = sd)),
    x = cos(points) * r,
    y = sin(points) * r
  )

  return(data)

}


grow_poly <- function(df, iter) {

  # first, need to increment the original point numbers, so
  # we can merge and sort in the new points
  df_old <- df |>
    dplyr::mutate(iteration = iteration + 1,
           point = point + 0:(dplyr::n()-1),
           r = side_length(0, x, 0, y),
           xend = dplyr::lead(x),
           yend = dplyr::lead(y),
           # x = cos(angle) * (1+split_distance),
           # y = sin(angle) * (1+split_distance),
           #           side_length = sqrt( (x - xend)^2 + (y - yend)^2),
           #           side_variability = rnorm(n = sides + 1, mean = .25, sd = .06),
           side_length = side_length(x, xend, y, yend),
           split_point = .5 + rnorm(n = dplyr::n(), sd = .1),
           split_angle = rnorm(n = dplyr::n(), mean = 0, sd = 20),
           split_distance = sqrt(side_length) * side_variability,
           split_x = x - (x - xend) * split_point,
           split_y = y - (y - yend) * split_point,
           new_angle = atan2(split_y - yend, split_x - xend) + pi/2 + deg2rad(split_angle),
           new_xend = split_x + cos(new_angle) * split_distance,
           new_yend = split_y + sin(new_angle) * split_distance) |>
    dplyr::slice(1:(dplyr::n()-1))

  df_new <- df_old |>
    rbind(df_old[1,]) |>
    dplyr::mutate(point = point + 1,
           x = new_xend,
           y = new_yend) |>
    dplyr::slice(1:(dplyr::n()-1))

  df_out <- rbind(df_old, df_new) |>
    dplyr::arrange(point) |>
    rbind(df_old[1,])
    # select(point, angle, x, y, side_length, side_variability)


  return(df_out)

}


splotch <- function(base, iterations = 3) {

  d <- base
  i <- 1
  while(i <= iterations) {
    d <- grow_poly(d)
    i <- i + 1
  }

  return(d)

}

ggsave_to_variable <- function(p, width = 10, height = 10, dpi = 300){
  pixel_width  = (width  * dpi) / 2.54
  pixel_height = (height * dpi) / 2.54

  img <- magick::image_graph(pixel_width, pixel_height, res = dpi)

  on.exit(utils::capture.output({
    grDevices::dev.off()}))
  plot(p)

  return(img)
}
# splotch_rcpp(make_poly(6), 1)
# i <- make_poly(4)
# j1 <- draw::rectangle(width = 5, height = 1) |>
#   dplyr::mutate(point = 1:dplyr::n(),
#                 iteration = 1,
#                 side_variability = runif(dplyr::n(), 0.1, 0.2))
# j2 <- draw::polygon(sides = 8) |>
#   dplyr::mutate(y = y * 3,
#                 point = 1:dplyr::n(),
#                 iteration = 1,
#                 side_variability = runif(dplyr::n(), 0.1, 0.3))
# dab1 <- purrr::accumulate(1:10, ~splotch(., iterations = 1), .init = splotch(j1, 2)) |>
#   dplyr::bind_rows(.id = "iter")
#
# dab2 <- purrr::accumulate(1:10, ~splotch(., iterations = 1), .init = splotch(j2, 2)) |>
#   dplyr::bind_rows(.id = "iter") |>
#   dplyr::mutate(x = x)
#
# library(ggplot2)
# library(ggblend)
#
# dplyr::bind_rows(dab1, dab2, .id = "layer") |>
#   dplyr::mutate(id = paste0(layer, "-", iter)) |>
#   # dplyr::filter(layer==2) |>
#   ggplot(aes(x, y, group = id, fill = layer, partition = layer)) +
#   geom_polygon(alpha = 0.1) * (blend("lighten")) +
#   coord_fixed() +
#   scale_fill_manual(values = c("plum", "tomato")) +
#   theme_void()
#
# ## birds -----
#
# img <- magick::image_read("C:/Users/rober/Desktop/audubon.jpg") |>
#   magick::image_resize("50") |>
#   # magick::image_quantize(max = 10) |>
#   imager::magick2cimg()
#
# img_df <- img |>
#   as.data.frame(wide = "c") |>
#   dplyr::mutate(rgb = rgb(c.1,c.2,c.3))
# # unique(img_df$rgb)
# # pal <- c(
# #   "#C5BBB2",
# #   "#D0CFB4",
# #   "#F4F4ED",
# #   "#ACABA0",
# #   "#B19669",
# #   "#5F5C57",
# #   "#53251E",
# #   "#564833",
# #   "#110D0D",
# #   "#BB3036"
# # )
# points <- img_df |>
#   # dplyr::filter(!rgb %in% pal[c(1:3)]) |>
#   # imager::grayscale() |>
#   # imager::threshold("20%") |>
#   # imager::as.cimg() |>
#   # as.data.frame() |>
#   # dplyr::mutate(value = 1 - value) |>
#   # dplyr::filter(value < .08) |>
#   dplyr::slice_sample(prop = 1) |>
#   dplyr::mutate(rgb = img_df$rgb[x + y * imager::width(img)],
#                 point_number = as.character(1:dplyr::n()))
#
# ggplot(points, aes(x, -y, color = rgb)) +
#   geom_point() +
#   scale_color_identity() +
#   coord_fixed() +
#   theme_void() +
#   theme(legend.position = "none")
#
# # check
# splotch_rcpp(make_poly(6), 1)
# p <- purrr::imap(1:nrow(points), ~make_poly(sides = 6, r = runif(1, 0.5, 0.8), sd = .2) |>
#                    dplyr::mutate(x = x + points$x[.],
#                                  y = y + points$y[.]))
#
# s <- purrr::map(p, ~splotch_rcpp(., 2))
#
# dabs <- purrr::map(s, ~purrr::accumulate(1:5, ~splotch_rcpp(., iterations = 1), .init = .x))
#
# dabs_list <- purrr::map(dabs, dplyr::bind_rows) |>
#   dplyr::bind_rows(.id = "point_number")
#
# # plot_df <- dabs |>
# #   dplyr::left_join(dplyr::select(points, point_number, rgb)) |>
# #   dplyr::mutate(id = paste0(point_number, "-", iteration))
#
# dabs_list |>
#   dplyr::left_join(dplyr::select(points, point_number, rgb)) |>
#   dplyr::mutate(id = paste0(point_number, "-", iteration)) |>
#   # dplyr::filter(as.numeric(point_number) < 3) |>
#   ggplot(aes(x, -y, group = id, fill = rgb, partition = rgb)) +
#   geom_polygon(alpha = 0.2) + #|> blend("darken") +
#   scale_fill_identity() +
#   coord_fixed() +
#   theme_void() +
#   theme(legend.position = "none")
#
#
#
# ### other stuff ----
#
# p1 <- ggsave_to_variable(pic)
# # ggplot2::ggsave(pic, "test1.png")
# magick::image_composite(p2, p1)
# magick::image_write(pic, path = "pic.png")
#
# set.seed(1)
# base <- splotch(base = make_poly(10), 0)
# base1 <- splotch(base = base, 1)
# base2 <- splotch(base = base1, 1)
# base3 <- splotch(base = base2, 1)
# base4 <- splotch(base = base3, 1)
# base5 <- splotch(base = base4, 1)
# base6 <- splotch(base = base5, 1)
#
# library(ggplot2)
# ggplot() +
#   geom_polygon(data = base,
#                mapping = aes(x = x,
#                              y = y),
#                fill = 'plum', alpha = .2) +
#   geom_polygon(data = base1,
#                mapping = aes(x = x,
#                              y = y),
#                fill = 'plum', alpha = .5) +
#   geom_polygon(data = base2,
#                mapping = aes(x = x,
#                              y = y),
#                fill = 'plum', alpha = .2) +
#   geom_polygon(data = base3,
#                mapping = aes(x = x,
#                              y = y),
#                fill = 'plum', alpha = .2) +
#   geom_polygon(data = base4,
#                mapping = aes(x = x,
#                              y = y),
#                fill = 'plum', alpha = .2) +
#   geom_polygon(data = base5,
#                mapping = aes(x = x,
#                              y = y),
#                fill = 'plum', alpha = .2) +
#   geom_polygon(data = base6,
#                mapping = aes(x = x,
#                              y = y),
#                fill = 'plum', alpha = .5) +
#   coord_fixed() +
#   lims(x = c(-3,3), y = c(-3, 3)) +
#   theme_void()

# set.seed(3)
# poly <- make_poly(10)
# #poly$side_variability <- c(2,2,2,2,2,1,1,1,1,1,2)
# base <- splotch(base = poly, 1)
# many <- map_df(.x = 1:20,
#                .f = ~splotch(base = base, iterations = 3),
#                .id = "group") |>
#   mutate(group = as.numeric(group))
# base2 <- splotch(base = base, 1)
# many2 <- map_df(.x = 1:20,
#                .f = ~splotch(base = base2, iterations = 3),
#                .id = "group") |>
#   mutate(group = as.numeric(group) + 20)
# base3 <- splotch(base = base2, 1)
# many3 <- map_df(.x = 1:20,
#                 .f = ~splotch(base = base3, iterations = 3),
#                 .id = "group") |>
#   mutate(group = as.numeric(group) + 40)
#
# all <- rbind(many, many2, many3)
#
#
#
#
# pic <- ggplot() +
#   # geom_polygon(data = many,
#   #              mapping = aes(x = x,
#   #                            y = y,
#   #                            group = group),
#   #              fill = 'thistle', alpha = .01) +
#   # geom_polygon(data = many2,
#   #              mapping = aes(x = x,
#   #                            y = y,
#   #                            group = group),
#   #              fill = 'thistle', alpha = .01) +
#   # geom_polygon(data = many3,
#   #              mapping = aes(x = x,
#   #                            y = y,
#   #                            group = group),
#   #              fill = 'thistle', alpha = .01) +
#   geom_polygon(data = all,
#                mapping = aes(x = x,
#                              y = y,
#                              group = group),
#                fill = 'tomato', alpha = .05) +
#   geom_polygon(data = base,
#                mapping = aes(x = x,
#                              y = y,
#                              group = NA),
#                fill = 'tomato', alpha = .5) +
#   # geom_polygon(data = base3,
#   #              mapping = aes(x = x,
#   #                            y = y,
#   #                            group = NA),
#   #              fill = 'thistle', alpha = .3) +
#   # geom_polygon(data = base2,
#   #              mapping = aes(x = x,
#   #                            y = y,
#   #                            group = NA),
#   #              fill = NA, color = 'black', alpha = .5) +
#   # geom_polygon(data = base3,
#   #              mapping = aes(x = x,
#   #                            y = y,
#   #                            group = NA),
#   #              fill = NA, color = 'red', alpha = .5) +
#   # geom_point(data = base,
#   #              mapping = aes(x = x,
#   #                            y = y,
#   #                            color = factor(point %% 2))) +
#   coord_fixed() +
#   theme_void()
#
# plot(pic)
#
# all2 <- all |>
#   mutate(x = x + 1.5,
#          y = y *1.5 - 2.5,
#          blob = 60) |>
#   bind_rows(mutate(all, blob = 0)) |>
#   arrange(group) |>
#   mutate(group = group + blob,
#          blob = factor(blob))
#
# ggplot(all2) +
#   geom_polygon(aes(x, y, group = group, fill = blob), alpha = .03) +
#   scale_fill_manual(values = c('coral','orchid')) +
#   theme_void() +
#   theme(legend.position = 'none') +
#   coord_fixed()

   # it's a start, but what I want is to grow the number of points with each division



# deprecated --------------------------------------------------------------
#
#
# d <- make_poly(8)
#
# e <- d |>
#   mutate(d = rnorm(n = nrow(.), mean = .5, sd = .15),
#          growth = 1 + abs(rnorm(n = nrow(.), sd = .05)),
#          x_halfway = x * growth + (lead(x) - x) * d,
#          y_halfway = y * growth + (lead(y) - y) * d) |>
#   slice(1:(nrow(.)-1)) |>
#   rbind(.[1,])
#
# e <- deform_poly(d)
#
# pic <- ggplot() +
#   geom_polygon(data = d,
#              mapping = aes(x, y),
#             color = NA, fill = 'thistle', alpha = .5) +
#   # geom_point(data = e,
#   #           mapping = aes(x_halfway, y_halfway)) +
#   geom_polygon(data = e,
#             mapping = aes(x, y),
#             color = NA, fill = 'thistle', alpha = .5) +
#   coord_fixed() +
#   theme_void()
#
# plot(pic)

#
# dd <- grow_poly(d) # 1 round
# ddd <- grow_poly(dd) # 2 rounds
# dddd <- grow_poly(ddd) # 3 rounds
# ddddd <- grow_poly(dddd) # 4 rounds
# dddddd <- grow_poly(ddddd) # 5 rounds
# ddddddd <- grow_poly(dddddd) # 6 rounds

# deform_poly <- function(df) {
#
#   df |>
#     mutate(d = rnorm(n = nrow(.), mean = .5, sd = .15),
#            growth = 1 + abs(rnorm(n = nrow(.), sd = .05)),
#            x = x * growth + (lead(x) - x) * d,
#            y = y * growth + (lead(y) - y) * d) |>
#     slice(1:(nrow(.)-1)) |>
#     rbind(.[1,]) |>
#     select(point, x, y)
#
# }

# so this takes the initial shape, and deforms it independently a bunch of times
# data <- accumulate(
#   .x = 1:30,
#   .f = ~deform_poly(.),
#   .init = make_poly(8)
# ) |>
#   bind_rows(.id = "group")

