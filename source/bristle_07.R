# set up ------------------------------------------------------------------

name <- "bristle"    # only change this for an entirely new system
version <- 7         # increment to 2 for "my-art-system_02.R", etc
format <- "png"      # png is usually a good choice!

# define common helper functions
source(here::here("source", "common.R"), echo = FALSE)

# make sure we haven't accidentally messed up the versions
assert_version_consistency(name)


# define the art system ---------------------------------------------------

art_generator <- function(seed) {

  set.seed(seed)

  n_strokes <- 100

  r_colors <- colours(distinct = TRUE)
  r_colhsv <- rgb2hsv(col2rgb(colours(distinct = TRUE)))
  base_colors <- r_colors[
    r_colhsv["s", ] > .2 & 
    r_colhsv["s", ] <  1 & 
    r_colhsv["v", ] > .2 &
    r_colhsv["v", ] < .9
  ]
  base <- sample(base_colors, 1)

  palette <- sketchpad::palette_harmony(
    base = base, scheme = "analogous", n = sample(3:6, 1)
  )
  palette <- sample(palette, n_strokes + 1L, replace = TRUE)

  #sketchpad::draw(sketchpad::effect_swatch(color = palette))

  locs_sml <- tidyr::expand_grid(
    x_mid = -5:5,
    y_mid = -2:2
  ) |> 
    dplyr::mutate(
      x_min = x_mid - .5,
      y_min = y_mid - .5,
      x_max = x_mid + .5,
      y_max = y_mid + .5,
      size = 1
    )
  
  locs_lrg <- tidyr::expand_grid(
    x_mid = -3:3,
    y_mid = -1:1
  ) |> 
    dplyr::mutate(
      x_min = x_mid - 1.5,
      y_min = y_mid - 1.5,
      x_max = x_mid + 1.5,
      y_max = y_mid + 1.5,
      size = 3
    )
  
  overlap <- function(row1, row2) {
    x1 <- seq(row1$x_min, row1$x_max, by = .5)
    x2 <- seq(row2$x_min, row2$x_max, by = .5)
    y1 <- seq(row1$y_min, row1$y_max, by = .5)
    y2 <- seq(row2$y_min, row2$y_max, by = .5)
    
    x_disjoint <- length(intersect(x1, x2)) == 0
    y_disjoint <- length(intersect(y1, y2)) == 0

    rect_disjoint <- x_disjoint | y_disjoint
    !rect_disjoint
  }
  
  n_sml <- nrow(locs_sml)
  n_lrg <- nrow(locs_lrg)
  n_all <- n_sml + n_lrg

  locs_all <- dplyr::bind_rows(locs_lrg, locs_sml) |> 
    dplyr::slice_sample(n = n_all) |> 
    dplyr::mutate(flag = FALSE)

  for (r2 in 2:n_all) {
    for (r1 in 1:(r2-1)) {
      r1_ok <- !locs_all$flag[r1] 
      r2_ok <- !locs_all$flag[r2] 
      if (r1_ok & r2_ok) {
        o <- overlap(locs_all[r1,], locs_all[r2,])
        if (o) locs_all$flag[r2] <- TRUE
      }
    }
  }

  locs_valid <- locs_all |> dplyr::filter(flag == FALSE)

  locs <- locs_valid |> 
    dplyr::slice_sample(n = n_strokes, replace = TRUE)

  stroke_dx <- locs$x_mid
  stroke_dy <- locs$y_mid
  stroke_sz <- locs$size

  amp <- sample(1:4, n_strokes, TRUE) * stroke_sz * .75
  turns <- stats::runif(n_strokes, min = .5, max = 3)

  bg_col <- sketchpad::color_darken(palette[length(palette)], amount = .5)

  bg <- sketchpad::fill_solid(bg_col)

  bristles <- sketchpad::sketch(
    canvas = sketchpad::canvas(
      xlim = c(-6, 6),
      ylim = c(-3, 3),
      clip = TRUE,
      background = bg
    )
  )

  for (s in 1:n_strokes) {
    nb <- sample(3:6, 1)
    sp <- stats::runif(1, .05, .1) * nb

    noise <- sketchpad::noise_field(
      noise = ambient::gen_simplex,
      fractal = ambient::billow,
      frequency = .2,
      octaves = 5,
      seed = seed + s
    )

    stroke <- sketchpad::shape_strokepath(
      path = sketchpad::curve_spiral(
        x = stroke_dx[s],
        y = stroke_dy[s],
        radius_start = .1 * amp[s],
        radius_end = .4 * amp[s],
        turns = turns[s]
      ),
      width = 0.15,
      fill = sketchpad::fill_vignette(color = palette[s]), 
      fill_alpha = 1, 
      distortion = noise,
      color = NA_character_
    )

    bristles <- bristles + 
      sketchpad::effect_bristle( 
        stroke,
        n_bristles = nb, 
        spread = sp, 
        fray = 0.4,
        n = 1000
      )
  }

  outfile <- output_path(name, version, seed, format)
  message("writing: ", outfile)
  sketchpad::save_png(bristles, outfile, width = 2400, height = 1200, units = "px")

}


# make the art ------------------------------------------------------------

seeds <- version * 100 + (0:99)
  for(seed in seeds) art_generator(seed)

