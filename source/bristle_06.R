# set up ------------------------------------------------------------------

name <- "bristle"    # only change this for an entirely new system
version <- 6         # increment to 2 for "my-art-system_02.R", etc
format <- "png"      # png is usually a good choice!

# define common helper functions
source(here::here("source", "common.R"), echo = FALSE)

# make sure we haven't accidentally messed up the versions
assert_version_consistency(name)


# define the art system ---------------------------------------------------

art_generator <- function(seed) {

  set.seed(seed)

  n_strokes <- sample(15:60, 1)

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

  x_loc <- -5:5
  y_loc <- -2:2
  stroke_dx <- sample(x_loc, n_strokes, replace = TRUE)
  stroke_dy <- sample(y_loc, n_strokes, replace = TRUE)

  offset <- stats::runif(n_strokes, min = -pi/2, max = pi/2)

  amp <- stats::runif(n_strokes, min = .2, max = .9)
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

  nb <- sample(3:6, 1)
  sp <- stats::runif(1, .05, .1) * nb

  for (s in 1:n_strokes) {

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
        radius_end = 1 * amp[s],
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

