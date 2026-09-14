# set up ------------------------------------------------------------------

name <- "bristle"    # only change this for an entirely new system
version <- 10        # increment to 2 for "my-art-system_02.R", etc
format <- "png"      # png is usually a good choice!

# define common helper functions
source(here::here("source", "common.R"), echo = FALSE)

# make sure we haven't accidentally messed up the versions
assert_version_consistency(name)


# define the art system ---------------------------------------------------

art_generator <- function(seed) {

  set.seed(seed)

  n_strokes <- sample(5:10, 1)

  r_colors <- colours(distinct = TRUE)
  r_colhsv <- rgb2hsv(col2rgb(colours(distinct = TRUE)))
  base_colors <- r_colors[
    r_colhsv["s", ] > .1 & 
    r_colhsv["s", ] < .9 & 
    r_colhsv["v", ] > .1 &
    r_colhsv["v", ] < .9
  ]
  base <- sample(base_colors, 1)

  palette <- sketchpad::palette_harmony(
    base = base, scheme = "analogous", n = sample(3:6, 1)
  )
  palette <- sample(palette, n_strokes + 1L, replace = TRUE)

  #sketchpad::draw(sketchpad::effect_swatch(color = palette))

  stroke_dx <- stats::runif(n_strokes, min = -9, max = 9)
  stroke_dy <- stats::runif(n_strokes, min = -3, max = 3)

  offset <- stats::runif(n_strokes, min = -pi/2, max = pi/2)

  amp <- stats::runif(1, min = .4, max = 1)
  len <- stats::runif(1, min = 6, max = 10)
  time <- seq(-len/2, len/2, length.out = 500)

  noise <- sketchpad::noise_field(
    noise = ambient::gen_simplex,
    fractal = ambient::ridged,
    frequency = 1,
    octaves = 10,
    seed = seed
  )

  bg <- sketchpad::fill_solid("#eee")

  bristles <- sketchpad::sketch(
    canvas = sketchpad::canvas(
      xlim = c(-15, 15),
      ylim = c(-3, 3),
      clip = TRUE,
      background = bg
    )
  )

  nb <- sample(5:15, 1)
  sp <- stats::runif(1, .1, .3) * nb

  for (s in 1:n_strokes) {

    stroke <- sketchpad::shape_stroke(
      x = time + stroke_dx[s], 
      y = sin(time + offset[s]) * amp + stroke_dy[s], 
      width = 0.15,
      fill = sketchpad::fill_solid(palette[s]), 
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
  sketchpad::save_png(bristles, outfile, width = 2500, height = 500, units = "px")

}


# make the art ------------------------------------------------------------

seeds <- version * 100 + (0:99)
  for(seed in seeds) art_generator(seed)

