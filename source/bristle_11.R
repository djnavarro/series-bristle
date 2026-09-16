# set up ------------------------------------------------------------------

name <- "bristle"    # only change this for an entirely new system
version <- 11         # increment to 2 for "my-art-system_02.R", etc
format <- "png"      # png is usually a good choice!

# define common helper functions
source(here::here("source", "common.R"), echo = FALSE)

# make sure we haven't accidentally messed up the versions
assert_version_consistency(name)


# define the art system ---------------------------------------------------

art_generator <- function(seed) {

  set.seed(seed)

  n_strokes <- sample(8:20, 1)

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

  stroke_dx <- stats::runif(n_strokes, min = -3, max = 3)
  stroke_dy <- stats::runif(n_strokes, min = -3, max = 3)

  bg <- sketchpad::fill_solid(sketchpad::color_darken(palette[length(palette)], amount = .6))
  palette <- palette |> 
    sketchpad::color_desaturate(amount = .2) |> 
    sketchpad::color_lighten(amount = .75)

  bristles <- sketchpad::sketch(
    canvas = sketchpad::canvas(
      xlim = c(-2, 2),
      ylim = c(-2, 2),
      clip = TRUE,
      background = bg
    )
  )

  nb <- sample(3:10, 1)
  sp <- stats::runif(1, .05, .1) * nb

  for (s in 1:n_strokes) {

    noise <- sketchpad::noise_field(
      noise = ambient::gen_simplex,
      fractal = ambient::billow,
      frequency = 5,
      octaves = 5,
      seed = seed + s
    )

    curve <- sketchpad::curve_scribble(
      amplitude = .5,
      n_harmonics = 10,
      direction = sample(c("horizontal", "vertical"), 1),
      seed = sample(1:1000, 1)
    ) + 
      sketchpad::trans_scale(sx = 3,sy = 3, about_x = .75, about_y = .75) +
      sketchpad::trans_translate(dx = stroke_dx[s], dy = stroke_dx[s])

    stroke <- sketchpad::shape_strokepath(
      path = curve,
      width = 0.05,
      fill = sketchpad::fill_solid(palette[s]), 
      fill_alpha = .5, 
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
  sketchpad::save_png(bristles, outfile, width = 2400, height = 2400, units = "px")

}


# make the art ------------------------------------------------------------

seeds <- version * 100 + (0:99)
  for(seed in seeds) art_generator(seed)

