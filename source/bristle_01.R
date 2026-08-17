# set up ------------------------------------------------------------------

name <- "bristle"    # only change this for an entirely new system
version <- 1         # increment to 2 for "my-art-system_02.R", etc
format <- "png"      # png is usually a good choice!

# define common helper functions
source(here::here("source", "common.R"), echo = FALSE)

# make sure we haven't accidentally messed up the versions
assert_version_consistency(name)


# define the art system ---------------------------------------------------

art_generator <- function(seed) {

  set.seed(seed)

  n_strokes <- 6L

  palette <- sketchpad::palette_cosine(
    n = n_strokes + 1L,
    seed = seed
  )

  stroke_dx <- stats::runif(n_strokes, min = -3, max = 3)
  stroke_dy <- stats::runif(n_strokes, min = -3, max = 3)

  offset <- stats::runif(n_strokes, min = -pi/2, max = pi/2)

  time <- seq(-4, 4, length.out = 200)

  bristles <- sketchpad::sketch(
    canvas = sketchpad::canvas(
      xlim = c(-6, 6),
      ylim = c(-3, 3),
      clip = TRUE,
      background = sketchpad::fill_solid("black")
    )
  )

  for (s in 1:n_strokes) {

    stroke <- sketchpad::shape_stroke(
      x = time + stroke_dx[s], 
      y = sin(time + offset[s]) * 1.2 + stroke_dy[s], 
      width = 0.1,
      fill = sketchpad::fill_solid(palette[s]), 
      fill_alpha = .4, 
      color = NA_character_
    )

    bristles <- bristles + 
      sketchpad::effect_bristle( 
        stroke,
        n_bristles = 15L, 
        spread = 1, 
        fray = 0.3
      )
  }

  outfile <- output_path(name, version, seed, format)
  message("writing: ", outfile)
  sketchpad::save_png(bristles, outfile, width = 1200, height = 600, units = "px")

}


# make the art ------------------------------------------------------------

seeds <- version * 100 + (0:99)
for(seed in seeds) art_generator(seed)

