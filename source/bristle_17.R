# set up ------------------------------------------------------------------

name <- "bristle"    # only change this for an entirely new system
version <- 17        # increment to 2 for "my-art-system_02.R", etc
format <- "png"      # png is usually a good choice!

# define common helper functions
source(here::here("source", "common.R"), echo = FALSE)

# make sure we haven't accidentally messed up the versions
assert_version_consistency(name)


# define the art system ---------------------------------------------------

art_generator <- function(seed) {

  set.seed(seed)

  n_strokes <- 150
  s_block <- 130

  palette <- sketchpad::palette_manual(n = 5, index = sample(1:300, 1))
  palette <- sample(palette, n_strokes + 1L, replace = TRUE)

  #sketchpad::draw(sketchpad::effect_swatch(color = palette))

  stroke_dx <- stats::runif(n_strokes, min = -2, max = 2)
  stroke_dy <- stats::runif(n_strokes, min = -2, max = 2)

  fill_seed <- sample(1:10000, 1)
  bg_col <- sketchpad::color_darken(palette[length(palette)], amount = runif(1, min = .3, max = .6))
  bg <- sketchpad::fill_solid(color = bg_col)

  bristles <- sketchpad::sketch(
    canvas = sketchpad::canvas(
      xlim = c(-2, 2),
      ylim = c(-2, 2),
      clip = TRUE,
      background = bg
    )
  )

  for (s in 1:n_strokes) {

    if (s == s_block) {
      bristles <- bristles + 
        sketchpad::shape_square(
          x = 0,
          y = 0, 
          side = runif(1, min = 1, max = 4),
          color = bg_col,
          fill = sketchpad::fill_solid(color = bg_col)
        )
    }

    nb <- sample(3:10, 1)
    sp <- stats::runif(1, .025, 1) * nb

    noise <- sketchpad::noise_field(
      noise = ambient::gen_simplex,
      fractal = ambient::billow,
      frequency = 5,
      octaves = 5,
      seed = seed + s
    )

    noise2 <- sketchpad::noise_field(
      noise = ambient::gen_simplex,
      fractal = ambient::ridged,
      frequency = 1.5,
      octaves = 3,
      seed = seed + s
    )

    curve <- sketchpad::curve_scribble(
      amplitude = .5,
      n_harmonics = 10,
      direction = sample(c("horizontal", "vertical"), 1),
      seed = sample(1:1000, 1)
    ) + 
      sketchpad::trans_scale(sx = 3,sy = 3, about_x = .75, about_y = .75) +
      sketchpad::trans_translate(dx = stroke_dx[s], dy = stroke_dy[s])

    stroke <- sketchpad::shape_strokepath(
      path = curve,
      width = 0.05,
      fill = sketchpad::fill_solid(color = sample(
        c(bg_col, palette[s]), 
        size = 1,
        prob = c(2, 1)
      )),
      fill_alpha = .75,
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
      ) + 
      sketchpad::shape_blob(
        x = stroke_dx[s] + runif(1, -0.1, 0.1),
        y = stroke_dy[s] + runif(1, -0.1, 0.1), 
        fill = sketchpad::fill_solid(
          color = sample(
            c(bg_col, palette[s]), 
            size = 1,
            prob = c(2, 1)
          )
        ),
        n = 1000,
        distortion = noise2,
        color = "#00000000",
        fill_alpha = 1,
        radius = runif(1, .01, .15),
        range = runif(1, 0.05, 0.1)
      )
  }

  outfile <- output_path(name, version, seed, format)
  message("writing: ", outfile)
  sketchpad::save_png(bristles, outfile, width = 4000, height = 4000, units = "px")

}


# make the art ------------------------------------------------------------

seeds <- version * 100 + (0:99)
  for(seed in seeds) art_generator(seed)

