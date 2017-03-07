<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->
simulateR 0.1.3
===============

MAJOR CHANGES

-   Added `output` parameter that controls how simulation output is provided. This change offers greater flexibility over how the provided function sends back its own output for a single simulation. By default, `simulate()` will now output a list with the data for each simulation; however, if output can be coerced to a data frame or vector, these are offered as options as well.

NEW FEATURES

-   Timing information for how long the simulations took is now provided.

simulateR 0.1.2
===============

NEW FEATURES

-   Added support for playing a sound once simulations finish.

simulateR 0.1.1
===============

NEW FEATURES

-   Added vignette about simulating statistical power.

simulateR 0.1.0
===============

NEW FEATURES

-   `simulate` function that runs simulations.

-   `gen_data` function for generating simulated data from a factor matrix and effects matrix.
