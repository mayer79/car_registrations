# Bar chart race with Swiss car registration data

We used publicly available data on monthly Swiss car registrations per model taken from 

https://www.auto.swiss/statistiken/pw-zulassungen-nach-marken.

The monthly counts per model since Januarly 2017 are collected and visualized in **bar chart race**. 

We used the statistical software [R](https://cran.r-project.org/) along with packages `ggplot2` and `gganimate` to create the visualization, see e.g. blog post https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/.

Not unlike a `facet_wrap` in `ggplot`, `gganimate` adds a time dimension to the figure.

Check out the self-contained R script "car_registrations.R" on my github page https://github.com/mayer79/race-chart to re-create the resulting movie. Among the options are an animated gif or this mp4 movie. The latter requires admin rights to install the mp4 renderer.



