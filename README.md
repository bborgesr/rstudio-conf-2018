## Drilldown data discovery with Shiny
_Barbara Borges Ribeiro_

Data science is often thought as _building up_ from data. However there are many cases where going the other away around, and _drilling down_ into the data, can also be extremely useful. Have you ever seen a plot where something seems off? Maybe it's a few egregious outliers or a quirk in the expected trend. Instead of going back to the drawing board immediately, returning to the spreadsheets of data and trying other visualizations and summaries, you can leverage the power of Shiny to allow you to interactively start from an aggregate visualization (or summary) and then drill down into the lower-level, finer-grained data. Whether it is by interactively creating new tabs, modal windows or other methods, _drilling down_ allows you to discover data that's been right under your nose, without having to leave your Shiny app. This capability also allows for more satisfying data presentation or data reporting Shiny apps, since its consumers can investigate the data to their heart's content. This talk will cover how you can create drilldown Shiny apps, using functions like `insertUI` and `removeUI`, `appendTab` and `removeTab`, and `showModal`.

[Link to presentation](https://rawgit.com/bborgesr/rstudio-conf-2018/master/presentation/presentation.html)
[Link to Sheets Data](https://docs.google.com/spreadsheets/d/1j-MysIF2mzuuD2u-E3JuWvRLIwCCIvx3_OxLYNIhQZU/edit?usp=sharing)
<!-- the rstudio::conf template is 16:9 0 make sure mine is too! -->
