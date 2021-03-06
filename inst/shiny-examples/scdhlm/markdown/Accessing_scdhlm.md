### Accessing scdhlm

There are two ways to access this app: via a website or on your own
computer, using the RStudio software. Since you are reading this now,
you have presumably already figure out at least one of these. Read on to
learn more about both options.

#### Access through the shinyapps.io website

The simplest way to access the app is via the web, at
<a href="https://jepusto.shinyapps.io/scdhlm/" class="uri">https://jepusto.shinyapps.io/scdhlm/</a>.
This version is hosted by a service called
[shinyapps.io](https://www.shinyapps.io/), which imposes limitations on
the number of concurrent users of the site and total hours of active use
of the site. Consequently, you might find that the site is not always
available. If you intend to use the tool extensively, I would encourage
you to follow the steps below in order to install it on your own
computer. Doing so has the further advantage that it will tend to run
faster on your own machine than it does over the web.

#### Access through RStudio

In order to run the app on your own computer, you will need to install
two pieces of software (R and RStudio), both of which are open-source
and freely available. You will then need to follow several further steps
to configure that software. The installation is more involved, but has
the benefit of letting you run the simulator as much as you want, at
faster speeds than over the web.

1.  Install R from
    <a href="http://cran.r-project.org/" class="uri">http://cran.r-project.org/</a>

2.  Install RStudio from
    <a href="http://www.rstudio.com/products/rstudio/download/" class="uri">http://www.rstudio.com/products/rstudio/download/</a>

3.  Once you have these programs installed, you will need to install
    several R packages that are required to run `scdhlm`. Do this by
    typing the following commands at the console prompt:

        install.packages("ggplot2")
        install.packages("shiny")
        install.packages("markdown")
        install.packages("readxl")
        install.packages("janitor")
        install.packages("scdhlm")

4.  After all of these packages are installed, type the following
    commands at the console prompt to start the app (which should then
    open in a new window):

        library(scdhlm)
        shine_scd()

5.  To exit the app, simply close the window in which it appears or
    click the red “Stop” icon in the upper right-hand corner of the
    RStudio console window.
