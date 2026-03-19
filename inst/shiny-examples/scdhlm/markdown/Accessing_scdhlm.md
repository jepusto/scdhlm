### Accessing scdhlm

There are two ways to access this app: on your own computer, using the
RStudio software, or via a website. Since you are reading this now, you
have presumably already figured out at least one of these methods.

#### Access through the shinyapps.io website

The simplest way to access the app is via the web, at
<https://jepusto.shinyapps.io/scdhlm/>. This version is hosted by a
service called [shinyapps.io](https://www.shinyapps.io/), which imposes
limitations on the number of concurrent users of the site and total
hours of active use of the site. Consequently, you might find that the
site is not always available, especially towards the end of each month.
Running the app on your own computer through RStudio requires a bit more
effort to get set up, but will allow for extensive use, run faster, and
maintain privacy of your data because everything is run locally from
your computer. If you intend to use the tool extensively, I would
encourage you to follow the steps below in order to install it on your
own computer.

#### Access through RStudio

To access the app on your own computer, you will need to install two
pieces of open-source and freely available software (R and RStudio) and
follow further steps to configure the software. Details for installation
and RStudio access are described below. A video walk-through is
available at <https://www.youtube.com/watch?v=R0n9eo4oB7k> (Mac) or
<https://www.youtube.com/watch?v=DhrIfIXjF4c> (Windows).

1.  Install R via <http://cran.r-project.org/> 1.1. Navigate to the
    “Download and Install R” section and select the relevant download
    for your computer. 1.2. Download the latest version of R and install
    it following the instructions given.

2.  If you are a Windows user, install Rtools via
    <http://cran.r-project.org/bin/windows/Rtools/>

3.  Install RStudio from
    <http://www.rstudio.com/products/rstudio/download/> 3.1. Navigate to
    the “All Installers” section of the home page and select the
    relevant download for your computer. 3.2. Follow the installation
    directions provided.

4.  Once you have all of these programs installed, open RStudio. You
    will need to install several R packages that are required to run the
    calculator. Navigate to the console in RStudio (usually in the lower
    left pane) and type the following commands:

        install.packages("ggplot2")
        install.packages("shiny")
        install.packages("markdown")
        install.packages("glue")
        install.packages("rclipboard")
        install.packages("readxl")
        install.packages("janitor")
        install.packages("scdhlm")
        install.packages("rclipboard")
        install.packages("rstan")
        install.packages("brms")

5.  After all of these packages are installed, type the following
    commands at the console prompt to start the app (which should then
    open in a new window):

        library(scdhlm)
        shine_scd()

6.  To exit the app, simply close the window in which it appears or
    click the red “Stop” icon in the upper right-hand corner of the
    RStudio console window.
