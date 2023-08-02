# change here is you want to pin R or shiny version
FROM rocker/shiny:4.2.2

# change maintainer here
LABEL maintainer="Man Chen <manchen9005@gmail.com>"

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libglpk-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

## app folder
COPY /inst/shiny-examples/scdhlm ./app

# RUN R -e 'install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))'
# RUN R -e 'install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))'
# Add brms if for bayesian
RUN install2.r --error remotes shiny markdown ggplot2 readxl janitor plyr glue rclipboard
RUN installGithub.r jepusto/scdhlm@main

# install renv & restore packages
# RUN Rscript -e 'install.packages("renv")'
# RUN Rscript -e 'renv::restore()'

# EXPOSE can be used for local testing, not supported in Heroku's container runtime
EXPOSE 3838

# web process/code should get the $PORT environment variable
ENV PORT=3838

# command we want to run
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port=as.numeric(Sys.getenv('PORT')))"]
