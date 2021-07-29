# This Dockerfile is for the Benchmark Histograms tool
# Get image with R and Shiny preinstalled
FROM rocker/shiny-verse:latest

# General system libraries that might be used
# I genuinely don't know which of these are necessary
# But they do show up in example Dockerfiles so they're here
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libgdal-dev \
    libudunits2-dev

# Install required R packages
RUN R -e "install.packages(c('rgdal', 'sf', 'leaflet', 'spdplyr', 'spsurvey'), repos='http://cran.rstudio.com/')"

# Select port so we can actually talk to the app
EXPOSE 3838

# Run app
# This makes sure that the app runs when the container is instanced
# The host and port are probably unnecessary????
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/sampling-tool', port = 3838)"]


