FROM landscapedatacommons/r-base:4.0.5
LABEL maintainer='Ken Ramsey <kramsey@jornada-vmail.nmsu.edu>'
# make app folder
RUN mkdir /balanced-design-tool
# copy app to image
COPY . /balanced-design-tool
# create Rprofile.site file in container
RUN echo "local({options(shiny.port = 3838, shiny.host = '0.0.0.0')})" > /usr/lib/R/etc/Rprofile.site
# select port
EXPOSE 3838
CMD R -e "shiny::runApp('/balanced-design-tool', host = '0.0.0.0', port = 3838)"
#CMD ["tail","-f", "/dev/null"]