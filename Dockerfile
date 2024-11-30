FROM rstudio/plumber

# RUN apt-get update -qq && apt-get install -y libssl-dev libcurl4-gnutls-dev libpng-dev libpng-dev pandoc

RUN R -e "install.packages(c('ggplot2'))"


COPY data/ /data
COPY src/ /src

EXPOSE 8000

ENTRYPOINT ["R", "-e", "r <- plumber::plumb('src/best_model.R'); r$run(host='0.0.0.0', port=8000)"]
