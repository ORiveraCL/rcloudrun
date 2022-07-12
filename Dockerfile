FROM rstudio/plumber:latest

WORKDIR /workspace
COPY . .

RUN  R -e "install.packages('dotenv')" \
  && R -e "install.packages('urltools')" \
  && R -e "install.packages('Rcpp')" \
  && R -e "install.packages('elastic')" \
  && R -e "install.packages('readxl')" \
  && R -e "install.packages('tidyverse')" \
  && R -e "install.packages('lubridate')" \
  && R -e "install.packages('randomForest')"


#CMD ["/workspace/src/api.R"]

#ENTRYPOINT ["./docker/entrypoint.sh"]
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('/workspace/src/api.R'); args <- list(host = '0.0.0.0', port = 8000); if (packageVersion('plumber') >= '1.0.0') { pr$setDocs(TRUE) } else { args$swagger <- TRUE }; do.call(pr$run, args)"]