library(plumber)
library(dotenv)

load_dot_env(file = "../.env")

make_server <- function(files) {
  pr <- Plumber$new()

  for (file in files) {
    prFile <- plumb(file)
    path <- gsub("\\..*$", "", basename(file))
    path <- paste0("/", path)
    pr$mount(path, prFile)
  }

  pr
}

server <- make_server(dir("routes", pattern = ".R", full.names = TRUE))
server$run(
  host = "0.0.0.0",
  port = 8000
)
