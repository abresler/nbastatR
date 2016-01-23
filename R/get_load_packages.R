function_packages <- #need all of these installed including some from github
  c(
    'dplyr',
    'magrittr',
    'jsonlite',
    'tidyr',
    'stringr',
    'lubridate',
    'stringr',
    'tidyr'
  )

install_needed_packages <-
  function(required_packages = function_packages) {
    needed_packages <-
      required_packages[!(required_packages %in% installed.packages()[,"Package"])]

    if (length(needed_packages) > 0) {
      if (!require("pacman")) install.packages("pacman")
      pacman::p_load(needed_packages)
    }
  }

load_needed_packages <-
  function(required_packages = function_packages) {
    loaded_packages <-
      gsub('package:', '', search())

    package_to_load <-
      required_packages[!required_packages %in% loaded_packages]
    lapply(package_to_load, library, character.only = T)
  }
install_needed_packages(function_packages)
load_needed_packages(function_packages)
