#Forked from Ed Küpfer
#https://gist.github.com/edkupfer
#https://gist.github.com/asteves/7266330
function_packages <-
  c('magrittr', 'ggplot2', 'dplyr')
install_needed_packages <-
  function(required_packages = function_packages) {
    needed_packages <-
      required_packages[!(required_packages %in% installed.packages()[, "Package"])]

    if (length(needed_packages) > 0) {
      if (!require("pacman"))
        install.packages("pacman")
      pacman::p_load(needed_packages)
    }
  }

load_needed_packages <-
  function(required_packages = function_packages) {
    loaded_packages <-
      gsub('package:', '', search())

    package_to_load <-
      required_packages[!required_packages %in% loaded_packages]
    if (length(package_to_load) > 0) {
      lapply(package_to_load, library, character.only = T)
    }
  }

# full court --------------------------------------------------------------

draw_fullcourt <-
  function(plot_title = "Title") {
    install_needed_packages(function_packages)
    load_needed_packages(function_packages)
    palette(
      c(
        "#E41A1C",
        "#377EB8",
        "#4DAF4A",
        "#984EA3",
        "#FF7F00",
        "#FFFF33",
        "#A65628",
        "#F781BF",
        "#999999"
      )
    )

    #Generate Data for the 3 point linea
    # Define the circle; add a point at the center if the 'pie slice' if the shape is to be filled
    circleFun <-
      function(center = c(0, 5.25),
               diameter = 20.9,
               npoints = 20000,
               start = 0,
               end = 1,
               filled = TRUE) {
        tt <- seq(start * pi, end * pi, length.out = npoints)
        df <- data_frame(y = center[1] + diameter / 2 * cos(tt),
                         x = center[2] + diameter / 2 * sin(tt))
        return(df)
      }

    halfCircle <-
      circleFun(c(0, 5.25),
                20.9 * 2,
                start = 0,
                end = 1,
                filled = FALSE)
    ggplot(data = data_frame(y = 1, x = 1), aes(x, y)) +
      ###halfcourt line:
      geom_path(data = data_frame(x = c(47, 47), y = c(0, 50))) +
      ###outside boy:
      geom_path(data = data_frame(y = c(0, 0, 50, 50, 0), x = c(0, 94, 94, 0, 0))) +
      ###solid FT semicircle above FT line:
      geom_path(data = data_frame(y = c((-6000:(
        -1
      ) / 1000) + 25, (1:6000 / 1000) + 25), x = c(19 + sqrt(
        6 ^ 2 - c(-6000:(-1) / 1000, 1:6000 / 1000) ^ 2
      ))), aes(y = y, x = x)) +
      geom_path(data = data_frame(y = c((-6000:(
        -1
      ) / 1000) + 25, (1:6000 / 1000) + 25), x = c(75 + sqrt(
        6 ^ 2 - c(-6000:(-1) / 1000, 1:6000 / 1000) ^ 2
      ))), aes(y = y, x = x)) +
      ###dashed FT semicircle below FT line:
      geom_path(data = data_frame(y = c((-6000:(
        -1
      ) / 1000) + 25, (1:6000 / 1000) + 25), x = c(19 - sqrt(
        6 ^ 2 - c(-6000:(-1) / 1000, 1:6000 / 1000) ^ 2
      ))),
      aes(y = y, x = x),
      linetype = 'dashed') +
      geom_path(data = data_frame(y = c((-6000:(
        -1
      ) / 1000) + 25, (1:6000 / 1000) + 25), x = c(75 - sqrt(
        6 ^ 2 - c(-6000:(-1) / 1000, 1:6000 / 1000) ^ 2
      ))),
      aes(y = y, x = x),
      linetype = 'dashed') +
      ###kex:
      geom_path(data = data_frame(y = c(17, 17, 33, 33, 17), x = c(0, 19, 19, 0, 0))) +
      geom_path(data = data_frame(
        y = c(17, 17, 33, 33, 17),
        x = c(94, 75, 75, 94, 94)
      )) +
      ###boy inside the kex:
      geom_path(data = data_frame(y = c(19, 19, 31, 31, 19), x = c(0, 19, 19, 0, 0))) +
      geom_path(data = data_frame(
        y = c(19, 19, 31, 31, 19),
        x = c(94, 75, 75, 94, 94)
      )) +
      ###restricted area semicircle:
      geom_path(data = data_frame(y = c((-4000:(
        -1
      ) / 1000) + 25, (1:4000 / 1000) + 25), x = c(5.25 + sqrt(
        4 ^ 2 - c(-4000:(-1) / 1000, 1:4000 / 1000) ^ 2
      ))), aes(y = y, x = x)) +
      geom_path(data = data_frame(y = c((-4000:(
        -1
      ) / 1000) + 25, (1:4000 / 1000) + 25), x = c(88.75 - sqrt(
        4 ^ 2 - c(-4000:(-1) / 1000, 1:4000 / 1000) ^ 2
      ))), aes(y = y, x = x)) +
      ###halfcourt semicircle:
      geom_path(data = data_frame(y = c((-6000:(
        -1
      ) / 1000) + 25, (1:6000 / 1000) + 25), x = c(47 - sqrt(
        6 ^ 2 - c(-6000:(-1) / 1000, 1:6000 / 1000) ^ 2
      ))), aes(y = y, x = x)) +
      geom_path(data = data_frame(y = c((-6000:(
        -1
      ) / 1000) + 25, (1:6000 / 1000) + 25), x = c(47 + sqrt(
        6 ^ 2 - c(-6000:(-1) / 1000, 1:6000 / 1000) ^ 2
      ))), aes(y = y, x = x)) +
      ###rim:
      geom_path(data = data_frame(y = c((-750:(-1) / 1000) + 25,
                                        (1:750 / 1000) + 25,
                                        (750:1 / 1000) + 25,
                                        (-1:-750 / 1000) + 25
      ), x = c(c(
        5.25 + sqrt(0.75 ^ 2 - c(-750:(-1) / 1000, 1:750 / 1000) ^ 2)
      ), c(
        5.25 - sqrt(0.75 ^ 2 - c(750:1 / 1000, -1:-750 / 1000) ^ 2)
      ))), aes(y = y, x = x)) +
      geom_path(data = data_frame(y = c((-750:(-1) / 1000) + 25,
                                        (1:750 / 1000) + 25,
                                        (750:1 / 1000) + 25,
                                        (-1:-750 / 1000) + 25
      ), x = c(c(
        88.75 + sqrt(0.75 ^ 2 - c(-750:(-1) / 1000, 1:750 / 1000) ^ 2)
      ), c(
        88.75 - sqrt(0.75 ^ 2 - c(750:1 / 1000, -1:-750 / 1000) ^ 2)
      ))), aes(y = y, x = x)) +
      ###backboard:
      geom_path(data = data_frame(y = c(22, 28), x = c(4, 4)), lineend = 'butt') +
      geom_path(data = data_frame(y = c(22, 28), x = c(90, 90)), lineend =
                  'butt') +
      ###three-point line:
      # geom_path(data=data_frame(y=c(-21,-21,-21000:(-1)/1000,1:21000/1000,21,21),x=c(0,169/12,5.25+sqrt(23.75^2-c(-21000:(-1)/1000,1:21000/1000)^2),169/12,0)),aes(y=y,x=x))+
      ###fiy aspect ratio to 1:1
      geom_path(data = halfCircle, aes(x = x, y = y + 25)) +
      ###Complete the three-point line
      geom_path(data = data_frame(
        y = c(4.1, 4.1, 45.9, 45.9),
        x = c(5.25, 0, 0, 5.25)
      )) +
      geom_path(data = halfCircle, aes(x = 94 - x, y = y + 25)) +
      geom_path(data = data_frame(
        y = c(4.1, 4.1, 45.9, 45.9),
        x = c(88.75, 94, 94, 88.75)
      )) +
      coord_fixed() +

      ###Clean up the Court
      theme_bw() + theme(
        panel.grid = element_blank(),
        legend.title = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "top"
      ) +
      ggtitle(plot_title)
  }


draw_halfcourt <- function(plot_title) {
  install_needed_packages(function_packages)
  load_needed_packages(function_packages)
  palette(
    c(
      "#E41A1C",
      "#377EB8",
      "#4DAF4A",
      "#984EA3",
      "#FF7F00",
      "#FFFF33",
      "#A65628",
      "#F781BF",
      "#999999"
    )
  )

  #Generate Data for the 3 point line
  # Define the circle; add a point at the center if the 'pie slice' if the shape is to be filled
  circleFun <-
    function(center = c(0, 5.25),
             diameter = 20.9,
             npoints = 20000,
             start = 0,
             end = 1,
             filled = TRUE) {
      tt <- seq(start * pi, end * pi, length.out = npoints)
      df <- data_frame(y = center[1] + diameter / 2 * cos(tt),
                       x = center[2] + diameter / 2 * sin(tt))
      return(df)
    }

  halfCircle <-
    circleFun(c(0, 5.25),
              20.9 * 2,
              start = 0,
              end = 1,
              filled = FALSE)
  ggplot(data = data_frame(y = 1, x = 1), aes(x, y)) +
    ###outside boy:
    geom_path(data = data_frame(y = c(0, 0, 50, 50, 0), x = c(0, 47, 47, 0, 0))) +
    ###solid FT semicircle above FT line:
    geom_path(data = data_frame(y = c((-6000:(
      -1
    ) / 1000) + 25, (1:6000 / 1000) + 25), x = c(19 + sqrt(
      6 ^ 2 - c(-6000:(-1) / 1000, 1:6000 / 1000) ^ 2
    ))), aes(y = y, x = x)) +
    ###dashed FT semicircle below FT line:
    geom_path(data = data_frame(y = c((-6000:(
      -1
    ) / 1000) + 25, (1:6000 / 1000) + 25), x = c(19 - sqrt(
      6 ^ 2 - c(-6000:(-1) / 1000, 1:6000 / 1000) ^ 2
    ))),
    aes(y = y, x = x),
    linetype = 'dashed') +
    ###kex:
    geom_path(data = data_frame(y = c(17, 17, 33, 33, 17), x = c(0, 19, 19, 0, 0))) +
    ###boy inside the kex:
    geom_path(data = data_frame(y = c(19, 19, 31, 31, 19), x = c(0, 19, 19, 0, 0))) +
    ###restricted area semicircle:
    geom_path(data = data_frame(y = c((-4000:(
      -1
    ) / 1000) + 25, (1:4000 / 1000) + 25), x = c(5.25 + sqrt(
      4 ^ 2 - c(-4000:(-1) / 1000, 1:4000 / 1000) ^ 2
    ))), aes(y = y, x = x)) +
    ###halfcourt semicircle:
    geom_path(data = data_frame(y = c((-6000:(
      -1
    ) / 1000) + 25, (1:6000 / 1000) + 25), x = c(47 - sqrt(
      6 ^ 2 - c(-6000:(-1) / 1000, 1:6000 / 1000) ^ 2
    ))), aes(y = y, x = x)) +
    ###rim:
    geom_path(data = data_frame(y = c((-750:(-1) / 1000) + 25,
                                      (1:750 / 1000) + 25,
                                      (750:1 / 1000) + 25,
                                      (-1:-750 / 1000) + 25
    ), x = c(c(
      5.25 + sqrt(0.75 ^ 2 - c(-750:(-1) / 1000, 1:750 / 1000) ^ 2)
    ), c(
      5.25 - sqrt(0.75 ^ 2 - c(750:1 / 1000, -1:-750 / 1000) ^ 2)
    ))), aes(y = y, x = x)) +
    ###backboard:
    geom_path(data = data_frame(y = c(22, 28), x = c(4, 4)), lineend = 'butt') +
    ###three-point line:
    # geom_path(data=data_frame(y=c(-21,-21,-21000:(-1)/1000,1:21000/1000,21,21),x=c(0,169/12,5.25+sqrt(23.75^2-c(-21000:(-1)/1000,1:21000/1000)^2),169/12,0)),aes(y=y,x=x))+
    ###fiy aspect ratio to 1:1
    geom_path(data = halfCircle, aes(x = x, y = y + 25)) +

    ###Complete the three-point line
    geom_path(data = data_frame(
      y = c(4.1, 4.1, 45.9, 45.9),
      x = c(5.25, 0, 0, 5.25)
    )) +
    coord_fixed() +

    ###Clean up the Court
    theme_bw() + theme(
      panel.grid = element_blank(),
      legend.title = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      legend.position = "top"
    ) +
    ggtitle(plot_title)
}
