if(!"dplyr" %in% installed.packages()) install.packages("dplyr")
if(!"audio" %in% installed.packages()) install.packages("audio")

library("dplyr")
library("audio")

notes <- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)

pitch <- paste("E E E",
               "E E E",
               "E G C D",
               "E",
               "F F F F",
               "F E E E",
               "E D D E",
               "D G",
               "E E E",
               "E E E",
               "E G C D",
               "E",
               "F F F F",
               "F E E E E",
               "G G F D",
               "C",
               "G3 E D C",
               "G3",
               "G3 G3 G3 E D C",
               "A3",
               "A3 F E D",
               "B3",
               "G G F D",
               "E",
               "G3 E D C",
               "G3",
               "G3 E D C",
               "A3 A3",
               "A3 F E D",
               "G G G G A G F D",
               "C C5 B A G F G",
               "E E E G C D",
               "E E E G C D",
               "E F G A C E D F",
               "E C D E F G A G",
               "F F F F F F",
               "F E E E E E",
               "E D D D D E",
               "D D E F G F E D",
               "E E E G C D",
               "E E E G C D",
               "E F G A C E D F",
               "E C D E F G A G",
               "F F F F F F",
               "F E E E E E",
               "G C5 B A G F E D",
               "C C E G C5")

duration <- c(1, 1, 2,
              1, 1, 2,
              1, 1, 1.5, 0.5,
              4,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              2, 2,
              1, 1, 2,
              1, 1, 2,
              1, 1, 1.5, 0.5,
              4,
              1, 1, 1, 1,
              1, 1, 1, 0.5, 0.5,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              3, .5, .5,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              3, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 0.5, 0.5, 1,
              1, 0.33, 0.33, 0.33, 1, 0.33, 0.33, 0.33,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.33, 0.33, 0.33, 2)

jbells <- data_frame(pitch = strsplit(pitch, " ")[[1]],
                     duration = duration)

jbells <- jbells %>%
  mutate(octave = substring(pitch, nchar(pitch)) %>%
  {suppressWarnings(as.numeric(.))} %>%
    ifelse(is.na(.), 4, .),
  note = notes[substr(pitch, 1, 1)],
  note = note + grepl("#", pitch) -
    grepl("b", pitch) + octave * 12 +
    12 * (note < 3),
  freq = 2 ^ ((note - 60) / 12) * 440)

tempo <- 250

sample_rate <- 44100

make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

jbells_wave <- mapply(make_sine, jbells$freq, jbells$duration) %>%
  do.call("c", .)

play(jbells_wave)


if(!"tidyverse" %in% installed.packages()) install.packages("tidyverse")

library("tidyverse")

n <- 100 
tibble(x = runif(n),  
       y = runif(n),  
       s = runif(n, min = 4, max = 20)) %>%
  ggplot(aes(x, y, size = s)) +
  geom_point(color = "white", pch = 42) +
  scale_size_identity() +
  coord_cartesian(c(0,1), c(0,1)) +
  theme_void() +
  theme(panel.background = element_rect("black"))


### ANIMATED SNOW === BY PAULVANDERLAKEN.COM
### PUT THIS FILE IN AN RPROJECT FOLDER

# load in packages
pkg <- c("here", "tidyverse", "devtools", "animation", "installr")
sapply(pkg, function(x){
  if (!x %in% installed.packages()){install.packages(x)}
  library(x, character.only = TRUE)
})

if (!"gganimate" %in% installed.packages()) {
  devtools::install_github("dgrtwo/gganimate")
  devtools::install_github("talgalili/installr")
  library(installr)
  install.ImageMagick()
}
library(gganimate)


# parameters
n <- 100 # number of flakes
times <- 100 # number of loops
xstart <- runif(n, max = 1) # random flake start x position
ystart <- runif(n, max = 1.1) # random flake start y position
size <- runif(n, min = 4, max = 20) # random flake size
xspeed <- seq(-0.02, 0.02, length.out = 100) # flake shift speeds to randomly pick from
yspeed <- runif(n, min = 0.005, max = 0.025) # random flake fall speed

# create storage vectors
xpos <- rep(NA, n * times)
ypos <- rep(NA, n * times)

# loop through simulations
for(i in seq(times)){
  if(i == 1){
    # initiate values
    xpos[1:n] <- xstart
    ypos[1:n] <- ystart
  } else {
    # specify datapoints to update
    first_obs <- (n*i - n + 1)
    last_obs <- (n*i)
    # update x position
    # random shift
    xpos[first_obs:last_obs] <- xpos[(first_obs-n):(last_obs-n)] - sample(xspeed, n, TRUE)
    # update y position
    # lower by yspeed
    ypos[first_obs:last_obs] <- ypos[(first_obs-n):(last_obs-n)] - yspeed
    # reset if passed bottom screen
    xpos <- ifelse(ypos < -0.1, runif(n), xpos) # restart at random x
    ypos <- ifelse(ypos < -0.1, 1.1, ypos) # restart just above top
  }
}

# store in dataframe
data_fluid <- cbind.data.frame(x = xpos,
                               y = ypos,
                               s = size,
                               t = rep(1:times, each = n))

# create animation
snow <- data_fluid %>%
  ggplot(aes(x, y, size = s, frame = t)) +
  geom_point(color = "white", pch = 42) +
  scale_size_identity() +
  coord_cartesian(c(0, 1), c(0, 1)) +
  theme_void() +
  theme(panel.background = element_rect("black"))



# save animation
gganimate(snow, filename = "snow.gif", title_frame = FALSE, interval = .1)

