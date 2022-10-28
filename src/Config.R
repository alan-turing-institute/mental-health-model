DIR_WORK <- "D:/ATI/SIPHER"

setwd(DIR_WORK)

Sys.setlocale("LC_ALL", "English.utf8")

DEBUG_MODE <- F

options(digits = 10)

RANDOM_SEED_MODE <- T

if (RANDOM_SEED_MODE == T)
  RANDOM_SEED <- 1

ZONE_ID <- "ZoneID"

GREEN <- palette()[3]
BLUE <- palette()[4]
RED <- palette()[2]

SKY <- palette()[5]
MAGENTA <- palette()[6]
YELLOW <- palette()[7]
GRAY <- palette()[8]

WHITE <- "#FFFFFF"

IMAGE_WIDTH <- 16
IMAGE_WIDTH_SMALL <- 12
IMAGE_HEIGHT <- 10

POINT_SIZE <- 2
LINE_SIZE <- 1
TEXT_SIZE <- 6
BASE_SIZE <- 18

TITLE_SIZE <- 1

myStop <- function(msg) {
  print(msg)
  
  quit()
}

DIR_OUTPUT <- paste0(DIR_WORK, "/output")
DIR_OUTPUT_LAD <- paste0(DIR_OUTPUT, "/LAD")
DIR_OUTPUT_MSOA <- paste0(DIR_OUTPUT, "/MSOA")
