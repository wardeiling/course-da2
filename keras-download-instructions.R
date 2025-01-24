# Let's set up a virtual python environment with reticulate and  
# install keras3 (incl tensorflow) from scratch. In other words,
# it is important that you do not have any python environments.

# Step 1: install Reticulate, install python 3.11 (not the newest version!) and create virtual environment

install.packages("reticulate")
library(reticulate)
reticulate::install_python(version = "3.11")
reticulate::virtualenv_create(envname = "r-reticulate", version = "3.11")
# reticulate::use_virtualenv("r-reticulate", required = TRUE)

# Step 2: select the installed r-reticulate python interpreter in the Rstudio settings (Global Setttings, Python, Python interpreter, Virtual Environment)

# Step 3: install keras3 (automatically also installs tensorflow)

install.packages("keras3")
library(keras3)
install_keras()

# Step 4: Select the new r-keras virtual environment in the python interpreter Rstudio settings

# Step 5: Load keras3 and tidyverse and use the packages (see example below)

library(keras3)
library(tidyverse)

mnist <- keras3::dataset_mnist()

plot_img <- function(img, col = gray.colors(255, start = 1, end = 0), ...) {
  image(t(img), asp = 1, ylim = c(1.1, -0.1), col = col, bty = "n", axes = FALSE, ...)
}

plot_img(mnist$train$x[1,,])

# Step 6 (Optional): delete the initial python environment r-reticulate from the file manager.
