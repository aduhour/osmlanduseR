# ## code to prepare datasets

# Often, the data you include in data/ is a cleaned up version of raw data you’ve
# gathered from elsewhere.
# We highly recommend taking the time to include the code used to do this
# in the source version of your package.
# This makes it easy for you to update or reproduce your version of the data.
# This data-creating script is also a natural place to leave comments about
# important properties of the data, i.e. which features are important
# for downstream usage in package documentation.
#
# We suggest that you keep this code in one or more .R files below data-raw/.
# You don’t want it in the bundled version of your package, so this folder should be listed in .Rbuildignore. usethis has a convenience function that can be called when you first adopt the data-raw/ practice or when you add an additional .R file to the folder:
#
# usethis::use_data(sh, overwrite = TRUE)

# library(usethis)
# use_data_raw()
# help("use_data")

clc <- read.csv("data-raw/clc.csv")
use_data(clc, overwrite = TRUE)


intafao <- read.csv("data-raw/intafao.csv")
use_data(intafao, overwrite = TRUE)
document()
