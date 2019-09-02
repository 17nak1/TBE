require("pomp")


# Generate covars, data and pomp object
# Generate functions
source("CreateModel.R")
source("CreateCovars.R")
source("CreateDataset.R")


# Generate covars, data and pomp object
covars <- create_covars(startTime, endTime, stepsize=dt)
data <- create_dataset(startTime, endTime)
out <- create_pomp_model(data, covars, t0 = 0, dt=dt) 


po1 <- out$model
rm(out)
