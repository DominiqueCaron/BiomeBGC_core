## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "BiomeBGC_core",
  description = "Runs Biome-BGC.",
  keywords = "",
  authors = c(
    person("Dominique", "Caron", email = "dominique.caron@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person(c("Alex", "M."), "Chubaty", email = "achubaty@for-cast.ca", role = "aut"),
    person("Céline", "Boisvenue", email = "celine.boisvenue@nrcan-rncan.gc.ca", role = "ctb")
  ),
  childModules = character(0),
  version = list(BiomeBGC_core = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "BiomeBGC_core.Rmd"),
  reqdPkgs = list("PredictiveEcology/SpaDES.core@box (>= 2.1.8.9013)", "ggplot2", "PredictiveEcology/BiomeBGCR@development", "parallel", "parallelly"),
  parameters = bindrows(
    defineParameter("argv", "character", "-a", NA, NA,
                    "Arguments for the BiomeBGC library (same as 'bgc' commandline application)"),
    defineParameter("bbgcPath", "character", tempdir(), NA, NA,
                    "Path to base directory to use for simulations."),
    defineParameter("bbgcInputPath", "character", inputPath(sim), NA, NA,
                    "Path to the Biome-BGC input directory."),
    defineParameter("parallel.cores", "integer", 1L, 1L, NA,
                    "Number of cores used to execute the simulation"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                    "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    expectsInput(
      objectName = "bbgcSpinup.ini",
      objectClass = "character",
      desc = paste("Biome-BGC initialization files for the spinup.", 
                   "Path to the .ini files (one path per site/scenario).")
    ),
    expectsInput(
      objectName = "bbgc.ini",
      objectClass = "character",
      desc = paste("Biome-BGC initialization files.", 
                   "Path to the .ini files (one path per site/scenario).")
    )
  ),
  outputObjects = bindrows(
    createsOutput(
      objectName = "annualSummary",
      objectClass = "data.table",
      desc = "A summary table a fixed set of outputs for each pixelGroup and year.",
      columns = c(
        pixelGroup = "The site/pixelGroup Id",
        year = "Simulation year",
        prcp = "annual total precipitation (mm/yr)",
        tavg = "annual average air temperature (deg C)",
        LAI = "annual maximum value of projected leaf area index (m2/m2)",
        ET = "annual total evapotranspiration (mm/yr)",
        OF = "annual total outflow (mm/yr)",
        NPP = "annual total net primary production (gC/m2/yr)",
        NBP = "annual total net biome production (gC/m2/yr)"
      )
    ),
    createsOutput(
      objectName = "dailyOutput",
      objectClass = "data.table",
      desc = paste(
        "The ouput variables for each pixelGroup and day.",
        "The units can be find here: https://raw.githubusercontent.com/PredictiveEcology/BiomeBGCR/refs/heads/development/src/Biome-BGC/src/include/bgc_struct.h"
      )
    ),
    createsOutput(
      objectName = "monthlyAverages",
      objectClass = "data.table",
      desc = paste(
        "The daily output variables averaged for each month.",
        "The same units than the dailyOutput."
      )
    ),
    createsOutput(
      objectName = "annualAverages",
      objectClass = "data.table",
      desc = paste(
        "The daily output variables averaged for each month.",
        "The same units than the dailyOutput."
      )
    )
  )
))

doEvent.BiomeBGC_core = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
      
      # do stuff for this event
      sim <- Init(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "BiomeBGC_core", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "BiomeBGC_core", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      # schedule future event(s)
      
      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "BiomeBGC_core", "plot")
      
      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function
      
      # schedule future event(s)
      
      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "BiomeBGC_core", "save")
      
      # ! ----- STOP EDITING ----- ! #
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

### template initialization
Init <- function(sim) {
  ## Arguments for the BiomeBGC library
  argv <- "-v1"
  
  ## Set the simulation directory
  bbgcPath <- params(sim)$BiomeBGC_core$bbgcPath
  createBGCdirs(sim)
  
  # paths to the spinup ini files
  spinupIniPaths <- file.path(
    bbgcPath,
    "inputs" ,
    "ini",
    paste0(sim$pixelGroupParameters$pixelGroup, "_spinup.ini")
  )
  # paths to the main simulation ini files
  iniPaths <- file.path(bbgcPath,
                        "inputs" ,
                        "ini",
                        paste0(sim$pixelGroupParameters$pixelGroup, ".ini"))
  
  # determine the number of cores to use
  n_pixelGroups <- length(iniPaths)
  n_cores <- min(P(sim)$parallel.cores, parallelly::availableCores() - 1, n_pixelGroups)
  
  # Either in parallel or sequentially
  if(n_cores > 1){
    
    # Set up the cluster
    cl <- parallelly::makeClusterPSOCK(
      n_cores,
      rscript_libs = .libPaths(),
      autoStop = TRUE
    )
    parallel::clusterEvalQ(cl, library(BiomeBGCR))
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(cl, c("argv", "bbgcPath", "readDailyOutput"), envir = environment())
    
    # Execute the spinup in parallel
    message("Running the spinup for ", n_pixelGroups, " pixelGroups in parallel using ", n_cores, " cores.")
    parLapply(cl, spinupIniPaths, function(iniPath) {
      resi <- bgcExecuteSpinup(argv, iniPath, normalizePath(bbgcPath))
      if (resi[[1]] != 0)
        stop("Spinup error.")
    })
    # Run the main simulation in parallel
    message("Running the main simulations in parallel using ", n_cores, " cores.")
    sim$dailyOutput <- parLapply(cl, iniPaths, function(iniPath) {
      log <- capture.output({
        resi <- bgcExecute(argv, iniPath, normalizePath(bbgcPath))
      })
      if (resi[[1]] != 0) stop("Simulation error.")
      dailyOutput <- readDailyOutput(resi[[2]][[1]])
      return(dailyOutput)
    })
    
    # Read the outputs
    message("Reading the output files.")
    sim$dailyOutput <- parLapply(cl, res, readDailyOutput) |> rbindlist(idcol = "pixelGroup")
    sim$dailyOutput$pixelGroup <- as.numeric(names(sim$bbgc.ini))[sim$dailyOutput$pixelGroup]
    
  } else {
    
    # Run the spinup
    res <- lapply(spinupIniPaths, function(iniPath) {
      message("Running the spinup for pixelGroup ", which(iniPath == spinupIniPaths), " of ", length(spinupIniPaths))
      
      resi <- bgcExecuteSpinup(argv, iniPath, bbgcPath)
      
      if (resi[[1]] != 0) stop("Spinup error.")
      
      return(resi[[2]][[1]])
    }) |> Cache()
    
    # Run the main simulation
    res <- lapply(iniPaths, function(iniPath) {
      message("Running simulation for pixelGroup ", which(iniPath == iniPaths), " of ", length(iniPaths))
      # Run simulation and silence the messaging
      log <- capture.output({
        resi <- bgcExecute(argv, iniPath, bbgcPath)
      })
      if (resi[[1]] != 0) stop("Simulation error.")
      return(resi[[2]][[1]])
    })
    
    # Read the outputs
    message("Reading the output files.")
    sim$dailyOutput <- lapply(res, readDailyOutput) |> rbindlist(idcol = "pixelGroup")
    sim$dailyOutput$pixelGroup <- as.numeric(names(sim$bbgc.ini))[sim$dailyOutput$pixelGroup]
    
  }
  
  # Get the annual outputs
  # Read the summary
  # sim$monthlyAverages <- lapply(res, readMonthlyAverages) |> rbindlist(idcol = "pixelGroup")
  # sim$monthlyAverages$pixelGroup <- as.numeric(names(sim$bbgc.ini))[sim$monthlyAverages$pixelGroup]
  # sim$annualSummary <- lapply(sim$bbgc.ini, readAnnualSummary, path = bbgcPath) |> rbindlist(idcol = "pixelGroup")
  # # Compute the annual average
  # outputCols <- setdiff(colnames(sim$monthlyAverages), c("pixelGroup", "year", "month"))
  # sim$annualAverages <- sim$dailyOutput[, lapply(.SD, mean), by = c("pixelGroup", "year"), .SDcols = outputCols]
  
  # remove the inputs/outputs folder of the temporary Biome-BGC folder
  # because it can fill up disk space.
  purgeBGCdirs(bbgcPath)
  
  return(invisible(sim))
}

createBGCdirs <- function(sim) {
  bbgcPath <- params(sim)$BiomeBGC_core$bbgcPath
  # Get all input files
  inputFiles <- extractInputFiles(c(sim$bbgcSpinup.ini, sim$bbgc.ini))
  
  # Create the folder structure
  vapply(unique(dirname(inputFiles)), function(d) {
    dir.create(file.path(bbgcPath, "inputs", d), recursive = TRUE, showWarnings = FALSE)
    
  }, logical(1))
  dir.create(file.path(bbgcPath, "outputs"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(bbgcPath, "inputs", "ini"), recursive = TRUE, showWarnings = FALSE)
  
  # Copy input files to simulation directory
  vapply(unique(inputFiles), function(d) {
    file.copy(from = file.path(params(sim)$BiomeBGC_core$bbgcInputPath, d),
              to = file.path(bbgcPath, "inputs", d), overwrite = TRUE)
  }, logical(1))
  
  # Copy ini files into input directory
  lapply(sim$pixelGroupParameters$pixelGroup, function(pixelGroup_i){
    # Copy ini file into input directory
    ini <- sim$bbgc.ini[[as.character(pixelGroup_i)]]
    fileName <- file.path(bbgcPath, "inputs" ,"ini", paste0(pixelGroup_i, ".ini"))
    iniWrite(ini, fileName = fileName)
    # Copy spinup ini file into input directory
    ini <- sim$bbgcSpinup.ini[[as.character(pixelGroup_i)]]
    fileName <- file.path(bbgcPath, "inputs" ,"ini", paste0(pixelGroup_i, "_spinup.ini"))
    iniWrite(ini, fileName = fileName)
  })
  return(invisible(sim))
}

extractInputFiles <- function(inis){
  inputFilePaths <- c()
  for (ini in inis){
    metInputPath <- iniGet(ini, "MET_INPUT", 1)
    restartInputPath <- ifelse(iniGet(ini, "RESTART", 1) == "0", NA, iniGet(ini, "RESTART", 5))
    co2Inputs <- ifelse(iniGet(ini, "CO2_CONTROL", 1) == "0",
                        NA,
                        iniGet(ini, "CO2_CONTROL", 3))
    epcInputs <- iniGet(ini, "EPC_FILE", 1)
    inputFilePaths <- c(inputFilePaths,
                        c(metInputPath, restartInputPath, co2Inputs, epcInputs)) |>
      unique() |> na.omit()
  }
  inputFilePaths <- gsub("inputs/", "",inputFilePaths)
  return(inputFilePaths)
}

readDailyOutput <- function(res){
  # file
  dailyOutputFile <- paste0(iniGet(res, "OUTPUT_CONTROL", 1), ".dayout")
  
  # get the number of entries in binFile
  nbYears <- as.integer(iniGet(res, "TIME_DEFINE", 2))
  nbDays <- nbYears * 365
  nbOutputs <- as.integer(iniGet(res, "DAILY_OUTPUT", 1))
  entries <- nbDays * nbOutputs
  
  # read the bin daily output file
  outputMatrix <- readBin(dailyOutputFile, double(), entries, size = 4)
  outputMatrix <- matrix(outputMatrix, nrow = nbDays, ncol = nbOutputs, byrow = TRUE)
  
  # assign column names
  colNames <- res$DAILY_OUTPUT$comment[-c(1,2)]
  colnames(outputMatrix) <- colNames
  
  # add year and julian date
  firstyear <- as.integer(iniGet(res, "TIME_DEFINE", 3))
  dailyOutput <- data.frame(
    year = rep(firstyear:(firstyear+nbYears-1), each = 365),
    day = rep(1:365),
    timestep = c(1:nbDays),
    outputMatrix
  )
  return(dailyOutput)
}

readMonthlyAverages <- function(res){
  # Get columns names
  colNames <- res$DAILY_OUTPUT$comment[-c(1,2)]
  # Get monthly averages output file location
  monAvgFile <- paste0(iniGet(res, "OUTPUT_CONTROL", 1), ".monavgout.ascii")
  # Read monthly averages output file
  monAvg <- read.table(monAvgFile, header = FALSE, col.names = colNames)
  # Add month and year
  firstyear <- as.integer(iniGet(res, "TIME_DEFINE", 3))
  nbYears <- as.integer(iniGet(res, "TIME_DEFINE", 2))
  monAvg <- data.frame(
    year = rep(firstyear:(firstyear+nbYears-1), each = 12),
    month = rep(1:12),
    monAvg
  )
  return(monAvg)
}

readAnnualSummary <- function(ini, path){
  
  # Get column names
  colNames <- c("year", "prcp", "tavg", "LAI", "ET", "OF", "NPP", "NPB")
  
  # Get annual output file location
  annualOutputFile <- paste0(iniGet(ini, "OUTPUT_CONTROL", 1), "_ann.txt")
  
  # Read annual output file
  annualOutput <- read.table(file.path(path, annualOutputFile), header = FALSE, col.names = colNames, skip = 10)
  
  return(annualOutput)
}


purgeBGCdirs <- function(path){
  unlink(file.path(path, "outputs"), recursive=TRUE)
  unlink(file.path(path, "inputs"), recursive=TRUE)
}

.inputObjects <- function(sim) {
  
  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  # ! ----- EDIT BELOW ----- ! #
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

