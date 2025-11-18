## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "BiomeBGC_core",
  description = "",
  keywords = "",
  authors = structure(list(list(given = "Dominique", family = "Caron", role = c("aut", "cre"), email = "dominique.caron@nrcan-rncan.gc.ca", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(BiomeBGC_core = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "BiomeBGC_core.Rmd"),
  reqdPkgs = list("PredictiveEcology/SpaDES.core@box (>= 2.1.8.9013)", "ggplot2", "PredictiveEcology/BiomeBGCR@development"),
  parameters = bindrows(
    defineParameter("argv", "character", "-a", NA, NA,
                    "Arguments for the BiomeBGC library (same as 'bgc' commandline application)"),
    defineParameter("bbgcPath", "character", tempdir(), NA, NA,
                    "Path to base directory to use for simulations."),
    defineParameter("bbgcInputPath", "character", inputPath(sim), NA, NA,
                    "Path to the Biome-BGC input directory."),
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
    createsOutput(objectName = "annualOutput", objectClass = "list", desc = NA),
    createsOutput(objectName = "dailyOutput", objectClass = "list", desc = NA),
    createsOutput(objectName = "monthlyAverages", objectClass = "list", desc = NA)
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
  argv <- params(sim)$BiomeBGC_core$argv
  
  ## Set the simulation directory
  bbgcPath <- params(sim)$BiomeBGC_core$bbgcPath
  createBGCdirs(sim)
  
  ## Spinup
  if(!is.null(sim$bbgcSpinup.ini)){
    # make sure the inputs for the spinups are available
    
    # execute spinups
    spinupIniPath <- file.path(bbgcPath, "inputs" ,"ini", basename(sim$bbgcSpinup.ini))
    res <- bgcExecuteSpinup(argv, spinupIniPath, bbgcPath)  
  }

  ## Simulate
  # make sure the inputs for the main simulations are available
  
  # execute the simulations
  iniPath <- file.path(bbgcPath, "inputs" ,"ini", basename(sim$bbgc.ini))
  res <- bgcExecute(argv, iniPath, bbgcPath)
  
  ## Output processing
  sim$dailyOutput <- list()
  sim$monthlyAverages <- list()
  #sim$annualAverages <- list()
  sim$annualOutput <- list()
  for (i in 1:length(res[[2]])){
    sim$dailyOutput[[i]] <- readDailyOutput(res[[2]][[i]])
    sim$monthlyAverages[[i]] <- readMonthlyAverages(res[[2]][[i]])
    #sim$annualAverages[[i]] <- readAnnualAverages(res[[2]][[i]])
    sim$annualOutput[[i]] <- readAnnualOutput(res[[2]][[i]])
  }
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
              to = file.path(bbgcPath, "inputs", d))
  }, logical(1))
  
  # Copy ini file into input directory
  iniPath <- file.path(bbgcPath, "inputs" ,"ini", basename(sim$bbgc.ini))
  file.copy(sim$bbgc.ini, iniPath)
  
  # If there is a spinup, copy spinup ini file into input directory
  if(!is.null(sim$bbgcSpinup.ini)){
    spinupIniPath <- file.path(bbgcPath, "inputs" ,"ini", basename(sim$bbgcSpinup.ini))
    file.copy(sim$bbgcSpinup.ini, spinupIniPath)
  }
}

extractInputFiles <- function(iniPaths){
  inputFilePaths <- c()
  for (i in iniPaths){
    ini <- iniRead(i)
    metInputPath <- iniGet(ini, "MET_INPUT", 1)
    restartInputPath <- ifelse(iniGet(ini, "RESTART", 1) == "0", NA, iniGet(ini, "RESTART", 5))
    co2Inputs <- ifelse(iniGet(ini, "CO2_CONTROL", 0) == "0", NA, iniGet(ini, "CO2_CONTROL", 3))
    epcInputs <- iniGet(ini, "EPC_FILE", 1)
    inputFilePaths <- c(inputFilePaths,
                        c(metInputPath, restartInputPath, co2Inputs, epcInputs)) |>
      unique() |> na.omit()
  }
  inputFilePaths <- gsub("inputs/", "",inputFilePaths)
  return(inputFilePaths)
}

readOutputControl <- function(res){
  # Get the path to the annual abiotic variable output
  outputControlFile <- paste0(iniGet(res, "OUTPUT_CONTROL", 1), "_ann.txt")
  # Set the column names - DC 2025-11-08: Is that fixed?
  colNames <- c("year", "annPRCP", "annTavg", "maxLAI", "annET", "annOF", "annNPP", "annNBP")
  # Read the file
  outputControl <- read.table(
    outputControlFile,
    skip = 10, # DC 2025-11-08: Is that always true?
    header = FALSE,
    col.names = colNames
  )
  return(outputControl)
  
}

readDailyOutput <- function(res){
  # Get columns names
  colNames <- res$DAILY_OUTPUT$comment[-c(1,2)]
  # Get daily output file location
  dailyOutputFile <- paste0(iniGet(res, "OUTPUT_CONTROL", 1), ".dayout.ascii")
  # Read daily output file
  dailyOutput <- read.table(dailyOutputFile, header = FALSE, col.names = colNames)
  # Add year and julian date
  firstyear <- as.integer(iniGet(res, "TIME_DEFINE", 3))
  nbYears <- as.integer(iniGet(res, "TIME_DEFINE", 2))
  dailyOutput <- data.frame(
    year = rep(firstyear:(firstyear+nbYears-1), each = 365),
    day = rep(1:365),
    dailyOutput
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

readAnnualAverages <- function(res){
  # Get columns names
  colNames <- res$DAILY_OUTPUT$comment[-c(1,2)]
  # Get annual averages output file location
  annualAvgFile <- paste0(iniGet(res, "OUTPUT_CONTROL", 1), ".annavgout.ascii")
  # Read annual averages output file
  annualAvg <- read.table(annualAvgFile, header = FALSE, col.names = colNames)
  # Add year
  firstyear <- as.integer(iniGet(res, "TIME_DEFINE", 3))
  nbYears <- as.integer(iniGet(res, "TIME_DEFINE", 2))
  annualAvg <- data.frame(
    year = firstyear:(firstyear+nbYears-1),
    annualAvg
  )
  return(annualAvg)
}

readAnnualOutput <- function(res){
  # Get column names
  colNames <- res$ANNUAL_OUTPUT$comment[-c(1,2)]
  colNames <- gsub(" ", "_", colNames)
  # Get annual output file location
  annualOutputFile <- paste0(iniGet(res, "OUTPUT_CONTROL", 1), ".annout.ascii")
  # Read annual output file
  annualOutput <- read.table(annualOutputFile, header = FALSE, col.names = colNames)
  # Add year
  firstyear <- as.integer(iniGet(res, "TIME_DEFINE", 3))
  nbYears <- as.integer(iniGet(res, "TIME_DEFINE", 2))
  annualOutput <- data.frame(
    year = firstyear:(firstyear+nbYears-1),
    annualOutput
  )
  return(annualOutput)
}

.inputObjects <- function(sim) {
  
  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

