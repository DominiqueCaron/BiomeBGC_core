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
    defineParameter("bgcPath", "character", tempdir(), NA, NA,
                    "Path to base directory to use for simulations."),
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
      objectName = "bbgc.co2",
      objectClass = "character",
      desc = paste("Biome-BGC carbon-dioxide concentration files.", 
                   "Path to the .txt files.")
    ),
    expectsInput(
      objectName = "bbgcSpinup.ini",
      objectClass = "character",
      desc = paste("Biome-BGC initialization files for the spinup.", 
                   "Path to the .ini files (one path per site/scenario).")
    ),
    expectsInput(
      objectName = "bbgc.epc",
      objectClass = "character",
      desc = paste("Biome-BGC ecophysiological constants files.", 
                   "Path to the .epc files.")
    ),
    expectsInput(
      objectName = "bbgc.ini",
      objectClass = "character",
      desc = paste("Biome-BGC initialization files.", 
                   "Path to the .ini files (one path per site/scenario).")
    ),
    expectsInput(
      objectName = "bbgc.met",
      objectClass = "character",
      desc = paste("Biome-BGC meterological data files.", 
                   "Path to the .met files (one path per site/scenario).")
    )
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "outputControl", objectClass = "data.frame", desc = NA),
    createsOutput(objectName = "dailyOutput", objectClass = "data.frame", desc = NA),
    createsOutput(objectName = "annualOutput", objectClass = "data.frame", desc = NA)
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
  bgcPath <- params(sim)$BiomeBGC_core$bgcPath
  createBGCdirs(sim)

  ## Spinup
  # make sure the inputs for the spinups are available

  # execute spinups
  spinupIniPath <- file.path(bgcPath, "inputs" ,"ini", basename(sim$bbgcSpinup.ini))
  res <- bgcExecuteSpinup(argv, spinupIniPath, bgcPath)  
  
  ## Simulate
  # make sure the inputs for the main simulations are available
  
  # execute the simulations
  iniPath <- file.path(bgcPath, "inputs" ,"ini", basename(sim$bbgc.ini))
  res <- bgcExecute(argv, iniPath, bgcPath)
  
  ## Output processing
  sim$outputControl <- list()
  sim$dailyOutput <- list()
  sim$annualOutput <- list()
  for (i in 1:length(res[[2]])){
    sim$outputControl[[i]] <- readOutputControl(res[[2]][[i]])
    nbYears <- nrow(sim$outputControl[[i]])
    sim$dailyOutput[[i]] <- readDailyOutput(res[[2]][[i]], nbYears)
    sim$annualOutput[[i]] <- readAnnualOutput(res[[2]][[i]])
  }
  return(invisible(sim))
}

createBGCdirs <- function(sim) {
  bgcPath <- params(sim)$BiomeBGC_core$bgcPath
  # Get all input files
  inputFiles <- extractInputFiles(c(sim$bbgcSpinup.ini, sim$bbgc.ini))
  
  # Create the folder structure
  vapply(unique(dirname(inputFiles)), function(d) {
    dir.create(file.path(bgcPath, d), recursive = TRUE, showWarnings = FALSE)
  }, logical(1))
  dir.create(file.path(bgcPath, "outputs"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(bgcPath, "inputs", "ini"), recursive = TRUE, showWarnings = FALSE)
  
  ## Copy inputs to simulation directory
  iniPath <- file.path(bgcPath, "inputs" ,"ini", basename(sim$bbgc.ini))
  spinupIniPath <- file.path(bgcPath, "inputs" ,"ini", basename(sim$bbgcSpinup.ini))
  file.copy(sim$bbgcSpinup.ini, spinupIniPath)
  file.copy(sim$bbgc.ini, iniPath)
  file.copy(sim$bbgc.co2, file.path(bgcPath, sub("^.*[/\\]inputs[/\\]", "inputs/", sim$bbgc.co2)))
  file.copy(sim$bbgc.epc, file.path(bgcPath, sub("^.*[/\\]inputs[/\\]", "inputs/", sim$bbgc.epc)))
  file.copy(sim$bbgc.met, file.path(bgcPath, sub("^.*[/\\]inputs[/\\]", "inputs/", sim$bbgc.met)))
}

extractInputFiles <- function(iniPaths){
  inputFilePaths <- c()
  for (i in iniPaths){
    ini <- iniRead(i)
    metInputPath <- iniGet(ini, "MET_INPUT", 1)
    restartInputPath <- ifelse(iniGet(ini, "RESTART", 1) == "0", NA, iniGet(ini, "RESTART", 5))
    co2Inputs <- iniGet(ini, "CO2_CONTROL", 3)
    epcInputs <- iniGet(ini, "EPC_FILE", 1)
    inputFilePaths <- c(inputFilePaths,
                        c(metInputPath, restartInputPath, co2Inputs, epcInputs)) |>
      unique() |> na.omit()
  }
  return(inputFilePaths)
}

readOutputControl <- function(res){
  # 
  outputControlFile <- paste0(res$OUTPUT_CONTROL$value[2], "_ann.txt")
  colNames <- c("year", "annPRCP", "annTavg", "maxLAI", "annET", "annOF", "annNPP", "annNBP")
  outputControl <- read.table(
    outputControlFile,
    skip = 10,
    header = FALSE,
    col.names = colNames
  )
  return(outputControl)
  
}

readDailyOutput <- function(res, nbYears){
  #
  colNames <- res$DAILY_OUTPUT$comment[-c(1,2)]
  dailyOutputFile <- paste0(res$OUTPUT_CONTROL$value[2], ".dayout.ascii")
  dailyOutput <- read.table(dailyOutputFile, header = FALSE, col.names = colNames)
  dailyOutput <- data.frame(
    year = rep(1:nbYears, each = 365),
    day = rep(1:365),
    dailyOutput
  )
  return(dailyOutput)
}

readAnnualOutput <- function(res){
  # 
  colNames <- res$ANNUAL_OUTPUT$comment[-c(1,2)]
  colNames <- gsub(" ", "_", colNames)
  annualOutputFile <- paste0(res$OUTPUT_CONTROL$value[2], ".annout.ascii")
  annualOutput <- read.table(annualOutputFile, header = FALSE, col.names = colNames)
  annualOutput <- data.frame(
    year = 1:nrow(annualOutput),
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

