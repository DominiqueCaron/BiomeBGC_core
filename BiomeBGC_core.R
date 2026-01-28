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
    defineParameter("argv", "character", "-v3", NA, NA,
                    "Arguments for the BiomeBGC library (same as 'bgc' commandline application)."),
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
      
      # schedule plotting
      if (anyPlotting(P(sim)$.plots)) sim <- scheduleEvent(sim, end(sim), "BiomeBGC_core", "plot", eventPriority = 12)
      
    },
    plot = {
      figPath <- file.path(outputPath(sim), "BiomeBGC_figures")
      
      if("daily_npp" %in% names(sim$annualAverages)){
        NPPtrend <- OutputTrendPlot(sim, "daily_npp", annualSum = TRUE, ylab = "NPP (gC/m2/yr)")
        SpaDES.core::Plots(NPPtrend,
                           filename = "NPPtrend",
                           path = figPath,
                           ggsaveArgs = list(width = 10, height = 7, units = "in", dpi = 300),
                           types = "png")
        
        NPPstart <- OutputRaster(sim, start(sim), "daily_npp", annualSum = TRUE)
        LandscapeAvg <- round(mean(values(NPPstart, na.rm = TRUE)), 2)
        SpaDES.core::Plots(NPPstart,
                           filename = "NPPstart",
                           fn = terra::plot,
                           main = paste0("Landscape average NPP for year ", start(sim), ": ", LandscapeAvg, " gC/m2/yr"),
                           path = figPath,
                           deviceArgs = list(width = 7, height = 7, units = "in", res = 300),
                           types = "png")
        
        NPPend <- OutputRaster(sim, end(sim), "daily_npp", annualSum = TRUE)
        LandscapeAvg <- round(mean(values(NPPend, na.rm = TRUE)), 2)
        SpaDES.core::Plots(NPPend,
                           filename = "NPPend",
                           main = paste0("Landscape average NPP for year ", end(sim), ": ", LandscapeAvg, " gC/m2/yr"),
                           path = figPath,
                           deviceArgs = list(width = 7, height = 7, units = "in", res = 300),
                           types = "png")
      }
      
      if("daily_nep" %in% names(sim$annualAverages)){
        NEPtrend <- OutputTrendPlot(sim, "daily_nep", annualSum = TRUE, ylab = "NEP (gC/m2/yr)")
        SpaDES.core::Plots(NEPtrend,
                           filename = "NEPtrend",
                           path = figPath,
                           ggsaveArgs = list(width = 10, height = 7, units = "in", dpi = 300),
                           types = "png")
        
        NEPstart <- OutputRaster(sim, start(sim), "daily_nep", annualSum = TRUE)
        LandscapeAvg <- round(mean(values(NEPstart, na.rm = TRUE)), 2)
        SpaDES.core::Plots(NEPstart,
                           filename = "NEPstart",
                           fn = terra::plot,
                           main = paste0("Landscape average NEP for year ", start(sim), ": ", LandscapeAvg, " gC/m2/yr"),
                           path = figPath,
                           deviceArgs = list(width = 7, height = 7, units = "in", res = 300),
                           types = "png")
        
        NEPend <- OutputRaster(sim, end(sim), "daily_nep", annualSum = TRUE)
        LandscapeAvg <- round(mean(values(NEPend, na.rm = TRUE)), 2)
        SpaDES.core::Plots(NEPend,
                           filename = "NEPend",
                           main = paste0("Landscape average NEP for year ", end(sim), ": ", LandscapeAvg, " gC/m2/yr"),
                           path = figPath,
                           deviceArgs = list(width = 7, height = 7, units = "in", res = 300),
                           types = "png")
        
      }
      
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

OutputRaster <- function(sim, yearToPlot, outputVar, annualSum){
  
  # filter the datatable to keep just the relevant year and variable
  dt <- sim$annualAverages[year == yearToPlot, .SD, .SDcols = c("pixelGroup", outputVar)]
  
  # expand the datable by converting the pixelGroup to pixels
  r <- sim$pixelGroupMap
  
  r <- classify(r,
                rcl = data.frame(is = dt$pixelGroup, becomes = dt[,get(outputVar)]))
  
  # if primary productivity transform units to gC/m2/yr
  if (outputVar %in% c("daily_npp", "daily_nep", "daily_nee", "daily_gpp"))
    r <- r * 1000
  
  # apply the annual sum
  if(annualSum) r <- r * 365
  
  return(r)
}

OutputTrendPlot <- function(sim, outputVar, annualSum = FALSE, ylab){
  # get the variables of interest and the dominant species
  dt <- merge.data.table(sim$annualAverages[, .SD, .SDcols = c("pixelGroup", "year", outputVar)],
                         sim$pixelGroupParameters[, .(pixelGroup, dominantSpecies, climatePolygon)])
  
  # expand the data table by converting pixelGroup to pixels
  forestedPixelGroups <- data.table(
    pixelGroup = values(sim$pixelGroupMap, na.rm=TRUE, mat = FALSE)
  )
  forestedPixelGroups[, pixId := .I]
  
  dt <- merge.data.table(forestedPixelGroups, dt, allow.cartesian = TRUE)
  
  # apply the annual sum
  if(annualSum) dt[, (outputVar) := get(outputVar) * 365 ]
  
  # if primary productivity transform units to gC/m2/yr
  if (outputVar %in% c("daily_npp", "daily_nep", "daily_nee", "daily_gpp"))
    dt[, (outputVar) := get(outputVar) * 1000 ]
  
  # calculate the across-pixel annual mean with 95% interval
  dt <- dt[ ,.(annMean = mean(get(outputVar)), annLower95perc = quantile(get(outputVar), 0), annUpper95perc = quantile(get(outputVar), 1)), by = .(year, dominantSpecies, climatePolygon)]
  
  # make the plot
  p <- ggplot(dt) +
    geom_ribbon(aes( x = year, ymin = annLower95perc, ymax = annUpper95perc, fill = dominantSpecies ), alpha = 0.5) +
    geom_line(aes(x = year, y = annMean, color = dominantSpecies)) +
    labs(x = "Year", y = ylab, color = "Dominant species", fill = "Dominant species") +
    theme_bw() +
    facet_wrap(~climatePolygon, labeller = as_labeller(function(labels) {paste0("Climate polygon: ", labels)}))
  
  return(p)
}

### template initialization
Init <- function(sim) {
  ## Arguments for the BiomeBGC library
  argv <- P(sim)$argv
  
  ## Set the simulation directory
  bbgcPath <- P(sim)$bbgcPath
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
    parallel::clusterExport(cl, c("argv", "bbgcPath", "readDailyOutput", "readMonthlyAverages", "readAnnualAverages"), envir = environment())
    
    # Execute the spinup in parallel
    message("Running the spinup for ", n_pixelGroups, " pixelGroups in parallel using ", n_cores, " cores.")
    parLapply(cl, spinupIniPaths, function(iniPath) {
      resi <- bgcExecuteSpinup(argv, iniPath, normalizePath(bbgcPath))
      if (resi[[1]] != 0)
        stop("Spinup error.")
    })
    # Run the main simulation in parallel
    message("Running the main simulations in parallel using ", n_cores, " cores.")
    res <- parLapply(cl, iniPaths, function(iniPath) {
      resi <- bgcExecute(argv, iniPath, normalizePath(bbgcPath))
      
      if (resi[[1]] != 0)
        stop("Simulation error.")
      
      return(resi[[2]][[1]])
    })
    
    # Read the outputs
    message("Reading the output files.")
    sim$dailyOutput <- parLapply(cl, res, readDailyOutput) |> rbindlist(idcol = "pixelGroup")
    sim$dailyOutput$pixelGroup <- as.numeric(names(sim$bbgc.ini))[sim$dailyOutput$pixelGroup]
    sim$monthlyAverages <- parLapply(cl, res, readMonthlyAverages) |> rbindlist(idcol = "pixelGroup")
    sim$monthlyAverages$pixelGroup <- as.numeric(names(sim$bbgc.ini))[sim$monthlyAverages$pixelGroup]
    sim$annualAverages <- parLapply(cl, res, readAnnualAverages) |> rbindlist(idcol = "pixelGroup")
    sim$annualAverages$pixelGroup <- as.numeric(names(sim$bbgc.ini))[sim$annualAverages$pixelGroup]
    
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
      resi <- bgcExecute(argv, iniPath, bbgcPath)
      
      if (resi[[1]] != 0)
        stop("Simulation error.")
      
      return(resi[[2]][[1]])
    })
    
    # Read the outputs
    message("Reading the output files.")
    sim$dailyOutput <- lapply(res, readDailyOutput) |> rbindlist(idcol = "pixelGroup")
    sim$dailyOutput$pixelGroup <- as.numeric(names(sim$bbgc.ini))[sim$dailyOutput$pixelGroup]
    sim$monthlyAverages <- lapply(res, readMonthlyAverages) |> rbindlist(idcol = "pixelGroup")
    sim$monthlyAverages$pixelGroup <- as.numeric(names(sim$bbgc.ini))[sim$monthlyAverages$pixelGroup]
    sim$annualAverages <- lapply(res, readAnnualAverages) |> rbindlist(idcol = "pixelGroup")
    sim$annualAverages$pixelGroup <- as.numeric(names(sim$bbgc.ini))[sim$annualAverages$pixelGroup]
    
  }
  
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
  # file
  monAvgFile <- paste0(iniGet(res, "OUTPUT_CONTROL", 1), ".monavgout")
  
  # get the number of entries in binFile
  nbYears <- as.integer(iniGet(res, "TIME_DEFINE", 2))
  nbMonths <- nbYears * 12
  nbOutputs <- as.integer(iniGet(res, "DAILY_OUTPUT", 1))
  entries <- nbMonths * nbOutputs
  
  # read the bin daily output file
  outputMatrix <- readBin(monAvgFile, double(), entries, size = 4)
  outputMatrix <- matrix(outputMatrix, nrow = nbMonths, ncol = nbOutputs, byrow = TRUE)
  
  # assign column names
  colNames <- res$DAILY_OUTPUT$comment[-c(1,2)]
  colnames(outputMatrix) <- colNames
  
  # add year and julian date
  firstyear <- as.integer(iniGet(res, "TIME_DEFINE", 3))
  nbYears <- as.integer(iniGet(res, "TIME_DEFINE", 2))
  monAvg <- data.frame(
    year = rep(firstyear:(firstyear+nbYears-1), each = 12),
    month = rep(1:12),
    outputMatrix
  )
  return(monAvg)
}

readAnnualAverages <- function(res){
  # file
  annAvgFile <- paste0(iniGet(res, "OUTPUT_CONTROL", 1), ".annavgout")
  
  # get the number of entries in binFile
  nbYears <- as.integer(iniGet(res, "TIME_DEFINE", 2))
  nbOutputs <- as.integer(iniGet(res, "DAILY_OUTPUT", 1))
  entries <- nbYears * nbOutputs
  
  # read the bin daily output file
  outputMatrix <- readBin(annAvgFile, double(), entries, size = 4)
  outputMatrix <- matrix(outputMatrix, nrow = nbYears, ncol = nbOutputs, byrow = TRUE)
  
  # assign column names
  colNames <- res$DAILY_OUTPUT$comment[-c(1,2)]
  colnames(outputMatrix) <- colNames
  
  # add year and julian date
  firstyear <- as.integer(iniGet(res, "TIME_DEFINE", 3))
  nbYears <- as.integer(iniGet(res, "TIME_DEFINE", 2))
  annAvg <- data.frame(
    year = firstyear:(firstyear+nbYears-1),
    outputMatrix
  )
  return(annAvg)
}

# Not used. The annual summary is only produced when argv includes -a, which is 
# unstable...
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

