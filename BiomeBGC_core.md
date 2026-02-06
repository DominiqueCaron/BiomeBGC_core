---
title: "BiomeBGC_core"
author: 
  - Dominique Caron
  - Céline Boisvenue
date: "February 2026"
output:
  html_document:
    keep_md: yes
editor_options:
  chunk_output_type: console
---



# Overview

Runs Biome-BGC version 4.2 through the R package `PredictiveEcology/BiomeBGCR`.

# Parameters

Provide a summary of user-visible parameters.


|paramName        |paramClass |default      |min |max |paramDesc                                                                                                                  |
|:----------------|:----------|:------------|:---|:---|:--------------------------------------------------------------------------------------------------------------------------|
|argv             |character  |-v3          |NA  |NA  |Arguments for the BiomeBGC library (same as 'bgc' commandline application).                                                |
|bbgcPath         |character  |C:\Users.... |NA  |NA  |Path to base directory to use for simulations.                                                                             |
|bbgcInputPath    |character  |C:\Users.... |NA  |NA  |Path to the Biome-BGC input directory.                                                                                     |
|parallel.cores   |integer    |1            |1   |NA  |Number of cores used to execute the simulation                                                                             |
|.plots           |character  |screen       |NA  |NA  |Used by Plots function, which can be optionally used here                                                                  |
|.plotInitialTime |numeric    |0            |NA  |NA  |Describes the simulation time at which the first plot event should occur.                                                  |
|.plotInterval    |numeric    |NA           |NA  |NA  |Describes the simulation time interval between plot events.                                                                |
|.saveInitialTime |numeric    |NA           |NA  |NA  |Describes the simulation time at which the first save event should occur.                                                  |
|.saveInterval    |numeric    |NA           |NA  |NA  |This describes the simulation time interval between save events.                                                           |
|.studyAreaName   |character  |NA           |NA  |NA  |Human-readable name for the study area used - e.g., a hash of the studyarea obtained using `reproducible::studyAreaName()` |
|.seed            |list       |             |NA  |NA  |Named list of seeds to use for each event (names).                                                                         |
|.useCache        |logical    |FALSE        |NA  |NA  |Should caching of events or module be used?                                                                                |

# Events

Describe what happens for each event type.

## Plotting

Write what is plotted.

## Saving

Write what is saved.

# Data dependencies

## Input data

Input data are Biome-BGC's ini inputs for the spinup and simulation. These can be prepared by another module (e.g., `BiomeBGC_dataPrep`) or read in with the function `BiomeBGCR::iniRead()`. If reading in ini files, make sure the other input files (e.g., ecophysiological constants, meteorological data, etc.) are in the project folder.


|objectName     |objectClass |desc                                                                                                |sourceURL |
|:--------------|:-----------|:---------------------------------------------------------------------------------------------------|:---------|
|bbgcSpinup.ini |character   |Biome-BGC initialization files for the spinup. Path to the .ini files (one path per site/scenario). |NA        |
|bbgc.ini       |character   |Biome-BGC initialization files. Path to the .ini files (one path per site/scenario).                |NA        |

## Output data

Description of the module outputs.


|objectName      |objectClass |desc                                                                                                                                                                                                     |columns      |
|:---------------|:-----------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:------------|
|annualSummary   |data.table  |A summary table a fixed set of outputs for each pixelGroup and year.                                                                                                                                     |The site.... |
|dailyOutput     |data.table  |The ouput variables for each pixelGroup and day. The units can be find here: https://raw.githubusercontent.com/PredictiveEcology/BiomeBGCR/refs/heads/development/src/Biome-BGC/src/include/bgc_struct.h |             |
|monthlyAverages |data.table  |The daily output variables averaged for each month. The same units than the dailyOutput.                                                                                                                 |             |
|annualAverages  |data.table  |The daily output variables averaged for each month. The same units than the dailyOutput.                                                                                                                 |             |

# Links to other modules

Inputs can be prepared by `BiomeBGC_dataPrep`, and outputs can be validated with `BiomeBGC_validationFluxTower`.
