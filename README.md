# PhosphoPep
An online retention time prediction tool focusing on peptides with phosphorylation modifications

## Description
PhosphoPep is a collection of statistical and machine learning models used for prediction the retention times of post-translationally modified peptides. PhosphoPep was developed specifically with the goal of creating a tool which excels in predictions peptides including phosphorylation modifications. The models support both phosphorylation and oxidation modifications.

A sample data set is provided within the app to demonstrate functionality. Users may also upload their own data that they wish to predict. Additionally, if users have their own data and known retention times for some or all of their data, a separate alignment data file may be uploaded to increase prediction accuracy for specific user data.

## Online Usage
The PhosphoPep app is hosted online, however, due to memory constraints, not all models included in PhosphoPep are available in the online version. To utilize all of the models included in PhosphoPep, please see the instructions for running the application offline locally.

[Use the Online App Here at shinyapps.io](http://kbertauche.shinyapps.io/phosphopep)

## Offline Usage

R Version Requried: R Version 4.1.0 or higher (https://www.r-project.org)

### Required Packages

```
install.packages(c("shiny","stringr","shinythemes","shinyBS","shinycssloaders","glmnet","xgboost","plotrix"))
```
List of Packages:
 - shiny
 - stringr
 - shinythemes
 - shinyBS
 - shinycssloaders
 - glmnet
 - xgboost
 - plotrix

Then, download or clone this repository.

---
### Starting Application:

#### From Command Line:

First, navigate into downloaded repository into the same directory as app.R

Then, execute the following command to launch the application:
```
R -e "library(shiny);runApp(launch.browser=TRUE)"
```
Subsequently, the application will launch and an interactive GUI will open in your default browser.

#### From R:

Working in the same directory that app.R is save in, execute the following R commands:
```
library(shiny)
runApp(launch.browser = TRUE)
```
## Formatting Custom Data

### Custom Data (no alignment data):
Data should be formatted into a single column with the header "PeptideSequence". Each peptide should be represented with a character string, with one character per amino acid. Unmodified peptides should use upper case letters, and modified peptides should be indicated with a lower case letter. 

"S", "T", and "Y" may be used for phosphorylated serine, threonine, or tryosine, respectively. "M" may be used for oxidated methionine. Other modifications are not supported and should not be included in custom data.

Users may choose to hold data in either a .csv or .tsv file. Both filetypes are supported in the application.

Example:
```
PeptideSequence,
AsMTyS,
AAStSyPGD,
HYQmmsDRS,
```

### Custom Data (with alignment data):
If using custom data with alignment data, two files should be proivded:
- The Alignment Data File
- The File of Peptides to Predict

The file of peptides to predict should follow the same formatting as a custom data without alignment data (see above).

The alignment data file should be formatted very similarly to the file of peptides to predict, with the addition of a second column hodling known retention times. Data should be formatted into two columns with the headers "PeptideSequence" and "Retention Time". Each peptide should be represented with a character string, with one character per amino acid. Unmodified peptides should use upper case letters, and modified peptides should be indicated with a lower case letter. 

"S", "T", and "Y" may be used for phosphorylated serine, threonine, or tryosine, respectively. "M" may be used for oxidated methionine. Other modifications are not supported and should not be included in custom data.

The retention time column should have the known retention time for the peptide denoted in minutes.

Users may choose to hold data in either a .csv or .tsv file. Both filetypes are supported in the application.

Example:
```
PeptideSequence, Retention Time,
AsMTyS, 12.67,
MTsYRRS, 20.32,
QHtSmY, 14.98,
```
