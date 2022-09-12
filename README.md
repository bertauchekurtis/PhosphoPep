# PhosphoPep
An online retention time prediction tool focusing on peptides with phosphorylation modifications

## How to Use:
Phosphopep is hosted online with a friendly GUI. If you have any issues with the online version and/or prefer to run the app locally offline, you may also clone this repository and run the app (see more information below).

Online:
[Use the Online App Here at shinyapps.io](http://kbertauche.shinyapps.io/phosphopep)

Offline:
-Clone/download this repository
-Run the following command in your terminal (replace the "~" with the path to the application's directory:
R -e "shiny::runApp('~/PhosphoPep/app.R')"

If succesful, the last line of the terminal will indicate "listening" with a message such as "Listening on http://xxx.x.x.x:xxxx"
Copy the address (http://xxx.x.x.x:xxxx) and paste into a browser to view and use the app.

For more information about running RShiny apps locally, see [this resource (click)](https://shiny.rstudio.com/articles/running.html)

Prerequisites to running PhosphoPep app:
-Installed version of R
Libraries:
-shiny
-stringr
-shinythemes
-shinyBS
-shinycssloaders
-glmnet
-xgboost
-plotrix


## Models
PhosphoPep feature the following models:
-Simple Linear Regression
-Best Subset Regression
-Ridge Regression
-Lasso Regression
-Elastic Net Regression
-Extreme Gradient Boosting
-Support Vector Regression
