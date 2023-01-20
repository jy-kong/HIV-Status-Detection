# R Shiny Application - HIV Status Detection 🦠
  A classification approach: HIV-positive (➕) or HIV-negative (➖)





🩸 This is a web application deployed using Shiny. 🩸
 
 Due to the limitation of shinyapps.io for Free Plan users, the bundle size that can be uploaded is limited to 1 GB only.
 Since this project involves a huge amount of datasets, deployment into a website link is not feasible!

The main purpose of this guideline is to illustrate how to run Shiny apps from a remote source. 
There are 2 ways to access the website applcation / data product in the local machine: 🔎

Method 1: 📖
1) Register an account on R Studio Cloud.
   Link: https://posit.cloud
2) Run this command on the console: 

shiny::runGitHub("HIV-Status-Detection", "jy-kong", subdir = "r-shiny/")


Method 2: 📖
1) Install R and R Studio.
   Link: https://posit.co/download/rstudio-desktop/
2) Run this command on the console: 

shiny::runGitHub("HIV-Status-Detection", "jy-kong", subdir = "r-shiny/")
