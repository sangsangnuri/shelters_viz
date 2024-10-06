# runGitHub(repo, username = getOption("github.user"),
#           ref = "master", subdir = NULL, port = NULL,
#           launch.browser = getOption("shiny.launch.browser", interactive()))
# 
# # Can run an app from a subdirectory in the repo
# runGitHub("shiny_example", "rstudio", subdir = "inst/shinyapp/")

shiny::runGitHub('shelters_viz', 'sangsangnuri')


                 