# to create the package (install usethis package if needed)
# this is all in the book "R packages" https://r-pkgs.org/


# step 1 (create the package)
usethis::create_package("/home/luisagi/GitHub")


# step 2 (connect it with git; answer the questions in the console)
usethis::use_git()


# step 3 (create a function)
## open new r file
## write your function
## save it in the directory "R"
## to edit it do [usethis::use_r("function_name")]


# step 4 (load your functon(s))
devtools::load_all()


# step 5 (commit changes)
## use preferred method (Rstudio git menu is easy)


# step 6 (check if pakacge works)
devtools::check()


# step 7 (edit DESCRIPTION)
## save after edits


# step 8 (add license)
usethis::use_gpl3_license()


# step 9 (prepare documentation)
## document function(s) using roxygen2 options
### go to function file, click the menu "Code" and then "nsert roxygen skeleton"
### shortcut (Alt+Ctrl+Shift+R)
### edit documentation
## run document function to prepare documentation files
devtools::document()

## see documentation
?f1


# step 10 (check again)
devtools::check()


# step 11 (install and use package)
## install
devtools::install()

## load package
library(testpack)

## use function
print(f1(text = "simple"))


# steps 12 add tests (not doing this, but check the book)


# using package developing version despite previous versions install
usethis::use_package("testpack")


# connecting package to GitHub
## remember to commit changes first

## store your token (copy and paste token, the next steps do not work for me, I will show you another way)
gitcreds::gitcreds_set()

## connect to github
usethis::use_github()

## the other ways is...
