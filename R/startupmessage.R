.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to mousetrap 2.0.0!")
  packageStartupMessage("If you have used previous versions of mousetrap,\nplease note that some function and argument names have changed.")
  packageStartupMessage("Your old code should still work, but you might get deprecation warnings at some points.")
  packageStartupMessage("All changes are summarized in the package news which are available at \nhttp://pascalkieslich.github.io/mousetrap/news")
}
