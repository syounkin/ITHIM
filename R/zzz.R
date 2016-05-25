THISPKG <- "ITHIM"
.onAttach <- function(libname, pkgname) {
    version <- packageDescription("ITHIM", fields="Version")
    #date <- packageDescription("ITHIM", fields="Date")
    packageStartupMessage(paste("Welcome to ITHIM version ", version, ". Three little ducks.\n",
                                "~~~~~~~~~~~~~~~~~", "\n",
                                ">()_  >()_  >()_ ", "\n",
                                " (__)  (__)  (__)", "\n",
                                "~~~~~~~~~~~~~~~~~", sep = "" ) )
    }
