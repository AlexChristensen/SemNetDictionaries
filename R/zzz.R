.onload <- function(libname, pkgname)
{library.dynam("SemNetDictionaries",package=pkgname,lib.loc=libname)}

.onAttach <- function(libname, pkgname)
{
    msg <- paste('For help getting started, type `browseVignettes("SemNetDictionaries")` \n')
    msg <- paste(msg,"Submit your own dictionaries to <https://github.com/AlexChristensen/SemNetDictionaries>")
    packageStartupMessage(msg)
}