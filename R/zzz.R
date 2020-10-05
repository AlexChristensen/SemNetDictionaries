.onload <- function(libname, pkgname)
{library.dynam("SemNetDictionaries",package=pkgname,lib.loc=libname)}

.onAttach <- function(libname, pkgname)
{
    msg <- paste('\nFor help getting started, see <https://doi.org/10.31234/osf.io/eht87> \n\n')
    msg <- paste(msg,"Submit your own dictionary and moniker glossaries to:\n <https://github.com/AlexChristensen/SemNetDictionaries/issues/new/choose>\n")
    packageStartupMessage(msg)
}