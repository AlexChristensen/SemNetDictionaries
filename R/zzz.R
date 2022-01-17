.onload <- function(libname, pkgname)
{library.dynam("SemNetDictionaries",package=pkgname,lib.loc=libname)}

.onAttach <- function(libname, pkgname)
{
    msg <- styletext(styletext(paste("\nSemNetDictionaries (version ", packageVersion("SemNetDictionaries"), ")", sep = ""), defaults = "underline"), defaults = "bold")
    
    msg <- paste(msg, '\nFor help getting started, see <https://doi.org/10.1037/met0000463> \n\n')
    msg <- paste(msg,"Submit your own dictionary and moniker glossaries to:\n <https://github.com/AlexChristensen/SemNetDictionaries/issues/new/choose>\n")
    packageStartupMessage(msg)
}