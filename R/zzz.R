.onAttach <- function(libname, pkgname)
{
    msg <- paste0(
        "\nSemNetDictionaries (version ", utils::packageVersion(pkgname), ")",
        "\nFor help getting started, see <https://doi.org/10.1037/met0000463>",
        "\nSubmit your own dictionary and moniker glossaries to:",
        "\n <https://github.com/AlexChristensen/SemNetDictionaries/issues/new/choose>\n"
    )

    packageStartupMessage(msg)
}
