### CRAN 0.1.8 | GitHub 0.1.8

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)[![Downloads Total](https://cranlogs.r-pkg.org/badges/grand-total/SemNetDictionaries?color=brightgreen)](https://cran.r-project.org/package=SemNetDictionaries) [![Downloads per month](http://cranlogs.r-pkg.org/badges/SemNetDictionaries?color=brightgreen)](https://cran.r-project.org/package=SemNetDictionaries) 

# How To Install
```
if(!"devtools" %in% row.names(installed.packages())){
  install.packages("devtools")
}

devtools::install_github("AlexChristensen/SemNetDictionaries")
```

# How To Use
See our tutorial: https://psyarxiv.com/eht87/

Christensen, A. P., & Kenett, Y. N. (in press). Semantic network analysis (SemNA): A tutorial on preprocessing, estimating, and analyzing semantic networks. *Psychological Methods*. doi:10.31234/osf.io/eht87. OSF:10.17605/OSF.IO/HQXTC

# SemNetDictionaries
Implements dictionaries that can be used in the [SemNetCleaner](https://github.com/AlexChristensen/SemNetCleaner) package. Also includes several functions aimed at facilitating the text cleaning analysis in the [SemNetCleaner](https://github.com/AlexChristensen/SemNetCleaner) package. This package is designed to integrate and update word lists and dictionaries based on each user's individual needs by allowing users to store and save their own dictionaries.

# Goals
The main goal for this package is to allow R users to feasibly generate and create their own dictionaries for text cleaning. This goal is achieved by allowing users to create their own dictionaries which can be saved and stored for later use (see 'append.dictionary' function in this package). The second goal of this package is to create an open-source database for dictionaries that can be shared and used by other users.

# Adding Dictionaries To SemNetDictionaries
You can create your own dictionary by using the 'append.dictionary' function in this package. Once created, you can locate the dictionary on your computer using the 'find.dictionaries' function. Grab the dictionary data file ("YOUR_DICTIONARY_NAME.dictionary.rds") and upload your dictionary as an issue or pull request on this page. I will verify the dictionary and add it to the package as soon as possible.
