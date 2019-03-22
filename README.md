# SemNetDictionaries
Implements dictionaries that can be used in the [SemNetCleaner](https://github.com/AlexChristensen/SemNetCleaner) package. Also includes several functions aimed at facilitating the text cleaning analysis in the [SemNetCleaner](https://github.com/AlexChristensen/SemNetCleaner) package. This package is designed to integrate and update word lists and dictionaries based on each user's individual needs by allowing users to store and save their own dictionaries.

# Goals
The main goal for this package is to allow R users to feasibly generate and create their own dictionaries for text cleaning. This goal is achieved by allowing users to create their own dictionaries which can be saved and stored for later use (see 'append.dictionary' function in this package). The second goal of this package is to create an open-source database for dictionaries that can be shared and used by other users.

# Adding Dictionaries To SemNetDictionaries
You can create your own dictionary by using the 'append.dictionary' function in this package. Once created, you can locate the dictionary on your computer using the 'find.dictionaries' function. Grab the dictionary data file ("YOUR_DICTIONARY_NAME.dictionary.Rdata") and upload your dictionary as an issue or pull request on this page. I will verify the dictionary and add it to the package as soon as possible.
