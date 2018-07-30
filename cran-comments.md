## Test environments
* macOS High Sierra, R 3.5.0
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs

## Submission comments

### Reviewer comments 20180730
Thanks, please add a DOI, arXiv or ISBN to your reference Hursh,
Raslear, Bauman, & Black, 1989 in your Description text in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
with no space after 'doi:', 'arXiv:' and angle brackets for auto-linking.

* Description has been updated with the DOI for that reference in brackets,
as requested.

We see code lines such as

Copyright 2015 - Baty and Delignette-Muller
Copyright 2016 - John C. Nash
Copyright 2016 - Hadley Wickham
Copyright 2013 - G. Grothendieck
Copyright - 2016 - Ben Bolker
Copyright - 2016 - Paul Johnson <pauljohn@@ku.edu>

Please add all authors and copyright holders in the Authors@R field with
the appropriate roles.

* Those lines indicated package dependencies prior to the submission to
CRAN. The lines are no longer needed as the package now states necessary
dependencies.

### Reviewer comments 20180720

Please write DOIs in brackets as suggested.

* DOIs are now in brackets

Please add examples to all exported functions.

* Examples have been added to all exported functions

### Reviewer comments 20180714

Version contains leading zeroes (0.1.00). Please change.

* Version has been updated to 0.1.0

Can you elaborate which methods are implemented for this task? Is there
some reference about the method you can add in the Description field in
the form Authors (year) <doi:.....>?

* Description has been updated with package capabilities and references.
