## Comments

Fix compilation problem identified by CRAN checks.

## Test environments

* Fedora 35, R 4.1.2
* Windows 10, R 4.1.2
* winbuilder (devel)

## R CMD check results

Status: 1 NOTE

Compilation used the following non-portable
  flag(s):
  ‘-Werror=format-security’  ‘-Wp,-D_FORTIFY_SOURCE=2’
  ‘-Wp,-D_GLIBCXX_ASSERTIONS’

These flags are set by Fedora 35.
