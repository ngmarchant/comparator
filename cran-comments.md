## Comments

Fix WARN and NOTE messages identified by CRAN checks. This involved fixing 
deprecated C++ syntax and no longer specifying C++11 as a SystemRequirement.

## Test environments

* Fedora 41, R 4.4.2
* winbuilder (devel)

## R CMD check results

Status: 1 NOTE

Compilation used the following non-portable flag(s):
     ‘-Werror=format-security’ ‘-Wp,-D_GLIBCXX_ASSERTIONS’
     ‘-Wp,-U_FORTIFY_SOURCE,-D_FORTIFY_SOURCE=3’ ‘-march=x86-64’
     ‘-mno-omit-leaf-frame-pointer’ ‘-mtls-dialect=gnu2’

These flags are set by Fedora 41.
