## Comments

Fix new-delete-type mismatch identified by gcc Asan.

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
