## Test environments

* Ubuntu-latest
* MacOS 13
* MacOS 15.1
* Windows Latest
* Windows, R-development
<<<<<<< HEAD
* Linux, R-development
=======
* Ubuntu 22.04.5 LTS, R-devel (2025-01-16 r87584)
* Fedora Linux 38
* Fedora Linux 40
>>>>>>> 82ec4e53c0d873a11a7c18fbe7dd9eeae7033ecf

## R CMD check results

This is a new submission, as fedmatch was archived on CRAN on 2025-01-10.

I received no WARNINGs or ERRORs in any checks.

I received one NOTE on Ubuntu 22.04.5. The NOTE about '-Wp,-D_FORTIFY_SOURCE=3' is due to Ubuntu's default compiler flags for security hardening. This flag is automatically added by the system compiler and doesn't affect package functionality on other platforms where this security feature isn't available.



<<<<<<< HEAD
- The package documentation generates several HTML NOTES regarding attributes like "type" in <link> and <script> tags, and missing "summary" in <table> tags. These are minor and do not affect the usability or functionality of the documentation. Necessary checks have been performed to ensure that the documentation remains accessible and informative.
=======
>>>>>>> 82ec4e53c0d873a11a7c18fbe7dd9eeae7033ecf

