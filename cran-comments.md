## Test environments

* Ubuntu-latest
* MacOS 13
* MacOS 15.1
* Windows Latest
* Windows, R-development
* Ubuntu 22.04.5 LTS, R-devel (2024-12-17 r87446)
* Fedora Linux 38
* Fedora Linux 40

## R CMD check results

I received no WARNINGs or ERRORs. 

I received one NOTE on Ubuntu 22.04.5. The NOTE about '-Wp,-D_FORTIFY_SOURCE=3' is due to Ubuntu's default compiler flags for security hardening. This flag is automatically added by the system compiler and doesn't affect package functionality on other platforms where this security feature isn't available.



