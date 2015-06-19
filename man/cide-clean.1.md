CIDE-CLEAN 1 "JUNE 2015" cide-clean "Cide Manual"
=================================================

NAME
----

cide-clean - Removes old containers

SYNOPSIS
--------

*cide clean* [`--days`=<*n*>] [`--count`=<*n*>]

DESCRIPTION
-----------

`cide` can generate a lot of data on the box in terms of docker images and
containers. This command tries to removes old images with a simple heuristic.

*NOTE*: This command might interfer with other users of the docker daemon.

OPTIONS
-------

`--days`=<*n*>
  Number of days to keep the images. Defaults to 7

`--count`=<*n*>
  Maximum number of images to keep. Defaults to 10

SEE ALSO
--------

cide(1)
