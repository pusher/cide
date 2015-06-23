CIDE-DEBUG 1 "JUNE 2015" cide-debug "Cide Manual"
=================================================

NAME
----

cide-debug - Opens a debug console in the last project image

SYNOPSIS
--------

*cide debug* [`-n`|`--name`=<*name*>] [`--user`=<*username*>]

DESCRIPTION
-----------

Sometimes it is useful to run commands inside of the same environment where the
CI tests are run from. For example if an error doesn't reproduce on the
developer's machine.

*cide debug* starts a new temporary container (and linked containers) in the
previously generated container. This requires the user to have run *cide
build* previously.

OPTIONS
-------

`-n`, `--name`=<*name*>
  Name of the image. This is generally not required unless a name has been
  given during the *cide build* phase as well.

  Defaults to the basename(1) of the current directory.

`--user`=<*username*>
  Selects the user to run the container under. By default the `cide` user is
  selected. In some situations it is useful to run the container as `root`
  (when poking at system dependencies)

SEE ALSO
--------

cide(1)
