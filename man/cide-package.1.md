CIDE-PACKAGE "FEBRUARY 2016" cide-package "Cide Manual"
=======================================================

NAME
----

cide-package - Builds a package from the run script

SYNOPSIS
--------

*cide package*  [`-n`|`-t`|`--name`=<*name*>] [`-s`|`--ssh-key`=<*path*>]
                [`--package`=<*package*>] [`--upload`]
                [`--set-version`]

DESCRIPTION
-----------

This tells `cide` to package a built package for you, and possibly upload it to
s3.

When invoke, cide will retrieve a previously created container (see `--name`),
perform a `cide` run, and package a given directory. The behavior is the same
as `exec` but will always export a directory, which it will upload to Amazon
s3.

OPTIONS
-------

`-n`, `-t`, `--name`
  The name (tag) of the container from which to retrieve the build.

`-s`, `--ssh-key`=<*path*>
  When the `use_ssh:` directive is set to true, where to get the ssh key to
  inject into the container. Defaults to `~/.ssh/id_rsa`.

`--package`=<*package*>
  The name of the package. The final archive will have the name
  `<package>.<timestamp>.<version>.tar.gz`, where timestamp is the time at
  which the packaging occured (in format `YYYY-mm-dd_HHMMSS`) and `<version>`
  is the package version (see `--set-version`).

`--upload`
  Whether the archive should be uploaded to Amazon s3. If the archive should be
  uploaded to s3 by the packager, the s3 credentials must be provided to
  `cide`. The s3 credentials must be exported as environment variables.

`--set-version`
  Sets a package version. Is is solely used when naming the archive (see
  `--package`). If no version is provided on the command line, a default
  version name will be extracted from git (short sha).

SEE ALSO
--------

cide(1)
