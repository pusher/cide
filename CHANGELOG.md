
0.9.0 / 2016-02-26
==================

  * NEW: setting to inject versions in your package
  
```yaml
package:
  add_version:auto
```  

  * NEW: add `cide package --build-id` to identify archives 

0.8.1 / 2016-02-15
==================

  * FIX: cide package --set-version
  * Make sure the build root is not imported into the next build

0.8.0 / 2016-02-15
==================

  * NEW: `cide package` command to build archives from a project
  * CHANGE: Renamed `cide build` to `cide exec`

0.7.0 / 2016-02-10
==================

  * NEW: --guest_export_dir to override the path on guest machine
  * CHANGE/FIX: --run now takes a string instead of an array

0.6.3 / 2015-02-09
==================

  * FIX: docker version comparison

0.6.2 / 2015-10-27
==================

  * FIX: docker version detection

0.6.1 / 2015-07-08
==================

  * FIX: cleaning wasn't removing all the expected images
  * CHANGE: clean is keeping the last 20 images (vs. 10 before)

0.6.0 / 2015-07-03
==================

  * CHANGE: Remove the runtime options. --force, --skip, ... are not available anymore
  * FIX: Doc fixes and updates

0.5.0 / 2015-06-19
==================

  * NEW: man pages and doc !
  * CHANGE: config file is `cide.yml` instead of `.cide.yml`

0.4.1 / 2015-06-02
==================

  * FIX: Work around volume cleanup #9

0.4.0 / 2015-04-19
==================

  * NEW: Allow for arbitrary env on multiple levels
  * NEW: Better link image to name coercion. `foo/bar:tag` will now get a `bar` name.
  * NEW: Error reporting of broken linked containers
  * NEW: More control over the `add` directive. It's now possible to set the target path.
  * NEW: The `as_root` key is now a build step making it possible to set environment and add files.
  * NEW: `cide build --no-pull`
  * NEW: `cide debug` command to debug builds
  * CHANGE: Force cleaning of old images
  * FIX: Work around issue #7
  * FIX: caching issues with ssh keys
  * FIX: cide was using the wrong user to run the ci script
  * FIX: export_dir and ssh_key command-line options
  * FIX: running commands with arguments

0.2.0 / 2015-04-15
==================

  * NEW: .cide.yml schema loader
  * NEW: Support for linked containers
  * NEW: Much better build output
  * CHANGE: Docker version: 1.5.0+ is required
  * FIX: Avoid name clashes with projects who have a Dockerfile already
  * FIX: cide --export

0.1.1 / 2015-01-27
==================

  * Fix dangling containers

0.1.0 / 2015-01-19
==================

  * cide is not compatible with ruby 1.9.3
  * misc cleanup
  * NEW: runtime options

0.0.8 / 2014-12-18
==================

  * Allow to import SSH keys into the container

0.0.7 / 2014-12-04
==================

  * Fixes docker setup for boot2docker v1.3.0+

0.0.6 / 2014-10-08
==================

  * Fixes parameter precedence issue in the builds

0.0.5 / 2014-10-03
==================

  * FIX: Regression with exit-status handling

0.0.4 / 2014-10-03
==================

  * ADD: forward_env to forward environment variables from the host
  * ADD: Adds a "prepare" step that allows to cache build dependencies
  * ADD: `cide init` to bootstrap projects
  * ADD: `cide clean` to remove old images
  * ADD: Can set export dir for artifacts
  * CHANGE: Renames the "image" attribute to "from"
  * CHANGE: Renames the "commmand" attribute to "run"
  * Changes the generated Dockerfile quite a bit
  * Use thor for command-line parsing, modularize

0.0.3 / 2014-09-29
==================

  * Cleanup and fixes DATA usage
  * Removes the GEM_HOME environment setting
  * Installation instructions

0.0.2 / 2014-09-15
==================

 * Gemfile added
 * MIT license
 * adds the missing cide executable in the gem

0.0.1 / 2014-09-13
==================

First release

