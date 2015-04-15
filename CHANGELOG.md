
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

