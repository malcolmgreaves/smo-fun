# smo-fun
[![Build Status](https://travis-ci.org/malcolmgreaves/smo-fun.svg?branch=master)](https://travis-ci.org/malcolmgreaves/smo-fun) [![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.malcolmgreaves/smo-fun_2.11/badge.svg?style=plastic)](https://maven-badges.herokuapp.com/maven-central/io.malcolmgreaves/smo-fun_2.11)

A efficient implementation of the Sequential Minimal Optimization (SMO) algorithm for training Support Vector Machines (SVMs). Exposes training and prediction in a side-effect free, functional programming style.

To use in your own project, add the following to your `build.sbt`:
```
libraryDependencies += "io.malcolmgreaves" %% "smo-fun" % "X.Y.Z"
```
Where `X.Y.Z` is the latest version (check the maven central badge in this README). 

# Project Structure

This repository is split into subprojects:

* [smo-fun-core](https://github.com/malcolmgreaves/smo-fun/tree/master/smo-fun-core)
  * contains algorithm implementations
  * intended to be consumed as a library
  
* [smo-fun-cmd](https://github.com/malcolmgreaves/smo-fun/tree/master/smo-fun-cmd)
  * contains command line applications that use the code from `smo-fun-core`
  * intended to be used as a suite of tools to assist fellow machine learniner practitioners
 
### Versioning
Only the `smo-fun-core` project adheres to the published version semantics.

# Legal

The original author (Malcolm Greaves) retains copyright over all material contained within this repository. [1] Use of this code is governed under the terms of the Apache 2.0 open source software license. See the [LICENSE](./LICENSE) file for more details.



[1] Excludes content from the data/ directory as most of this was obtained from free and open sources on the internet (inclding the wonderful UCI ML Repository!). 
