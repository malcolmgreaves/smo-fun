# smo-fun
AN efficient, functional implementation of the Sequential Minimal Optimization (SMO) algorithm for training Support Vector Machines (SVMs).


**WARNING** This project is a _work in progress_ and is not yet stable.
It is undergoing active, radpid development: it is not stable. See the "WIP" section for progress, notes.


# Project Structure

This repository is split into subprojects:

* [smo-fun-core](https://github.com/malcolmgreaves/smo-fun/tree/master/smo-fun-core)
  * contains algorithm implementations
  * intended to be consumed as a library
  
* [smo-fun-cmd](https://github.com/malcolmgreaves/smo-fun/tree/master/smo-fun-cmd)
  * contains command line applications that use the code from `smo-fun-core`
  * intended to be used as a suite of tools to assist fellow machine learniner practitioners

# Legal

The original author retains copyright over all material contained within this repository. Use of this code is governed under the terms of the Apache 2.0 open source software license. See the [LICENSE](./LICENSE) file for more details.
