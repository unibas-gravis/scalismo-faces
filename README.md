scalismo-faces
==============

[![Build Status](https://travis-ci.org/unibas-gravis/scalismo-faces.svg?branch=master)](https://travis-ci.org/unibas-gravis/scalismo-faces)

[![Download](https://api.bintray.com/packages/unibas-gravis/maven/scalismo-faces/images/download.svg) ](https://bintray.com/unibas-gravis/maven/scalismo-faces/_latestVersion)
 
This is a framework for shape modeling and model-based image analysis in scala.
It is based on the [scalismo](https://github.com/unibas-gravis/scalismo)
library for shape modelling. It originates from the [Graphics
and Vision](http://gravis.cs.unibas.ch) research group at the [University of
Basel](http://www.unibas.ch), Switzerland.

The library contains tools for image processing, rendering and handling of the Morphable Model. A tutorial on how to use this software and about the concepts of Analysis-by-Synthesis face image analysis is available from the University of Basel under http://gravis.dmi.unibas.ch/PMM/.

Usage
-----

### sbt

Add the dependency to your `build.sbt`:

```scala
libraryDependencies += "ch.unibas.cs.gravis" %% "scalismo-faces" % "0.9.0"
resolvers += Resolver.bintrayRepo("unibas-gravis", "maven")
```

### Getting Started

- The following code creates a checkerboard image and save it to disk:

```scala
import scalismo.color.RGB
import scalismo.faces.image.PixelImage
import scalismo.faces.io.PixelImageIO
import java.io.File

object ChessExample extends App {
  val checkerboard = PixelImage(128, 128, {(x, y) => if ((x+y)%2 == 0) RGB.White else RGB.Black})
  PixelImageIO.write(checkerboard, new File("checkerboard.png")).get
}
```
To execute it place the code in a file `src/main/scala/ChessExample.scala` and run `sbt run` from your project directory where you have the `build.sbt` file with at least the lines described under the former point **sbt**.

Documentation
-------------

- [Probabilistic Fitting Tutorial](http://gravis.dmi.unibas.ch/PMM/) from the University of Basel

There is also a [scalismo-faces google group](https://groups.google.com/forum/#!forum/scalismo-faces) for general questions and discussion.

Contributors
------------

- Sandro Schönborn
- Andreas Schneider
- Andreas Forster
- Bernhard Egger

Maintainers
-----------

- University of Basel, Graphics and Vision research: [@unibas-gravis](https://github.com/unibas-gravis), [homepage](http://gravis.cs.unibas.ch)

Contribution
------------

Contribution is welcome:

- You can report bugs and issues using the issue tracker on Github
- You can submit small pull requests to contribute:
    - Open an issue and discuss what you would like to add
    - Submit work as a small pull request, suitable for quick review
- If you would like to get involved in development, contact us.

License
-------

[Apache License, Version 2.0](https://www.apache.org/licenses/LICENSE-2.0), details see LICENSE

    Copyright 2016, University of Basel, Graphics and Vision Research

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
