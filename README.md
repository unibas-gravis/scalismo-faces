scalismo-faces [![Build Status](https://travis-ci.org/unibas-gravis/scalismo-faces.svg?branch=master)](https://travis-ci.org/unibas-gravis/scalismo-faces)
==============

This is a framework for shape modeling and model-based image analysis in scala.
It is based on the [scalismo](https://github.com/unibas-gravis/scalismo)
library for shape modelling. It originates from the [Graphics
and Vision](http://gravis.cs.unibas.ch) research group at the [University of
Basel](http://www.unibas.ch), Switzerland.

The library contains tools for image processing, rendering and handling of the Morphable Model. A tutorial on how to use this software and about the concepts of Analysis-by-Synthesis face image analysis will soon be available from the University of Basel. Until then, enjoy the shape modelling online course about scalismo at http://shapemodelling.cs.unibas.ch/.

Usage
-----

### sbt

**Warning:** artefact currently not published yet, will be coming soon. Until then compile for yourself with `sbt publish-local`.

Add the dependency to your `build.sbt`:

```scala
libraryDependencies += "ch.unibas.cs.gravis" %% "scalismo-faces" % "0.1.+"
resolvers += "scalismo (public)" at "http://shapemodelling.cs.unibas.ch/repository/public"
```

### Getting Started

- Tutorial from the University of Basel: *coming soon*
- Create a checkerboard image and save it to disk:

```scala
import scalismo.color.RGB
import scalismo.image.{PixelImage, PixelImageIO}
import java.io.File

val checkerboard = PixelImage(128, 128, {(x, y) => if ((x+y)%2 == 0) RGB.White else RGB.Black})
PixelImageIO.write(checkerboard, new File("checkerboard.png")).get
```
(you can open an appropriate Scala REPL via `sbt console`)


Contributors
------------

- Sandro Schönborn
- Andreas Schneider
- Andreas Forster
- Bernhard Egger

Maintainers
-----------

- University of Basel, Graphics and Vision research: [@unibas-gravis](https://github.com/unibas-gravis), [homepage](http://gravis.cs.unibas.ch)
- Sandro Schönborn, [@sschoenborn](https://github.com/sschoenborn)

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
