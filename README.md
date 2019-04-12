# fpgabits
Collection of utilities to simplify FPGA accelerator design

## Running

### Requirements

We assume that the user utilize ensime, because of it, we require it in the build.sbt
See Line:
`ensimeScalaVersion in ThisBuild := "2.12.7"`

Using sbt, the user should follow the steps in the
[ensime sbt guide](http://ensime.github.io/build_tools/sbt/) to initialize the ensime directory.


### Tests

No main is provided for now, but tests are provided for the existing modules.

Running:
~~~
sbt testOnly
~~~

Will run all tests.




