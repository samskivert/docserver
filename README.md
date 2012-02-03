docserver serves up Javadoc documentation from Maven `-javadoc` jars that have
been downloaded to your local Maven repository.

Building
--------

The project is built and run using [SBT].

Start the sbt console and run `container-start` to start up the docserver. It
will listen on port 9999 by default. Edit `build.sbt` to change the port.

Distribution
------------

docserver is released under the New BSD License. The most recent version of the
code is available at http://github.com/samskivert/docserver

Contact
-------

Questions, comments, and other communications should be directed to the [Three
Rings Libraries](http://groups.google.com/group/ooo-libs) Google Group.

[SBT]: http://github.com/harrah/xsbt/wiki/Setup
