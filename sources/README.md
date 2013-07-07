# Simple implementation of parsing combinators #

You'll need [sbt](http://www.scala-sbt.org/). I suppose you are at sources/ directory:
```
$ sbt console
... lots of output ...
scala> Calculator("2 + 2")
=> 4

scala> Calculator("(245 + 283) / 2 + (200 - 45) / 5")
=> 295

scala> Calculator("(4 + 3))")
Error: End of string expected but `)' found

scala> :quit
```

To generate API docs enter `$ sbt docs`. The generated docs can be
found at `target/scala-<version>/api/`.
