name := "AquaUpdater"

version := "0.1"

scalaVersion := "2.13.7"

resolvers += Resolver.JCenterRepository
resolvers += "jitpack" at "https://jitpack.io"
resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies += "net.katsstuff" %% "ackcord" % "0.17.1"

libraryDependencies += "com.github.stellar" %% "java-stellar-sdk" % "0.29.0"

libraryDependencies += "com.github.cb372" %% "scalacache-guava" % "0.28.0"

libraryDependencies += "com.typesafe.play" %% "play-ahc-ws-standalone" % "2.1.3"
libraryDependencies += "com.typesafe.play" %% "play-ws-standalone-json" % "2.1.3"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.9.2"
libraryDependencies += "com.typesafe.play" %% "play-json-joda" % "2.9.2"