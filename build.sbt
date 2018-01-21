lazy val game = project
  .settings(
    scalaVersion := "2.12.2",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % "2.4.12",
      "org.scalatest" %% "scalatest" % "3.0.0" % "test"
    )
  )
