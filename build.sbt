lazy val commonSettings = Seq(
  organization := "dev.habla",
  scalaVersion := "2.12.8",
  addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
  scalacOptions ++= Seq(
      "-Xlint",
      "-unchecked",
      "-deprecation",
      "-feature",
      "-Ypartial-unification",
      "-language:postfixOps",
      "-language:implicitConversions",
      "-language:higherKinds",
      "-language:experimental.macros"),
  libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.2.18",
      "com.github.julien-truffaut" %% "monocle-core" % "1.5.0",
      "io.getquill" %% "quill-sql" % "3.1.0",
      "org.scalactic" %% "scalactic" % "3.0.5", 
      "org.scalatest" %% "scalatest" % "3.0.5" % "test"
    ),
  parallelExecution in Test := false
)
lazy val core = project
  .settings(
    name := "scp19-core",
    commonSettings
  )

lazy val example = project
  .dependsOn(core)
  .settings(
    name := "scp19-example",
    commonSettings,
    libraryDependencies ++= Seq(
      "org.xerial" % "sqlite-jdbc" % "3.27.2.1" % "test",
      "org.tpolecat" %% "doobie-core"      % "0.6.0" % "test",
      "org.tpolecat" %% "doobie-hikari"    % "0.6.0" % "test", // HikariCP transactor.
      "org.tpolecat" %% "doobie-specs2"    % "0.6.0" % "test", // Specs2 support for typechecking statements.
      "org.tpolecat" %% "doobie-scalatest" % "0.6.0" % "test",  // ScalaTest support for typechecking statements.
      "org.basex" % "basex" % "9.2.2" % "test"
    )
  )

