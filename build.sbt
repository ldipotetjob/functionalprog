// This an example of a simple project definition.
// It should build on both sbt 0.13.15 and sbt 1.0.0
lazy val root = (project in file("."))
  .settings(
    organization in ThisBuild := "com.example",
    scalaVersion in ThisBuild := "2.12.2",
    version      in ThisBuild := "0.1.0-SNAPSHOT",
    name := "functional-programing",
    libraryDependencies ++= Seq(

      // Start with this one
      "org.tpolecat" %% "doobie-core"      % "0.12.1",

      // And add any of these as needed
      "org.tpolecat" %% "doobie-h2"        % "0.12.1",          // H2 driver 1.4.200 + type mappings.
      "org.tpolecat" %% "doobie-hikari"    % "0.12.1",          // HikariCP transactor.
      "org.tpolecat" %% "doobie-postgres"  % "0.12.1",          // Postgres driver 42.2.19 + type mappings.
      "org.tpolecat" %% "doobie-specs2"    % "0.12.1" % "test", // Specs2 support for typechecking statements.
      "org.tpolecat" %% "doobie-scalatest" % "0.12.1" % "test"  // ScalaTest support for typechecking statements.

    )
  )