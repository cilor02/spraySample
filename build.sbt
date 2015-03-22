name := "spraySample"

version := "1.0"

scalaVersion := "2.10.2"

resolvers += "spray repo" at "http://repo.spray.io"

resolvers += "spray nightlies" at "http://nightlies.spray.io"

libraryDependencies ++= Seq(
  "com.typesafe.akka"  %% "akka-actor"       % "2.2.0",
  "com.typesafe.akka"  %% "akka-slf4j"       % "2.2.0",
  "ch.qos.logback"      % "logback-classic"  % "1.0.13",
  "io.spray"            % "spray-can"        % "1.2-20130712",
  "io.spray"            % "spray-routing"    % "1.2-20130712",
  "io.spray"           %% "spray-json"       % "1.3.1",
  "org.json4s" %% "json4s-native" % "3.2.10",
  "log4j" % "log4j" % "1.2.17",
  "xerces" % "xerces" % "2.4.0"
)

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-Xlint",
  "-Ywarn-dead-code",
  "-language:_",
  "-target:jvm-1.7",
  "-encoding", "UTF-8"
)
    