addSbtPlugin("org.scala-js"        % "sbt-scalajs"         % "1.3.1")
addSbtPlugin("org.scalameta"       % "sbt-scalafmt"        % "2.2.0")
addSbtPlugin("com.typesafe.sbt"    % "sbt-native-packager" % "1.7.5")
addCompilerPlugin("org.typelevel" %% "kind-projector"      % "0.11.0" cross CrossVersion.full)
addCompilerPlugin("com.olegpy"    %% "better-monadic-for"  % "0.3.1")
