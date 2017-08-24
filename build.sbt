name := "msm"
 
version := "1.0" 
      
lazy val `msm` = (project in file(".")).enablePlugins(PlayScala)

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"

scalaVersion := "2.11.11"

libraryDependencies ++= Seq(cache , ws , specs2 % Test )
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "3.0.0" % Test

libraryDependencies += "com.typesafe.play" %% "play-slick" % "2.0.2"
libraryDependencies += "com.typesafe.play" %% "play-slick-evolutions" % "2.0.2"
libraryDependencies += "org.postgresql" % "postgresql" % "9.3-1100-jdbc4"

libraryDependencies += "com.h2database" % "h2" % "1.4.194"

unmanagedResourceDirectories in Test <+=  baseDirectory ( _ /"target/web/public/test" )

      