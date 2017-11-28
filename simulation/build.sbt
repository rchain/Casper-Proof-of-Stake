lazy val root = (project in file(".")).
  settings(
    name := "Casper for RChain",
    version := "0.1",
    scalaVersion := "2.12.4"
  )

resolvers ++= Seq(
  "NetBeans Repository" at "http://bits.netbeans.org/maven2/",
  "Gephi" at "https://raw.github.com/gephi/gephi/mvn-thirdparty-repo/"
)

libraryDependencies += "org.gephi" % "gephi-toolkit" % "0.9.2"