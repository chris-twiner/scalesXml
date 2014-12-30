//aggregate in packageBin := false

//aggregate in (Compile, packageBin) := false

libraryDependencies ++= Seq(
  // from our repo
  "net.sf.saxon" % "Saxon-HE" % "9.5.1-6" intransitive()
)