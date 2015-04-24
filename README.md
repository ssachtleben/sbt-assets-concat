# sbt-assets-concat

An SBT Plugin to optimize project assets.

Add the repository resolver to the build.sbt:

    resolvers += Resolver.url("ssachtleben sbt repository (snapshots)", url("http://ssachtleben.github.io/sbt-plugins/repository/snapshots/"))(Resolver.ivyStylePatterns)

To use this plugin use the addSbtPlugin command within your project's `plugins.sbt` file:

    addSbtPlugin("com.ssachtleben.sbt" % "sbt-assets-concat" % "1.0.0-SNAPSHOT")
    
&copy; Sebastian Sachtleben, 2015

Check: https://github.com/sbt/sbt-web/blob/master/src/main/scala/com/typesafe/sbt/web/SbtWeb.scala