package com.ssachtleben.sbt.assets

import com.typesafe.sbt.web.SbtWeb
import com.typesafe.sbt.web.Import._
import com.typesafe.sbt.web.pipeline.Pipeline
import com.typesafe.sbt.web.PathMapping
import collection.mutable
import mutable.ListBuffer
import sbt._
import Keys._
import scala.collection.SortedMap
import scala.collection.immutable.TreeMap
import scala.collection.immutable.ListMap

object Import {

  val concat = TaskKey[Pipeline.Stage]("js transformer", "transforms js")

  object Concat {
    val groups = SettingKey[Seq[ConcatGroup]]("web-concat-groups", "List of ConcatGroup items")
  }

  def group(o: AnyRef): Either[Seq[String], PathFinder] = o match {
    case o: Seq[_] => Left(o.asInstanceOf[Seq[String]])
    case o: PathFinder => Right(o)
    case u =>
      sys.error(s"Can't create a concat group from $u. Must provide either Seq[String] or a PathFinder for the concat group values")
  }
}

object AssetsPlugin extends sbt.AutoPlugin {

  override def requires = SbtWeb

  override def trigger = AllRequirements

  val autoImport = Import

  import SbtWeb.autoImport._
  import WebKeys._
  import autoImport._
  import Concat._

  override def projectSettings: Seq[Setting[_]] = Seq(
    groups := ListBuffer.empty[ConcatGroup],
    includeFilter in concat := "*.js",
    excludeFilter in concat := HiddenFileFilter,
    concat := transformFiles.value)

  private def toFileNames(logger: Logger, values: Seq[ConcatGroup],
    srcDirs: Seq[File],
    webModuleDirs: Seq[File],
    publicDirs: Seq[File]): Seq[(String, Iterable[String])] = values.map {
    case (groupName, fileNames) =>
      logger.info(groupName + " - " + fileNames.toString())
      fileNames match {
        case Left(fileNamesSeq) => (groupName, fileNamesSeq)
        case Right(fileNamesPathFinder) =>
          val r = fileNamesPathFinder.pair(relativeTo(srcDirs ++ webModuleDirs ++ publicDirs) | flat)
          (groupName, ListMap(r: _*).values) // use ListMap to preserve insert order (not efficient, maybe there is a better solution)
        case u => sys.error(s"Expected Seq[String] or PathFinder, but got $u")
      }
  }

  def transformFiles: Def.Initialize[Task[Pipeline.Stage]] = Def.task { (mappings: Seq[PathMapping]) =>
    var reducedMappings = Seq.empty[PathMapping]
    val targetDir = (public in Assets).value / "javascripts"
    val groupsValue = toFileNames(streams.value.log, groups.value,
      (sourceDirectories in Assets).value,
      (webModuleDirectories in Assets).value,
      Seq[File]((public in Assets).value))
    val concatGroups = mutable.Map.empty[String, StringBuilder]
    streams.value.log.info(s"Building ${groupsValue.size} concat group(s)")
    if (groupsValue.size > 0) {
	    groupsValue.foreach {
	      case (groupName, fileNames) =>
	        if (fileNames.size > 0) {
		        fileNames.foreach { fileName =>
		          streams.value.log.info("Concat " + groupName + " <- " + fileName)		          
		          reducedMappings = reducedMappings ++ mappings.filter(f => { f._2.equals(fileName) })
		          val file = new java.io.File(targetDir, groupName)
		          concatGroups.getOrElseUpdate(groupName, new StringBuilder)
		            .append(IO.read(new java.io.File((public in Assets).value, fileName)) + System.lineSeparator)
		        }
	        }
	    }
    }
    
    concatGroups.map {
          case (groupName, concatenatedContents) =>
        val outputFile = targetDir / groupName
            IO.write(outputFile, concatenatedContents.toString())
            outputFile
        }.pair(relativeTo(webTarget.value))
    (mappings.toSet -- reducedMappings.toSet).toSeq
  }
}