package com.ssachtleben.sbt.assets

import java.nio.charset.Charset

import akka.dispatch.ThreadPoolConfig
import com.typesafe.sbt.web.pipeline.Pipeline
import com.typesafe.sbt.web.{PathMapping, SbtWeb}
import sbt.Keys._
import sbt._

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}


object Import {

  val concat = TaskKey[Pipeline.Stage]("assets-concat", "concat specified assets")

  object Concat {
    val groups = SettingKey[Seq[ConcatGroup]]("web-concat-groups", "List of ConcatGroup items")
    val srcDirs = SettingKey[Seq[java.io.File]]("web-src-dirs", "List of source directories")
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

  val utf8 = Charset.forName("UTF-8")

  import SbtWeb.autoImport._
  import WebKeys._
  import autoImport._
  import Concat._

  override def projectSettings: Seq[Setting[_]] = Seq(
    groups := ListBuffer.empty[ConcatGroup],
    srcDirs := Seq.empty[java.io.File],
    resourceManaged in concat in Assets := webTarget.value / concat.key.label / "main",
    resourceManaged in concat in TestAssets := webTarget.value / concat.key.label / "test",
    includeFilter in concat := "*.js",
    excludeFilter in concat := HiddenFileFilter,
    concat := transformFiles.value)

  private def toFileNames(logger: Logger, values: Seq[ConcatGroup], mappings: Seq[PathMapping],
                          srcDirs: Seq[File]): Seq[(String, Iterable[String])] = values.map {
    case (groupName, fileNames) =>
      val groupNameEscaped = groupName.replace('/', java.io.File.separator.charAt(0))
      //logger.info("Concat: " + groupNameEscaped + " -> " + fileNames.toString())
      fileNames match {
        case Left(fileNamesSeq) => (groupNameEscaped, fileNamesSeq)
        case Right(fileNamesPathFinder) =>
          val r = fileNamesPathFinder.pair(relativeTo(srcDirs) | flat)
          (groupNameEscaped, ListMap(r: _*).values) // use ListMap to preserve insert order (not efficient, maybe there is a better solution)
        case u => sys.error(s"Expected Seq[String] or PathFinder, but got $u")
      }
  }

  def transformFiles: Def.Initialize[Task[Pipeline.Stage]] = Def.task { (mappings: Seq[PathMapping]) =>
    //streams.value.log.info("Mapping: " + mappings.toString)
    var reducedMappings = Seq.empty[PathMapping]
    var newMappings = Seq.empty[PathMapping]
    val outputfolder = (resourceManaged in concat in Assets).value
    //val targetDir = (public in Assets).value / "javascripts"
    val groupsValue = toFileNames(streams.value.log, groups.value, mappings, srcDirs.value)
    //(sourceDirectories in Assets).value,
    //(webModuleDirectories in Assets).value,
    //Seq[File]((public in Assets).value))
    val concatGroups = mutable.Map.empty[String, StringBuilder]
    streams.value.log.info(s"Building ${groupsValue.size} concat group(s)")
    //TODO only write groups when sources have changed

    val blockNewMappings = Boolean.box(true)
    if (groupsValue.size > 0) {
      val blockReducedMappings = Boolean.box(true)
      val futures = groupsValue.map((tuple: (String, Iterable[String])) =>
        Future {
          val groupName = tuple._1
          val fileNames = tuple._2

          if (fileNames.size > 0) {
            val groupFile = (outputfolder / groupName)
            var changed = false
            val lastModified = {
              if (groupFile.exists()) {
                groupFile.lastModified()
              } else {
                0L
              }
            }
            //Read of modifiedDate from source files to check for a change
            fileNames.foreach { fileName =>
              //TODO call mappings filter only one time - for performance ...
              val entries = mappings.filter(f => {
                f._2.equals(fileName)
              })
              if (!entries.isEmpty) {
                if (entries.head._1.lastModified() > lastModified)
                  changed = true
                blockReducedMappings.synchronized {
                  reducedMappings = reducedMappings ++ entries
                }
              }
            }

            if (changed) {
              // Write fileContent to concatGroups Map
              fileNames.foreach { fileName =>
                //TODO call mappings filter only one time
                val entries = mappings.filter(f => {
                  f._2.equals(fileName)
                })
                if (!entries.isEmpty) {
                  //streams.value.log.info("Concat " + groupName + " <- " + entries.head._1)
                  concatGroups.getOrElseUpdate(groupName, new StringBuilder)
                    .append(IO.read(entries.head._1, utf8) + System.lineSeparator)
                }
              }
            } else {
              val newEntry = Seq.apply(((outputfolder / groupName), groupName))
              blockNewMappings.synchronized {
                newMappings = newMappings ++ newEntry
              }
            }
          }
        })
      Await.ready( Future.sequence(futures), Duration.Inf)
    }

   val futures = concatGroups.map {
      case (groupName, concatenatedContents) =>
        Future {
          val outputFile = outputfolder / groupName
          outputFile.relativeTo(webTarget.value)
          val newEntry = Seq.apply((outputFile, groupName))
          //streams.value.log.info("Entry " + newEntry.toString())
          blockNewMappings.synchronized {
            newMappings = newMappings ++ newEntry
          }
          IO.write(outputFile, concatenatedContents.toString(), utf8)
      }
    }

    Await.ready( Future.sequence(futures), Duration.Inf)

    (mappings.toSet -- reducedMappings.toSet ++ newMappings.toSet).toSeq
  }
}