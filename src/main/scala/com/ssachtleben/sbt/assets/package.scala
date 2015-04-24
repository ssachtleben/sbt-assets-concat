package com.ssachtleben.sbt

import sbt.PathFinder

package object assets {
  type ConcatGroup = (String, Either[Seq[String], PathFinder])
}