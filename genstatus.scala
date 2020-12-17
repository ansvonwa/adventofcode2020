#!/usr/bin/env scala

import scala.sys.process._
import scala.language.postfixOps

"git checkout build-status" !!

"git merge --ff origin/master" !!

reflect.io.File("status.svg")
  .writeAll(
    try {
      val lines = "./solutions.scala".lineStream.toVector
      lines
        .zipWithIndex
        .map{case (line, idx) =>
          val y = 20+idx*30
          "<text x=\"0\" y=\""+y+"\">" +
            line.replace("\u001B[32m", "</text><text x=\"90\" y=\""+y+"\" fill=\"#00CF00\">")
                .replace("\u001B[31m", "</text><text x=\"90\" y=\""+y+"\" fill=\"#CF0000\">")
                .replace("\u001B[0m", "</text><text>") +
            "</text>"
        }
        .mkString("<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"200\" height=\""+(lines.size*30)+"\">\n", "\n", "</svg>")
    } catch {
      case e: Exception =>
        e.printStackTrace()
        "<svg xmlns=\"http://www.w3.org/2000/svg\"><text x=\"0\" y=\"20\" fill=\"#AA0000\">failed: " + e + "</text>"
    }
  )

Thread.sleep(200)

"git add status.svg" !!

Seq("git", "commit", "-m", "update status.svg") !!

