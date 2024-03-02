package adapter

import org.apache.commons.compress.archivers.ArchiveEntry
import org.apache.commons.compress.archivers.ArchiveInputStream
import org.apache.commons.compress.archivers.ArchiveStreamFactory
import org.apache.commons.compress.utils.IOUtils
import os.Path

import java.io.BufferedInputStream
import scala.util.Using

extension [T <: ArchiveEntry](ais: ArchiveInputStream[T])
  def foreach(f: T => Unit) =
    var e = ais.getNextEntry()
    while e != null do
      f(e)
      e = ais.getNextEntry()

def decompress[T <: ArchiveEntry](p: Path) =
  val out = Path(p.toString.stripSuffix(s".${p.ext}"))

  if !os.exists(out) then os.makeDir(out)

  Using.Manager: use =>
    val is = use(p.getInputStream)
    val bis = BufferedInputStream(is)
    val ais: ArchiveInputStream[T] =
      use(ArchiveStreamFactory().createArchiveInputStream(bis))

    ais.foreach: e =>
      val f = out / e.getName()

      if !os.exists(f) then
        if e.isDirectory() then os.makeDir(f)
        else
          val out = use(os.write.outputStream(f))
          IOUtils.copy(ais, out)

  println(s"decompressed ${p.baseName}")
