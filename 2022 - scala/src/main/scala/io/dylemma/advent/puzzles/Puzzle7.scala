package io.dylemma.advent.puzzles

import io.dylemma.advent.util.Inputs

import scala.collection.mutable
import scala.util.chaining._

object Puzzle7 {

	// really just for debugging
	case class Path(segmentsRev: List[String]) {
		lazy val segments = segmentsRev.reverse
		def /(child: String) = Path(child :: segmentsRev)
		def parent = Path(segmentsRev.tail)
		def canonical = segments match {
			case Nil => "/"
			case multi => ("" :: multi).mkString("/")
		}
	}
	val RootPath = Path(Nil)

	case class Directory(files: Map[String, Int], dirs: Map[String, Directory]) {
		def dirsIterator(base: Path): Iterator[(Path, Directory)] = {
			Iterator.single(base -> this) ++
				dirs.iterator.flatMap { case (name, fs) => fs.dirsIterator(base / name) }
		}
		lazy val size: Long = files.valuesIterator.map(_.toLong).sum + dirs.valuesIterator.map(_.size).sum
	}
	object Directory {
		def newBuilder = new DirectoryBuilder(None, mutable.Map.empty, mutable.Map.empty)
	}

	// There's probably some clever way to do this immutably with a Zipper structure, but I gave up and went with a mutable builder
	class DirectoryBuilder(parent: Option[DirectoryBuilder], files: mutable.Map[String, Int], dirs: mutable.Map[String, DirectoryBuilder]) {
		def insertFile(name: String, size: Int) = {
			files.update(name, size)
			this
		}
		def insertDir(name: String) = {
			dirs.getOrElseUpdate(name, new DirectoryBuilder(Some(this), mutable.Map.empty, mutable.Map.empty))
			this
		}
		def cursorInto(dirName: String) = dirs.getOrElseUpdate(dirName, new DirectoryBuilder(Some(this), mutable.Map.empty, mutable.Map.empty))
		def cursorUp() = parent.getOrElse { ??? }
		def cursorToRoot(): DirectoryBuilder = parent match {
			case None => this
			case Some(p) => p.cursorToRoot()
		}
		def result(): Directory = Directory(files.toMap, dirs.view.mapValues(_.result()).toMap)
	}

	def main(args: Array[String]): Unit = {

		val CD = "\\$ cd (.*)".r
		val LS = "$ ls"
		val Dir = "dir (.*)".r
		val File = raw"(\d+) (.*)".r

		val filesystem = Inputs
			.lines[LazyList]("/input_2022_7.txt")
			.map[DirectoryBuilder => DirectoryBuilder] {
				case CD("..") => _.cursorUp()
				case CD("/") => _.cursorToRoot()
				case CD(child) => _.cursorInto(child)
				case LS => identity
				case File(rawSize, name) => _.insertFile(name, rawSize.toInt)
				case Dir(name) => _.insertDir(name)
			}
			.foldLeft(Directory.newBuilder)(_ pipe _)
			.cursorToRoot()
			.result()

		val smallDirTotal = (for {
			(path, dir) <- filesystem.dirsIterator(RootPath)
			if dir.size <= 100000
		} yield {
			println(s"Small Dir ${ path.canonical } -> ${ dir.size }")
			dir.size
		}).sum

		println(s"Small Dirs Total: $smallDirTotal")

		val totalFsSpace = 70000000
		val desiredFreeSpace = 30000000
		val actualUsed = filesystem.size
		val minDeleteThreshold = actualUsed - (totalFsSpace - desiredFreeSpace)
		println(s"Need to free $minDeleteThreshold from $actualUsed to reach ${ totalFsSpace - desiredFreeSpace } free bytes")

		val deleteCandidates = for {
			(path, dir) <- filesystem.dirsIterator(RootPath)
			if dir.size >= minDeleteThreshold
		} yield {
			println(s"Candidate ${ path.canonical } = ${ dir.size }")
			path.canonical -> dir.size
		}
		val (bestCandidate, winnerSize) = deleteCandidates.minBy(_._2)
		println(s"Best candidate is $bestCandidate at $winnerSize bytes")
	}
}
