//
// $Id$

package docserver

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

import java.io.{File, FileInputStream, InputStream}
import java.util.Properties
import java.util.jar.{JarEntry, JarInputStream}

/**
 * Contains an index of all classes in all jar files in a Maven repository, along with information
 * on their source and javadoc counterparts.
 */
class DocRepo (private var artifacts :Map[String,DocRepo.Artifact])
{
  import DocRepo._

  /** Returns the number of (unique) artifacts found in the repository. */
  def artifactCount = artifacts.size

  /** Finds all entries with a classname that contains the supplied substring.
   * The substring should be in lowercase. */
  def find (frag :String) :Seq[(Entry,Artifact)] = artifacts.values.toSeq.flatMap(_.find(frag))

  /** Returns the artifact with the supplied groupId and artifactId, if available. */
  def getArtifact (groupId :String, artifactId :String) :Option[Artifact] =
    artifacts.get(groupId + ":" + artifactId)

  /** Refreshes this repository, reloading indexes for any artifacts that have been updated, adding
   * any newly added artifacts and removing any that have gone away. */
  def refresh () {
    // rescan the repository, then for each POM, look up the preexisting artifact (if any) and if
    // the POM is not newer than the artifact, reuse the existing artifact (which will probably
    // have already expanded its index)
    def newopt (p :POM)(a :Artifact) =
      if (a.pom.pom.lastModified >= p.pom.lastModified) Some(a) else None
    artifacts = Map() ++ DocRepo.getPOMs.mapValues(
      p => artifacts.get(p.fqId).flatMap(newopt(p)).getOrElse(p.toArtifact))
  }
}

object DocRepo
{
  class Entry (path :String) {
    /** The lower-cased class name (including outer classes) of this entry. */
    lazy val key = _clean.substring(_clean.lastIndexOf("/")+1).toLowerCase

    /** The lower-cased class name (not including outer classes) of this entry. */
    def simpleKey = key.substring(key.lastIndexOf(".")+1)

    /** The path in the source jar that (most likely) corresponds to this entry. */
    def sourcePath = (_clean.indexOf(".") match {
      case -1 => _clean
      case n => _clean.substring(0, n)
    }) + ".java"

    /** The path in the docs jar that (most likely) corresponds to this entry. */
    def docPath = _clean + ".html"

    override def toString = _clean.replace('/', '.')
    override def hashCode = _clean.hashCode
    override def equals (other :Any) = _clean.equals(other.asInstanceOf[Entry]._clean)

    private val _clean = path.dropRight(".class".length).replace('$', '.')
  }

  case class Artifact (pom :POM, jar :File, sourceJar :Option[File], docJar :Option[File]) {
    lazy val index :Seq[Entry] = {
      val contents = ArrayBuffer[Entry]()
      val jin = new JarInputStream(new FileInputStream(jar))
      try {
        var jentry = jin.getNextJarEntry
        while (jentry != null) {
          val name = jentry.getName
          if (name.endsWith(".class") && !innerRe.matcher(name).find) contents += new Entry(name)
          jentry = jin.getNextJarEntry
        }
      } catch {
        case e => log.warning("Error reading entries in " + jar + ": " + e)
      } finally {
        jin.close
      }
      contents.toSeq
    }

    def find (frag :String) :Seq[(Entry, Artifact)] =
      index.filter(_.key.contains(frag)).map(e => (e, this))

    def sourceStream (path :String) = jarStream(sourceJar, path)
    def docStream (path :String) = jarStream(docJar, path)

    override def toString = jar.getPath

    private def jarStream (jarOpt :Option[File], name :String) = {
      def findEntry (jin :JarInputStream) :Option[JarInputStream] = {
        var jentry = jin.getNextJarEntry
        if (jentry == null) None
        else if (jentry.getName == name) Some(jin)
        else findEntry(jin)
      }

      jarOpt.flatMap { jar =>
        val jin = new JarInputStream(new FileInputStream(jar))
        try {
          findEntry(jin)
        } catch {
          case e => {
            log.warning("Error reading entries in " + jar + ": " + e)
            jin.close
            None
          }
        }
      }
    }
  }

  case class POM (groupId :String, artifactId :String, version :String, pom :File) {
    def fqId = groupId + ":" + artifactId
    def isJar = !retype(".jar").isEmpty
    def toArtifact =
      Artifact(this, retype(".jar").get, retype("-sources.jar"), retype("-javadoc.jar"))

    override def toString = groupId + " : " + artifactId + " : " + version

    private def retype (suff :String) = {
      val file = new File(pom.getParentFile, pom.getName.replaceAll(".pom", suff))
      if (file.exists) Some(file) else None
    }
  }

  // obtain our Maven repository directory from docserver.properties:maven_repo and fall back to
  // ~/.m2/repository as needed
  lazy val mavenRoot = new File(
    Option(getClass.getClassLoader.getResourceAsStream("docserver.properties")).
      flatMap(readRepoDir).getOrElse(defaultRoot)).getCanonicalFile

  // def main (args :Array[String]) {
  //   if (args.isEmpty) {
  //     System.err.println("Usage: classnamefrag")
  //     System.exit(255)
  //   }
  //   getRepo.find(args(0)) foreach {
  //     case (ent, arts) => println(ent + " in\n  " + arts.mkString("\n  "))
  //   }
  // }

  def getRepo = new DocRepo(Map() ++ getPOMs.mapValues(_.toArtifact))

  private def getPOMs :Map[String,POM] =
    collectPOMs(mavenRoot).filter(_.isJar).groupBy(_.fqId).mapValues(_.maxBy(_.pom.lastModified))

  private def collectPOMs (dir :File) :Seq[POM] = {
    val (dirs, files) = dir.listFiles.partition(_.isDirectory)
    files.filter(_.getName.endsWith(".pom")).map(toPOM) ++ dirs.flatMap(collectPOMs)
  }

  private def toPOM (pom :File) = {
    val versDir = pom.getParentFile
    val artifactDir = versDir.getParentFile
    val groupDir = artifactDir.getParentFile
    POM(groupDir.getPath.substring(mavenRoot.getPath.length+1).replaceAll(fs, "."),
        artifactDir.getName, versDir.getName, pom)
  }

  private def readRepoDir (in :InputStream) = {
    try {
      val props = new Properties
      props.load(in)
      Option(props.getProperty("maven_repo"))
    } catch {
      case e => log.warning(e.getMessage) ; None
    }
  }

  private def defaultRoot = System.getProperty("user.home") + fs + ".m2" + fs + "repository"

  private val innerRe = java.util.regex.Pattern.compile("\\$[0-9]+")
  private val log = java.util.logging.Logger.getLogger("docserver")
  private val fs = File.separator
}
