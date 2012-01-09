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

  /** Finds all entries with a classname that contains or is equal to (depending on the value of
   * `exact`) the supplied substring. The substring should be in lowercase. */
  def find (frag :String, exact: Boolean) :Seq[(Entry,Artifact)] =
    artifacts.values.toSeq.flatMap(_.find(frag, exact))

  /** Returns the artifact with the supplied groupId and artifactId, if available. */
  def getArtifact (groupId :String, artifactId :String) :Option[Artifact] =
    artifacts.get(groupId + ":" + artifactId)

  /** Refreshes this repository, reloading indexes for any artifacts that have been updated, adding
   * any newly added artifacts and removing any that have gone away. */
  def refresh () {
    _log.info("Refreshing repository...")
    val oartifacts = artifacts

    // rescan the repository, then for each POM, look up the preexisting artifact (if any) and if
    // the POM is not newer than the artifact, reuse the existing artifact (which will probably
    // have already expanded its index)
    def newopt (p :POM)(a :Artifact) =
      if (a.pom.lastModified >= p.lastModified) Some(a) else None
    artifacts = Map() ++ DocRepo.getPOMs.mapValues(
      p => artifacts.get(p.fqId).flatMap(newopt(p)).getOrElse(p.toArtifact))

    // report on what changed
    val removed = oartifacts.keySet -- artifacts.keySet
    if (!removed.isEmpty) _log.info("Removed artifacts " + removed)
    val added = artifacts.keySet -- oartifacts.keySet
    if (!added.isEmpty) _log.info("Added artifacts " + added)
    val updated = (artifacts.keySet union oartifacts.keySet).filter(
      key => artifacts(key).pom.lastModified > oartifacts(key).pom.lastModified)
    if (!updated.isEmpty) _log.info("Updated artifacts " + updated)
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
      val classes = getContents(jar).filter(_.endsWith(".class"))
      // filter out anonymous inner classes and turn the remainder into entries
      classes.filterNot(isNonDocClass).map(e => new Entry(e))
    }

    def find (frag :String, exact :Boolean) :Seq[(Entry, Artifact)] = {
      val matcher = if (exact) (s :String) => s == frag
                    else (s :String) => s.contains(frag)
      index.filter(e => matcher(e.simpleKey)).map(e => (e, this))
    }

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
            _log.warning("Error reading entries in " + jar + ": " + e)
            jin.close
            None
          }
        }
      }
    }
  }

  case class POM (groupId :String, artifactId :String, version :String, pom :File) {
    // a snapshot of our last modified time
    val lastModified = pom.lastModified

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
    POM(groupDir.getPath.substring(mavenRoot.getPath.length+1).replaceAll(FS, "."),
        artifactDir.getName, versDir.getName, pom)
  }

  private def readRepoDir (in :InputStream) = {
    try {
      val props = new Properties
      props.load(in)
      Option(props.getProperty("maven_repo"))
    } catch {
      case e => _log.warning(e.getMessage) ; None
    }
  }

  private def getContents (jar :File) :Seq[String] = {
    val contents = ArrayBuffer[String]()
    val jin = new JarInputStream(new FileInputStream(jar))
    try {
      var jentry = jin.getNextJarEntry
      while (jentry != null) {
        if (!jentry.isDirectory) contents += jentry.getName
        jentry = jin.getNextJarEntry
      }
    } catch {
      case e => _log.warning("Error reading entries in " + jar + ": " + e)
    } finally {
      jin.close
    }
    contents
  }

  private val javaInnerRe = java.util.regex.Pattern.compile("\\$[0-9]+")
  private val scalaInnerRe = java.util.regex.Pattern.compile("\\$anonfun")
  private def isNonDocClass (name :String) = {
    javaInnerRe.matcher(name).find || scalaInnerRe.matcher(name).find || name.endsWith("$.class")
  }

  private def defaultRoot = System.getProperty("user.home") + FS + ".m2" + FS + "repository"

  private val _log = java.util.logging.Logger.getLogger("docserver")
  private val FS = File.separator
}
