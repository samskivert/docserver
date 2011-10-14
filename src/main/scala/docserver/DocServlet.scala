//
// $Id$

package docserver

import collection.JavaConversions._

import java.io.BufferedReader
import java.io.InputStream
import java.io.InputStreamReader
import java.io.PrintWriter

import javax.servlet.http._

import com.samskivert.io.StreamUtil
import com.samskivert.mustache.{Mustache, DefaultCollector}

/**
 * Handles the following queries:
 * <li>
 * <ul> ?search=classfrag
 * <ul> ?docs=groupId:artifactId:com.foo.bar.Foo.Bar
 * <ul> ?source=groupId:artifactId:com.foo.bar.Foo.Bar
 * </li>
 */
class DocServlet extends HttpServlet
{
  override def doGet (req :HttpServletRequest, rsp :HttpServletResponse) {
    val path = req.getPathInfo
    try {
      if (path == null || path == "/") {
        val out = rsp.getWriter
        val search = req.getParameter("search")
        if (search != null) handleSearch(out, search)
        else _index.execute(new AnyRef {
          val repo = DocRepo.mavenRoot
          val artifacts = _repo.artifactCount
        }, out)
      }
      else if (path.startsWith("/docs/")) handleDocs(rsp, path.substring(6))
      else if (path.startsWith("/source/")) handleSource(rsp, path.substring(8))
      else rsp.sendError(HttpServletResponse.SC_NOT_FOUND)
    } catch {
      case iae :IllegalArgumentException => reportError(rsp, iae.getMessage)
    }
  }

  private def handleSearch (out :PrintWriter, query_ :String) {
    _results.execute(new AnyRef {
      val query = query_
      val results = _repo.find(query_.toLowerCase)
    }, out)
  }

  private def handleDocs (rsp :HttpServletResponse, qualPath :String) {
    val (groupId, artifactId, path) = parsePath(qualPath)
    val fqId = groupId + ":" + artifactId
    _repo.getArtifact(groupId, artifactId) match {
      case None => reportError(rsp, "No such artifact: " + fqId)
      case Some(art) => art.docStream(path) match {
        case None => reportError(rsp, "File not found " + path +  " in " + fqId)
        case Some(stream) => copyStream(rsp, path, stream)
      }
    }
  }

  private def handleSource (rsp :HttpServletResponse, qualPath :String) {
    val (groupId, artifactId, path) = parsePath(qualPath)
    val fqId = groupId + ":" + artifactId
    _repo.getArtifact(groupId, artifactId) match {
      case None => reportError(rsp, "No such artifact: " + fqId)
      case Some(art) => art.sourceStream(path) match {
        case None => reportError(rsp, "File not found " + path +  " in " + fqId)
        case Some(stream) => copyStream(rsp, path, stream)
      }
    }
  }

  private def parsePath (path :String) = {
    try {
      val sidx = path.indexOf("/")
      val Array(groupId, artifactId) = path.substring(0, sidx).split(":")
      (groupId, artifactId, path.substring(sidx+1))
    } catch {
      case _ => throw new IllegalArgumentException("Invalid path " + path)
    }
  }

  private def reportError (rsp :HttpServletResponse, message_ :String) {
    _error.execute(new AnyRef {
      val message = message_
    }, rsp.getWriter)
  }

  private def copyStream (rsp :HttpServletResponse, path :String, stream :InputStream) {
    rsp.setContentType(getServletContext.getMimeType(path))
    try {
      StreamUtil.copy(stream, rsp.getOutputStream)
    } finally {
      stream.close
    }
  }

  private def getTemplate (path :String) =
    new InputStreamReader(getClass.getClassLoader.getResourceAsStream(path))

  private val _repo = DocRepo.getRepo

  private val (_index, _results, _error) = {
    val compiler = Mustache.compiler.withCollector(new DefaultCollector {
      override  def toIterator (value :AnyRef) :java.util.Iterator[_] = {
        val iter = super.toIterator(value)
        if (iter != null) iter else value match {
          case iable :Iterable[_] => iable.iterator
          case ier :Iterator[_] => ier
          case _ => null
        }
      }
      override def createFetcher (cclass :Class[_], name :String) :Mustache.VariableFetcher = {
        val fetcher = super.createFetcher(cclass, name)
        if (fetcher != null) fetcher else {
          null
        }
      }
    })
    (compiler.compile(getTemplate("index.tmpl")),
     compiler.compile(getTemplate("results.tmpl")),
     compiler.compile(getTemplate("error.tmpl")))
  }
}
