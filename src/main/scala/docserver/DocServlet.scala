//
// $Id$

package docserver

import collection.JavaConversions._

import java.io.BufferedReader
import java.io.InputStream
import java.io.InputStreamReader
import java.io.PrintWriter
import java.util.logging.{Logger, Level}

import javax.servlet.http._

import com.samskivert.io.StreamUtil
import com.samskivert.mustache.{Mustache, Template, DefaultCollector}

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
  override def init {
    _refresher.start
  }

  override def destroy {
    _running = false
  }

  override def doGet (req :HttpServletRequest, rsp :HttpServletResponse) {
    val path = req.getPathInfo
    try {
      if (path == null || path == "/") {
        val out = rsp.getWriter
        val query = req.getParameter("search")
        if (query == null) {
          if (req.getParameter("refresh") != null) {
            _repo.refresh()
          }
          sendTemplate(rsp, _index, new AnyRef {
            val repo = DocRepo.mavenRoot
            val artifacts = _repo.artifactCount
          })
        }
        else if (query.isEmpty) reportError(rsp, "Query must be non-empty")
        else handleSearch(rsp, req.getParameter("action"), query)
      }
      else if (path.startsWith("/docs/")) handleDocs(rsp, path.substring(6))
      else if (path.startsWith("/source/")) handleSource(rsp, path.substring(8))
      else rsp.sendError(HttpServletResponse.SC_NOT_FOUND)
    } catch {
      case iae :IllegalArgumentException => reportError(rsp, iae.getMessage)
    }
  }

  case class Result (lquery :String, entry :DocRepo.Entry, artifact :DocRepo.Artifact) {
    def qname = highlight(entry.toString, lquery, 0)

    private def highlight (text :String, lquery :String, idx :Int) :String = {
      val start = text.toLowerCase.indexOf(lquery, idx)
      if (start == -1) text else {
        val end = start+lquery.size
        highlight(text.substring(0, start) + "<b>" + text.substring(start, end) + "</b>" +
                  text.substring(end), lquery, end+"<b></b>".length)
      }
    }
  }

  private def handleSearch (rsp :HttpServletResponse, action :String, query :String) {
    val lquery = query.toLowerCase
    // perform the search and sort the results in order of most information
    val results = _repo.find(lquery).sortBy {
      case (e, a) => if (e.simpleKey == lquery && !a.docJar.isEmpty) 0
                     else if (e.simpleKey == lquery) 1
                     else if (!a.docJar.isEmpty) 2
                     else if (!a.sourceJar.isEmpty) 3
                     else 4
    }

    // if they want the Best result, send them the top result iff it has docs
    def hasDocs (r :(DocRepo.Entry,DocRepo.Artifact)) = !r._2.docJar.isEmpty
    if ("Best" == action && results.headOption.map(hasDocs).getOrElse(false)) {
      val (beste, besta) = results.head
      rsp.sendRedirect("docs/" + besta.pom.fqId + "/" + beste.docPath)
    }
    // otherwise send them a list of results, possibly filtering out non-doc-havers
    else {
      val filtered = if ("Everything" == action) results else results.filter(hasDocs)
      sendTemplate(rsp, _results, Map(
        "query" -> query,
        "results" -> filtered.map(t => Result(lquery, t._1, t._2))
      ))
    }
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
    sendTemplate(rsp, _error, new AnyRef {
      val message = message_
    })
  }

  private def sendTemplate (rsp :HttpServletResponse, tmpl :Template, ctx :AnyRef) {
    rsp.setContentType("text/html")
    tmpl.execute(ctx, rsp.getWriter)
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

  // the period on which we rescan the repository for artifacts
  private val RefreshPeriod = 5 * 60 * 1000L
  // whether or not the servlet has been shut down
  private var _running = true
  // the thread that refreshes our artifacts
  private val _refresher = new Thread {
    override def run {
      var nextRefresh = System.currentTimeMillis + RefreshPeriod
      while (_running) {
        Thread.sleep(1000)
        val now = System.currentTimeMillis
        if (now >= nextRefresh) {
          try {
            _repo.refresh()
          } catch {
            case e => _log.log(Level.WARNING, "Failure refreshing repo", e)
          }
          nextRefresh = now + RefreshPeriod
        }
      }
    }
  }

  private val _repo = DocRepo.getRepo
  private val _log = Logger.getLogger("docserver")

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
        if (fetcher != null) fetcher
        else if (classOf[Map[_,_]].isAssignableFrom(cclass)) MapFetcher
        else null
      }
    })
    (compiler.compile(getTemplate("index.tmpl")),
     compiler.compile(getTemplate("results.tmpl")),
     compiler.compile(getTemplate("error.tmpl")))
  }

  private val MapFetcher = new Mustache.VariableFetcher {
    override def get (ctx :AnyRef, name :String) :AnyRef = {
      ctx.asInstanceOf[Map[String,Object]].get(name).getOrElse(null)
    }
  }
}
