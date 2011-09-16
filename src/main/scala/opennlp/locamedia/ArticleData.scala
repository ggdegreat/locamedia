package opennlp.locamedia

import NlpUtil._
import util.control.Breaks._

/////////////////////////////////////////////////////////////////////////////
//                                  Main code                               //
/////////////////////////////////////////////////////////////////////////////

object ArticleData {
  val minimum_latitude = -90.0
  val maximum_latitude = 90.0
  val minimum_longitude = -180.0
  val maximum_longitude = 180.0 - 1e-10

  val combined_article_data_outfields = List("id", "title", "split", "redir",
      "namespace", "is_list_of", "is_disambig", "is_list", "coord",
      "incoming_links")

  // Read in the article data file.  Call PROCESS on each article.
  // The type of the article created is given by ARTICLE_TYPE, which defaults
  // to Article.  MAXTIME is a value in seconds, which limits the total
  // processing time (real time, not CPU time) used for reading in the
  // file, for testing purposes.
  def read_article_data_file(filename:String, process:Map[String,String]=>Unit,
                             maxtime:Double=0.0) = {
    errprint("Reading article data from %s...", filename)
    val status = StatusMessage("article")

    val fi = Source.fromFile(filename).getLines()
    val fields = fi.next().split('\t')
    for (line <- fi) breakable {
      val fieldvals = line.split('\t')
      if (fieldvals.length != field_types.length)
        warning(
        """Strange record at line #%s, expected %s fields, saw %s fields;
    skipping line=%s""", status.num_processed(), field_types.length,
                         fieldvals.length, line)
      else
        process((fields zip fieldvals).toMap)
      if (status.item_processed(maxtime=maxtime))
        break
    }
    errprint("Finished reading %s articles.", status.num_processed())
    output_resource_usage()
    fields
  }

  def write_article_data_file(outfile:File, outfields:Seq[String], articles:Iterable[Article]) {
    uniprint(outfields mkString "\t", outfile=outfile)
    for (art <- articles)
      uniprint(art.get_fields(outfields) mkString "\t", outfile=outfile)
    outfile.close()
  }
}

// A 2-dimensional coordinate.
//
// The following fields are defined:
//
//   lat, long: Latitude and longitude of coordinate.

case class Coord(lat:Double, long:Double) {
  // Not sure why this code was implemented with coerce_within_bounds,
  // but either always coerce, or check the bounds ...
  require(lat >= mininum_latitude)
  require(lat <= maximum_latitude)
  require(long >= minimum_longitude)
  require(long <= maximum_longitude)
  override def toString() = "(%.2f,%.2f)".format(lat, long)
}

object Coord {
  //// If coerce_within_bounds=true, then force the values to be within
  //// the allowed range, by wrapping longitude and bounding latitude.
  def apply(lat:Double, long:Double, coerce_within_bounds:Boolean) = {
    var newlat = lat
    var newlong = long
    if (coerce_within_bounds) {
      if (newlat > maximum_latitude) newlat = maximum_latitude
      while (newlong > maximum_longitude) newlong -= 360.
      if (newlat < minimum_latitude) newlat = minimum_latitude
      while (newlong < minimum_longitude) newlong += 360.
    }
    new Coord(newlat, newlong)
  }
}

object Article {
  // Compute the short form of an article name.  If short form includes a
  // division (e.g. "Tucson, Arizona"), return a tuple (SHORTFORM, DIVISION);
  // else return a tuple (SHORTFORM, None).
  
  def compute_short_form(name: String) {
    val includes_div_re = """(.*?), (.*)$""".r
    val includes_parentag_re = """(.*) \(.*\)$""".r
    name match {
      case includes_div_re(tucson, arizona) => (tucson, arizona)
      case includes_parentag_re(tucson, city) => (tucson, null)
      case _ => (name, null)
    }
  }

  def adjust_incoming_links(links: Double) = {
    if (links == 0) // Whether from unknown count or count is actually zero
      0.01 // So we don't get errors from log(0)
    else links
  }

  def adjust_incoming_links(incoming_links: Option[Int]) = {
    val ail =
      incoming_links match {
        case None => {
          if (debug("some"))
            warning("Strange, %s has no link count", obj)
          0
        }
        case Some(il) => {
          if (debug("some"))
            errprint("--> Link count is %s", il)
          il
        }
      }
    adjust_incoming_links(ail)
  }
}

// A Wikipedia article.  Defined fields:
//
//   title: Title of article.
//   id: ID of article, as an int.
//   coord: Coordinates of article.
//   incoming_links: Number of incoming links, or None if unknown.
//   split: Split of article ("training", "dev", "test")
//   redir: If this is a redirect, article title that it redirects to; else
//          an empty string.
//   namespace: Namespace of article (e.g. "Main", "Wikipedia", "File")
//   is_list_of: Whether article title is "List of *"
//   is_disambig: Whether article is a disambiguation page.
//   is_list: Whether article is a list of any type ("List of *", disambig,
//            or in Category or Book namespaces)
class Article(params:Map[String,String]) {
  var title="unknown"
  var id=0
  var coord:Coord=null
  var incoming_links:Option[Int]=None
  var split="unknown"
  var redir=""
  var namespace="Main"
  var is_list_of=false
  var is_disambig=false
  var is_list=false

  for ((name, v) <- params) {
    name match {
      case "id" => id = v.toInt
      case "title" => title = v
      case "split" => split = v
      case "redir" => redir = v
      case "namespace" => namespace = v
      case "is_list_of" => is_list_of = yesno_to_boolean(v)
      case "is_disambig" => is_disambig = yesno_to_boolean(v)
      case "is_list" => is_list = yesno_to_boolean(v)
      case "coord" => coord = commaval_to_coord(v)
      case "incoming_links" => incoming_links = get_int_or_blank(v)
      }
  }

  def get_fields(fields:Traversable[String]) = {
    for (field <- fields) yield {
      field match {
        case "id" => id.toString
        case "title" => title
        case "split" => split
        case "redir" => redir
        case "namespace" => namespace
        case "is_list_of" => boolean_to_yesno(is_list_of)
        case "is_disambig" => boolean_to_yesno(is_disambig)
        case "is_list" => boolean_to_yesno(is_list)
        case "coord" => coord_to_commaval(coord)
        case "incoming_links" => put_int_or_blank(incoming_links)
      }
    }
  }

  def toString() = {
    val coordstr = if (coord) " at %s".format(coord) else ""
    val redirstr = if (redir) ", redirect to %s".format(redir) else ""
    "%s(%s)%s%s".format(title, id, coordstr, redirstr)
  }

 def get_adjusted_incoming_links = adjust_incoming_links(incoming_links)
}

object Article {
  def yesno_to_boolean(foo:String)  = {
    foo match {
      case "yes" => true
      case "no" => false
      case _ => {
        warning("Expected yes or no, saw '%s'", foo)
        false
      }
    }
  }
  
  def boolean_to_yesno(foo:Boolean) = if (foo) "yes" else "no"
  
  def commaval_to_coord(foo:String) = {
    if (foo != "") {
      val Array(lat, long) = foo.split(',')
      Coord(lat.toDouble, long.toDouble)
    } else null
  }
  
  def coord_to_commaval(foo:Coord) =
    if (foo != null) "%s,%s".format(foo.lat, foo.long) else ""
  
  def get_int_or_blank(foo:String) =
    if (foo == "") None else Option[Int](foo.toInt)
  
  def put_int_or_blank(foo:Option[Int]) = {
    foo match {
      case None => ""
      case Some(x) => x.toString
    }
  }
}
