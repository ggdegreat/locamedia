/

////////
//////// Disambig.scala
////////
//////// Copyright (c) 2010, 2011 Ben Wing.
////////

import util.matching.Regex
import math._
import collection.mutable
import util.control.Breaks._
import KLDiv._
import NlpUtil._
import ArticleData._
import WordDist._

//import sys
//import os
//import os.path
//import traceback
//from itertools import *
//import random
//import gc
//import time

/////////////////////////////////////////////////////////////////////////////
//                              Documentation                              //
/////////////////////////////////////////////////////////////////////////////

////// Quick start

// This program does disambiguation of geographic names on the TR-CONLL corpus.
// It uses data from Wikipedia to do this.  It is "unsupervised" in the sense
// that it does not do any supervised learning using the correct matches
// provided in the corpus; instead, it uses them only for evaluation purposes.

/////////////////////////////////////////////////////////////////////////////
//                                  Globals                                //
/////////////////////////////////////////////////////////////////////////////

object Disambig {
  
  /////////////////////////////////////////////////////////////////////////////
  //                               Structures                                //
  /////////////////////////////////////////////////////////////////////////////
  
  def print_structure(struct:Any, indent:Int=0) {
    val indstr = " "*indent
    if (struct == null)
      errprint("%snull", indstr)
    else if (struct.isInstanceOf[Tuple2[Any,Any]]) {
      val (x,y) = struct
      print_structure(List(x,y), indent)
    } else if (!(struct.isInstanceOf[Seq[Any]]) || struct.length == 0)
      errprint("%s%s", indstr, struct)
    else {
      if (struct(0).isInstanceOf[String]) {
        errprint("%s%s:", indstr, struct[0])
        indstr += "  "
        indent += 2
        struct = struct[1:]
      }
      for (s <- struct) {
        if (isinstance(s, Seq))
          print_structure(s, indent + 2)
        else if (isinstance(s, tuple)) {
          (key, val) = s
          if (isinstance(val, Seq)) {
            errprint("%s%s:", indstr, key)
            print_structure(val, indent + 2)
          }
          else
            errprint("%s%s: %s", indstr, key, val)
        }
        else
          errprint("%s%s", indstr, s)
      }
    }
  }
  
  def main() {
    WikiDisambigProgram()
  }
}

object RegionDist {
  // Height of highest bar in meters
  val kml_max_height = 2000000
  
  // Minimum and maximum colors
  val kml_mincolor = (255, 255, 0)    // yellow
  val kml_maxcolor = (255, 0, 0)      // red
}

object Distances {
  /////////////////////////////////////////////////////////////////////////////
  //                       Coordinates and regions                           //
  /////////////////////////////////////////////////////////////////////////////
  
  // The coordinates of a point are spherical coordinates, indicating a
  // latitude and longitude.  Latitude ranges from -90 degrees (south) to
  // +90 degrees (north), with the Equator at 0 degrees.  Longitude ranges
  // from -180 degrees (west) to +179.9999999.... degrees (east). -180 and +180
  // degrees correspond to the same north-south parallel, and we arbitrarily
  // choose -180 degrees over +180 degrees.  0 degrees longitude has been
  // arbitrarily chosen as the north-south parallel that passes through
  // Greenwich, England (near London).  Note that longitude wraps around, but
  // latitude does not.  Furthermore, the distance between latitude lines is
  // always the same (about 69 miles per degree), but the distance between
  // longitude lines varies according to the latitude, ranging from about
  // 69 miles per degree at the Equator to 0 miles at the North and South Pole.
  //
  // We divide the earth's surface into "tiling regions", using the value
  // of --region-size, which is specified in miles; we convert it to degrees
  // using 'miles_per_degree', which is derived from the value for the
  // Earth's radius in miles.  In addition, we form a square of tiling regions
  // in order to create a "statistical region", which is used to compute a
  // distribution over words.  The numbe of tiling regions on a side is
  // determined by --width-of-stat-region.  Note that if this is greater than
  // 1, different statistical regions will overlap.
  //
  // To specify a region, we use region indices, which are derived from
  // coordinates by dividing by degrees_per_region.  Hence, if for example
  // degrees_per_region is 2, then region indices are in the range [-45,+45]
  // for latitude and [-90,+90) for longitude.  In general, an arbitrary
  // coordinate will have fractional region indices; however, the region
  // indices of the corners of a region (tiling or statistical) will be
  // integers.  Normally, we use the southwest corner to specify a region.
  //
  // Near the edges, tiling regions may be truncated.  Statistical regions
  // will wrap around longitudinally, and will still have the same number
  // of tiling regions, but may be smaller.
  
  // Size of each region in degrees.  Determined by the --region-size option
  // (however, that option is expressed in miles).
  val degrees_per_region = 0.0
  
  // Minimum, maximum latitude/longitude in indices (integers used to index the
  // set of regions that tile the earth)
  val minimum_latind = null
  val maximum_latind = null
  val minimum_longind = null
  val maximum_longind = null
  
  // Radius of the earth in miles.  Used to compute spherical distance in miles,
  // and miles per degree of latitude/longitude.
  val earth_radius_in_miles = 3963.191
  
  // Number of miles per degree, at the equator.  For longitude, this is the
  // same everywhere, but for latitude it is proportional to the degrees away
  // from the equator.
  val miles_per_degree = Pi * 2 * earth_radius_in_miles / 360.
  
  // Compute spherical distance in miles (along a great circle) between two
  // coordinates.
  
  def spheredist(p1:Coord, p2:Coord):Double = {
    if (p1 == null || p2 == null) return 1000000.
    val thisRadLat = (p1.lat / 180.) * Pi
    val thisRadLong = (p1.long / 180.) * Pi
    val otherRadLat = (p2.lat / 180.) * Pi
    val otherRadLong = (p2.long / 180.) * Pi
          
    val anglecos = (sin(thisRadLat)*sin(otherRadLat)
                + cos(thisRadLat)*cos(otherRadLat)*
                  cos(otherRadLong-thisRadLong))
    // If the values are extremely close to each other, the resulting cosine
    // value will be extremely close to 1.  In reality, however, if the values
    // are too close (e.g. the same), the computed cosine will be slightly
    // above 1, and acos() will complain.  So special-case this.
    if (abs(anglecos) > 1.0) {
      if (abs(anglecos) > 1.000001) {
        warning("Something wrong in computation of spherical distance, out-of-range cosine value %f", anglecos)
        return 1000000.
      } else
        return 0.
    }
    return earth_radius_in_miles * acos(anglecos)
  }
  
  // Convert a coordinate to the indices of the southwest corner of the
  // corresponding tiling region.
  def coord_to_tiling_region_indices(coord:Coord) = {
    val latind = floor(coord.lat / degrees_per_region).toInt
    val longind = floor(coord.long / degrees_per_region).toInt
    (latind, longind)
  }
  
  // Convert a coordinate to the indices of the southwest corner of the
  // corresponding statistical region.
  def coord_to_stat_region_indices(coord:Coord) = {
    // When Opts.width_of_stat_region = 1, don't subtract anything.
    // When Opts.width_of_stat_region = 2, subtract 0.5*degrees_per_region.
    // When Opts.width_of_stat_region = 3, subtract degrees_per_region.
    // When Opts.width_of_stat_region = 4, subtract 1.5*degrees_per_region.
    // In general, subtract (Opts.width_of_stat_region-1)/2.0*degrees_per_region.
  
    // Compute the indices of the southwest region
    val subval = (Opts.width_of_stat_region-1)/2.0*degrees_per_region
    val lat = coord.lat - subval
    val long = coord.long - subval
  
    coord_to_tiling_region_indices(Coord(lat, long))
  }
  
  // Convert region indices to the corresponding coordinate.  This can also
  // be used to find the coordinate of the southwest corner of a tiling region
  // or statistical region, as both are identified by the region indices of
  // their southwest corner.
  def region_indices_to_coord(latind:Int, longind:Int,
                              coerce_within_bounds:Boolean=false) = {
    Coord(latind * degrees_per_region, longind * degrees_per_region,
        coerce_within_bounds=coerce_within_bounds)
  }
  
  // Add 'offset' to both latind and longind and then convert to a
  // coordinate.  Coerce the coordinate to be within bounds.
  def offset_region_indices_to_coord(latind:Int, longind:Int, offset:Double) = {
    region_indices_to_coord(latind + offset, longind + offset,
        coerce_within_bounds=true)
  }
  
  // Convert region indices of a tiling region to the coordinate of the
  // near (i.e. southwest) corner of the region.
  def tiling_region_indices_to_near_corner_coord(latind:Int, longind:Int) = {
    region_indices_to_coord(latind, longind)
  }
  
  // Convert region indices of a tiling region to the coordinate of the
  // center of the region.
  def tiling_region_indices_to_center_coord(latind:Int, longind:Int) = {
    offset_region_indices_to_coord(latind, longind, 0.5)
  }
  
  // Convert region indices of a tiling region to the coordinate of the
  // far (i.e. northeast) corner of the region.
  def tiling_region_indices_to_far_corner_coord(latind:Int, longind:Int) = {
    offset_region_indices_to_coord(latind, longind, 1)
  }
  
  // Convert region indices of a tiling region to the coordinate of the
  // near (i.e. southwest) corner of the region.
  def stat_region_indices_to_near_corner_coord(latind:Int, longind:Int) = {
    region_indices_to_coord(latind, longind)
  }
  
  // Convert region indices of a statistical region to the coordinate of the
  // center of the region.
  def stat_region_indices_to_center_coord(latind:Int, longind:Int) = {
    offset_region_indices_to_coord(latind, longind,
        Opts.width_of_stat_region/2.0)
  }
  
  // Convert region indices of a statistical region to the coordinate of the
  // far (i.e. northeast) corner of the region.
  def stat_region_indices_to_far_corner_coord(latind:Int, longind:Int) = {
    offset_region_indices_to_coord(latind, longind,
        Opts.width_of_stat_region)
  }
  
  // Convert region indices of a statistical region to the coordinate of the
  // northwest corner of the region.
  def stat_region_indices_to_nw_corner_coord(latind:Int, longind:Int) = {
    region_indices_to_coord(latind + Opts.width_of_stat_region, longind,
        coerce_within_bounds=true)
  }
  
  // Convert region indices of a statistical region to the coordinate of the
  // southeast corner of the region.
  def stat_region_indices_to_se_corner_coord(latind:Int, longind:Int) = {
    region_indices_to_coord(latind, longind + Opts.width_of_stat_region,
        coerce_within_bounds=true)
  }
  
  // Convert region indices of a statistical region to the coordinate of the
  // southwest corner of the region.
  def stat_region_indices_to_sw_corner_coord(latind:Int, longind:Int) = {
    stat_region_indices_to_near_corner_coord(latind, longind)
  }
  
  // Convert region indices of a statistical region to the coordinate of the
  // northeast corner of the region.
  def stat_region_indices_to_ne_corner_coord(latind:Int, longind:Int) = {
    stat_region_indices_to_far_corner_coord(latind, longind)
  }
}

// A class holding the boundary of a geographic object.  Currently this is
// just a bounding box, but eventually may be expanded to including a
// convex hull or more complex model.

class Boundary(botleft:Coord, topright:Coord) {
  def toString(self) = {
    "%s-%s" format (self.botleft, self.topright)
  }

  def __repr__(self)  = {
    "Boundary(%s)" format self.toString()
  }

  def struct(self) = {
    List("Boundary", ("boundary", "%s-%s" format (self.botleft, self.topright)))
  }

  def contains(self, coord:Coord) = {
    if !(coord.lat >= self.botleft.lat && coord.lat <= self.topright.lat)
      false
    if (self.botleft.long <= self.topright.long)
      (coord.long >= self.botleft.long && coord.long <= self.topright.long)
    else
      // Handle case where boundary overlaps the date line.
      (coord.long >= self.botleft.long &&
       coord.long <= self.topright.long + 360.) ||
      (coord.long >= self.botleft.long - 360. &&
       coord.long <= self.topright.long)
    }
  }

  def square_area(self) = {
    var (lat1, lon1) = (self.botleft.lat, self.botleft.long)
    var (lat2, lon2) = (self.topright.lat, self.topright.long)
    lat1 = (lat1 / 180.) * Pi
    lat2 = (lat2 / 180.) * Pi
    lon1 = (lon1 / 180.) * Pi
    lon2 = (lon2 / 180.) * Pi

    (earth_radius_in_miles ** 2) *
     abs(sin(lat1) - sin(lat2)) *
     abs(lon1 - lon2)
  }

  // Iterate over the regions that overlap the boundary.  If
  // 'nonempty_word_dist' is true, only yield regions with a non-empty
  // word distribution; else, yield all non-empty regions.
  def iter_nonempty_tiling_regions(self) {
    val (latind1, longind1) = coord_to_tiling_region_indices(self.botleft)
    val (latind2, longind2) = coord_to_tiling_region_indices(self.topright)
    for {i <- latind1 to latind2 view
         val it = if (longind1 <= longind2) longind1 to longind2 view
                  else (longind1 to maximum_longind view) ++
                       (minimum_longind to longind2 view)
         j <- it
         if (StatRegion.tiling_region_to_articles contains (i, j))
        } yield (i, j)
  }
}

/////////////////////////////////////////////////////////////////////////////
//                             Word distributions                          //
/////////////////////////////////////////////////////////////////////////////

// Distribution over words corresponding to a statistical region.  The following
// fields are defined in addition to base class fields:
//
//   articles: Articles used in computing the distribution.
//   num_arts_for_word_dist: Number of articles included in word distribution.
//   num_arts_for_links: Number of articles included in incoming-link
//                       computation.
//   incoming_links: Total number of incoming links.

class RegionWordDist extends WordDist {
  val num_arts_for_links = 0
  val incoming_links = 0
  val num_arts_for_word_dist = 0

  def is_empty_for_word_dist(self) = self.num_arts_for_word_dist == 0

  def is_empty(self) = self.num_arts_for_links == 0

  // Add the given articles to the total distribution seen so far
  def add_articles(self, articles:Iterable[Article]) {
    var this_incoming_links = 0
    if (debug("lots"))
      errprint("Region dist, number of articles = %s", num_arts_for_word_dist)
    val old_total_tokens = self.total_tokens
    var this_num_arts_for_links = 0
    var this_num_arts_for_word_dist = 0
    for (art <- articles) {
      // Might be None, for unknown link count
      art.incoming_links match {
        case Some(x) => this_incoming_links += x
        case _ =>
      }
      this_num_arts_for_links += 1
      if (!art.dist) {
        if (Opts.max_time_per_stage == 0 && Opts.num_training_docs == 0)
          warning("Saw article %s without distribution", art)
      } else {
        assert(art.dist.finished)
        if (art.split == "training") {
          self.add_word_distribution(art.dist)
          this_num_arts_for_word_dist += 1
        }
      }
    }
    self.num_arts_for_links += this_num_arts_for_links
    self.num_arts_for_word_dist = this_num_arts_for_word_dist
    self.incoming_links += this_incoming_links
    if (this_num_arts_for_word_dist && debug("lots")) {
      errprint("""--> Finished processing, number articles handled = %s/%s,
    skipped articles = %s, total tokens = %s/%s, incoming links = %s/%s""",
               this_num_arts_for_word_dist,
               self.num_arts_for_word_dist,
               this_num_arts_for_links - this_num_arts_for_word_dist,
               self.total_tokens - old_total_tokens,
               self.total_tokens, this_incoming_links, self.incoming_links)
    }
  }

  def add_locations(self, locs:Iterable[Article]) {
    val arts = for (loc <- locs if loc.artmatch) yield loc.artmatch
    self.add_articles(arts)
  }

  def finish(self, minimum_word_count:Int=0) {
    super(RegionWordDist, self).finish(minimum_word_count=minimum_word_count)

    if debug("lots")
      errprint("""For region dist, num articles = %s, total tokens = %s,
    unseen_mass = %s, incoming links = %s, overall unseen mass = %s""",
               self.num_arts_for_word_dist, self.total_tokens,
               self.unseen_mass, self.incoming_links,
               self.overall_unseen_mass)
  }

  // For a document described by its distribution 'worddist', return the
  // log probability log p(worddist|reg) using a Naive Bayes algorithm.
  def get_nbayes_logprob(self, worddist:WordDist) = {
    val logprob = 0.0
    for ((word, count) <- worddist.counts.iteritems()) {
      val val = self.lookup_word(word)
      if (val <= 0) {
        // FIXME: Need to figure out why this happens (perhaps the word was
        // never seen anywhere in the training data? But I thought we have
        // a case to handle that) and what to do instead.
        errprint("Warning! For word %s, prob %s out of range", word, val)
      }
      else
        logprob += log(val)
    }
    // FIXME: Also use baseline (prior probability)
    logprob
  }

 def incoming_links_or_zero = {
   incoming_links match {
     case Some(x) => x
     case None => 0
   }
 }
 def get_adjusted_incoming_links = adjust_incoming_links(incoming_links)
}


/////////////////////////////////////////////////////////////////////////////
//                             Region distributions                        //
/////////////////////////////////////////////////////////////////////////////

// Distribution over regions, as might be attached to a word.  If we have a
// set of regions, each with a word distribution, then we can imagine
// conceptually inverting the process to generate a region distribution over
// words.  Basically, for a given word, look to see what its probability is
// in all regions; normalize, and we have a region distribution.

// Fields defined:
//
//   word: Word for which the region is computed
//   regionprobs: Hash table listing probabilities associated with regions

class RegionDist(word:String=null,
                 regionprobs:Map[Region, Double]=Map[Region, Double]()) {
  var normalized = false

  private def init() {
    // It's expensive to compute the value for a given word so we cache word
    // distributions.
    val totalprob = 0.0
    // Compute and store un-normalized probabilities for all regions
    for (reg <- StatRegion.iter_nonempty_regions(nonempty_word_dist=true)) {
      val prob = reg.worddist.lookup_word(word)
      // Another way of handling zero probabilities.
      /// Zero probabilities are just a bad idea.  They lead to all sorts of
      /// pathologies when trying to do things like "normalize".
      //if (prob == 0.0)
      //  prob = 1e-50
      self.regionprobs[reg] = prob
      totalprob += prob
    }
    // Normalize the probabilities; but if all probabilities are 0, then
    // we can't normalize, so leave as-is. (FIXME When can this happen?
    // It does happen when you use --mode=generate-kml and specify words
    // that aren't seen.  In other circumstances, the smoothing ought to
    // ensure that 0 probabilities don't exist?  Anything else I missed?)
    if (totalprob != 0) {
      self.normalized = true
      for ((reg, prob) <- self.regionprobs.iteritems())
        self.regionprobs[reg] /= totalprob
    }
    else
      self.normalized = false
  }

  if (word != null) init()

  def get_ranked_regions(self) = {
    // sort by second element of tuple, in reverse order
    self.regionprobs sortWith(_._2 > _._2)
  }
  // Convert region to a KML file showing the distribution
  def generate_kml_file(self, filename:String) {
    val xform = if (Opts.kml_transform == "log") x => log(x)
      else if (Opts.kml_transform == "logsquared") x => -log(x)*log(x)
      else x => x

    val minxformprob = xform(self.regionprobs.values min)
    val maxxformprob = xform(self.regionprobs.values max)

    // Generate KML for a single region
    def one_reg_kml(reg:StatRegion, prob:Double) = {
      val (latind, longind) = reg.latind, reg.longind
      val offprob = xform(prob) - minxformprob
      val fracprob = offprob / (maxxformprob - minxformprob)
      val swcoord = stat_region_indices_to_near_corner_coord(latind, longind)
      val necoord = stat_region_indices_to_far_corner_coord(latind, longind)
      val nwcoord = Coord(necoord.lat, swcoord.long)
      val secoord = Coord(swcoord.lat, necoord.long)
      val center = stat_region_indices_to_center_coord(latind, longind)
      var coordtext = "\n"
      for (coord <- (swcoord, nwcoord, necoord, secoord, swcoord)) {
        val lat = (center.lat + coord.lat) / 2
        val long = (center.long + coord.long) / 2
        coordtext += "%s,%s,%s\n" format (long, lat, fracprob*kml_max_height)
      }
      val name = if (reg.most_popular_article) reg.most_popular_article.title else ""

      // Placemark indicating name
      val name_placemark = 
        <Placemark>
          <name>{name}</name>,
          <Region>
            <LatLonAltBox>
              <north>((center.lat + necoord.lat) / 2).toString</north>
              <south>((center.lat + swcoord.lat) / 2).toString</south>
              <east>((center.long + necoord.long) / 2).toString</east>
              <west>((center.long + swcoord.long) / 2).toString</west>
            </LatLonAltBox>
            <Lod>
              <minLodPixels>16</minLodPixels>
            </Lod>
          </Region>
          <styleURL>#bar</styleURL>
          <Point>
            <coordinates>{"%s,%s" format (center.long, center.lat)}</coordinates>
          </Point>
        </Placemark>

      // Interpolate colors
      val color = Array(0, 0, 0)
      for (i <- 1 to 3) {
        color[i] = (kml_mincolor[i] +
            fracprob*(kml_maxcolor[i] - kml_mincolor[i]))
      }
      // Original color dc0155ff
      //rgbcolor = "dc0155ff"
      val rgbcolor = "ff%02x%02x%02x" format tuple(reversed(color))

      // Yield cylinder indicating probability by height and color

      val cylinder_placemark =
        <Placemark>
          <name>{"%s POLYGON" format name}</name>
            <styleUrl>#bar</styleUrl>
            <Style>
              <PolyStyle>
                <color>{rgbcolor}</color>
                <colorMode>normal</colorMode>
              </PolyStyle>
            </Style>
            <Polygon>
              <extrude>1</extrude>
              <tessellate>1</tesselate>
              <altitudeMode>relativeToGround</altitudeMode>
              <outerBoundaryIs>
                <LinearRing>
                  <coordinates>{coordtext}</coordinates>
                </LinearRing>
              </outerBoundaryIs>
            </Polygon>
          </Placemark>
      List(name_placemark, cylinder_placemark)
    }

    def yield_reg_kml() {
      for {(reg, prob) <- self.regionprobs
          kml <- one_reg_kml(reg, prob)
          expr <- kml} yield expr
    }

    val allregkml = yield_reg_kml()

    val kml =
      <kml xmlns="http://www.opengis.net/kml/2.2"
           xmlns:gx="http://www.google.com/kml/ext/2.2"
           xmlns:kml="http://www.opengis.net/kml/2.2"
           xmlns:atom="http://www.w3.org/2005/Atom">
        <Document>
          <Style id="bar">
            <PolyStyle>
              <outline>0</outline>
            </PolyStyle>
            <IconStyle>
              <Icon/>
            </IconStyle>
          </Style>
          <Style id="downArrowIcon">
            <IconStyle>
              <Icon>
                <href>http://maps.google.com/mapfiles/kml/pal4/icon28.png</href>
              </Icon>
            </IconStyle>
          </Style>
          <Folder>
            <name>{word}</name>
            <open>1</open>
            <description>{"Region distribution for word '%s'" format word}</description>
            <LookAt>
              <latitude>42</latitude>
              <longitude>-102</longitude>
              <altitude>0</altitude>
              <range>5000000</range>
              <tilt>53.454348562403</tilt>
              <heading>0</heading>
            </LookAt>
            {allregkml}
          </Folder>
        </Document>
      </kml>

    output_xml_file(filename, kml)
  }
}

object RegionDist {
  var cached_dists:LRUCache = null

  // Return a region distribution over a given word, using a least-recently-used
  // cache to optimize access.
  def get_region_dist(word:String) {
    if (cached_dists == null)
      cached_dists = LRUCache(maxsize=Opts.lru_cache_size)
    cached_dists.get(word) match {
      case Some[dist] => dist
      case null => {
        val dist = new RegionDist(word)
        cached_dists(word) = dist
        dist
      }
    }
  }

  // Return a region distribution over a distribution over words.  This works
  // by adding up the distributions of the individual words, weighting by
  // the count of the each word.
  def get_region_dist_for_word_dist(cls, worddist:WordDist) = {
    val regprobs = gendoublemap[RegionDist]()
    for ((word, count) <- worddist.counts) {
      val dist = cls.get_region_dist(word)
      for ((reg, prob) <- dist.regionprobs)
        regprobs[reg] += count*prob
    }
    val totalprob = sum(regprobs.values())
    for ((reg, prob) <- regprobs)
      regprobs[reg] /= totalprob
    RegionDist(regionprobs=regprobs)
  }
}

/////////////////////////////////////////////////////////////////////////////
//                           Geographic locations                          //
/////////////////////////////////////////////////////////////////////////////

///////////// statistical regions ////////////

// This class contains values used in computing the distribution over all
// locations in the statistical region surrounding the locality in question.
// The statistical region is currently defined as a square of NxN tiling
// regions, for N = Opts.width_of_stat_region.
// The following fields are defined: 
//
//   latind, longind: Region indices of southwest-most tiling region in
//                    statistical region.
//   worddist: Distribution corresponding to region.

class StatRegion(latind:Int, longind:Int) {
  val self.worddist = RegionWordDist()
  val self.most_popular_article:Article = null
  val self.mostpopart_links = 0

  def boundstr(self) = {
    if (self.latind != null) {
      val near = stat_region_indices_to_near_corner_coord(self.latind, self.longind)
      val far = stat_region_indices_to_far_corner_coord(self.latind, self.longind)
      "%s-%s" format (near, far)
    }
    else "nowhere"
  }

  def toString(self) = {
    val unfinished = "" if self.worddist.finished else ", unfinished"
    val contains = 
      if (self.most_popular_article)
         ", most-pop-art %s(%d links)" format (
           self.most_popular_article, self.mostpopart_links)
      else ""

    "StatRegion(%s%s%s, %d articles(dist), %d articles(links), %d links)" format (
        self.boundstr(), unfinished, contains,
        self.worddist.num_arts_for_word_dist, self.worddist.num_arts_for_links,
        self.worddist.incoming_links)
  }

  def __repr__(self) = {
    self.toString.encode("utf-8")
  }

  def shortstr(self) = {
    var str = "Region %s" format self.boundstr()
    val mostpop = self.most_popular_article
    if (mostpop != null)
      str += ", most-popular %s" format mostpop.shortstr()
    str
  }

  def struct(self) = {
    val foo = mutable.Seq[Any]("StatRegion")
    foo :+= ("bounds", self.boundstr())
    if (!self.worddist.finished)
      foo :+= ("finished", true)
    if (self.most_popular_article) {
      foo :+= ("most-popular-article", self.most_popular_article.struct())
      foo :+= ("most-popular-article-links", self.mostpopart_links)
    }
    foo :+= ("num-articles-dist", self.worddist.num_arts_for_word_dist)
    foo :+= ("num-articles-link", self.worddist.num_arts_for_links)
    foo :+= ("incoming-links", self.worddist.incoming_links)
    foo
  }

  // Generate the distribution for a statistical region from the tiling regions.
  def generate_dist(self) {

    val reglat = self.latind
    val reglong = self.longind

    if debug("lots")
      errprint("Generating distribution for statistical region centered at %s",
               region_indices_to_coord(reglat, reglong))

    // Accumulate counts for the given region
    def process_one_region(latind:Int, longind:Int) {
      val arts = StatRegion.tiling_region_to_articles.getOrElse((latind, longind), null)
      if (arts == null)
        return
      if debug("lots")
        errprint("--> Processing tiling region %s",
                 region_indices_to_coord(latind, longind))
      self.worddist.add_articles(arts)
      for (art <- arts) {
        if (art.incoming_links > self.mostpopart_links) {
          self.mostpopart_links = art.incoming_links
          self.most_popular_article = art
        }
      }
    }

    // Process the tiling regions making up the statistical region;
    // but be careful around the edges.  Truncate the latitude, wrap the
    // longitude.
    for (i <- reglat until (maximum_latind + 1 min
                            reglat + Opts.width_of_stat_region)) {
      for (j <- reglong until reglong + Opts.width_of_stat_region) {
        val jj = j
        if (jj > maximum_longind) jj -= 360.
        process_one_region(i, jj)
      }
    }

    self.worddist.finish(minimum_word_count=Opts.minimum_word_count)
  }
}

object StatRegion {
  // Mapping of region->locations in region, for region-based Naive Bayes
  // disambiguation.  The key is a tuple expressing the integer indices of the
  // latitude and longitude of the southwest corner of the region. (Basically,
  // given an index, the latitude or longitude of the southwest corner is
  // index*degrees_per_region, and the region includes all locations whose
  // latitude or longitude is in the half-open interval
  // [index*degrees_per_region, (index+1)*degrees_per_region).
  //
  // We don't just create an array because we expect many regions to have no
  // articles in them, esp. as we decrease the region size.  The idea is that
  // the regions provide a first approximation to the regions used to create the
  // article distributions.
  var tiling_region_to_articles = seqmap[String]()

  // Mapping from center of statistical region to corresponding region object.
  // A "statistical region" is made up of a square of tiling regions, with
  // the number of regions on a side determined by `Opts.width_of_stat_region'.  A
  // word distribution is associated with each statistical region.
  val corner_to_stat_region = Map()

  var empty_stat_region:StatRegion = null // Can't compute this until class is initialized
  var all_regions_computed = false
  var num_empty_regions = 0
  var num_non_empty_regions = 0
  var total_num_arts_for_word_dist = 0
  var total_num_arts_for_links = 0

  // Find the correct StatRegion for the given coordinates.
  // If none, create the region.
  def find_region_for_coord(cls, coord:Coord) = {
    val (latind, longind) = coord_to_stat_region_indices(coord)
    cls.find_region_for_region_indices(latind, longind)
  }

  // Find the StatRegion with the given indices at the southwest point.
  // If none, create the region unless 'no_create' is true.  Otherwise, if
  // 'no_create_empty' is true and the region is empty, a default empty
  // region is returned.
  def find_region_for_region_indices(cls,
      latind:Int, longind:Int, no_create:Boolean=false,
      no_create_empty:Boolean=false):StatRegion = {
    var statreg = cls.corner_to_stat_region.getOrElse((latind, longind), null)
    if (statreg == null) {
      if (no_create)
        return null
      if (cls.all_regions_computed) {
        if (!cls.empty_stat_region) {
          cls.empty_stat_region = cls(null, null)
          cls.empty_stat_region.worddist.finish()
        }
        return cls.empty_stat_region
      }
      statreg = cls(latind, longind)
      statreg.generate_dist()
      val empty = statreg.worddist.is_empty()
      if (empty)
        cls.num_empty_regions += 1
      else
        cls.num_non_empty_regions += 1
      if (!empty || !no_create_empty)
        cls.corner_to_stat_region[(latind, longind)] = statreg
    }
    return statreg
  }

  // Generate all StatRegions that are non-empty.  Don't do anything if
  // called multiple times.
  def initialize_regions(cls) {
    if (cls.all_regions_computed)
      return

    errprint("Generating all non-empty statistical regions...")
    val status = StatusMessage("statistical region")

    for (i <- minimum_latind to maximum_latind view) {
      for (j <- minimum_longind to maximum_longind view) {
        val reg = cls.find_region_for_region_indices(i, j, no_create_empty=true)
        if (debug("region") && reg.worddist.size > 0)
          errprint("--> (%d,%d): %s", i, j, reg)
        status.item_processed()
      }
    }
    cls.all_regions_computed = true

    cls.total_num_arts_for_links = 0
    cls.total_num_arts_for_word_dist = 0
    for (reg <- StatRegion.iter_nonempty_regions()) {
      cls.total_num_arts_for_word_dist += reg.worddist.num_arts_for_word_dist
      cls.total_num_arts_for_links += reg.worddist.num_arts_for_links
    }

    errprint("Number of non-empty regions: %s", cls.num_non_empty_regions)
    errprint("Number of empty regions: %s", cls.num_empty_regions)
    // Save some memory by clearing this after it's not needed
    cls.tiling_region_to_articles = null
    ArticleTable.clear_training_article_distributions()
  }

  // Add the given article to the region map, which covers the earth in regions
  // of a particular size to aid in computing the regions used in region-based
  // Naive Bayes.
  def add_article_to_region(cls, article:Article) {
    val (latind, longind) = coord_to_tiling_region_indices(article.coord)
    cls.tiling_region_to_articles[(latind, longind)] :+= article
  }

  // Iterate over all non-empty regions.  If 'nonempty_word_dist' is given,
  // distributions must also have a non-empty word distribution; otherwise,
  // they just need to have at least one point in them. (Not all points
  // have word distributions, esp. when --max-time-per-stage is set so
  // that we only load the word distributions for a fraction of the whole
  // set of articles with distributions.)
  def iter_nonempty_regions(cls, nonempty_word_dist:Boolean=false) {
    assert(cls.all_regions_computed)
    for {
      v <- cls.corner_to_stat_region.values
      empty = (
        if (nonempty_word_dist) v.worddist.is_empty_for_word_dist()
        else v.worddist.is_empty()
      )
      if (!empty)
    } yield v
  }
}

///////////// Locations ////////////

// A general location (either locality or division).  The following
// fields are defined:
//
//   name: Name of location.
//   altnames: List of alternative names of location.
//   typ: Type of location (locality, agglomeration, country, state,
//                           territory, province, etc.)
//   artmatch: Wikipedia article corresponding to this location.
//   div: Next higher-level division this location is within, or None.

class Location(name:String, altnames:List[String], typ:String) {
  var artmatch:Article = null
  var div:Division = null
}

// A location corresponding to an entry in a gazetteer, with a single
// coordinate.
//
// The following fields are defined, in addition to those for Location:
//
//   coord: Coordinates of the location, as a Coord object.
//   stat_region: The statistical region surrounding this location, including
//             all necessary information to determine the region-based
//             distribution.

class Locality(name:String, coord:Coord, altnames:List[String], typ:String)
  extends Location(name, altnames, typ) {
  var stat_region:StatRegion = null

  def toString(self, no_article:Boolean=false) = {
    var artmatch = ""
    if (!no_article)
      artmatch = ", match=%s" format self.artmatch
    "Locality %s (%s) at %s%s" format (self.name, self.div && self.div.path.mkString("/"), self.coord, artmatch)
  }

  def __repr__(self) = {
    self.toString.encode("utf-8")
  }

  def shortstr(self) = {
    "Locality %s (%s)" format (
        self.name, self.div && self.div.path.mkString("/"))
  }

  def struct(self, no_article:Boolean=false) = {
    var foo = mutable.Seq[Any]("Locality")
    foo :+= ("name", self.name)
    foo :+= ("in division", self.div && self.div.path.mkString("/"))
    foo :+= ("at coordinate", self.coord)
    if (!no_article) {
      if (self.artmatch)
        foo :+= ("matching", self.artmatch.struct())
      else
        foo :+= ("matching", "none")
    }
    foo
  }

  def distance_to_coord(self, coord:Coord) = spheredist(self.coord, coord)

  def matches_coord(self, coord:Coord) = {
    self.distance_to_coord(coord) <= Opts.max_dist_for_close_match
  }
}


// A division higher than a single locality.  According to the World
// gazetteer, there are three levels of divisions.  For the U.S., this
// corresponds to country, state, county.
//
// The following fields are defined:
//
//   level: 1, 2, or 3 for first, second, or third-level division
//   path: Tuple of same size as the level #, listing the path of divisions
//         from highest to lowest, leading to this division.  The last
//         element is the same as the "name" of the division.
//   locs: List of locations inside of the division.
//   goodlocs: List of locations inside of the division other than those
//             rejected as outliers (too far from all other locations).
//   boundary: A Boundary object specifying the boundary of the area of the
//             division.  Currently in the form of a rectangular bounding box.
//             Eventually may contain a convex hull or even more complex
//             region (e.g. set of convex regions).
//   worddist: For region-based Naive Bayes disambiguation, a distribution
//           over the division's article and all locations within the region.

class Division(path:String)
    extends Location(path(path.length-1), List[String](), "unknown") {
  
  val level = path.length
  var locs = List[Location]()
  var goodlocs = List[Location]()
  var boundary = Boundary()
  var artmatch:Article = null
  val worddist:WordDist = null

  def toString(self, no_article:Boolean=false) = {
    val artmatch =
      if (no_article) "" else artmatch = ", match=%s" format self.artmatch
    "Division %s (%s)%s, boundary=%s" format (self.name, self.path.mkString("/"), artmatch, self.boundary)
  }

  def __repr__(self) = self.toString.encode("utf-8")
  }

  def shortstr(self) = {
    ("Division %s" format self.name) + (
      if (self.level > 1) " (%s)" format (self.path.mkString("/")) else "")
  }

  def struct(self, no_article:Boolean=false) = {
    var foo = mutable.Seq[Any]("Division")
    foo :+= ("name", self.name)
    foo :+= ("path", self.path.mkString("/"))
    if (!no_article) {
      if (self.artmatch)
        foo :+= ("matching", self.artmatch.struct())
      else
        foo :+= ("matching", "none")
    }
    foo :+= ("boundary", self.boundary.struct())
    foo
  }

  def distance_to_coord(self, coord:Coord) = "Unknown"

  def matches_coord(self, coord:Coord) = self contains coord

  // Compute the boundary of the geographic region of this division, based
  // on the points in the region.
  def compute_boundary(self) {
    // Yield up all points that are not "outliers", where outliers are defined
    // as points that are more than Opts.max_dist_for_outliers away from all
    // other points.
    def iter_non_outliers() {
      // If not enough points, just return them; otherwise too much possibility
      // that all of them, or some good ones, will be considered outliers.
      if (self.locs.length <= 5) {
        for (p <- self.locs) yield p
      } else {
        // FIXME: Actually look for outliers.
        for (p <- self.locs) yield p
        //for {
        //  p <- locs
        //  // Find minimum distance to all other points and check it.
        //  mindist = (for (x <- locs if !(x eq p)) yield spheredist(p, x)) min
        //  if (mindist <= Opts.max_dist_for_outliers)
        //} yield p
      }
    }

    if debug("lots")
      errprint("Computing boundary for %s, path %s, num points %s",
               self.name, self.path, self.locs.length)
               
    self.goodlocs = iter_non_outliers()
    // If we've somehow discarded all points, just use the original list
    if (!self.goodlocs.length) {
      if debug("some")
        warning("All points considered outliers?  Division %s, path %s",
                self.name, self.path)
      self.goodlocs = self.locs
    }
    // FIXME! This will fail for a division that crosses the International
    // Date Line.
    val topleft = Coord((for (x <- self.goodlocs) yield x.coord.lat) min,
                        (for (x <- self.goodlocs) yield x.coord.long) min)
    val botright = Coord((for (x <- self.goodlocs) yield x.coord.lat) max,
                         (for (x <- self.goodlocs) yield x.coord.long) max)
    self.boundary = Boundary(topleft, botright)
  }

  def generate_worddist(self) {
    self.worddist = RegionWordDist()
    self.worddist.add_locations([self])
    self.worddist.add_locations(self.goodlocs)
    self.worddist.finish(minimum_word_count=Opts.minimum_word_count)
  }

  def contains(self, coord:Coord) = self.boundary contains coord
  }
}

object Division {
  // Find the division for a point in the division with a given path,
  // add the point to the division.  Create the division if necessary.
  // Return the corresponding Division.
  def find_division_note_point(cls, loc:Location, path:List[String]) = {
    val higherdiv = if (path.length > 1)
      // Also note location in next-higher division.
        cls.find_division_note_point(loc, path[0:-1])
      else null
    // Skip divisions where last element in path is empty; this is a
    // reference to a higher-level division with no corresponding lower-level
    // division.
    if (!path[-1]) higherdiv
    else {
      val division = {
        if (cls.path_to_division contains path)
          cls.path_to_division[path]
        else {
          // If we haven't seen this path, create a new Division object.
          // Record the mapping from path to division, and also from the
          // division's "name" (name of lowest-level division in path) to
          // the division.
          val newdiv = Division(path)
          newdiv.div = higherdiv
          cls.path_to_division[path] = newdiv
          Gazetteer.lower_toponym_to_division[path.last.toLowerCase] :+= newdiv
          newdiv
        }
      }
      division.locs :+= loc
      division
    }
  }

  // Finish all computations related to Divisions, after we've processed
  // all points (and hence all points have been added to the appropriate
  // Divisions).
  def finish_all(cls) {
    val divs_by_area = mutable.Seq[(Division, Double)]()
    for (division <- cls.path_to_division.itervalues()) {
      if (debug("lots")) {
        errprint("Processing division named %s, path %s",
                 division.name, division.path)
      }
      division.compute_boundary()
      val artmatch = ArticleTable.find_match_for_division(division)
      if (artmatch != null) {
        if debug("lots")
          errprint("Matched article %s for division %s, path %s",
                   artmatch, division.name, division.path)
        division.artmatch = artmatch
        artmatch.location = division
      }
      else {
        if debug("lots")
          errprint("Couldn't find match for division %s, path %s",
                   division.name, division.path)
      }
      for (inds <- division.boundary.iter_nonempty_tiling_regions())
        cls.tiling_region_to_divisions[inds] :+= division
      if debug("region")
        divs_by_area :+= (division, division.boundary.square_area())
    }
    if debug("region")
      // sort by second element of tuple, in reverse order
      for ((div, area) <- divs_by_area sortWith(_._2 > _._2))
        errprint("%.2f square miles: %s", area, div)
  }
}

/////////////////////////////////////////////////////////////////////////////
//                             Wikipedia articles                          //
/////////////////////////////////////////////////////////////////////////////

//////////////////////  Article table

// Static class maintaining tables listing all articles and mapping between
// names, ID's and articles.  Objects corresponding to redirect articles
// should not be present anywhere in this table; instead, the name of the
// redirect article should point to the article object for the article
// pointed to by the redirect.
object ArticleTable {
  // Map from short name (lowercased) to list of Wikipedia articles.  The short
  // name for an article is computed from the article's name.  If the article
  // name has a comma, the short name is the part before the comma, e.g. the
  // short name of "Springfield, Ohio" is "Springfield".  If the name has no
  // comma, the short name is the same as the article name.  The idea is that
  // the short name should be the same as one of the toponyms used to refer to
  // the article.
  val short_lower_name_to_articles = seqmap[String]()

  // Map from tuple (NAME, DIV) for Wikipedia articles of the form
  // "Springfield, Ohio", lowercased.
  val lower_name_div_to_articles = seqmap[String]()

  // Mapping from article names to Article objects, using the actual case of
  // the article.
  val name_to_article = {}

  // For each toponym, list of Wikipedia articles matching the name.
  val lower_toponym_to_article = seqmap[String]()

  // Mapping from lowercased article names to Article objects
  val lower_name_to_articles = seqmap[String]()

  // List of articles in each split.
  val articles_by_split = seqmap[String]()

  // Num of articles with word-count information but not in table.
  val num_articles_with_word_counts_but_not_in_table = 0

  // Num of articles with word-count information (whether or not in table).
  val num_articles_with_word_counts = 0

  // Num of articles in each split with word-count information seen.
  val num_word_count_articles_by_split = intmap()

  // Num of articles in each split with a computed distribution.
  // (Not the same as the previous since we don't compute the distribution of articles in
  // either the test or dev set depending on which one is used.)
  val num_dist_articles_by_split = intmap()

  // Total # of word tokens for all articles in each split.
  val word_tokens_by_split = intmap()

  // Total # of incoming links for all articles in each split.
  val incoming_links_by_split = intmap()

  // Look up an article named NAME and return the associated article.
  // Note that article names are case-sensitive but the first letter needs to
  // be capitalized.
  def lookup_article(cls, name:String) = {
    assert(name)
    cls.name_to_article.getOrElse(capfirst(name), null)
  }

  // Record the article as having NAME as one of its names (there may be
  // multiple names, due to redirects).  Also add to related lists mapping
  // lowercased form, short form, etc.
  def record_article_name(cls, name:String, art:Article) {
    // Must pass in properly cased name
    assert(name == capfirst(name))
    cls.name_to_article[name] = art
    val loname = name.toLowerCase
    cls.lower_name_to_articles[loname] :+= art
    (short, div) = compute_short_form(loname)
    if (div != null)
      cls.lower_name_div_to_articles[(short, div)] :+= art
    cls.short_lower_name_to_articles[short] :+= art
    if (art !in cls.lower_toponym_to_article[loname])
      cls.lower_toponym_to_article[loname] :+= art
    if (short != loname && art !in cls.lower_toponym_to_article[short])
      cls.lower_toponym_to_article[short] :+= art
  }

  // Record either a normal article ('artfrom' same as 'artto') or a
  // redirect ('artfrom' redirects to 'artto').
  def record_article(cls, artfrom:Article, artto:Article) {

    cls.record_article_name(artfrom.title, artto)
    val redir = !(artfrom eq artto)
    val split = artto.split
    val fromlinks = artfrom.incoming_links_or_zero
    cls.incoming_links_by_split[split] += fromlinks
    if (!redir) {
      cls.articles_by_split[split] :+= artto
    }
    else if (fromlinks != 0) {
      // Add count of links pointing to a redirect to count of links
      // pointing to the article redirected to, so that the total incoming
      // link count of an article includes any redirects to that article.
      artto.incoming_links = Some(artto.incoming_links_or_zero + fromlinks)
    }
  }

  def finish_article_distributions(cls) {
    // Figure out the value of OVERALL_UNSEEN_MASS for each article.
    for ((split, table) <- cls.articles_by_split.iteritems()) {
      val totaltoks = 0
      val numarts = 0
      for (art <- table) {
        if (art.dist) {
          art.dist.finish(minimum_word_count=Opts.minimum_word_count)
          totaltoks += art.dist.total_tokens
          numarts += 1
        }
      }
      cls.num_dist_articles_by_split[split] = numarts
      cls.word_tokens_by_split[split] = totaltoks
    }
  }

  def clear_training_article_distributions(cls) {
    for (art <- cls.articles_by_split["training"])
      art.dist = null
  }

  // Find Wikipedia article matching name NAME for location LOC.  NAME
  // will generally be one of the names of LOC (either its canonical
  // name or one of the alternate name).  CHECK_MATCH is a function that
  // is passed two aruments, the location and the Wikipedia article,
  // and should return true if the location matches the article.
  // PREFER_MATCH is used when two or more articles match.  It is passed
  // three arguments, the location and two Wikipedia articles.  It
  // should return TRUE if the first is to be preferred to the second.
  // Return the article matched, or None.

  def find_one_wikipedia_match(cls, loc:Location, name:String,
        check_match:(Location, Article)=>Boolean,
        prefer_match(Location, Article, Article)=>Boolean):Article = {

    val loname = name.toLowerCase

    // Look for any articles with same name (case-insensitive) as the location,
    // check for matches
    for (art <- cls.lower_name_to_articles[loname])
      if (check_match(loc, art)) return art

    // Check whether there is a match for an article whose name is
    // a combination of the location's name and one of the divisions that
    // the location is in (e.g. "Augusta, Georgia" for a location named
    // "Augusta" in a second-level division "Georgia").
    if (loc.div) {
      for {div <- loc.div.path
           art <- cls.lower_name_div_to_articles[(loname, div.toLowerCase)]}
        if (check_match(loc, art)) return art
      }
    }

    // See if there is a match with any of the articles whose short name
    // is the same as the location's name
    val arts = cls.short_lower_name_to_articles[loname]
    if (arts != null) {
      val goodarts = (for (art <- arts if check_match(loc, art)) yield art)
      if (goodarts.length == 1)
        return goodarts[0] // One match
      else if (goodarts.length > 1) {
        // Multiple matches: Sort by preference, return most preferred one
        if debug("lots")
          errprint("Warning: Saw %s toponym matches: %s",
                   goodarts.length, goodarts)
        val sortedarts = goodarts sortWith (prefer_match(loc,_,_))
        return sortedarts[0]
      }
    }

    // No match.
    return null
  }

  // Find Wikipedia article matching location LOC.  CHECK_MATCH and
  // PREFER_MATCH are as above.  Return the article matched, or None.

  def find_wikipedia_match(cls, loc:Location,
        check_match:(Location, String)=>Boolean,
        prefer_match(Location, String, String)=>Boolean):Article = {
    // Try to find a match for the canonical name of the location
    val artmatch = cls.find_one_wikipedia_match(loc, loc.name, check_match,
                                                prefer_match)
    if (artmatch != null) return artmatch

    // No match; try each of the alternate names in turn.
    for (altname <- loc.altnames) {
      val artmatch2 = cls.find_one_wikipedia_match(loc, altname, check_match,
                                                   prefer_match)
      if (artmatch2 != null) return artmatch2
    }

    // No match.
    return null
  }

  // Find Wikipedia article matching locality LOC; the two coordinates must
  // be at most MAXDIST away from each other.

  def find_match_for_locality(cls, loc:Location, maxdist:Double) = {

    def check_match(loc:Location, art:Article) = {
      val dist = spheredist(loc.coord, art.coord)
      if (dist <= maxdist) true
      else {
        if debug("lots")
          errprint("Found article %s but dist %s > %s",
                   art, dist, maxdist)
        false
      }
    }

    def prefer_match(loc:Location, art1:Article, art2:Article) = {
      spheredist(loc.coord, art1.coord) < spheredist(loc.coord, art2.coord)
    }

    cls.find_wikipedia_match(loc, check_match, prefer_match)
  }

  // Find Wikipedia article matching division LOC; the article coordinate
  // must be inside of the division's boundaries.

  def find_match_for_division(cls, loc:Location) = {

    def check_match(loc:Location, art:Article) = {
      if (art.coord && loc contains art.coord) true
      else {
        if debug("lots")
          if (!art.coord) {
            errprint("Found article %s but no coordinate, so not in location named %s, path %s",
                     art, loc.name, loc.path)
          }
          else {
            errprint("Found article %s but not in location named %s, path %s",
                     art, loc.name, loc.path)
          }
        false
      }
    }

    def prefer_match(loc, art1:Article, art2:Article) = {
      val l1 = art1.incoming_links
      val l2 = art2.incoming_links
      // Prefer according to incoming link counts, if that info is available
      if (l1 != null) l1 > l2
      else {
        // FIXME: Do something smart here -- maybe check that location is farther
        // in the middle of the bounding box (does this even make sense???)
        true
      }
    }

    cls.find_wikipedia_match(loc, check_match, prefer_match)
    }
  }
}


///////////////////////// Articles

// A Wikipedia article for geotagging.  Defined fields, in addition to those
// of the base classes:
//
//   dist: Object containing word distribution of this article.
//   location: Corresponding location for this article.
//   stat_region: StatRegion object corresponding to this article.

class StatArticle(params:Map[String,String]) extends Article(params) {

  var location:Location = null
  var stat_region:StatRegion = null
  val dist:WordDist = null

  def toString(self) = {
    var coordstr = " at %s" format self.coord if self.coord else ""
    if (self.location) {
      coordstr += (", matching location %s" format
                   self.location.toString(no_article=true))
    }
    val redirstr = ", redirect to %s" format self.redir if self.redir else ""
    val divs = self.find_covering_divisions()
    val top_divs = (for (div <- divs if div.level == 1) yield div.toString(no_article=true))
    var topdivstr =
      if (top_divs != null)
        ", in top-level divisions %s" format (top_divs.mkString(", "))
      else
        ", not in any top-level divisions"
    "%s(%s)%s%s%s" format (self.title, self.id, coordstr, redirstr, topdivstr)
  }

  def __repr__(self) = "Article(%s)" format self.toString.encode("utf-8")

  def shortstr(self) = {
    var str = "%s" format self.title
    if (self.location)
      str += ", matching %s" format self.location.shortstr()
    val divs = self.find_covering_divisions()
    val top_divs = (for (div <- divs if div.level == 1) yield div.name)
    if (top_divs != null)
      str += ", in top-level divisions %s" format (top_divs.mkString(", "))
    str
  }

  def struct(self) = {
    var foo = mutable.Seq[Any]("StatArticle")
    foo :+= ("title", self.title)
    foo :+= ("id", self.id)
    if (self.coord)
      foo :+= ("location", self.coord)
    if (self.location)
      foo :+= ("matching", self.location.struct(no_article=true))
    if (self.redir)
      foo :+= ("redirect to", self.redir.struct())
    val divs = self.find_covering_divisions()
    val top_divs = (for (div <- divs if div.level == 1) yield div.struct(no_article=true))
    if (top_divs != null)
      foo :+= ("top level divisions", top_divs)
    else
      foo :+= ("top level divisions", "none")
    foo
  }

  def distance_to_coord(self, coord:Coord) = spheredist(self.coord, coord)

  def matches_coord(self, coord:Coord) = {
    if (self.distance_to_coord(coord) <= Opts.max_dist_for_close_match) true
    if (self.location && (self.location.isInstanceOf[Division]) &&
        self.location.matches_coord(coord)) true
    false
  }

  // Determine the region word-distribution object for a given article:
  // Create and populate one if necessary.
  def find_regworddist(self):WordDist = {
    val loc = self.location
    if (loc && loc.isInstanceOf[Division]) {
      if (!loc.worddist)
        loc.generate_worddist()
      return loc.worddist
    }
    if (!self.stat_region)
      self.stat_region = StatRegion.find_region_for_coord(self.coord)
    return self.stat_region.worddist
  }

  // Find the divisions that cover the given article.
  def find_covering_divisions(self) = {
    val inds = coord_to_tiling_region_indices(self.coord)
    val divs = Division.tiling_region_to_divisions[inds]
    (for (div in divs if self.coord <- div) yield div)
  }
}


/////////////////////////////////////////////////////////////////////////////
//                             Accumulate results                          //
/////////////////////////////////////////////////////////////////////////////

// incorrect_reasons is a map from ID's for reasons to strings describing
// them.
class Eval(incorrect_reasons:Map[String,String]) {
  // Statistics on the types of instances processed
  // Total number of instances
  var self.total_instances = 0
  var self.correct_instances = 0
  var self.incorrect_instances = 0
  val self.other_stats = intmap()
  // Map from reason ID's to counts
  var results = Map[String,Int]()
  for ((attrname, engname) <- self.incorrect_reasons)
    results(attrname) = 0
  
  def record_result(self, correct:Boolean, reason:String=null) {
    self.total_instances += 1
    if (correct)
      self.correct_instances += 1
    else {
      self.incorrect_instances += 1
      if (reason != null)
        results[reason] += 1
    }
  }

  def record_other_stat(self, othertype:String) {
    self.other_stats[othertype] += 1
  }

  def output_fraction(self, header:String, amount:Int, total:Int) {
    if (amount > total) {
      warning("Something wrong: Fractional quantity %s greater than total %s"
              , (amount, total))
    }
    if (total == 0)
      var percent = "indeterminate percent"
    else
      percent = "%5.2f%%" format (100*amount.toDouble/total)
    errprint("%s = %s/%s = %s", header, amount, total, percent)
  }

  def output_correct_results(self) {
    self.output_fraction("Percent correct", self.correct_instances,
                         self.total_instances)
  }

  def output_incorrect_results(self) {
    self.output_fraction("Percent incorrect", self.incorrect_instances,
                         self.total_instances)
    for ((reason, descr) <- self.incorrect_reasons) {
      self.output_fraction("  %s" format descr, getattr(self, reason),
                           self.total_instances)
    }
  }

  def output_other_stats(self) {
    for ((ty, count) <- self.other_stats.iteritems())
      errprint("%s = %s", ty, count)
  }

  def output_results(self) {
    if (!self.total_instances) {
      warning("Strange, no instances found at all; perhaps --eval-format is incorrect?")
      return
    }
    errprint("Number of instances = %s", self.total_instances)
    self.output_correct_results()
    self.output_incorrect_results()
    self.output_other_stats()
  }
}

class EvalWithCandidateList(incorrect_reasons:Map[String,String],
  max_individual_candidates:Int=5) extends Eval(incorrect_reasons) {
  // Toponyms by number of candidates available
  val self.total_instances_by_num_candidates = intmap()
  val self.correct_instances_by_num_candidates = intmap()
  val self.incorrect_instances_by_num_candidates = intmap()

  def record_result(self, correct:Boolean, reason:String, num_candidates:Int) {
    super(EvalWithCandidateList, self).record_result(correct, reason)
    self.total_instances_by_num_candidates[num_candidates] += 1
    if (correct)
      self.correct_instances_by_num_candidates[num_candidates] += 1
    else
      self.incorrect_instances_by_num_candidates[num_candidates] += 1
  }

  def output_table_by_num_candidates(self, table:Map[Int,Int], total:Int) {
    for (i <- 0 to self.max_individual_candidates)
      self.output_fraction("  With %d  candidates" format i, table[i], total)
    val items = sum(val for key, val in table.iteritems()
                if key > self.max_individual_candidates)
    self.output_fraction("  With %d+ candidates" format
                           (1+self.max_individual_candidates),
                         items, total)
  }

  def output_correct_results(self) {
    super(EvalWithCandidateList, self).output_correct_results()
    self.output_table_by_num_candidates(
      self.correct_instances_by_num_candidates, self.correct_instances)
  }

  def output_incorrect_results(self) {
    super(EvalWithCandidateList, self).output_incorrect_results()
    self.output_table_by_num_candidates(
      self.incorrect_instances_by_num_candidates, self.incorrect_instances)
  }
}

class EvalWithRank(max_rank_for_credit:Int=10)
    extends Eval(Map[String,String]()) {
  val self.max_rank_for_credit = max_rank_for_credit
  val self.incorrect_by_exact_rank = intmap()
  val self.correct_by_up_to_rank = intmap()
  var self.incorrect_past_max_rank = 0
  var self.total_credit = 0
  
  def record_result(self, rank:Int) {
    assert(rank >= 1)
    val correct = rank == 1
    super(EvalWithRank, self).record_result(correct, reason=null)
    if (rank <= self.max_rank_for_credit) {
      self.total_credit += self.max_rank_for_credit + 1 - rank
      self.incorrect_by_exact_rank[rank] += 1
      for (i <- rank to self.max_rank_for_credit)
        self.correct_by_up_to_rank[i] += 1
    }
    else
      self.incorrect_past_max_rank += 1
  }

  def output_correct_results(self) {
    super(EvalWithRank, self).output_correct_results()
    val possible_credit = self.max_rank_for_credit*self.total_instances
    self.output_fraction("Percent correct with partial credit",
                         self.total_credit, possible_credit)
    for (i <- 2 to self.max_rank_for_credit) {
      self.output_fraction("  Correct is at or above rank %s" format i,
                           self.correct_by_up_to_rank[i], self.total_instances)
    }
  }

  def output_incorrect_results(self) {
    super(EvalWithRank, self).output_incorrect_results()
    for (i <- 2 to self.max_rank_for_credit) {
      self.output_fraction("  Incorrect, with correct at rank %s" format i,
                           self.incorrect_by_exact_rank[i],
                           self.total_instances)
    }
    self.output_fraction("  Incorrect, with correct not in top %s" format
                           self.max_rank_for_credit,
                           self.incorrect_past_max_rank, self.total_instances)
  }
}

class GeotagDocumentEval(max_rank_for_credit:Int=10)
    extends EvalWithRank(max_rank_for_credit) {
  val self.true_dists = mutable.Seq[Double]()
  val self.degree_dists = mutable.Seq[Double]()

  def record_result(self, rank:Int, true_dist:Double, degree_dist:Double) {
    super(GeotagDocumentEval, self).record_result(rank)
    self.true_dists :+= true_dist
    self.degree_dists :+= degree_dist
  }

  def output_incorrect_results(self) {
    super(GeotagDocumentEval, self).output_incorrect_results()
    self.true_dists.sort()
    self.degree_dists.sort()
    def miles_and_km(miledist:Double) = {
      val km_per_mile = 1.609
      "%.2f miles (%.2f km)" format (miledist, miledist*km_per_mile)
    }
    errprint("  Mean true error distance = %s",
             miles_and_km(mean(self.true_dists)))
    errprint("  Median true error distance = %s",
             miles_and_km(median(self.true_dists)))
    errprint("  Mean degree error distance = %.2f degrees",
             mean(self.degree_dists))
    errprint("  Median degree error distance = %.2f degrees",
             median(self.degree_dists))
  }
}

object GeotagToponymResults {
  val incorrect_geotag_toponym_reasons = Seq(
    ("incorrect_with_no_candidates",
     "Incorrect, with no candidates"),
    ("incorrect_with_no_correct_candidates",
     "Incorrect, with candidates but no correct candidates"),
    ("incorrect_with_multiple_correct_candidates",
     "Incorrect, with multiple correct candidates"),
    ("incorrect_one_correct_candidate_missing_link_info",
     "Incorrect, with one correct candidate, but link info missing"),
    ("incorrect_one_correct_candidate",
     "Incorrect, with one correct candidate"),
  )
}

//////// Results for geotagging toponyms
class GeotagToponymResults {

  // Overall statistics
  val self.all_toponym = EvalWithCandidateList(self.incorrect_geotag_toponym_reasons) 
  // Statistics when toponym not same as true name of location
  val self.diff_surface = EvalWithCandidateList(self.incorrect_geotag_toponym_reasons)
  // Statistics when toponym not same as true name or short form of location
  val self.diff_short = EvalWithCandidateList(self.incorrect_geotag_toponym_reasons)

  def record_geotag_toponym_result(self, correct:Boolean, toponym:String,
                                   trueloc:String, reason:String,
                                   num_candidates:Int) {
    self.all_toponym.record_result(correct, reason, num_candidates)
    if (toponym != trueloc) {
      self.diff_surface.record_result(correct, reason, num_candidates)
      (short, div) = compute_short_form(trueloc)
      if (toponym != short)
        self.diff_short.record_result(correct, reason, num_candidates)
    }
  }

  def output_geotag_toponym_results(self) {
    errprint("Results for all toponyms:")
    self.all_toponym.output_results()
    errprint("")
    errprint("Results for toponyms when different from true location name:")
    self.diff_surface.output_results()
    errprint("")
    errprint("Results for toponyms when different from either true location name")
    errprint("  or its short form:")
    self.diff_short.output_results()
    output_resource_usage()
  }
}

//////// Results for geotagging documents/articles
object GeotagDocumentResults {
}

class GeotagDocumentResults {

  val self.all_document = GeotagDocumentEval()
  
  // naitr = "num articles in true region"
  val self.docs_by_naitr = TableByRange([1, 10, 25, 100], GeotagDocumentEval)
  
  // Results for documents where the location is at a certain distance
  // from the center of the true statistical region.  The key is measured in
  // fractions of a tiling region (determined by 'dist_fraction_increment',
  // e.g. if dist_fraction_increment = 0.25 then values in the range of
  // [0.25, 0.5) go in one bin, [0.5, 0.75) go in another, etc.).  We measure
  // distance is two ways: true distance (in miles or whatever) and "degree
  // distance", as if degrees were a constant length both latitudinally
  // and longitudinally.
  val self.dist_fraction_increment = 0.25
  val self.docs_by_degree_dist_to_true_center = collections.defaultdict(GeotagDocumentEval)
  val self.docs_by_true_dist_to_true_center = collections.defaultdict(GeotagDocumentEval)
  
  // Similar, but distance between location and center of top predicted
  // region.
  val self.dist_fractions_for_error_dist = Seq(
        0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4, 6, 8,
        12, 16, 24, 32, 48, 64, 96, 128, 192, 256,
        // We're never going to see these
        384, 512, 768, 1024, 1536, 2048)
  val self.docs_by_degree_dist_to_pred_center = TableByRange(self.dist_fractions_for_error_dist, GeotagDocumentEval)
  val self.docs_by_true_dist_to_pred_center = TableByRange(self.dist_fractions_for_error_dist, GeotagDocumentEval)
  
  def record_geotag_document_result(self, rank:Int, coord:Coord,
                                    pred_latind:Int, pred_longind:Int,
                                    num_arts_in_true_region:Int,
                                    return_stats:Boolean=false) = {
    def degree_dist(c1:Coord, c2:Coord) = {
      sqrt((c1.lat - c2.lat)**2 + (c1.long - c2.long)**2)
    }

    val pred_center = stat_region_indices_to_center_coord(pred_latind, pred_longind)
    val pred_truedist = spheredist(coord, pred_center)
    val pred_degdist = degree_dist(coord, pred_center)

    self.all_document.record_result(rank, pred_truedist, pred_degdist)
    val naitr = self.docs_by_naitr.get_collector(rank)
    naitr.record_result(rank, pred_truedist, pred_degdist)

    val (true_latind, true_longind) = coord_to_stat_region_indices(coord)
    val true_center = stat_region_indices_to_center_coord(true_latind, true_longind)
    val true_truedist = spheredist(coord, true_center)
    val true_degdist = degree_dist(coord, true_center)
    val fracinc = self.dist_fraction_increment
    val rounded_true_truedist = fracinc * floor(true_truedist / fracinc)
    val rounded_true_degdist = fracinc * floor(true_degdist / fracinc)

    self.docs_by_true_dist_to_true_center[rounded_true_truedist].
      record_result(rank, pred_truedist, pred_degdist)
    self.docs_by_degree_dist_to_true_center[rounded_true_degdist].
      record_result(rank, pred_truedist, pred_degdist)

    self.docs_by_true_dist_to_pred_center.get_collector(pred_truedist).
      record_result(rank, pred_truedist, pred_degdist)
    self.docs_by_degree_dist_to_pred_center.get_collector(pred_degdist).
      record_result(rank, pred_truedist, pred_degdist)

    if (return_stats) {
      Map("pred_center"->pred_center,
          "pred_truedist"->pred_truedist,
          "pred_degdist"->pred_degdist,
          "true_center"->true_center,
          "true_truedist"->true_truedist,
          "true_degdist"->true_degdist)
    } else Map[String, Double]()
  }

  def record_geotag_document_other_stat(self, othertype:String) {
    self.all_document.record_other_stat(othertype)
  }

  def output_geotag_document_results(self, all_results:Boolean=false) {
    errprint("")
    errprint("Results for all documents/articles:")
    self.all_document.output_results()
    //if (all_results)
    if (false) {
      errprint("")
      for ((lower, upper, obj) <- self.docs_by_naitr.iter_ranges()) {
        errprint("")
        errprint("Results for documents/articles where number of articles")
        errprint("  in true region is in the range [%s,%s]:",
                 lower, upper - 1 if type(upper) is int else upper)
        obj.output_results()
      }
      errprint("")
      for ((truedist, obj) <-
             self.docs_by_true_dist_to_true_center sortBy (_._1)) {
        val lowrange = truedist * Opts.miles_per_region
        val highrange = ((truedist + self.dist_fraction_increment) *
                         Opts.miles_per_region)
        errprint("")
        errprint("Results for documents/articles where distance to center")
        errprint("  of true region in miles is in the range [%.2f,%.2f):",
                 lowrange, highrange)
        obj.output_results()
      }
      errprint("")
      for ((degdist, obj) <-
             self.docs_by_degree_dist_to_true_center sortBy (_._1)) {
        val lowrange = degdist * degrees_per_region
        val highrange = ((degdist + self.dist_fraction_increment) *
                         degrees_per_region)
        errprint("")
        errprint("Results for documents/articles where distance to center")
        errprint("  of true region in degrees is in the range [%.2f,%.2f):",
                 lowrange, highrange)
        obj.output_results()
      }
    }
    // FIXME: Output median and mean of true and degree error dists; also
    // maybe move this info info EvalByRank so that we can output the values
    // for each category
    errprint("")
    output_resource_usage()
  }
}

/////////////////////////////////////////////////////////////////////////////
//                             Main geotagging code                        //
/////////////////////////////////////////////////////////////////////////////

object Toponym {
  // Construct the list of possible candidate articles for a given toponym
  def construct_candidates(toponym:String) = {
    val lotop = toponym.toLowerCase
    val articles = ArticleTable.lower_toponym_to_article[lotop]
    val locs = (Gazetteer.lower_toponym_to_location[lotop] +
            Gazetteer.lower_toponym_to_division[lotop])
    for (loc <- locs) {
      if (loc.artmatch && loc.artmatch !in articles)
        articles :+= loc.artmatch
    }
    articles
  }

  def word_is_toponym(word:String) = {
    word = word.toLowerCase
    (ArticleTable.lower_toponym_to_article contains word ||
     Gazetteer.lower_toponym_to_location contains word ||
     Gazetteer.lower_toponym_to_division contains word)
  }
}


// Class of word in a file containing toponyms.  Fields:
//
//   word: The identity of the word.
//   is_stop: true if it is a stopword.
//   is_toponym: true if it is a toponym.
//   coord: For a toponym with specified ground-truth coordinate, the
//          coordinate.  Else, none.
//   location: true location if given, else None.
//   context: Vector including the word and 10 words on other side.
//   document: The document (article, etc.) of the word.  Useful when a single
//             file contains multiple such documents.
//
class GeogWord(val word:String) {
  var self.is_stop = false
  var self.is_toponym = false
  var self.coord = null
  var self.location = null
  var self.context = null
  var self.document = null
}

// Abstract class for reading documents from a test file and evaluating on
// them.
abstract class TestFileEvaluator(stratname:String) {
  var self.documents_processed = 0

  type Document

  // Return an Iterable listing the documents retrievable from the given
  // filename.
  def iter_documents(self, filename:String):Iterable[Document]

  // Return true if document would be skipped; false if processed and
  // evaluated.
  def would_skip_document(self, doc:Document, doctag:String) = false

  // Return true if document was actually processed and evaluated; false
  // if skipped.
  def evaluate_document(self, doc:Document, doctag:String):Boolean

  // Output results so far.  If 'isfinal', this is the last call, so
  // output more results.
  def output_results(self, isfinal:Boolean=false):Unit

  def evaluate_and_output_results(self, files:Iterable[String]) {
    def output_final_results() {
      errprint("")
      errprint("Final results for strategy %s: All %d documents processed:",
               self.stratname, status.num_processed())
      errprint("Ending operation at %s", time.ctime())
      self.output_results(isfinal=true)
      errprint("Ending final results for strategy %s", self.stratname)
    }

    val status = StatusMessage("document")
    var last_elapsed = 0
    var last_processed = 0
    val skip_initial = Opts.skip_initial_test_docs
    var skip_n = 0
    for (filename <- files) {
      errprint("Processing evaluation file %s...", filename)
      for (doc <- self.iter_documents(filename)) {
        // errprint("Processing document: %s", doc)
        val num_processed = status.num_processed()
        val doctag = "#%d" format (1+num_processed)
        if (self.would_skip_document(doc, doctag))
          errprint("Skipped document %s", doc)
        else {
          var do_skip = false
          if (skip_initial != 0) {
            skip_initial -= 1
            do_skip = true
          }
          else if (skip_n != 0) {
            skip_n -= 1
            do_skip = true
          }
          else
            skip_n = Opts.every_nth_test_doc - 1
          if (do_skip)
            errprint("Passed over document %s", doctag)
          else
            assert(self.evaluate_document(doc, doctag))
          status.item_processed()
          val new_elapsed = status.elapsed_time()
          val new_processed = status.num_processed()

          // If max # of docs reached, stop
          if ((Opts.num_test_docs &&
              new_processed >= Opts.num_test_docs)) {
            errprint("")
            errprint("Finishing evaluation after %d documents",
                new_processed)
            output_final_results()
            return
          }

          // If five minutes and ten documents have gone by, print out results
          if ((new_elapsed - last_elapsed >= 300 &&
              new_processed - last_processed >= 10)) {
            errprint("Results after %d documents (strategy %s):",
                status.num_processed(), self.stratname)
            self.output_results(isfinal=false)
            errprint("End of results after %d documents (strategy %s):",
                status.num_processed(), self.stratname)
            last_elapsed = new_elapsed
            last_processed = new_processed
          }
        }
      }
    }

    output_final_results()
  }
}
  
abstract class GeotagToponymStrategy {
  def need_context(self)
  def compute_score(self, geogword:String, art:Article)
}

// Find each toponym explicitly mentioned as such and disambiguate it
// (find the correct geographic location) using the "link baseline", i.e.
// use the location with the highest number of incoming links.
class BaselineGeotagToponymStrategy extends GeotagToponymStrategy {
  val self.baseline_strategy = baseline_strategy

  def need_context(self) = false

  def compute_score(self, geogword:GeogWord, art:Article) = {
    if (self.baseline_strategy == "internal-link") {
      if (Opts.context_type == "region")
        art.find_regworddist().get_adjusted_incoming_links
      else
        art.get_adjusted_incoming_links
    }
    else if (self.baseline_strategy == "num-articles") {
      if (Opts.context_type == "region")
        art.find_regworddist().num_arts_for_links
      else {
        val location = art.location
        if (location.isInstanceOf[Division])
          location.locs.length
        else
          1
      }
    }
    else random.random()
  }
}

// Find each toponym explicitly mentioned as such and disambiguate it
// (find the correct geographic location) using Naive Bayes, possibly
// in conjunction with the baseline.
class NaiveBayesToponymStrategy extends GeotagToponymStrategy {
  val self.use_baseline = use_baseline

  def need_context(self) = true

  def compute_score(self, geogword:GeogWord, art:Article) = {
    // FIXME FIXME!!! We are assuming that the baseline is "internal-link",
    // regardless of its actual settings.
    val thislinks = art.get_adjusted_incoming_links

    if (Opts.context_type == "article")
      var distobj = art.dist
    else
      distobj = art.find_regworddist()
    val totalprob = 0.0
    val total_word_weight = 0.0
    if (!self.use_baseline) {
      var word_weight = 1.0
      var baseline_weight = 0.0
    }
    else if (Opts.naive_bayes_weighting == "equal") {
      word_weight = 1.0
      baseline_weight = 1.0
    }
    else {
      baseline_weight = Opts.baseline_weight
      word_weight = 1 - baseline_weight
    }
    for ((dist, word) <- geogword.context) {
      if (!Opts.preserve_case_words) word = word.toLowerCase
      val wordprob = distobj.lookup_word(word)

      // Compute weight for each word, based on distance from toponym
      val thisweight =
        if (Opts.naive_bayes_weighting == "equal" ||
            Opts.naive_bayes_weighting == "equal-words") 1.0
        else 1.0/(1+dist)

      total_word_weight += thisweight
      totalprob += thisweight*log(wordprob)
    }
    if debug("some")
      errprint("Computed total word log-likelihood as %s", totalprob)
    // Normalize probability according to the total word weight
    if (total_word_weight > 0)
      totalprob /= total_word_weight
    // Combine word and prior (baseline) probability acccording to their
    // relative weights
    totalprob *= word_weight
    totalprob += baseline_weight*log(thislinks)
    if debug("some")
      errprint("Computed total log-likelihood as %s", totalprob)
    totalprob
  }

  def need_context(self) = true

  def compute_score(geogword:GeogWord, art:Article) =
    art.get_adjusted_incoming_links
}

abstract class GeotagToponymEvaluator(strategy:GeotagToponymStrategy,
    stratname:String) extends TestFileEvaluator(stratname) {
  val self.results = GeotagToponymResults()
  
  type Document = Iterable[GeogWord]

  // Given an evaluation file, read in the words specified, including the
  // toponyms.  Mark each word with the "document" (e.g. article) that it's
  // within.
  def iter_geogwords(self, filename:String)

  // Retrieve the words yielded by iter_geowords() and separate by "document"
  // (e.g. article); yield each "document" as a list of such GeogWord objects.
  // If self.compute_context, also generate the set of "context" words used for
  // disambiguation (some window, e.g. size 20, of words around each
  // toponym).
  def iter_documents(self, filename:String) {
    def return_word(word) = {
      if (word.is_toponym) {
        if debug("lots")
          errprint("Saw loc %s with true coordinates %s, true location %s",
                   word.word, word.coord, word.location)
      }
      else {
        if debug("tons")
          errprint("Non-toponym %s", word.word)
      }
      word
    }

    for ((k, g) <- self.iter_geogwords(filename).groupBy(_.document)) yield {
      if (k != null)
        errprint("Processing document %s...", k)
      val results = (for (word <- g) yield return_word(word))

      // Now compute context for words
      val nbcl = Opts.naive_bayes_context_len
      if (self.strategy.need_context()) {
        // First determine whether each word is a stopword
        for (i <- 0 until results.length) {
          // If a word tagged as a toponym is homonymous with a stopword, it
          // still isn't a stopword.
          results[i].is_stop = (!results[i].coord &&
                                stopwords contains results[i].word)
        }
        // Now generate context for toponyms
        for (i <- 0 until results.length) {
          if (results[i].coord) {
            // Select up to naive_bayes_context_len words on either side;
            // skip stopwords.  Associate each word with the distance away from
            // the toponym.
            val minind = 0 max i-nbcl
            val maxind = results.length min i+nbcl+1
            results[i].context =
               for {
                 (dist, x) <-
                  ((i-minind until i-maxind) zip results.slice(minind, maxind))
                 if (!(stopwords contains x.word))
               } yield (dist, x.word)
          }
        }
      }

      (for (word <- results if word.coord) yield word)
    }
  }

  // Disambiguate the toponym, specified in GEOGWORD.  Determine the possible
  // locations that the toponym can map to, and call COMPUTE_SCORE on each one
  // to determine a score.  The best score determines the location considered
  // "correct".  Locations without a matching Wikipedia article are skipped.
  // The location considered "correct" is compared with the actual correct
  // location specified in the toponym, and global variables corresponding to
  // the total number of toponyms processed and number correctly determined are
  // incremented.  Various debugging info is output if 'debug' is set.
  // COMPUTE_SCORE is passed two arguments: GEOGWORD and the location to
  // compute the score of.

  def disambiguate_toponym(self, geogword:GeogWord) {
    val toponym = geogword.word
    val coord = geogword.coord
    if (coord == null) return // If no ground-truth, skip it
    val articles = construct_candidates(toponym)
    var bestscore = -1e308
    var bestart = null
    if (articles == null) {
      if debug("some")
        errprint("Unable to find any possibilities for %s", toponym)
      var correct = false
    }
    else {
      if debug("some")
        errprint("Considering toponym %s, coordinates %s",
                 toponym, coord)
        errprint("For toponym %s, %d possible articles",
                 toponym, articles.length)
      for (art <- articles) {
        if debug("some")
            errprint("Considering article %s", art)
        val thisscore = self.strategy.compute_score(geogword, art)
        if (thisscore > bestscore) {
          bestscore = thisscore
          bestart = art 
        }
      }
      if (bestart != null)
        correct = bestart.matches_coord(coord)
      else
        correct = false
    }

    val num_candidates = articles.length

    if (correct)
      var reason = null
    else {
      if (num_candidates == 0)
        reason = "incorrect_with_no_candidates"
      else {
        val good_arts = (for (art <- articles if art.matches_coord(coord)) yield art)
        if (good_arts == null)
          reason = "incorrect_with_no_correct_candidates"
        else if (good_arts.length > 1)
          reason = "incorrect_with_multiple_correct_candidates"
        else {
          val goodart = good_arts[0]
          if (goodart.incoming_links == null)
            reason = "incorrect_one_correct_candidate_missing_link_info"
          else
            reason = "incorrect_one_correct_candidate"
        }
      }
    }

    errprint("Eval: Toponym %s (true: %s at %s),"
             , (toponym, geogword.location, coord), nonl=true)
    if (correct)
      errprint("correct")
    else
      errprint("incorrect, reason = %s", reason)

    self.results.record_geotag_toponym_result(correct, toponym,
        geogword.location, reason, num_candidates)

    if (debug("some") && bestart) {
      errprint("Best article = %s, score = %s, dist = %s, correct %s"
               , (bestart, bestscore, bestart.distance_to_coord(coord),
                  correct))
    }
  }

  def evaluate_document(self, doc:Iterable[GeogWord], doctag:String) = {
    for (geogword <- doc)
       self.disambiguate_toponym(geogword)
    true
  }

  def output_results(self, isfinal:Boolean=false) {
    self.results.output_geotag_toponym_results()
  }
} 

class TRCoNLLGeotagToponymEvaluator(strategy:GeotagToponymStrategy,
    stratname:String) extends GeotagToponymEvaluator(strategy, stratname) {
  // Read a file formatted in TR-CONLL text format (.tr files).  An example of
  // how such files are fomatted is:
  //
  //...
  //...
  //last    O       I-NP    JJ
  //week    O       I-NP    NN
  //&equo;s O       B-NP    POS
  //U.N.    I-ORG   I-NP    NNP
  //Security        I-ORG   I-NP    NNP
  //Council I-ORG   I-NP    NNP
  //resolution      O       I-NP    NN
  //threatening     O       I-VP    VBG
  //a       O       I-NP    DT
  //ban     O       I-NP    NN
  //on      O       I-PP    IN
  //Sudanese        I-MISC  I-NP    NNP
  //flights O       I-NP    NNS
  //abroad  O       I-ADVP  RB
  //if      O       I-SBAR  IN
  //Khartoum        LOC
  //        >c1     NGA     15.5833333      32.5333333      Khartoum > Al Khar<BA>om > Sudan
  //        c2      NGA     -17.8833333     30.1166667      Khartoum > Zimbabwe
  //        c3      NGA     15.5880556      32.5341667      Khartoum > Al Khar<BA>om > Sudan
  //        c4      NGA     15.75   32.5    Khartoum > Al Khar<BA>om > Sudan
  //does    O       I-VP    VBZ
  //not     O       I-NP    RB
  //hand    O       I-NP    NN
  //over    O       I-PP    IN
  //three   O       I-NP    CD
  //men     O       I-NP    NNS
  //...
  //...
  //
  // Yield GeogWord objects, one per word.
  def iter_geogwords(self, filename:String) {
    var in_loc = false
    var wordstruct = null:GeogWord
    for (line <- uchompopen(filename, errors="replace")) {
      try {
        val ss = """\t""".r.split(line)
        require(ss.length == 2)
        val (word, ty) = (ss(0), ss(1))
        if (word != null) {
          if (in_loc) {
            in_loc = false
            yield wordstruct
          }
          wordstruct = GeogWord(word)
          wordstruct.document = filename
          if (ty.startswith("LOC")) {
            in_loc = true
            wordstruct.is_toponym = true
          }
          else
            yield wordstruct
        }
        else if (in_loc && ty[0] == '>') {
          val ss = """\t""".r.split(ty)
          require(splits.length == 5)
          val (lat, long, fulltop) = (ss(2), ss(3), ss(4))
          wordstruct.coord = Coord(lat.toDouble, long.toDouble)
          wordstruct.location = fulltop
        }
      }
      catch {
        case exc:Exception => {
          errprint("Bad line %s", line)
          errprint("Exception is %s", exc)
          exc match {
            case NumberFormatException =>
            case _ => exc.printStackTrace()
          }
        }
      }
    }
    if (in_loc)
      yield wordstruct
  }
}

class WikipediaGeotagToponymEvaluator(strategy:GeotagToponymStrategy,
    stratname:String) extends GeotagToponymEvaluator(strategy, stratname) {
  def iter_geogwords(self, filename:String) {
    var title = null
    for (line <- uchompopen(filename, errors="replace")) {
      if (rematch("Article title: (.*)$", line))
        title = m_[1]
      else if (rematch("Link: (.*)$", line)) {
        val args = m_[1].split('|')
        val trueart = args[0]
        var linkword = trueart
        if (args.length > 1)
          linkword = args[1]
        var word = GeogWord(linkword)
        word.is_toponym = true
        word.location = trueart
        word.document = title
        val art = ArticleTable.lookup_article(trueart)
        if (art != null)
          word.coord = art.coord
        yield word
      }
      else {
        word = GeogWord(line)
        word.document = title
        yield word
      }
    }
  }
}

abstract class GeotagDocumentStrategy {
  def return_ranked_regions(self, worddist:WordDist)
}

class BaselineGeotagDocumentStrategy(baseline_strategy:String)
    extends GeotagDocumentStrategy {
  var self.cached_ranked_mps = null

  def ranked_regions_random(self, worddist:WordDist) = {
    val regions = list(StatRegion.iter_nonempty_regions())
    random.shuffle(regions)
    (for (reg <- regions) yield (reg, 0))
  }

  def ranked_most_popular_regions(self, worddist:WordDist) = {
    if (self.cached_ranked_mps == null) {
      self.cached_ranked_mps = (
        (for (reg <- StatRegion.iter_nonempty_regions)
          yield (reg, if (baseline_strategy == "internal_link")
                         reg.worddist.get_adjusted_incoming_links
                      else reg.worddist.num_arts_for_links))
        sortWith (_._2 > _._2)
      )
    }
    self.cached_ranked_mps
  }

  def ranked_regions_regdist_most_common_toponym(self, worddist:WordDist) = {
    // Look for a toponym, then a proper noun, then any word.
    var maxword = find_most_common_word(worddist,
        word => word != null && word[0].isUpper() && word_is_toponym(word))
    if (maxword == null) {
      maxword = find_most_common_word(worddist,
        word => word != null && word[0].isUpper())
    }
    if (maxword == null)
      maxword = find_most_common_word(worddist, x => true)
    RegionDist.get_region_dist(maxword).get_ranked_regions()
  }

  def ranked_regions_link_most_common_toponym(self, worddist:WordDist) = {
    var maxword = find_most_common_word(worddist,
        word => word != null && word[0].isUpper() && word_is_toponym(word))
    if (maxword == null) {
      maxword = find_most_common_word(worddist,
          word => word_is_toponym(word))
    }
    if debug("commontop")
      errprint("  maxword = %s", maxword)
    val cands = 
      if (maxword != null) construct_candidates(maxword) else Seq[Article]()
    if debug("commontop")
      errprint("  candidates = %s", cands)
    // Sort candidate list by number of incoming links
    val candlinks =
       (for (cand <- cands) yield (cand, cand.get_adjusted_incoming_links)).
         // sort by second element of tuple, in reverse order
         sortWith(_._2 > _._2)
    if debug("commontop")
      errprint("  sorted candidates = %s", candlinks)

    def find_good_regions_for_coord(
        cands:Iterable[Tuple2[Article, Double]]) = {
      for ((cand, links) <- candlinks) {
        val reg = StatRegion.find_region_for_coord(cand.coord)
        if (reg.latind == null)
          errprint("Strange, found no region for candidate %s", cand)
        else
          yield (reg, links)
      }
    }

    // Convert to regions
    val candregs = find_good_regions_for_coord(candlinks)

    if debug("commontop")
      errprint("  region candidates = %s", candregs)

    // Return an iterator over all elements in all the given sequences, omitting
    // elements seen more than once and keeping the order.
    def merge_numbered_sequences_uniquely(seqs:Iterable[A,B]*) = {
      val keys_seen= mutable.Set[A]()
      for {
        seq <- seqs
        (s, vall) <- seq
        if (!keys_seen contains s)
      } yield {
        keys_seen += s
        (s, vall)
      }
    }

    // Append random regions and remove duplicates
    merge_numbered_sequences_uniquely(candregs,
      self.ranked_regions_random(worddist))
  }

  def return_ranked_regions(self, worddist:WordDist) = {
    if (self.baseline_strategy == "link-most-common-toponym")
      self.ranked_regions_link_most_common_toponym(worddist)
    else if (self.baseline_strategy == "regdist-most-common-toponym")
      self.ranked_regions_regdist_most_common_toponym(worddist)
    else if (self.baseline_strategy == "random")
      self.ranked_regions_random(worddist)
    else
      self.ranked_most_popular_regions(worddist)
  }
}

class KLDivergenceStrategy(partial:Boolean=true, symmetric:Boolean=false)
    extends GeotagDocumentStrategy {

  def return_ranked_regions(self, worddist:WordDist) = {
    val article_pq = PriorityQueue()
    for (stat_region <- StatRegion.iter_nonempty_regions(nonempty_word_dist=true)) {
      val inds = (stat_region.latind, stat_region.longind)
      if debug("lots")
        (latind, longind) = inds
        val coord = region_indices_to_coord(latind, longind)
        errprint("Nonempty region at indices %s,%s = coord %s, num_articles = %s"
                 , (latind, longind, coord,
                    stat_region.worddist.num_arts_for_word_dist))

      var kldiv = fast_kl_divergence(worddist, stat_region.worddist,
                                 var partial=self.partial)
      if (self.symmetric) {
        val kldiv2 = fast_kl_divergence(stat_region.worddist, worddist,
                                    partial=self.partial)
        kldiv = (kldiv + kldiv2) / 2.0
      }
      //kldiv = worddist.test_kl_divergence(stat_region.worddist,
      //                           partial=self.partial)
      //errprint("For region %s, KL divergence %.3f", stat_region, kldiv)
      article_pq.add_task(kldiv, stat_region)
    }

    val regions = mutable.Seq[(StatRegion, Double)]()
    while (true) breakable {
      try
        regions :+= article_pq.get_top_priority(return_priority=true)
      catch {
        case IndexOutOfBoundsException => break
      }
    }
    if debug("kldiv")
      // Print out the words that contribute most to the KL divergence, for
      // the top-ranked regions
      val num_contrib_regions = 5
      val num_contrib_words = 25
      errprint("")
      errprint("KL-divergence debugging info:")
      for (i <- 0 until (regions.length min num_contrib_regions)) {
        val (region, kldiv) = regions[i]
        val (kldiv, contribs) = worddist.slow_kl_divergence(region.worddist,
            partial=self.partial, return_contributing_words=true)
        errprint("  At rank #%s, region %s:", i + 1, region)
        errprint("    %30s  %s", "Word", "KL-div contribution")
        errprint("    %s", "-"*50)
        // sort by absolute value of second element of tuple, in reverse order
        val items = (contribs sortWith(abs(_._2) > abs(_._2))).
                      take(num_contrib_words)
        for ((word, contribval) <- items)
          errprint("    %30s  %s", word, contribval)
        errprint("")
      }

    regions
  }
}

// FIXME: Duplicates code from KLDivergenceStrategy

class CosineSimilarityStrategy(smoothed:Boolean=false, partial:Boolean=false)
    extends GeotagDocumentStrategy {

  def return_ranked_regions(self, worddist:WordDist) = {
    val article_pq = PriorityQueue()
    for (stat_region <- StatRegion.iter_nonempty_regions(nonempty_word_dist=true)) {
      val inds = (stat_region.latind, stat_region.longind)
      if debug("lots")
        (latind, longind) = inds
        val coord = region_indices_to_coord(latind, longind)
        errprint("Nonempty region at indices %s,%s = coord %s, num_articles = %s"
                 , (latind, longind, coord,
                    stat_region.worddist.num_arts_for_word_dist))
      if (self.smoothed) {
        var cossim = fast_smoothed_cosine_similarity(worddist, stat_region.worddist,
                                   var partial=self.partial)
      }
      else {
        cossim = fast_cosine_similarity(worddist, stat_region.worddist,
                                   partial=self.partial)
      }
      assert(cossim >= 0.0)
      // Just in case of round-off problems
      assert(cossim <= 1.002)
      cossim = 1.002 - cossim
      article_pq.add_task(cossim, stat_region)
    }

    val regions = mutable.Seq[(StatRegion, Double)]()
    while (true) breakable {
      try
        regions :+= article_pq.get_top_priority(return_priority=true)
      catch {
        case IndexOutOfBoundsException => break
      }
    }

    regions
  }
}

// Return the probability of seeing the given document 
class NaiveBayesDocumentStrategy(use_baseline:Boolean=true)
    extends GeotagDocumentStrategy {

  def return_ranked_regions(self, worddist:WordDist) = {

    // Determine respective weightings
    val (word_weight, baseline_weight) = (
      if (self.use_baseline) {
        if (Opts.naive_bayes_weighting == "equal") (1.0, 1.0)
        else {
          val bw = Opts.baseline_weight.toDouble
          ((1.0 - bw) / worddist.total_tokens, bw)
        }
      } else (1.0, 0.0)
    )

    val regprobs = Map[StatRegion, Double]()
    (for {reg <- StatRegion.iter_nonempty_regions(nonempty_word_dist=true)
          val word_logprob = reg.worddist.get_nbayes_logprob(worddist, Opts)
          val baseline_logprob = log(reg.worddist.num_arts_for_links.toDouble /
                  StatRegion.total_num_arts_for_links)
          val logprob = (word_weight*word_logprob +
                         baseline_weight*baseline_logprob)
         } yield (reg -> logprob)).
       // Scala nonsense: sort on the second element of the tuple (foo._2),
       // reserved (_ > _).
       sortWith(_._2 > _._2)
  }
}

class PerWordRegionDistributionsStrategy extends GeotagDocumentStrategy {
  def return_ranked_regions(self, worddist:WordDist) = {
    val regdist = RegionDist.get_region_dist_for_word_dist(worddist)
    regdist.get_ranked_regions()
  }
}

abstract class GeotagDocumentEvaluator(strategy:GeotagDocumentStrategy,
    stratname:String) extends TestFileEvaluator(stratname) {
  val self.results = GeotagDocumentResults()

  // FIXME: Seems strange to have a static function like this called here
  StatRegion.initialize_regions()

  def output_results(self, isfinal:Boolean=false) {
    self.results.output_geotag_document_results(all_results=isfinal)
  }
}

class WikipediaGeotagDocumentEvaluator(strategy:GeotagDocumentStrategy,
    stratname:String) extends GeotagDocumentEvaluator(strategy, stratname) {

  type Document = Article

  // Debug flags:
  //
  //  gridrank: For the given test article number (starting at 1), output
  //            a grid of the predicted rank for regions around the true
  //            region.  Multiple articles can have the rank output by
  //            specifying this option multiple times, e.g.
  //
  //            --debug 'gridrank=45,gridrank=58'
  //
  //  gridranksize: Size of the grid, in numbers of articles on a side.
  //                This is a single number, and the grid will be a square
  //                centered on the true region.
  register_list_debug_param("gridrank")
  debugint("gridranksize") = "11"

  def iter_documents(self, filename:String) = {
    for (art <- ArticleTable.articles_by_split[Opts.eval_set])
      yield art
  }

    //title = None
    //words = []
    //for line in uchompopen(filename, errors="replace"):
    //  if (rematch("Article title: (.*)$", line))
    //    if (title != null)
    //      yield (title, words)
    //    title = m_[1]
    //    words = []
    //  else if (rematch("Link: (.*)$", line))
    //    args = m_[1].split('|')
    //    trueart = args[0]
    //    linkword = trueart
    //    if (len(args) > 1)
    //      linkword = args[1]
    //    words.append(linkword)
    //  else:
    //    words.append(line)
    //if (title != null)
    //  yield (title, words)

  override def would_skip_document(self, article:Article, doctag:String) = {
    if (article.dist == null) {
      // This can (and does) happen when --max-time-per-stage is set,
      // so that the counts for many articles don't get read in.
      if (Opts.max_time_per_stage == 0 && Opts.num_training_docs == 0)
        warning("Can't evaluate article %s without distribution", article)
      self.results.record_geotag_document_other_stat("Skipped articles")
      true
    }
    false
  }

  def evaluate_document(self, article:Article, doctag:String):Boolean = {
    if (self.would_skip_document(article, doctag))
      return false
    assert(article.dist.finished)
    val (true_latind, true_longind) = coord_to_stat_region_indices(article.coord)
    val true_statreg = StatRegion.find_region_for_coord(article.coord)
    val naitr = true_statreg.worddist.num_arts_for_word_dist
    if debug("lots") || debug("commontop")
      errprint("Evaluating article %s with %s word-dist articles in true region",
               article, naitr)
    val regs = self.strategy.return_ranked_regions(article.dist)
    var rank = 1
    for (reg, val <- regs) breakable {
      if (reg.latind == true_latind && reg.longind == true_longind)
        break
      rank += 1
    }
    else
      rank = 1000000000
    val want_indiv_results = !Opts.no_individual_results
    val stats = self.results.record_geotag_document_result(rank, article.coord,
        regs[0][0].latind, regs[0][0].longind, num_arts_in_true_region=naitr,
        val return_stats=want_indiv_results)
    if (naitr == 0) {
      self.results.record_geotag_document_other_stat(
          "Articles with no training articles in region")
    }
    if (want_indiv_results) {
      errprint("%s:Article %s:", doctag, article)
      errprint("%s:  %d types, %d tokens",
        doctag, article.dist.counts.length, article.dist.total_tokens)
      errprint("%s:  true region at rank: %s", doctag, rank)
      errprint("%s:  true region: %s", doctag, true_statreg)
      for (i <- 0 until 5) {
        errprint("%s:  Predicted region (at rank %s): %s",
            doctag, i+1, regs[i][0])
      }
      errprint("%s:  Distance %.2f miles to true region center at %s",
               doctag, stats["true_truedist"], stats["true_center"])
      errprint("%s:  Distance %.2f miles to predicted region center at %s",
               doctag, stats["pred_truedist"], stats["pred_center"])
      assert(doctag[0] == '#')
      if ((debug("gridrank") || debuglist("gridrank") contains doctag[1)]) {
        val grsize = debugval("gridranksize").toInt
        val min_latind = true_latind - grsize // 2
        val max_latind = min_latind + grsize - 1
        val min_longind = true_longind - grsize // 2
        val max_longind = min_longind + grsize - 1
        val grid = {}
        rank = 1
        for (reg, val <- regs) {
          if ((reg.latind >= min_latind && reg.latind <= max_latind &&
              reg.longind >= min_longind && reg.longind <= max_longind))
            grid[(reg.latind,reg.longind)] = (reg, val, rank)
          rank += 1
        }

        errprint("Grid ranking, gridsize %dx%d", grsize, grsize)
        errprint("NW corner: %s",
            stat_region_indices_to_nw_corner_coord(max_latind, min_longind))
        errprint("SE corner: %s",
            stat_region_indices_to_se_corner_coord(min_latind, max_longind))
        for (doit <- Seq(0, 1)) {
          if (doit == 0)
            errprint("Grid for ranking:")
          else
            errprint("Grid for goodness/distance:")
          for (lat <- fromto(max_latind, min_latind)) {
            for (long <- fromto(min_longind, max_longind)) {
              val regvalrank = grid.getOrElse((lat,long), null)
              if (regvalrank == null)
                errout(" %-8s", "empty")
              else {
                val (reg, vall, rank) = regvalrank
                val showit = if (doit == 0) rank else vall
                if (lat == true_latind && long == true_longind)
                  errout("!%-8.6s", showit)
                else
                  errout(" %-8.6s", showit)
              }
            }
            errout("\n")
          }
        }
      }
    }

    return true
  }
}


class PCLTravelGeotagDocumentEvaluator(strategy:GeotagDocumentStrategy,
    stratname:String) extends GeotagDocumentEvaluator(strategy, stratname) {
  case class TitledDocument(val title:String, val text:String)
  type Document = TitledDocument

  def iter_documents(self, filename:String) = {
    // Find XML nodes using depth-first search.  "match" is either a
    // string (match localName on that string) or a predicate.
    def find_node_dfs(node:Elem, localname:String, artmatch:Elem => Boolean) = {
      if (artmatch == null)
        depth_first_search(node, _.localName == localname, _.childNodes)
      else
        depth_first_search(node, artmatch, _.childNodes)
    }

    val dom = try {
    // On error, just return, so that we don't have problems when called
    // on the whole PCL corpus dir (which includes non-XML files).
      xml.XML.loadFile(filename)
    } catch {
      warning("Unable to parse XML filename: %s", filename)
      null
    }

    if (dom == null) Seq[TitledDocument]()
    else for {
      chapter <- dom \\ "div" if (chapter \ "@type").text == "chapter"
      val (heads, nonheads) =
        chapter.child.partition(_.label == "head")
      val headtext = (for (x <- heads) yield x.text) mkString ""
      val text = (for (x <- nonheads) yield x.text) mkString ""
      //errprint("Head text: %s", headtext)
      //errprint("Non-head text: %s", text)
    } yield TitledDocument(headtext, text)
  }

  def evaluate_document(self, doc:TitledDocument, doctag:String) = {
    val dist = WordDist()
    val the_stopwords =
      if (Opts.include_stopwords_in_article_dists) Set[String]()
      else stopwords
    for (text <- (doc.title, doc.text)) {
      dist.add_words(split_text_into_words(text, ignore_punc=true),
                     ignore_case=!Opts.preserve_case_words,
                     stopwords=the_stopwords)
    }
    dist.finish(minimum_word_count=Opts.minimum_word_count)
    val regs = self.strategy.return_ranked_regions(dist)
    errprint("")
    errprint("Article with title: %s", doc.title)
    val num_regs_to_show = 5
    for ((rank, regval) <- (1 to num_regs_to_show) zip regs) {
      val (reg, vall) = regval
      if debug("struct")
        errprint("  Rank %d, goodness %g:", rank, vall)
        print_structure(reg.struct(), indent=4)
      else
        errprint("  Rank %d, goodness %g: %s", rank, vall, reg.shortstr())
    }
    
    true
  }
}

/////////////////////////////////////////////////////////////////////////////
//                                Segmentation                             //
/////////////////////////////////////////////////////////////////////////////

// General idea: Keep track of best possible segmentations up to a maximum
// number of segments.  Either do it using a maximum number of segmentations
// (e.g. 100 or 1000) or all within a given factor of the best score (the
// "beam width", e.g. 10^-4).  Then given the existing best segmentations,
// we search for new segmentations with more segments by looking at all
// possible ways of segmenting each of the existing best segments, and
// finding the best score for each of these.  This is a slow process -- for
// each segmentation, we have to iterate over all segments, and for each
// segment we have to look at all possible ways of splitting it, and for
// each split we have to look at all assignments of regions to the two
// new segments.  It also seems that we're likely to consider the same
// segmentation multiple times.
//
// In the case of per-word region dists, we can maybe speed things up by
// computing the non-normalized distributions over each paragraph and then
// summing them up as necessary.

/////////////////////////////////////////////////////////////////////////////
//                               Process files                             //
/////////////////////////////////////////////////////////////////////////////

object ProcessFiles {
  // List of stopwords
  var stopwords:Set[String] = null

  // Read in the list of stopwords from the given filename.
  def read_stopwords(filename:String) {
    errprint("Reading stopwords from %s...", filename)
    val stopwords = Source.fromFile(filename).getLines().toSet
  }
  
  def read_article_data(filename:String) {
    val redirects = mutable.Seq[Article]()
  
    def process(art:Article) {
      if (art.namespace != "Main")
        return
      if (art.redir)
        redirects :+= art
      else if (art.coord) {
        ArticleTable.record_article(art, art)
        StatRegion.add_article_to_region(art)
      }
    }
  
    read_article_data_file(filename, process, article_type=StatArticle,
                           val maxtime=Opts.max_time_per_stage)
  
    for (x <- redirects) {
      val redart = ArticleTable.lookup_article(x.redir)
      if (redart != null)
        ArticleTable.record_article(x, redart)
    }
  }
  
  
  // Parse the result of a previous run of --output-counts and generate
  // a unigram distribution for Naive Bayes matching.  We do a simple version
  // of Good-Turing smoothing where we assign probability mass to unseen
  // words equal to the probability mass of all words seen once, and rescale
  // the remaining probabilities accordingly.
  
  def read_word_counts(filename:String) {
  
    // This is basically a one-off debug statement because of the fact that
    // the experiments published in the paper used a word-count file generated
    // using an older algorithm for determining the geotagged coordinate of
    // a Wikipedia article.  We didn't record the corresponding article-data
    // file, so we need a way of regenerating it using the intersection of
    // articles in the article-data file we actually used for the experiments
    // and the word-count file we used.
    if debug("wordcountarts")
      // Change this if you want a different file name
      val wordcountarts_filename = "wordcountarts-combined-article-data.txt"
      val wordcountarts_file = open(wordcountarts_filename, "w")
      // See write_article_data_file() in process_article_data.py
      val outfields = combined_article_data_outfields
      val field_types = get_output_field_types(outfields)
      uniprint(outfields.mkString("\t"), outfile=wordcountarts_file)
  
    def one_article_probs() {
      if (total_tokens == 0) return
      val art = ArticleTable.lookup_article(title)
      if (art == null) {
        warning("Skipping article %s, not in table", title)
        ArticleTable.num_articles_with_word_counts_but_not_in_table += 1
        return
      }
      if debug("wordcountarts")
        art.output_row(wordcountarts_file, outfields, field_types)
      ArticleTable.num_word_count_articles_by_split[art.split] += 1
      // If we are evaluating on the dev set, skip the test set and vice
      // versa, to save memory and avoid contaminating the results.
      if (art.split != "training" && art.split != Opts.eval_set)
        return
      art.dist = WordDist()
      // Don't train on test set
      art.dist.set_word_distribution(total_tokens, wordhash,
          val note_globally=(art.split == "training"))
    }
  
    errprint("Reading word counts from %s...", filename)
    val status = StatusMessage("article")
    var total_tokens = 0
  
    var title = null:String
    // Written this way because there's another line after the for loop,
    // corresponding to the else clause of the Python for loop
    breakable {
      for (line <- Source.fromFile(filename).getLines()) {
        if (line.startsWith("Article title: ")) {
          var m = "Article title: (.*)$".r(line)
          if (title != null)
            one_article_probs()
          // Stop if we've reached the maximum
          if (status.item_processed(maxtime=Opts.max_time_per_stage))
            break
          if ((Opts.num_training_docs &&
              status.num_processed() >= Opts.num_training_docs)) {
            errprint("")
            errprint("Finishing reading word counts after %d documents",
                status.num_processed())
            break
          }
    
          title = m.group(1)
          val wordhash = intmap()
          total_tokens = 0
        }
        else if ((line.startswith("Article coordinates) ") || line.startswith("Article ID: "))
          ()
        else {
          ("(.*) = ([0-9]+)$", line) match {
            case Re(xword, xcount) => {
              var word = xword
              if (!Opts.preserve_case_words) word = word.toLowerCase
              val count = xcount.toInt
              if (!(stopwords contains word) ||
                  Opts.include_stopwords_in_article_dists) {
                total_tokens += count
                wordhash[word] += count
              }
            }
            case _ =>
              warning("Strange line, can't parse: title=%s: line=%s",
                      title, line)
          }
        }
      }
      one_article_probs()
    }
  
    if debug("wordcountarts")
      wordcountarts_file.close()
    errprint("Finished reading distributions from %s articles.", status.num_processed())
    ArticleTable.num_articles_with_word_counts = status.num_processed()
    output_resource_usage()
  }
  
  def finish_word_counts() {
    WordDist.finish_global_distribution()
    ArticleTable.finish_article_distributions()
    errprint("")
    errprint("-------------------------------------------------------------------------")
    errprint("Article count statistics:")
    val total_arts_in_table = 0
    val total_arts_with_word_counts = 0
    val total_arts_with_dists = 0
    for ((split, totaltoks) <- ArticleTable.word_tokens_by_split.iteritems()) {
      errprint("For split '%s':", split)
      val arts_in_table = ArticleTable.articles_by_split[split].length
      val arts_with_word_counts = ArticleTable.num_word_count_articles_by_split[split]
      val arts_with_dists = ArticleTable.num_dist_articles_by_split[split]
      total_arts_in_table += arts_in_table
      total_arts_with_word_counts += arts_with_word_counts
      total_arts_with_dists += arts_with_dists
      errprint("  %s articles in article table", arts_in_table)
      errprint("  %s articles with word counts seen (and in table)", arts_with_word_counts)
      errprint("  %s articles with distribution computed, %s total tokens, %.2f tokens/article"
          , (arts_with_dists, totaltoks,
            // Avoid division by zero
            totaltoks.toDouble/(arts_in_table + 1e-100)))
    }
    errprint("Total: %s articles with word counts seen",
             ArticleTable.num_articles_with_word_counts)
    errprint("Total: %s articles in article table", total_arts_in_table)
    errprint("Total: %s articles with word counts seen but not in article table",
             ArticleTable.num_articles_with_word_counts_but_not_in_table)
    errprint("Total: %s articles with word counts seen (and in table)",
             total_arts_with_word_counts)
    errprint("Total: %s articles with distribution computed",
             total_arts_with_dists)
  }

  // If given a directory, yield all the files in the directory; else just
  // yield the file.
  def iter_directory_files(dir:String) {
    if (os.path.isdir(dir)) 
      for (fname <- os.listdir(dir)) {
        val fullname = os.path.join(dir, fname)
        yield fullname
      }
    else
      yield dir
  }
    
  // Given an evaluation file, count the toponyms seen and add to the global count
  // in toponyms_seen_in_eval_files.
  def count_toponyms_in_file(fname:String) {
    def count_toponyms(geogword:GeogWord) {
      toponyms_seen_in_eval_files[geogword.word.toLowerCase] += 1
    }
    process_eval_file(fname, count_toponyms, compute_context=false,
                      val only_toponyms=true)
  }


object Gazetteer {
  // For each toponym (name of location), value is a list of Locality items,
  // listing gazetteer locations and corresponding matching Wikipedia articles.
  val lower_toponym_to_location = seqmap[String]()

  // For each toponym corresponding to a division higher than a locality,
  // list of divisions with this name.
  val lower_toponym_to_division = seqmap[String]()

  // Table of all toponyms seen in evaluation files, along with how many times
  // seen.  Used to determine when caching of certain toponym-specific values
  // should be done.
  //val toponyms_seen_in_eval_files = intmap()
}

object WorldGazetteer {

  // Find the Wikipedia article matching an entry in the gazetteer.
  // The format of an entry is
  //
  // ID  NAME  ALTNAMES  ORIG-SCRIPT-NAME  TYPE  POPULATION  LAT  LONG  DIV1  DIV2  DIV3
  //
  // where there is a tab character separating each field.  Fields may be empty;
  // but there will still be a tab character separating the field from others.
  //
  // The ALTNAMES specify any alternative names of the location, often including
  // the equivalent of the original name without any accent characters.  If
  // there is more than one alternative name, the possibilities are separated
  // by a comma and a space, e.g. "Dongshi, Dongshih, Tungshih".  The
  // ORIG-SCRIPT-NAME is the name in its original script, if that script is not
  // Latin characters (e.g. names in Russia will be in Cyrillic). (For some
  // reason, names in Chinese characters are listed in the ALTNAMES rather than
  // the ORIG-SCRIPT-NAME.)
  //
  // LAT and LONG specify the latitude and longitude, respectively.  These are
  // given as integer values, where the actual value is found by dividing this
  // integer value by 100.
  //
  // DIV1, DIV2 and DIV3 specify different-level divisions that a location is
  // within, from largest to smallest.  Typically the largest is a country.
  // For locations in the U.S., the next two levels will be state and county,
  // respectively.  Note that such divisions also have corresponding entries
  // in the gazetteer.  However, these entries are somewhat lacking in that
  // (1) no coordinates are given, and (2) only the top-level division (the
  // country) is given, even for third-level divisions (e.g. counties in the
  // U.S.).
  //
  // For localities, add them to the region-map that covers the earth if
  // ADD_TO_REGION_MAP is true.

  def match_world_gazetteer_entry(cls, line:String) {
    // Split on tabs, make sure at least 11 fields present and strip off
    // extra whitespace
    var fields = re.split("""\t""", line.trim) ++ Seq.fill(11)("")
    fields = (for (x <- fields.slice(0, 11)) yield x.trim)
    Seq(id, name, altnames, orig_script_name, typ, population, lat, long,
     div1, div2, div3) = fields

    // Skip places without coordinates
    if (lat == "" || long == "") {
      if debug("lots")
        errprint("Skipping location %s (div %s/%s/%s) without coordinates",
                 name, div1, div2, div3)
      return
    }

    if (lat == "0" && long == "9999") {
      if debug("lots")
        errprint("Skipping location %s (div %s/%s/%s) with bad coordinates",
                 name, div1, div2, div3)
      return
    }

    // Create and populate a Locality object
    val loc = Locality(name, Coord(lat.toInt / 100., long.toInt / 100.),
      ty=typ, altnames=if (altnames != null) re.split(", ", altnames) else null)
    loc.div = Division.find_division_note_point(loc, (div1, div2, div3))
    if debug("lots")
      errprint("Saw location %s (div %s/%s/%s) with coordinates %s",
               loc.name, div1, div2, div3, loc.coord)

    // Record the location.  For each name for the location (its
    // canonical name and all alternates), add the location to the list of
    // locations associated with the name.  Record the name in lowercase
    // for ease in matching.
    for (name <- Seq(loc.name) + loc.altnames) {
      val loname = name.toLowerCase
      if debug("lots")
        errprint("Noting lower_toponym_to_location for toponym %s, canonical name %s"
                 , (name, loc.name))
      cls.lower_toponym_to_location[loname] :+= loc
    }

    // We start out looking for articles whose distance is very close,
    // then widen until we reach Opts.max_dist_for_close_match.
    val maxdist = 5
    while (maxdist <= Opts.max_dist_for_close_match) breakable {
      val artmatch = ArticleTable.find_match_for_locality(loc, maxdist)
      if (artmatch != null) break
      maxdist *= 2
    }

    if (artmatch == null) 
      if debug("lots")
        errprint("Unmatched name %s", loc.name)
      return
    
    // Record the match.
    loc.artmatch = artmatch
    artmatch.location = loc
    if debug("lots")
      errprint("Matched location %s (coord %s) with article %s, dist=%s"
               , (loc.name, loc.coord, artmatch,
                  spheredist(loc.coord, artmatch.coord)))
  }

  // Read in the data from the World gazetteer in FILENAME and find the
  // Wikipedia article matching each entry in the gazetteer.  For localities,
  // add them to the region-map that covers the earth if ADD_TO_REGION_MAP is
  // true.
  def read_world_gazetteer_and_match(cls, filename:String) {
    errprint("Matching gazetteer entries in %s...", filename)
    val status = StatusMessage("gazetteer entry")

    // Match each entry in the gazetteer
    for (line <- uchompopen(filename)) breakable {
      if debug("lots")
        errprint("Processing line: %s", line)
      cls.match_world_gazetteer_entry(line)
      if (status.item_processed(maxtime=Opts.max_time_per_stage))
        break
    }

    Division.finish_all()
    errprint("Finished matching %s gazetteer entries.", status.num_processed())
    output_resource_usage()
  }
}  

/////////////////////////////////////////////////////////////////////////////
//                                  Main code                              //
/////////////////////////////////////////////////////////////////////////////

class WikiDisambigProgram extends NLPProgram {

  def populate_options(self, op:OptionParser) = {
    val canon_options = Map[String,String]()

    //val parser = new OptionParser("scopt")
    //////////// Input files
    //opt("stopwords-file", "<file>", """File containing list of stopwords.""")
    //opt("a", "article-data-file", "<file>",
    //    //action="append",
    //    """File containing info about Wikipedia articles.""")
    //opt("gf", "gazetteer-file", "<file>",
    //    """File containing gazetteer information to match.""")
    //choiceOpt("gt", "gazetteer-type",
    //              //default="world",
    //              var choices=["world", "db"],
    //              help="""Type of gazetteer file specified using --gazetteer;
//default '%default'.""")
    op.add_option("--counts-file", "--cf", action="append",
                  help="""File containing output from a prior run of
--output-counts, listing for each article the words in the article and
associated counts.""",
                  metavar="FILE")
    op.add_option("-e", "--eval-file", "--e",
                  help="""File or directory containing files to evaluate on.
Each file is read in and then disambiguation is performed.""",
                  metavar="FILE")
    op.add_option("-f", "--eval-format", "--f", type="choice",
                  var default="wiki", choices=["tr-conll", "wiki", "raw-text",
                                           "pcl-travel"],
                  help="""Format of evaluation file(s).  Default '%default'.""")
    op.add_option("--eval-set", "--es", type="choice",
                  default="dev", choices=["dev", "devel", "test"],
                  help="""Set to use for evaluation when --eval-format=wiki
and --mode=geotag-documents ('dev' or 'devel' for the development set,
'test' for the test set).  Default '%default'.""")
    canon_options["eval_set"] = {"devel":"dev"}

    /////////// Misc options for handling distributions
    op.add_option("--preserve-case-words", "--pcw", action="store_true",
                  default=false,
                  help="""Don't fold the case of words used to compute and
match against article distributions.  Note that this does not apply to
toponyms; currently, toponyms are always matched case-insensitively.""")
    op.add_option("--include-stopwords-in-article-dists", action="store_true",
                  default=false,
                  help="""Include stopwords when computing word
distributions.""")
    op.add_option("--naive-bayes-context-len", "--nbcl", type="int", default=10,
                  help="""Number of words on either side of a toponym to use
in Naive Bayes matching.  Default %default.""")
    op.add_option("--minimum-word-count", "--mwc", type="int", default=1,
                  help="""Minimum count of words to consider in word
distributions.  Words whose count is less than this value are ignored.""")

    /////////// Misc options for controlling matching
    op.add_option("--max-dist-for-close-match", "--mdcm", type="float",
                  default=80,
                  help="""Maximum number of miles allowed when looking for a
close match.  Default %default.""")
    op.add_option("--max-dist-for-outliers", "--mdo", type="float", default=200,
                  help="""Maximum number of miles allowed between a point and
any others in a division.  Points farther away than this are ignored as
"outliers" (possible errors, etc.).  Default %default.""")

    /////////// Basic options for determining operating mode and strategy
    op.add_option("-m", "--mode", "--m", type="choice",
                  default="geotag-documents",
                  choices=["geotag-toponyms",
                           "geotag-documents",
                           "generate-kml",
                           "segment-geotag-documents"],
                  help="""Action to perform.

'geotag-documents' finds the proper location for each document (or article)
in the test set.

'geotag-toponyms' finds the proper location for each toponym in the test set.
The test set is specified by --eval-file.  Default '%default'.

'segment-geotag-documents' simultaneously segments a document into sections
covering a specific location and determines that location. (Not yet
implemented.)

'generate-kml' generates KML files for some set of words, showing the
distribution over regions that the word determines.  Use '--kml-words' to
specify the words whose distributions should be outputted.  See also
'--kml-prefix' to specify the prefix of the files outputted, and
'--kml-transform' to specify the function to use (if any) to transform
the probabilities to make the distinctions among them more visible.
""")

    op.add_option("-s", "--strategy", "--s", type="choice", action="append", 
        default=null,
        choices=[
          "baseline", "none",
          "full-kl-divergence", "full-kldiv", "full-kl",
          "partial-kl-divergence", "partial-kldiv", "partial-kl",
          "symmetric-full-kl-divergence", "sym-kldiv", "sym-kl",
          "symmetric-partial-kl-divergence", "sym-partial-kldiv",
            "sym-partial-kl",
          "cosine-similarity", "cossim",
          "partial-cosine-similarity", "partial-cossim",
          "smoothed-cosine-similarity", "smoothed-cossim",
          "smoothed-partial-cosine-similarity", "smoothed-partial-cossim",
          "average-cell-probability", "avg-cell-prob", "acp",
          "naive-bayes-with-baseline", "nb-base",
          "naive-bayes-no-baseline", "nb-nobase"
          ],
        help="""Strategy/strategies to use for geotagging.
'baseline' means just use the baseline strategy (see --baseline-strategy).

'none' means don't do any geotagging.  Useful for testing the parts that
read in data and generate internal structures.

The other possible values depend on which mode is in use
(--mode=geotag-toponyms or --mode=geotag-documents).

For geotag-toponyms:

'naive-bayes-with-baseline' (or 'nb-base') means also use the words around the
toponym to be disambiguated, in a Naive-Bayes scheme, using the baseline as the
prior probability; 'naive-bayes-no-baseline' (or 'nb-nobase') means use uniform
prior probability.  Default is 'baseline'.

For geotag-documents:

'full-kl-divergence' (or 'full-kldiv') searches for the region where the KL
divergence between the article and region is smallest.
'partial-kl-divergence' (or 'partial-kldiv') is similar but uses an
abbreviated KL divergence measure that only considers the words seen in the
article; empirically, this appears to work just as well as the full KL
divergence. 'average-cell-probability' (or
'regdist') involves computing, for each word, a probability distribution over
regions using the word distribution of each region, and then combining the
distributions over all words in an article, weighted by the count the word in
the article.  Default is 'partial-kl-divergence'.

NOTE: Multiple --strategy options can be given, and each strategy will
be tried, one after the other.""")
    canon_options["strategy"] = {
        "full-kldiv":"full-kl-divergence",
        "full-kl":"full-kl-divergence",
        "partial-kldiv":"partial-kl-divergence",
        "partial-kl":"partial-kl-divergence",
        "sym-full-kldiv":"symmetric-full-kl-divergence",
        "sym-full-kl":"symmetric-full-kl-divergence",
        "sym-partial-kldiv":"symmetric-partial-kl-divergence",
        "sym-partial-kl":"symmetric-partial-kl-divergence",
        "cossim":"cosine-similarity",
        "partial-cossim":"partial-cosine-similarity",
        //"regdist":"average-cell-probability",
        //"per-word-region-distribution":"average-cell-probability",
        "avg-cell-prob":"average-cell-probability",
        "acp":"average-cell-probability",
        "nb-base":"naive-bayes-with-baseline",
        "nb-nobase":"naive-bayes-no-baseline"}

    op.add_option("--baseline-strategy", "--bs", type="choice", action="append",
                  default=null,
                  choices=["internal-link", "link",
                           "random",
                           "num-articles", "num-arts", "numarts",
                           "link-most-common-toponym",
                           "region-distribution-most-common-toponym",
                           "regdist-most-common-toponym"],
                  help="""Strategy to use to compute the baseline.

'internal-link' (or 'link') means use number of internal links pointing to the
article or region.

'random' means choose randomly.

'num-articles' (or 'num-arts' or 'numarts'; only in region-type matching) means
use number of articles in region.

'link-most-common-toponym' (only in --mode=geotag-documents) means to look
for the toponym that occurs the most number of times in the article, and
then use the internal-link baseline to match it to a location.

'regdist-most-common-toponym' (only in --mode=geotag-documents) is similar,
but uses the region distribution of the most common toponym.

Default '%default'.

NOTE: Multiple --baseline-strategy options can be given, and each strategy will
be tried, one after the other.  Currently, however, the *-most-common-toponym
strategies cannot be mixed with other baseline strategies, or with non-baseline
strategies, since they require that --preserve-case-words be set internally.""")
    canon_options["baseline_strategy"] = (
        {"link":"internal-link",
         "num-arts":"num-articles",
         "numarts":"num-articles",
         "region-distribution-most-common-toponym":
           "regdist-most-common-toponym"}
        )

    op.add_option("--baseline-weight", "--bw", type="float", metavar="WEIGHT",
                  default=0.5,
                  help="""Relative weight to assign to the baseline (prior
probability) when doing weighted Naive Bayes.  Default %default.""")
    op.add_option("--naive-bayes-weighting", "--nbw", type="choice",
                  default="equal",
                  choices=["equal", "equal-words", "distance-weighted"],
                  help="""Strategy for weighting the different probabilities
that go into Naive Bayes.  If 'equal', do pure Naive Bayes, weighting the
prior probability (baseline) and all word probabilities the same.  If
'equal-words', weight all the words the same but collectively weight all words
against the baseline, giving the baseline weight according to --baseline-weight
and assigning the remainder to the words.  If 'distance-weighted', similar to
'equal-words' but don't weight each word the same as each other word; instead,
weight the words according to distance from the toponym.""")
    op.add_option("--width-of-stat-region", type="int", default=1,
                  help="""Width of the region used to compute a statistical
distribution for geotagging purposes, in terms of number of tiling regions.
Default %default.""")
    op.add_option("--degrees-per-region", "--dpr", type="float", default=null,
                  help="""Size (in degrees) of the tiling regions that cover
the earth.  Some number of tiling regions are put together to form the region
used to construct a statistical distribution.  No default; the default of
'--miles-per-region' is used instead.""")
    op.add_option("--miles-per-region", "--mpr", type="float", default=100.0,
                  help="""Size (in miles) of the tiling regions that cover
the earth.  Some number of tiling regions are put together to form the region
used to construct a statistical distribution.  Default %default.""")
    op.add_option("--context-type", "--ct", type="choice",
                  default="region-dist-article-links",
                  choices=["article", "region", "region-dist-article-links"],
                  help="""Type of context used when doing disambiguation.
There are two cases where this choice applies: When computing a word
distribution, and when counting the number of incoming internal links.
'article' means use the article itself for both.  'region' means use the
region for both. 'region-dist-article-links' means use the region for
computing a word distribution, but the article for counting the number of
incoming internal links.  Note that this only applies when
--mode='geotag-toponyms'; in --mode='geotag-documents', only regions are
considered.  Default '%default'.""")

    op.add_option("-k", "--kml-words", "--kw",
                  help="""Words to generate KML distributions for, when
--mode='generate-kml'.  Each word should be separated by a comma.  A separate
file is generated for each word, using '--kml-prefix' and adding '.kml'.""")
    op.add_option("--kml-prefix", "--kp",
                  default="kml-dist.",
                  help="""Prefix to use for KML files outputted.
Default '%default',""")
    op.add_option("--kml-transform", "--kt", "--kx", type="choice",
                  default="none",
                  choices=["none", "log", "logsquared"],
                  help="""Type of transformation to apply to the probabilities
when generating KML, possibly to try and make the low values more visible.
Possibilities are 'none' (no transformation), 'log' (take the log), and
'logsquared' (negative of squared log).  Default '%default'.""")

    op.add_option("--num-training-docs", "--ntrain", type="int", default=0,
                  help="""Maximum number of training documents to use.
0 means no limit.  Default %default.""")
    op.add_option("--num-test-docs", "--ntest", type="int", default=0,
                  help="""Maximum number of test documents to process.
0 means no limit.  Default %default.""")
    op.add_option("--skip-initial-test-docs", "--skip-initial", type="int", default=0,
                  help="""Skip this many test docs at beginning.  Default 0.""")
    op.add_option("--every-nth-test-doc", "--every-nth", type="int", default=1,
                  help="""Only process every Nth test doc.  Default 1, i.e. process all.""")
//    op.add_option("--skip-every-n-test-docs", "--skip-n", type="int", default=0,
//                  help="""Skip this many after each one processed.  Default 0.""")
    op.add_option("--no-individual-results", "--no-results",
                  action="store_true", default=false,
                  help="""Don't show individual results for each test document.""")
    op.add_option("--lru-cache-size", "--lru",
                  type="int", default=400,
                  help="""Number of entries in the LRU cache.""")

    canon_options
  }

  def handle_arguments(self, opts:Map[String,String], op:OptionParser,
                       args:List[String]) = {
    global Opts
    val Opts = opts
    global debug
    if (opts.debug) {
      val params = re.split("""[:;\s]+""", opts.debug)
      // Allow params with values, and allow lists of values to be given
      // by repeating the param
      for (f <- params) {
        if ('=' in f) {
          (param, value) = re.split("=", f, 1)
          if (list_debug_params contains param) {
            val values = re.split("[,]", value)
            debug[param] += values
          }
          else
            debug[param] = value
        }
        else
          debug[f] = true
      }
      WordDist.set_debug(debug)
    }

    class Params {
      ()
    }

    params = Params()
    params.need_to_read_stopwords = false
   
    // Canonicalize options
    if (!opts.strategy) {
      if (opts.mode == "geotag-documents")
        opts.strategy = Seq("partial-kl-divergence")
      else if (opts.mode == "geotag-toponyms")
        opts.strategy = Seq("baseline")
      else
        opts.strategy = Seq[String]()
    }

    if (!opts.baseline_strategy)
      opts.baseline_strategy = Seq("internal-link")

    if ("baseline" in opts.strategy) {
      var need_case = false
      var need_no_case = false
      for (bstrat <- opts.baseline_strategy) {
        if (bstrat.endswith("most-common-toponym"))
          need_case = true
        else
          need_no_case = true
      }
      if (need_case) {
        if (opts.strategy.length > 1 || need_no_case) {
          // That's because we have to set --preserve-case-words, which we
          // generally don't want set for other strategies and which affects
          // the way we construct the training-document distributions.
          op.error("Can't currently mix *-most-common-toponym baseline strategy with other strategies")
        }
        opts.preserve_case_words = true
      }
    }

    // FIXME! Can only currently handle World-type gazetteers.
    if (opts.gazetteer_type != "world")
      op.error("Currently can only handle world-type gazetteers")

    if (opts.miles_per_region <= 0)
      op.error("Miles per region must be positive")
    global degrees_per_region
    if (opts.degrees_per_region)
      var degrees_per_region = opts.degrees_per_region
    else
      degrees_per_region = opts.miles_per_region / miles_per_degree
    global maximum_latind, minimum_latind, maximum_longind, minimum_longind
    // The actual maximum latitude is exactly 90 (the North Pole).  But if we
    // set degrees per region to be a number that exactly divides 180, and we
    // use maximum_latitude = 90 in the following computations, then we would
    // end up with the North Pole in a region by itself, something we probably
    // don't want.
    val (maximum_latind, maximum_longind) = coord_to_tiling_region_indices(Coord(maximum_latitude - 1e-10,
                                           maximum_longitude))
    val (minimum_latind, minimum_longind) = coord_to_tiling_region_indices(Coord(minimum_latitude,
                                           minimum_longitude))

    if (opts.width_of_stat_region <= 0)
      op.error("Width of statistical region must be positive")

    //// Start reading in the files and operating on them ////

    if (opts.mode.startswith("geotag")) {
      params.need_to_read_stopwords = true
      if ((opts.mode == "geotag-toponyms" && opts.strategy == Seq("baseline")))
        ()
      else if (!opts.counts_file)
        op.error("Must specify counts file")
    }

    if (opts.mode == "geotag-toponyms")
      self.need("gazetteer_file")

    if (opts.eval_format == "raw-text") {
      // FIXME!!!!
      op.error("Raw-text reading not implemented yet")
    }

    if (opts.mode == "geotag-documents") {
      if (!(Seq("pcl-travel", "wiki") contains opts.eval_format))
        op.error("For --mode=geotag-documents, eval-format must be 'pcl-travel' or 'wiki'")
    }
    else if (opts.mode == "geotag-toponyms") {
      if (opts.baseline_strategy.endswith("most-common-toponym")) {
        op.error("--baseline-strategy=%s only compatible with --mode=geotag-documents"
            format opts.baseline_strategy)
      }
      for (stratname <- opts.strategy) {
        if (!(Seq("baseline", "naive-bayes-with-baseline",
                  "naive-bayes-no-baseline") contains stratname)) {
          op.error("Strategy '%s' invalid for --mode=geotag-toponyms" format
                   stratname)
        }
      }
      if (!(Seq("tr-conll", "wiki") contains opts.eval_format))
        op.error("For --mode=geotag-toponyms, eval-format must be 'tr-conll' or 'wiki'")
    }

    if (opts.mode == "geotag-documents" && opts.eval_format == "wiki")
      () // No need for evaluation file, uses the counts file
    else if (opts.mode.startswith("geotag"))
      self.need("eval_file", "evaluation file(s)")

    if (opts.mode == "generate-kml")
      self.need("kml_words")
    else if (opts.kml_words)
      op.error("--kml-words only compatible with --mode=generate-kml")

    self.need("article_data_file")

    params
  }

  def implement_main(self, opts:Map[String,String], params:Map[String,?],
                     args:List[String]) {
    if (params.need_to_read_stopwords)
      read_stopwords(opts.stopwords_file)
    for (fn <- opts.article_data_file)
      read_article_data(fn)

    //errprint("Processing evaluation file(s) %s for toponym counts...", opts.eval_file)
    //process_dir_files(opts.eval_file, count_toponyms_in_file)
    //errprint("Number of toponyms seen: %s", len(toponyms_seen_in_eval_files))
    //errprint("Number of toponyms seen more than once: %s", //  len([foo for (foo,count) in toponyms_seen_in_eval_files.iteritems() if
    //       count > 1]))
    //output_reverse_sorted_table(toponyms_seen_in_eval_files,
    //                            outfile=sys.stderr)

    // Read in the words-counts file
    for (fn <- opts.counts_file)
      read_word_counts(fn)
    if (opts.counts_file)
      finish_word_counts()

    if (opts.gazetteer_file)
      WorldGazetteer.read_world_gazetteer_and_match(opts.gazetteer_file)

    if (opts.mode == "generate-kml") {
      StatRegion.initialize_regions()
      val words = opts.kml_words.split(',')
      for (word <- words) {
        val regdist = RegionDist.get_region_dist(word)
        if (!regdist.normalized) {
          warning("""Non-normalized distribution, apparently word %s not seen anywhere.
Not generating an empty KML file.""", word)
        }
        else
          regdist.generate_kml_file("%s%s.kml" format (opts.kml_prefix, word))
      }
      return
    }

    def process_strategies[T](
        strat_unflat:Seq[Seq[T]])(geneval:(String, T) => TestFileEvaluator) {
      val strats = strat_unflat reduce (_ ++ _)
      for ((stratname, strategy) <- strats) {
        val evalobj = geneval(stratname, strategy)
        errprint("Processing evaluation file/dir %s...", opts.eval_file)
        val iterfiles =
          if (opts.eval_file) iter_directory_files(opts.eval_file)
          else Seq("foo")
        evalobj.evaluate_and_output_results(iterfiles)
      }
    }

    if (opts.mode == "geotag-toponyms") {
      val strats = (
        for (stratname <- opts.strategy) yield {
          // Generate strategy object
          if (stratname == "baseline") {
            for (basestratname <- opts.baseline_strategy) yield
              ("baseline " + basestratname,
                  BaselineGeotagToponymStrategy(opts, basestratname))
          }
          else {
            val strategy = NaiveBayesToponymStrategy(opts,
                use_baseline=(stratname == "naive-bayes-with-baseline"))
            Seq((stratname, strategy))
          }
        }
      )
      process_strategies(strats)((stratname, strategy) => {
        // Generate reader object
        if (opts.eval_format == "tr-conll")
          TRCoNLLGeotagToponymEvaluator(opts, strategy, stratname)
        else
          WikipediaGeotagToponymEvaluator(opts, strategy, stratname)
      })
    } else if (opts.mode == "geotag-documents") {
      val strats = (
        for (stratname <- opts.strategy) yield {
          if (stratname == "baseline") {
            for (basestratname <- opts.baseline_strategy) yield
              ("baseline " + basestratname,
                  BaselineGeotagDocumentStrategy(basestratname))
          }
          else {
            val strategy =
              if (stratname.startswith("naive-bayes-"))
                NaiveBayesDocumentStrategy(opts,
                  use_baseline=(stratname == "naive-bayes-with-baseline"))
              else stratname match {
                case "average-cell-probability" =>
                  PerWordRegionDistributionsStrategy()
                case "cosine-similarity" =>
                  CosineSimilarityStrategy(smoothed=false, partial=false)
                case "partial-cosine-similarity" =>
                  CosineSimilarityStrategy(smoothed=false, partial=true)
                case "smoothed-cosine-similarity" =>
                  CosineSimilarityStrategy(smoothed=true, partial=false)
                case "smoothed-partial-cosine-similarity" =>
                  CosineSimilarityStrategy(smoothed=true, partial=true)
                case "full-kl-divergence" =>
                  KLDivergenceStrategy(symmetric=false, partial=false)
                case "partial-kl-divergence" =>
                  KLDivergenceStrategy(symmetric=false, partial=true)
                case "symmetric-full-kl-divergence" =>
                  KLDivergenceStrategy(symmetric=true, partial=false)
                case "symmetric-partial-kl-divergence" =>
                  KLDivergenceStrategy(symmetric=true, partial=true)
                case "none" =>
                  null
              }
            if (strategy != null)
              Seq((stratname, strategy))
            else
              Seq()
          }
        }
      )
      process_strategies(strats)((stratname, strategy) => {
        // Generate reader object
        if (opts.eval_format == "pcl-travel")
          PCLTravelGeotagDocumentEvaluator(opts, strategy, stratname)
        else
          WikipediaGeotagDocumentEvaluator(opts, strategy, stratname)
      })
    }
  }
}
