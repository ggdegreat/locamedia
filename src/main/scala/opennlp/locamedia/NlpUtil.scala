package opennlp.locamedia

import collection.mutable
import collection.mutable.{Builder, MapBuilder}
import collection.generic.CanBuildFrom
// from __future__ import with_statement // For chompopen(), uchompopen()
// from optparse import OptionParser
// from itertools import *
// import itertools
// import re // For regexp wrappers
// import sys, codecs // For uchompopen()
// import math // For float_with_commas()
// import bisect // For sorted lists
// import time // For status messages, resource usage
// from heapq import * // For priority queue
// import UserDict // For SortedList, LRUCache
// import resource // For resource usage
// from collections import deque // For breadth-first search
// from subprocess import * // For backquote
// from errno import * // For backquote
// import os // For get_program_memory_usage_ps()
// import os.path // For exists of /proc, etc.
// import fileinput // For uchompopen() etc.

object NlpUtil {

  // Debug params.  Different params indicate different info to output.
  // Specified using --debug.  Multiple params are separated by commas or
  // spaces.  Params can be boolean, if given alone, or valueful, if given as
  // PARAM=VALUE.  Certain params are list-valued; multiple values are specified
  // by including the parameter multiple times, or by separating values by
  // a semicolon or colon.
  val debug = booleanmap()
  val debugval = stringmap()
  val debuglist = listmap[String]()
  
  var list_debug_params = Set[String]()
  
  // Register a list-valued debug param.
  def register_list_debug_param(param:String) {
    list_debug_params += param
  }

  /**
    * Return floating-point value, number of seconds since the Epoch
    **/
  def curtimesecs() = (new Date()).getTime()/1000.0

  def curtimehuman() = (new Date()) toString
  /*
    A simple object to make regexps a bit less awkward.  Works like this:

    ("foo (.*)", "foo bar") match {
      case Re(x) => println("matched 1 %s" format x)
      case _ => println("no match 1")
    }

    This will print out "matched 1 bar".
   */

  object Re {
    def unapplySeq(x:Tuple2[String, String]) = {
      val (re, str) = x
      re.r.unapplySeq(str)
    }
  }
    
  //////////////////////////////////////////////////////////////////////////////
  //                        Regular expression functions                      //
  //////////////////////////////////////////////////////////////////////////////
//  
//  
//  ///// Some simple wrappers around basic text-processing Python functions to
//  ///// make them easier to use.
//  /////
//  ///// 1. rematch() and research():
//  /////
//  ///// The functions 'rematch' and 'research' are wrappers around re.match()
//  ///// and re.search(), respectively, but instead of returning a match object
//  ///// None, they return true or false, and in the case a match object would
//  ///// have been returned, a corresponding WreMatch object is stored in the
//  ///// global variable m_.  Groups can be accessed from this variable using
//  ///// m_.group() or m_.groups(), but they can also be accessed through direct
//  ///// subscripting, i.e. m_[###] = m_.group(###).
//
//  class WreMatch(object):
//    def setmatch(self, match):
//      self.match = match
//  
//    def groups(self, *foo):
//      return self.match.groups(*foo)
//  
//    def group(self, *foo):
//      return self.match.group(*foo)
//  
//    def __getitem__(self, key):
//      return self.match.group(key)
//  
//  m_ = WreMatch()
//  
//  def rematch(pattern, string, flags=0):
//    m = re.match(pattern, string, flags)
//    if m:
//      m_.setmatch(m)
//      return true
//    return false
//  
//  def research(pattern, string, flags=0):
//    global m_
//    m = re.search(pattern, string, flags)
//    if m:
//      m_.setmatch(m)
//      return true
//    return false
//
  //////////////////////////////////////////////////////////////////////////////
  //                            File reading functions                        //
  //////////////////////////////////////////////////////////////////////////////
  
  //// NOTE NOTE NOTE: Only works on Python 2.5 and above, due to using the
  //// "with" statement.
  
  ///// 1. chompopen():
  /////
  ///// A generator that yields lines from a file, with any terminating newline
  ///// removed (but no other whitespace removed).  Ensures that the file
  ///// will be automatically closed under all circumstances.
  /////
  ///// 2. uchompopen():
  /////
  ///// Same as chompopen() but specifically open the file as 'utf-8' and
  ///// return Unicode strings.
  
  //"""
  //Test gopen
  //
  //import nlputil
  //for line in nlputil.gopen("foo.txt"):
  //  print line
  //for line in nlputil.gopen("foo.txt", chomp=true):
  //  print line
  //for line in nlputil.gopen("foo.txt", encoding="utf-8"):
  //  print line
  //for line in nlputil.gopen("foo.txt", encoding="utf-8", chomp=true):
  //  print line
  //for line in nlputil.gopen("foo.txt", encoding="iso-8859-1"):
  //  print line
  //for line in nlputil.gopen(["foo.txt"], encoding="iso-8859-1"):
  //  print line
  //for line in nlputil.gopen(["foo.txt"], encoding="utf-8"):
  //  print line
  //for line in nlputil.gopen(["foo.txt"], encoding="iso-8859-1", chomp=true):
  //  print line
  //for line in nlputil.gopen(["foo.txt", "foo2.txt"], encoding="iso-8859-1", chomp=true):
  //  print line
  //"""

//  // General function for opening a file, with automatic closure after iterating
//  // through the lines.  The encoding can be specified (e.g. "utf-8"), and if so,
//  // the error-handling can be given.  Whether to remove the final newline
//  // (chomp=true) can be specified.  The filename can be either a regular
//  // filename (opened with open) or codecs.open(), or a list of filenames or
//  // None, in which case the argument is passed to fileinput.input()
//  // (if a non-empty list is given, opens the list of filenames one after the
//  // other; if an empty list is given, opens stdin; if None is given, takes
//  // list from the command-line arguments and proceeds as above).  When using
//  // fileinput.input(), the arguments "inplace", "backup" and "bufsize" can be
//  // given, appropriate to that function (e.g. to do in-place filtering of a
//  // file).  In all cases, 
//  def gopen(filename, mode="r", encoding=None, errors="strict", chomp=false,
//      inplace=0, backup="", bufsize=0):
//    if isinstance(filename, basestring):
//      def yieldlines():
//        if encoding is None:
//          mgr = open(filename)
//        else:
//          mgr = codecs.open(filename, mode, encoding=encoding, errors=errors)
//        with mgr as f:
//          for line in f:
//            yield line
//      iterator = yieldlines()
//    else:
//      if encoding is None:
//        openhook = None
//      else:
//        def openhook(filename, mode):
//          return codecs.open(filename, mode, encoding=encoding, errors=errors)
//      iterator = fileinput.input(filename, inplace=inplace, backup=backup,
//          bufsize=bufsize, mode=mode, openhook=openhook)
//    if chomp:
//      for line in iterator:
//        if line and line[-1] == "\n": line = line[:-1]
//        yield line
//    else:
//      for line in iterator:
//        yield line
//  
  // Open a filename with UTF-8-encoded input and yield lines converted to
  // Unicode strings, but with any terminating newline removed (similar to
  // "chomp" in Perl).  Basically same as gopen() but with defaults set
  // differently.
  def uchompopen(filename:String=null, mode:String="r",
        encoding:String="utf-8", errors:String="strict", chomp:Boolean=true,
        inplace:Int=0, backup:String="", bufsize:Int=0) = {
    // FIXME!! Implement the various optional args, or at least some of them.
    // At least we probably want the encoding to work properly.
    Source.fromFile(filename).getLines()
  }
  //  return gopen(filename, mode=mode, encoding=encoding, errors=errors,
  //      chomp=chomp, inplace=inplace, backup=backup, bufsize=bufsize)
  
//  // Open a filename and yield lines, but with any terminating newline
//  // removed (similar to "chomp" in Perl).  Basically same as gopen() but
//  // with defaults set differently.
//  def chompopen(filename, mode="r", encoding=None, errors="strict",
//      chomp=true, inplace=0, backup="", bufsize=0):
//    return gopen(filename, mode=mode, encoding=encoding, errors=errors,
//        chomp=chomp, inplace=inplace, backup=backup, bufsize=bufsize)
//  
//  // Open a filename with UTF-8-encoded input.  Basically same as gopen()
//  // but with defaults set differently.
//  def uopen(filename, mode="r", encoding="utf-8", errors="strict",
//      chomp=false, inplace=0, backup="", bufsize=0):
//    return gopen(filename, mode=mode, encoding=encoding, errors=errors,
//        chomp=chomp, inplace=inplace, backup=backup, bufsize=bufsize)
//
//  //////////////////////////////////////////////////////////////////////////////
//  //                         Other basic utility functions                    //
//  //////////////////////////////////////////////////////////////////////////////
//  /**
//   Intern a string (for more efficient memory use, potentially faster lookup.
//  If string is Unicode, automatically convert to UTF-8.
//   */
//  def internasc(text):
//    if type(text) is unicode: text = text.encode("utf-8")
//    return intern(text)
//  
//  /**
//   Print text string using 'print', converting Unicode as necessary.
//  If string is Unicode, automatically convert to UTF-8, so it can be output
//  without errors.  Send output to the file given in OUTFILE (default is
//  stdout).  Uses the 'print' command, and normally outputs a newline; but
//  this can be suppressed using NONL.  Output is not normally flushed (unless
//  the stream does this automatically); but this can be forced using FLUSH.
//   */
//  def uniprint(text, outfile=sys.stdout, nonl=false, flush=false):
//    
//    if type(text) is unicode:
//      text = text.encode("utf-8")
//    if nonl:
//      print >>outfile, text,
//    else:
//      print >>outfile, text
//    if flush:
//      outfile.flush()
//  
//  /**
//   Output text string, converting Unicode as necessary.
//  If string is Unicode, automatically convert to UTF-8, so it can be output
//  without errors.  Send output to the file given in OUTFILE (default is
//  stdout).  Uses the write() function, which outputs the text directly,
//  without adding spaces or newlines.  Output is not normally flushed (unless
//  the stream does this automatically); but this can be forced using FLUSH.
//   */
//  def uniout(text, outfile=sys.stdout, flush=false):
//    
//    if type(text) is unicode:
//      text = text.encode("utf-8")
//    outfile.write(text)
//    if flush:
//      outfile.flush()
//  
//  /**
//   Print text to stderr using 'print', converting Unicode as necessary.
//  If string is Unicode, automatically convert to UTF-8, so it can be output
//  without errors.  Uses the 'print' command, and normally outputs a newline; but
//  this can be suppressed using NONL.
//   */
//  def errprint(text, nonl=false):
//    uniprint(text, outfile=sys.stderr, nonl=nonl)
//  
//  /**
//   Output text to stderr, converting Unicode as necessary.
//  If string is Unicode, automatically convert to UTF-8, so it can be output
//  without errors.  Uses the write() function, which outputs the text directly,
//  without adding spaces or newlines.
//   */
//  def errout(text):
//    uniout(text, outfile=sys.stderr)
//  
  /**
    Output a warning, formatting into UTF-8 as necessary.
    */
  def warning(format:String, args:Any*) {
    errprint(format, args: _*)
  }
  
  def uniprint(text:String, outfile:PrintStream=System.out) {
    // FIXME!! Print to the given outfile.
    println(text)
  }
  def uniout(text:String, outfile:PrintStream=System.out) {
    // FIXME!! Print to the given outfile.
    print(text)
  }
  
  def errprint(format:String, args:Any*) {
    // If no arguments, assume that we've been passed a raw string to print,
    // so print it directly rather than passing it to 'format', which might
    // munge % signs
    if (args.length == 0)
      println(format)
    else
      println(format format (args: _*))
  }
  def errout(format:String, args:Any*) {
    if (args.length == 0)
      print(format)
    else
      print(format format (args: _*))
  }
  
  /**
   Convert a string to floating point, but don't crash on errors;
  instead, output a warning.
   */
  def safe_float(x:String) = {
    try {
      x.toFloat
    } catch {
      case _ => {
        val y = x.trim()
        if (y != "") warning("Expected number, saw %s", y)
        0.
      }
    }
  }
  
  /**
   Pluralize an English word, using a basic but effective algorithm.
   */
  def pluralize(word:String) = {
    val upper = word.last >= 'A' && word.last <= 'Z'
    val lowerword = word.toLower()
    if (re.match(""".*[b-df-hj-np-tv-z]y$""", lowerword)) {
      if upper: return word(:-1) + "IES"
      else: return word(:-1) + "ies"
    }
    else if (re.match(""".*([cs]h|[sx])$""", lowerword)) {
      if upper: return word + "ES"
      else: return word + "es"
    }
    else {
      if upper: return word + "S"
      else: return word + "s"
    }
  }
  
  /**
   Capitalize the first letter of string, leaving the remainder alone.
   */
  def capfirst(st:String) = {
    if !st: return st
    return st(0).capitalize() + st(1:)
  }
  
  // From: http://stackoverflow.com/questions/1823058/how-to-print-number-with-commas-as-thousands-separators-in-python-2-x
  def int_with_commas(x:Int) = {
    if (x < 0)
      return "-" + int_with_commas(-x)
    var result = ""
    while (x >= 1000) {
      x, r = divmod(x, 1000)
      result = ",%03d%s" format (r, result)
    }
    return "%d%s" format (x, result)
  }
  
  // My own version
  def float_with_commas(x:Double) = {
    val intpart = int(math.floor(x))
    val fracpart = x - intpart
    return int_with_commas(intpart) + ("%.2f" format fracpart)[1:]
  }
  
  def median(list:Seq[Double]) = {
    "Return the median value of a sorted list."
    var l = list.length
    if (l % 2 == 1)
      return list(l / 2)
    else {
      l = l / 2
      return 0.5*(list(l-1) + list(l))
    }
  }
  
  def mean(list:Seq[Double]) {
    "Return the mean of a list."
    return sum(list) / float(list.length)
  }
  
  // A function to make up for a missing feature in Scala.  Split a text
  // into segments but also return the delimiters.  Regex matches the
  // delimiters.  Return a list of tuples (TEXT, DELIM).  The last tuple
  // with have an empty delim.
  def re_split_with_delimiter(regex:util.matching.Regex, text:String) = {
    val splits = regex.split(text)
    val delim_intervals =
      for (m <- regex.findAllIn(text).matchData) yield List(m.start, m.end)
    val flattened = List(0) ++ (delim_intervals reduce (_ ++ _)) ++
      List(text.length, text.length)
    val interval_texts = flattened.iterator.sliding(2) map (text.slice(_,_))
    interval_texts grouped 2
  }

  def split_text_into_words(text:String, ignore_punc:Boolean=false,
    include_nl:Boolean=false) = {
    // This regexp splits on whitespace, but also handles the following cases:
    // 1. Any of , ; . etc. at the end of a word
    // 2. Parens or quotes in words like (foo) or "bar"
    // These punctuation characters are returned as separate words, unless
    // 'ignore_punc' is given.  Also, if 'include_nl' is given, newlines are
    // returned as their own words; otherwise, they are treated like all other
    // whitespace (i.e. ignored).
    (for ((word, punc) <-
         re_split_with_delimiter("""([,;."):]*\s+[("]*)""".r, text)) yield
       List(word) ++ (
         for (p <- punc; !(" \t\r\f\v" contains p)) yield (
           if (p == '\n') (if (include_nl) p else "")
           else (if (!ignore_punc) p else "")
         )
       )
    ) reduce (_ ++ _) filter (_ != "")
  }
 
  def fromto(from:Int, to_:Int) = {
    if (from <= to) (from to to_)
    else (to_ to from)
  }

  //////////////////////////////////////////////////////////////////////////////
  //                             Default dictionaries                         //
  //////////////////////////////////////////////////////////////////////////////
  
  // Another way to do this, using subclassing.
  //
  // abstract class gendefaultmap[From,To] extends HashMap[From, To] {
  //   val defaultval:To
  //   override def default(key: From) = defaultval
  // }
  //
  // class genintmap[T] extends gendefaultmap[T, Int] { val defaultval = 0 }
  //
  // def intmap() = new genintmap[String]()
  //

  // The original way
  //
  // def booleanmap() = {
  //   new HashMap[String, Boolean] {
  //     override def default(key: String) = false
  //   }
  // }

  // Note the delayed evaluation of `defaultval', using =>.  This is done
  // on purpose to exactly mimic the semantics of the "original way" above.
  // This would matter, for example, if we use mutable Vectors or Sets as the
  // value type.  We want a *different* empty vector or set each time we call
  // default(), so that different keys get different empty vectors.  Otherwise,
  // adding an element to the vector associated with one key will also add
  // it to the vectors for other keys, which is not what we want.
  def gendefaultmap[F,T](defaultval: => T) = {
    new mutable.Map[F,T] {
      override def default(key:F) = defaultval
    }
  } 
  def genintmap[T]() = gendefaultmap[T,Int](0)
  def intmap() = genintmap[String]()
  def gendoublemap[T]() = gendefaultmap[T,Double](0.0)
  def doublemap() = gendoublemap[String]()
  def genbooleanmap[T]() = gendefaultmap[T,Boolean](false)
  def booleanmap() = genbooleanmap[String]()
  def genstringmap[T]() = gendefaultmap[T,String]("")
  def stringmap() = genstringmap[String]()
  // Similar but the default value is an empty collection.
  def genseqmap[T,U]() = gendefaultmap[T,mutable.Seq[U]](mutable.Seq[U]())
  def seqmap[T]() = genseqmap[String,T]()
  
  //def seqmap[T]() = {
  //  new HashMap[String, mutable.Seq[T]] {
  //      override def default(key: String) = mutable.Seq[T]()
  //  }
  //} 

  // ORIGINAL: ---------------------------------------

  // Our own version similar to collections.defaultdict().  The difference is
  // that we can specify whether or not simply referencing an unseen key
  // automatically causes the key to permanently spring into existence with
  // the "missing" value.  collections.defaultdict() always behaves as if
  // 'add_upon_ref'=true, same as our default.  Basically:
  //
  // foo = defdict(list, add_upon_ref=false)
  // foo["bar"]          -> []
  // "bar" in foo        -> false
  //
  // foo = defdict(list, add_upon_ref=true)
  // foo["bar"]          -> []
  // "bar" in foo        -> true
  //
  // The former may be useful where you may make many queries involving
  // non-existent keys, and you don't want all these keys added to the dict.
  // The latter is useful with mutable objects like lists.  If I create
  //
  //   foo = defdict(list, add_upon_ref=false)
  //
  // and then call
  //
  //   foo["glorplebargle"].append("shazbat")
  //
  // where "glorplebargle" is a previously non-existent key, the call to
  // 'append' will "appear" to work but in fact nothing will happen, because
  // the reference foo["glorplebargle"] will create a new list and return
  // it, but not store it in the dict, and 'append' will add to this
  // temporary list, which will soon disappear.  Note that using += will
  // actually work, but this is fragile behavior, not something to depend on.
  //

//  class defdict(dict):
//    def __init__(self, factory, add_upon_ref=true):
//      super(defdict, self).__init__()
//      self.factory = factory
//      self.add_upon_ref = add_upon_ref
//  
//    def __missing__(self, key):
//      val = self.factory()
//      if self.add_upon_ref:
//        self[key] = val
//      return val
//
//  def dictdict():
//    return defdict(dict, add_upon_ref=true)
//  
//  def tupledict():
//    return defdict(tuple, add_upon_ref=false)
//  
//  def setdict():
//    return defdict(set, add_upon_ref=true)
  
  //////////////////////////////////////////////////////////////////////////////
  //                                 Sorted lists                             //
  //////////////////////////////////////////////////////////////////////////////
  
  // Return a tuple (keys, values) of lists of items corresponding to a hash
  // table.  Stored in sorted order according to the keys.  Use
  // lookup_sorted_list(key) to find the corresponding value.  The purpose of
  // doing this, rather than just directly using a hash table, is to save
  // memory.

//  def make_sorted_list(table):
//    items = sorted(table.items(), key=lambda x:x[0])
//    keys = [""]*len(items)
//    values = [""]*len(items)
//    for i in xrange(len(items)):
//      item = items[i]
//      keys[i] = item[0]
//      values[i] = item[1]
//    return (keys, values)
//  
//  // Given a sorted list in the tuple form (KEYS, VALUES), look up the item KEY.
//  // If found, return the corresponding value; else return None.
//  
//  def lookup_sorted_list(sorted_list, key, default=None):
//    (keys, values) = sorted_list
//    i = bisect.bisect_left(keys, key)
//    if i != len(keys) and keys[i] == key:
//      return values[i]
//    return default
//  
//  // A class that provides a dictionary-compatible interface to a sorted list
//  
//  class SortedList(object, UserDict.DictMixin):
//    def __init__(self, table):
//      self.sorted_list = make_sorted_list(table)
//  
//    def __len__(self):
//      return len(self.sorted_list[0])
//  
//    def __getitem__(self, key):
//      retval = lookup_sorted_list(self.sorted_list, key)
//      if retval is None:
//        raise KeyError(key)
//      return retval
//  
//    def __contains__(self, key):
//      return lookup_sorted_list(self.sorted_list, key) is not None
//  
//    def __iter__(self):
//      (keys, values) = self.sorted_list
//      for x in keys:
//        yield x
//  
//    def keys(self):
//      return self.sorted_list[0]
//  
//    def itervalues(self):
//      (keys, values) = self.sorted_list
//      for x in values:
//        yield x
//  
//    def iteritems(self):
//      (keys, values) = self.sorted_list
//      for (key, value) in izip(keys, values):
//        yield (key, value)
//
  //////////////////////////////////////////////////////////////////////////////
  //                                Table Output                              //
  //////////////////////////////////////////////////////////////////////////////

//  def key_sorted_items(d) {
//    return sorted(d.iteritems(), key=x => x[0])
//  }
//  
//  def value_sorted_items(d) {
//    return sorted(d.iteritems(), key=x => x[1])
//  }
//  
//  def reverse_key_sorted_items(d) {
//    return sorted(d.iteritems(), key=x => x[0], reverse=true)
//  }
//  
//  def reverse_value_sorted_items(d) {
//    return sorted(d.iteritems(), key=x => x[1], reverse=true)
//  }
//  
  // Given a list of tuples, where the second element of the tuple is a number and
  // the first a key, output the list, sorted on the numbers from bigger to
  // smaller.  Within a given number, sort the items alphabetically, unless
  // keep_secondary_order is true, in which case the original order of items is
  // left.  If 'outfile' is specified, send output to this stream instead of
  // stdout.  If 'indent' is specified, indent all rows by this string (usually
  // some number of spaces).  If 'maxrows' is specified, output at most this many
  // rows.
  def output_reverse_sorted_list[T,U](var items:Seq[(T,U)],
    outfile:PrintStream=System.out, indent:String="",
    keep_secondary_order:Boolean=false, maxrows:Int=-1) {
    if (!keep_secondary_order)
      items = items sortBy (_._1)
    items = items sortWith (_._2 > _._2)
    if (maxrows >= 0)
      items = items.slice(0, maxrows)
    for ((key, value) <- items)
      uniprint("%s%s = %s" format (indent, key, value), outfile=outfile)
  }
  
  // Given a table with values that are numbers, output the table, sorted
  // on the numbers from bigger to smaller.  Within a given number, sort the
  // items alphabetically, unless keep_secondary_order is true, in which case
  // the original order of items is left.  If 'outfile' is specified, send
  // output to this stream instead of stdout.  If 'indent' is specified, indent
  // all rows by this string (usually some number of spaces).  If 'maxrows'
  // is specified, output at most this many rows.
  def output_reverse_sorted_table[T,U](table:Map[T,U],
    outfile:PrintStream=System.out, indent:String="",
    keep_secondary_order:Boolean=false, maxrows:Int=-1) {
    output_reverse_sorted_list(table toList)
  }

  //////////////////////////////////////////////////////////////////////////////
  //                             Status Messages                              //
  //////////////////////////////////////////////////////////////////////////////

  // Output status messages periodically, at some multiple of
  // 'secs_between_output', measured in real time. 'item_name' is the name
  // of the items being processed.  Every time an item is processed, call
  // item_processed()
  class StatusMessage(item_name:String, secs_between_output:Double=15) {
    import NlpUtil._
    val plural_item_name = pluralize(item_name)
    var items_processed = 0
    val first_time = curtimesecs()
    var last_time = first_time
  
    def num_processed() = items_processed
  
    def elapsed_time() = curtimesecs() - first_time
  
    def item_unit() = {
      if (items_processed == 1)
        item_name
      else
        plural_item_name
    }
  
    def item_processed(maxtime:Double=0) = {
      val curtime = curtimesecs()
      items_processed += 1
      val total_elapsed_secs =
        (curtime - first_time) toInt
      val last_elapsed_secs = (curtime - last_time) toInt
      if (last_elapsed_secs >= secs_between_output) {
        // Rather than directly recording the time, round it down to the nearest
        // multiple of secs_between_output; else we will eventually see something
        // like 0, 15, 45, 60, 76, 91, 107, 122, ...
        // rather than
        // like 0, 15, 45, 60, 76, 90, 106, 120, ...
        val rounded_elapsed = ((total_elapsed_secs / secs_between_output).toInt() *
                           secs_between_output)
        last_time = first_time + rounded_elapsed
        errprint("Elapsed time: %s minutes %s seconds, %s %s processed",
                 (total_elapsed_secs / 60).toInt, total_elapsed_secs % 60,
                 items_processed, item_unit())
      }
      if (maxtime > 0 && total_elapsed_secs >= maxtime) {
        errprint("Maximum time reached, interrupting processing after %s %s",
                 items_processed, item_unit())
        true
      }
      else false
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //                               File Splitting                             //
  //////////////////////////////////////////////////////////////////////////////

  // Return the next file to output to, when the instances being output to the
  // files are meant to be split according to SPLIT_FRACTIONS.  The absolute
  // quantities in SPLIT_FRACTIONS don't matter, only the values relative to
  // the other values, i.e. [20, 60, 10] is the same as [4, 12, 2].  This
  // function implements an algorithm that is deterministic (same results
  // each time it is run), and spreads out the instances as much as possible.
  // For example, if all values are equal, it will cycle successively through
  // the different split files; if the values are [1, 1.5, 1], the output
  // will be [1, 2, 3, 2, 1, 2, 3, ...]; etc.
  
  def next_split_set(split_fractions:Seq[Double]) {
  
    val num_splits = split_fractions.length
    val cumulative_articles = mutable.Seq.fill(num_splits)(0.0)
  
    // Normalize so that the smallest value is 1.
  
    val minval = split_fractions min
    val normalized_split_fractions =
      (for (val <- split_fractions) yield val.toDouble/minval)
  
    // The algorithm used is as follows.  We cycle through the output sets in
    // order; each time we return a set, we increment the corresponding
    // cumulative count, but before returning a set, we check to see if the
    // count has reached the corresponding fraction and skip this set if so.
    // If we have run through an entire cycle without returning any sets,
    // then for each set we subtract the fraction value from the cumulative
    // value.  This way, if the fraction value is not a whole number, then
    // any fractional quantity (e.g. 0.6 for a value of 7.6) is left over,
    // any will ensure that the total ratios still work out appropriately.
 
    def fuckme_no_yield() {
      var yieldme = mutable.List[Int]()
      for (j <- 0 until num_splits) {
        //println("j=%s, this_output=%s" format (j, this_output))
        if (cumulative_articles(j) < normalized_split_fractions(j)) {
          yieldme += j
          cumulative_articles(j) += 1
        }
      }
      if (yieldme.length == 0) {
        for (j <- 0 until num_splits) {
          while (cumulative_articles(j) >= normalized_split_fractions(j))
            cumulative_articles(j) -= normalized_split_fractions(j)
        }
      }
      yieldme.toStream ++ fuckme_no_yield()
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //                               NLP Programs                               //
  //////////////////////////////////////////////////////////////////////////////

  def output_options(op:OptionParser) {
    errprint("Parameter values:")
    for ((name, opt) <- op.get_argmap)
      errprint("%30s: %s", name, op.value)
    errprint("")
  }
  
  abstract object NLPProgram extends App {
    // Things that must be implemented
    val opts:AnyRef
    val op:OptionParser
    def handle_arguments(op:OptionParser, args:List[String])
    def implement_main(op:OptionParser, args:List[String])

    // Things that may be overridden
    def output_parameters() {}

    def need(errmsg:String) { op.need(errmsg) }

    def main() = {
      errprint("Beginning operation at %s" format curtimehuman())
      errprint("Arguments: %s" format (args mkString " "))
      op.parse(opts, args)
      handle_arguments(op, args)
      output_options(op)
      output_parameters()
      val retval = implement_main(op, args)
      errprint("Ending operation at %s" format curtimehuman())
      retval
    }

    main()
    }
  }
//
//  //////////////////////////////////////////////////////////////////////////////
//  //                               Priority Queues                            //
//  //////////////////////////////////////////////////////////////////////////////
//
//  // Priority queue implementation, based on Python heapq documentation.
//  // Note that in Python 2.6 and on, there is a priority queue implementation
//  // in the Queue module.
//  class PriorityQueue(object):
//    INVALID = 0                     // mark an entry as deleted
//  
//    def __init__(self):
//      self.pq = []                         // the priority queue list
//      self.counter = itertools.count(1)    // unique sequence count
//      self.task_finder = {}                // mapping of tasks to entries
//  
//    def add_task(self, priority, task, count=None):
//      if count is None:
//        count = self.counter.next()
//      entry = [priority, count, task]
//      self.task_finder[task] = entry
//      heappush(self.pq, entry)
//  
//    //Return the top-priority task. If 'return_priority' is false, just
//    //return the task itself; otherwise, return a tuple (task, priority).
//    def get_top_priority(self, return_priority=false):
//      while true:
//        priority, count, task = heappop(self.pq)
//        if count is not PriorityQueue.INVALID:
//          del self.task_finder[task]
//          if return_priority:
//            return (task, priority)
//          else:
//            return task
//  
//    def delete_task(self, task):
//      entry = self.task_finder[task]
//      entry[1] = PriorityQueue.INVALID
//  
//    def reprioritize(self, priority, task):
//      entry = self.task_finder[task]
//      self.add_task(priority, task, entry[1])
//      entry[1] = PriorityQueue.INVALID
//
  //////////////////////////////////////////////////////////////////////////////
  //                      Least-recently-used (LRU) Caches                    //
  //////////////////////////////////////////////////////////////////////////////

  class LRUCache[T,U](maxsize:Int=1000) extends mutable.Map[T,U]
    with mutable.MapLike[T,U,LRUCache[T,U]] {
    val cache = mutable.LinkedHashMap[T,U]()

    // def length = return cache.length

    private def reprioritize(key: T) {
      val value = cache(key)
      cache -= key
      cache(key) = value
    }

    def get(key: T): Option[U] = {
      if (cache contains key) {
        reprioritize(key)
        Some(cache(key))
      }
      else None
    }
 
    override def update(key:T, value:U) {
      if (cache contains key)
        reprioritize(key)
      else {
        while (cache.length >= maxsize) {
          val (key2, value) = cache.head()
          cache -= key2
        }
        cache(key) = value
      }
    }

    override def remove(key: T): Option[U] = cache.remove(key)
 
    def iterator: Iterator[(T, U)] = cache.iterator

    // All the rest Looks like pure boilerplate!  Why necessary?
    def += (kv: (T, U)): this.type = {
      update(kv._1, kv._2); this }
    def -= (key: T): this.type = { remove(key); this }

    override def empty = new LRUCache[T,U]
    }

  // This whole object looks like boilerplate!  Why necessary?
  object LRUCache extends {
    def empty[T,U] = new LRUCache[T,U]

    def apply[U](kvs: (T,U)*): LRUCache[T,U] = {
      val m: LRUCache[T,U] = empty
      for (kv <- kvs) m += kv
       m
    }

    def newBuilder[T,U]: Builder[(T,U), LRUCache[T,U]] =
      new MapBuilder[T, U, LRUCache[T,U]](empty)

    implicit def canBuildFrom[T,U]
      : CanBuildFrom[LRUCache(_), (T,U), LRUCache[T]] =
        new CanBuildFrom[LRUCache(_), (T,U), LRUCache[T]] {
          def apply(from: LRUCache(_)) = newBuilder[T,U]
          def apply() = newBuilder[T,U]
        }
  }

  //////////////////////////////////////////////////////////////////////////////
  //                               Resource Usage                             //
  //////////////////////////////////////////////////////////////////////////////

  val beginning_prog_time = curtimesecs()
  
  def get_program_time_usage() = curtimesecs() - beginning_prog_time
  
  def get_program_memory_usage() = {
    if (os.path.exists("/proc/self/status"))
      get_program_memory_usage_proc()
    else {
      try
        get_program_memory_usage_ps()
      catch
        get_program_memory_rusage()
    }
  }
  
  
  def get_program_memory_usage_rusage() = {
    val res = resource.getrusage(resource.RUSAGE_SELF)
    // FIXME!  This is "maximum resident set size".  There are other more useful
    // values, but on the Mac at least they show up as 0 in this structure.
    // On Linux, alas, all values show up as 0 or garbage (e.g. negative).
    res.ru_maxrss
  }
  
  // Get memory usage by running 'ps'; getrusage() doesn't seem to work very
  // well.  The following seems to work on both Mac OS X and Linux, at least.
  def get_program_memory_usage_ps():Int = {
    val pid = os.getpid()
    val input = backquote("ps -p %s -o rss" format pid)
    val lines = re.split("""\n""", input)
    for (line <- lines if line.trim != "RSS")
      return 1024*line.trim.toInt
  }
  
  // Get memory usage by running 'proc'; this works on Linux and doesn't require
  // spawning a subprocess, which can crash when your program is very large.
  def get_program_memory_usage_proc():Int = {
    with open("/proc/self/status") as f:
      for (line <- f) {
        val line = line.trim
        if (line.startsWith("VmRSS:")) {
          val rss = (line.split()(1)).toInt
          return 1024*rss
        }
      }
    return 0
  }
  
  def format_minutes_seconds(var secs:Double) {
    var mins = (secs / 60) toInt
    secs = secs % 60
    val hours = (mins / 60) toInt
    mins = mins % 60
    var hourstr = (
      if (hours > 0) "%s hour%s " format (hours, if (hours == 1) "" else "s")
      else "")
    val secstr = (if (secs.toInt == secs) "%s" else "%0.1f") format secs
    "%s%s minute%s %s second%s" format (
        hourstr,
        mins, if (mins == 1) "" else "s",
        secstr, if (secs == 1) "" else "s")
  }
  
  def output_resource_usage() {
    errprint("Total elapsed time since program start: %s",
             format_minutes_seconds(get_program_time_usage()))
    errprint("Memory usage: %s bytes",
        int_with_commas(get_program_memory_usage()))
  }

  //////////////////////////////////////////////////////////////////////////////
  //                             Hash tables by range                         //
  //////////////////////////////////////////////////////////////////////////////

  // A table that groups all keys in a specific range together.  Instead of
  // directly storing the values for a group of keys, we store an object (termed a
  // "collector") that the user can use to keep track of the keys and values.
  // This way, the user can choose to use a list of values, a set of values, a
  // table of keys and values, etc.
  
  class TableByRange[Coll](ranges:List[Double], create:()=>Coll, lowest_bound:Double=0.0) {
    // Create a new object. 'ranges' is a sorted list of numbers, indicating the
    // boundaries of the ranges.  One range includes all keys that are
    // numerically below the first number, one range includes all keys that are
    // at or above the last number, and there is a range going from each number
    // up to, but not including, the next number.  'collector' is used to create
    // the collectors used to keep track of keys and values within each range;
    // it is either a type or a no-argument factory function.  We only create
    // ranges and collectors as needed. 'lowest_bound' is the value of the
    // lower bound of the lowest range; default is 0.  This is used only
    // it iter_ranges() when returning the lower bound of the lowest range,
    // and can be an item of any type, e.g. the number 0, the string "-infinity",
    // etc.
    val items_by_range = Map[Double,Coll]()
  
    def get_collector(key:Double) {
      var lower_range = lowest_bound
      // upper_range = "infinity"
      breakable {
        for (i <- ranges) {
          if (i <= key)
            lower_range = i
          else {
            // upper_range = i
            break
          }
        }
      }
      if (!(lower_range contains items_by_range))
        items_by_range(lower_range) = create()
      return items_by_range(lower_range)
    }
  
    /**
     Return an iterator over ranges in the table.  Each returned value is
     a tuple (LOWER, UPPER, COLLECTOR), giving the lower and upper bounds
     (inclusive and exclusive, respectively), and the collector item for this
     range.  The lower bound of the lowest range comes from the value of
     'lowest_bound' specified during creation, and the upper bound of the range
     that is higher than any numbers specified during creation in the 'ranges'
     list will be the string "infinity" is such a range is returned.
  
     The optional arguments 'unseen_between' and 'unseen_all' control the
     behavior of this iterator with respect to ranges that have never been seen
     (i.e. no keys in this range have been passed to 'get_collector').  If
     'unseen_all' is true, all such ranges will be returned; else if
     'unseen_between' is true, only ranges between the lowest and highest
     actually-seen ranges will be returned.
     */
    def iter_ranges(unseen_between:Boolean=true, unseen_all:Boolean=false) {
      var highest_seen = null
      def iteration_range =
        (List(lowest_bound) ++ ranges) zip
         (ranges ++ List(Double.PositiveInfinity))
      for ((lower, upper) <- iteration_range) {
        if (items_by_range contains lower)
          highest_seen = upper
      }
  
      var seen_any = false
      for {(lower, upper) <- iteration_range
           val collector = items_by_range.get(lower, null)
           if (collector != null || unseen_all ||
               (unseen_between && seen_any &&
                upper != Double.PositiveInfinity && upper <= highest_seen))
           val col2 = if (collector != null) collector else create()
          }
      yield {
        if (collector != null) seen_any = true
        (lower, upper, collector)
      }
    }
  }

 
//  //////////////////////////////////////////////////////////////////////////////
//  //                          Depth-, breadth-first search                    //
//  //////////////////////////////////////////////////////////////////////////////
//
//  // General depth-first search.  'node' is the node to search, the top of a
//  // tree.  'matches' indicates whether a given node matches.  'children'
//  // returns a list of child nodes.
//  def depth_first_search(node, matches, children):
//    nodelist = [node]
//    while len(nodelist) > 0:
//      node = nodelist.pop()
//      if matches(node):
//        yield node
//      nodelist.extend(reversed(children(node)))
//  
//  // General breadth-first search.  'node' is the node to search, the top of a
//  // tree.  'matches' indicates whether a given node matches.  'children'
//  // returns a list of child nodes.
//  def breadth_first_search(node, matches, children):
//    nodelist = deque([node])
//    while len(nodelist) > 0:
//      node = nodelist.popLeft()
//      if matches(node):
//        yield node
//      nodelist.extend(children(node))
//
//  //////////////////////////////////////////////////////////////////////////////
//  //                               Merge sequences                            //
//  //////////////////////////////////////////////////////////////////////////////
//
//  // Return an iterator over all elements in all the given sequences, omitting
//  // elements seen more than once and keeping the order.
//  def merge_sequences_uniquely(*seqs):
//    table = {}
//    for seq in seqs:
//      for s in seq:
//        if s not in table:
//          table[s] = true
//          yield s
//
//  
//  //////////////////////////////////////////////////////////////////////////////
//  //                                Subprocesses                              //
//  //////////////////////////////////////////////////////////////////////////////
//
//  // Run the specified command; return its combined output and stderr as a string.
//  // 'command' can either be a string or a list of individual arguments.  Optional
//  // argument 'shell' indicates whether to pass the command to the shell to run.
//  // If unspecified, it defaults to true if 'command' is a string, false if a
//  // list.  If optional arg 'input' is given, pass this string as the stdin to the
//  // command.  If 'include_stderr' is true, stderr will be included along with
//  // the output.  If return code is non-zero, throw CommandError if 'throw' is
//  // specified; else, return tuple of (output, return-code).
//  def backquote(command, input=None, shell=None, include_stderr=true, throw=true):
//    //logdebug("backquote called: %s" % command)
//    if shell is None:
//      if isinstance(command, basestring):
//        shell = true
//      else:
//        shell = false
//    stderrval = STDOUT if include_stderr else PIPE
//    if input is not None:
//      popen = Popen(command, stdin=PIPE, stdout=PIPE, stderr=stderrval,
//                    shell=shell, close_fds=true)
//      output = popen.communicate(input)
//    else:
//      popen = Popen(command, stdout=PIPE, stderr=stderrval,
//                    shell=shell, close_fds=true)
//      output = popen.communicate()
//    if popen.returncode != 0:
//      if throw:
//        if output[0]:
//          outputstr = "Command's output:\n%s" % output[0]
//          if outputstr[-1] != '\n':
//            outputstr += '\n'
//        errstr = output[1]
//        if errstr and errstr[-1] != '\n':
//          errstr += '\n'
//        errmess = ("Error running command: %s\n\n%s\n%s" %
//            (command, output[0], output[1]))
//        //log.error(errmess)
//        oserror(errmess, EINVAL)
//      else:
//        return (output[0], popen.returncode)
//    return output[0]
//  
//  def oserror(mess, err):
//    e = OSError(mess)
//    e.errno = err
//    raise e
//
//  //////////////////////////////////////////////////////////////////////////////
//  //                              Generating XML                              //
//  //////////////////////////////////////////////////////////////////////////////
//
//  // This is old code I wrote originally for ccg.ply (the ccg2xml converter),
//  // for generating XML.  It doesn't use the functions in xml.dom.minidom,
//  // which in any case are significantly more cumbersome than the list/tuple-based
//  // structure used below.
//  
//  // --------- XML ----------
//  //
//  // Thankfully, the structure of XML is extremely simple.  We represent
//  // a single XML statement of the form
//  //
//  // <biteme foo="1" blorp="baz">
//  //   <bitemetoo ...>
//  //     ...
//  //   gurgle
//  // </biteme>
//  //
//  // as a list
//  //
//  // ["biteme", [("foo", "1"), ("blorp", "baz")],
//  //    ["bitemetoo", ...],
//  //    "gurgle"
//  // ]
//  //
//  // i.e. an XML statement corresponds to a list where the first element
//  // is the statement name, the second element lists any properties, and
//  // the remaining elements list items inside the statement.
//  //
//  // ----------- Property lists -------------
//  //
//  // The second element of an XML statement in list form is a "property list",
//  // a list of two-element tuples (property and value).  Some functions below
//  // (e.g. `getprop', `putprop') manipulate property lists.
//  //
//  // FIXME: Just use a hash table.
//  
//  def check_arg_type(errtype, arg, ty):
//    if type(arg) is not ty:
//      raise TypeError("%s: Type is not %s: %s" % (errtype, ty, arg))
//  
//  def xml_sub(text):
//    if not isinstance(text, basestring):
//      text = text.__str__()
//    if type(text) is unicode:
//      text = text.encode("utf-8")
//    text = text.replace("&", "&amp;")
//    text = text.replace("<", "&lt;")
//    text = text.replace(">", "&gt;")
//    return text
//  
//  def print_xml_1(file, xml, indent=0):
//    //if xml_debug > 1:
//    //  errout("%sPrinting: %s\n" % (" " * indent, str(xml)))
//    if type(xml) is not list:
//      file.write("%s%s\n" % (" " * indent, xml_sub(xml)))
//    else:
//      check_arg_type("XML statement", xml[0], str)
//      file.write(" " * indent)
//      file.write("<%s" % xml_sub(xml[0]))
//      for x in xml[1]:
//        check_arg_type("XML statement", x, tuple)
//        if len(x) != 2:
//          raise TypeError("Bad tuple pair: " + str(x))
//        file.write(" %s=\"%s\"" % (xml_sub(x[0]), xml_sub(x[1])))
//      subargs = xml[2:]
//      if len(subargs) == 1 and type(subargs[0]) is not list:
//        file.write(">%s</%s>\n" % (xml_sub(subargs[0]), xml_sub(xml[0])))
//      elif not subargs:
//        file.write("/>\n")
//      else:
//        file.write(">\n")
//        for x in subargs:
//          print_xml_1(file, x, indent + 2)
//        file.write(" " * indent)
//        file.write("</%s>\n" % xml_sub(xml[0]))
//  
//  // Pretty-print a section of XML, in the format above, to FILE.
//  // Start at indent INDENT.
//  
//  def print_xml(file, xml):
//    print_xml_1(file, xml)
//  
//  // Function to output a particular XML file
//  def output_xml_file(filename, xml):
//    fil = open(filename, "w")
//    fil.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
//    print_xml(fil, xml)
//    fil.close()
//  
//  // Return true if PROP is seen as a property in PROPLIST, a list of tuples
//  // of (prop, value)
//  def property_specified(prop, proplist):
//    return not not ["foo" for (x,y) in proplist if x == prop]
//  
//  // Return value of property PROP in PROPLIST; signal an error if not found.
//  def getprop(prop, proplist):
//    for (x,y) in proplist:
//      if x == prop:
//        return y
//    raise ValueError("Property %s not found in %s" % (prop, proplist))
//  
//  // Return value of property PROP in PROPLIST, or DEFAULT.
//  def getoptprop(prop, proplist, default=None):
//    for (x,y) in proplist:
//      if x == prop:
//        return y
//    return default
//  
//  // Replace value of property PROP with VALUE in PROPLIST.
//  def putprop(prop, value, proplist):
//    for i in xrange(len(proplist)):
//      if proplist[i][0] == prop:
//        proplist[i] = (prop, value)
//        return
//    else:
//      proplist += [(prop, value)]
//      
//  // Replace property named PROP with NEW in PROPLIST.  Often this is called with
//  // with PROP equal to None; the None occurs when a PROP=VALUE clause is expected
//  // but a bare value is supplied.  The context will supply a particular default
//  // property (e.g. 'name') to be used when the property name is omitted, but the
//  // generic code to handle property-value clauses doesn't know what this is.
//  // The surrounding code calls property_name_replace() to fill in the proper name.
//  
//  def property_name_replace(prop, new, proplist):
//    for i in xrange(len(proplist)):
//      if proplist[i][0] == prop:
//        proplist[i] = (new, proplist[i][1])
//  
//
//  //////////////////////////////////////////////////////////////////////////////
//  //                 Extra functions for working with sequences               //
//  //                   Part of the Python docs for itertools                  //
//  //////////////////////////////////////////////////////////////////////////////
//
//  def take(n, iterable):
//      '''Return first n items of the iterable as a list'''
//      return list(islice(iterable, n))
//  
//  def tabulate(function, start=0):
//      '''Return function(0), function(1), ...'''
//      return imap(function, count(start))
//  
//  def consume(iterator, n):
//      '''Advance the iterator n-steps ahead. If n is none, consume entirely.'''
//      // Use functions that consume iterators at C speed.
//      if n is None:
//          // feed the entire iterator into a zero-length deque
//          collections.deque(iterator, maxlen=0)
//      else:
//          // advance to the empty slice starting at position n
//          next(islice(iterator, n, n), None)
//  
//  def nth(iterable, n, default=None):
//      '''Returns the nth item or a default value'''
//      return next(islice(iterable, n, None), default)
//  
//  def quantify(iterable, pred=bool):
//      '''Count how many times the predicate is true'''
//      return sum(imap(pred, iterable))
//  
//  def padnone(iterable):
//      '''Returns the sequence elements and then returns None indefinitely.
//  
//      Useful for emulating the behavior of the built-in map() function.
//      '''
//      return chain(iterable, repeat(None))
//  
//  def ncycles(iterable, n):
//      '''Returns the sequence elements n times'''
//      return chain.from_iterable(repeat(tuple(iterable), n))
//  
//  def dotproduct(vec1, vec2):
//      return sum(imap(operator.mul, vec1, vec2))
//  
//  def flatten(listOfLists):
//      '''Flatten one level of nesting'''
//      return chain.from_iterable(listOfLists)
//  
//  def repeatfunc(func, times=None, *args):
//      '''Repeat calls to func with specified arguments.
//  
//      Example:  repeatfunc(random.random)
//      '''
//      if times is None:
//          return starmap(func, repeat(args))
//      return starmap(func, repeat(args, times))
//  
//  def pairwise(iterable):
//      '''s -> (s0,s1), (s1,s2), (s2, s3), ...'''
//      a, b = tee(iterable)
//      next(b, None)
//      return izip(a, b)
//  
//  def grouper(n, iterable, fillvalue=None):
//      '''grouper(3, "ABCDEFG", "x") --> ABC DEF Gxx'''
//      args = [iter(iterable)] * n
//      return izip_longest(fillvalue=fillvalue, *args)
//  
//  def roundrobin(*iterables):
//      '''roundrobin("ABC", "D", "EF") --> A D E B F C'''
//      // Recipe credited to George Sakkis
//      pending = len(iterables)
//      nexts = cycle(iter(it).next for it in iterables)
//      while pending:
//          try:
//              for next in nexts:
//                  yield next()
//          except StopIteration:
//              pending -= 1
//              nexts = cycle(islice(nexts, pending))
//  
//  def powerset(iterable):
//      '''powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)'''
//      s = list(iterable)
//      return chain.from_iterable(combinations(s, r) for r in range(len(s)+1))
//  
//  def unique_everseen(iterable, key=None):
//      '''List unique elements, preserving order. Remember all elements ever seen.'''
//      // unique_everseen("AAAABBBCCDAABBB") --> A B C D
//      // unique_everseen("ABBCcAD", str.lower) --> A B C D
//      seen = set()
//      seen_add = seen.add
//      if key is None:
//          for element in ifilterfalse(seen.__contains__, iterable):
//              seen_add(element)
//              yield element
//      else:
//          for element in iterable:
//              k = key(element)
//              if k not in seen:
//                  seen_add(k)
//                  yield element
//  
//  def unique_justseen(iterable, key=None):
//      '''List unique elements, preserving order. Remember only the element just seen.'''
//      // unique_justseen("AAAABBBCCDAABBB") --> A B C D A B
//      // unique_justseen("ABBCcAD", str.lower) --> A B C A D
//      return imap(next, imap(itemgetter(1), groupby(iterable, key)))
//  
//  def iter_except(func, exception, first=None):
//      ''' Call a function repeatedly until an exception is raised.
//  
//      Converts a call-until-exception interface to an iterator interface.
//      Like __builtin__.iter(func, sentinel) but uses an exception instead
//      of a sentinel to end the loop.
//  
//      Examples:
//          bsddbiter = iter_except(db.next, bsddb.error, db.first)
//          heapiter = iter_except(functools.partial(heappop, h), IndexError)
//          dictiter = iter_except(d.popitem, KeyError)
//          dequeiter = iter_except(d.popleft, IndexError)
//          queueiter = iter_except(q.get_nowait, Queue.Empty)
//          setiter = iter_except(s.pop, KeyError)
//  
//      '''
//      try:
//          if first is not None:
//              yield first()
//          while 1:
//              yield func()
//      except exception:
//          pass
//  
//  def random_product(*args, **kwds):
//      '''Random selection from itertools.product(*args, **kwds)'''
//      pools = map(tuple, args) * kwds.get("repeat", 1)
//      return tuple(random.choice(pool) for pool in pools)
//  
//  def random_permutation(iterable, r=None):
//      '''Random selection from itertools.permutations(iterable, r)'''
//      pool = tuple(iterable)
//      r = len(pool) if r is None else r
//      return tuple(random.sample(pool, r))
//  
//  def random_combination(iterable, r):
//      '''Random selection from itertools.combinations(iterable, r)'''
//      pool = tuple(iterable)
//      n = len(pool)
//      indices = sorted(random.sample(xrange(n), r))
//      return tuple(pool[i] for i in indices)
//  
//  def random_combination_with_replacement(iterable, r):
//      '''Random selection from itertools.combinations_with_replacement(iterable, r)'''
//      pool = tuple(iterable)
//      n = len(pool)
//      indices = sorted(random.randrange(n) for i in xrange(r))
//      return tuple(pool[i] for i in indices)
//
}
