package opennlp.locamedia
import org.clapper.argot._
import util.control.Breaks._
import collection.mutable

package object locamedia {
  implicit def convertInt(rawval:String, op:OptionSingle[Int]) = {
    try { rawval.toInt }
    catch {
      case e: NumberFormatException =>
        throw new ArgotConversionException(
          "Cannot convert argument \"" + rawval + "\" to a number."
        )
    }
  }

  implicit def convertDouble(rawval:String, op:OptionSingle[Double]) = {
    try { rawval.toDouble }
    catch {
      case e: NumberFormatException =>
        throw new ArgotConversionException(
          "Cannot convert argument \"" + rawval + "\" to a number."
        )
    }
  }

  implicit def convertString(rawval:String, op:OptionSingle[String]) = {
    rawval
  }
}

class InvalidChoiceException(str:String) extends ArgotConversionException(str) {}

object OptionParser {
  def controllingOpt(opt:Seq[String]):String = {
    assert(opt.length > 0)
    for (o <- opt) {
      if (o.length > 1) return o
    }
    return opt(0)
  }

  def computeMetavar(metavar:String, opt:Seq[String]) = {
    if (metavar != null) metavar
    else controllingOpt(opt).toUpperCase
  }

  def nonNullVals(opt1:String, opt2:String, opt3:String, opt4:String,
                  opt5:String, opt6:String, opt7:String, opt8:String,
                  opt9:String) = {
    val retval =
      Seq(opt1, opt2, opt3, opt4, opt5, opt6, opt7, opt8, opt9) filter
        (_ != null)
    if (retval.length == 0)
      throw new IllegalArgumentException("Need to specify at least one command-line option")
    retval
  }
    
  def checkChoices[T](converted:T, choices:Seq[T], canon:Map[T,T]) = {
    var retval = converted
    if (canon != null)
      retval = canon.getOrElse(retval, retval)
    if (choices == null || (choices contains retval)) retval
    else {
      // Mapping for all options, listing alternative 
      // FIXME: Implement this; should list choices along with non-canonical
      // versions of them
      val allopts = mutable.Map[String, mutable.Seq[String]]()

      throw new InvalidChoiceException("Choice '%s' not one of the recognized choices: %s" format (retval, choices))
    }
  }
}

abstract class Option[T] {
  def value:T
  def apply() = value
}

class OptionSingle[T](default:T) extends Option[T] {
  var wrap:SingleValueOption[T] = null
  def value = {
    wrap.value match {
      case Some(x) => x
      case None => default
    }
  }
  def specified = (wrap != null && wrap.value != None)
}

class OptionMulti[T](default:T) extends Option[Seq[T]] {
  var wrap:MultiValueOption[T] = null
  val wrapSingle = new OptionSingle[T](default)
  def value = wrap.value
  def specified = (wrap != null && wrap.value != None)
}

class OptionParser(prog:String) {
  import OptionParser._
  val op = new ArgotParser(prog)
  val opts = mutable.Map[String, AnyRef]()

  def optionSeq[T](opt:Seq[String],
                   default:T=null.asInstanceOf[T],
                   metavar:String=null,
                   choices:Seq[T]=null,
                   canonicalize:Map[T, T]=null,
                   help:String="")
                  (implicit convert: (String, OptionSingle[T]) => T) = {
    val control = controllingOpt(opt)
    if (opts contains control) opts(control).asInstanceOf[Option[T]].value
    else {
      val met2 = computeMetavar(metavar, opt)
      val option = new OptionSingle(default)
      option.wrap =
        op.option[T](opt.toList, met2, help) {
          (rawval:String, op:SingleValueOption[T]) => {
            val converted = convert(rawval, option)
            checkChoices(converted, choices, canonicalize)
          }
        }
      opts(control) = option.asInstanceOf[AnyRef]
      null.asInstanceOf[T]
    }
  }

  def option[T](opt1:String, opt2:String=null, opt3:String=null,
                opt4:String=null, opt5:String=null, opt6:String=null,
                opt7:String=null, opt8:String=null, opt9:String=null,
                default:T=null.asInstanceOf[T],
                metavar:String=null,
                choices:Seq[T]=null,
                canonicalize:Map[T, T]=null,
                help:String="")
               (implicit convert: (String, OptionSingle[T]) => T) = {
    optionSeq[T](nonNullVals(opt1, opt2, opt3, opt4, opt5, opt6, opt7, opt8,
                             opt9),
                 metavar=metavar, default=default, choices=choices,
                 canonicalize=canonicalize, help=help)(convert)
  }

  def multiOptionSeq[T](opt:Seq[String],
                   default:T=null.asInstanceOf[T],
                   metavar:String=null,
                   choices:Seq[T]=null,
                   canonicalize:Map[T, T]=null,
                   help:String="")
                  (implicit convert: (String, OptionSingle[T]) => T) = {
    val control = controllingOpt(opt)
    if (opts contains control) opts(control).asInstanceOf[Option[Seq[T]]].value
    else {
      val met2 = computeMetavar(metavar, opt)
      val option = new OptionMulti(default)
      option.wrap =
        op.multiOption[T](opt.toList, met2, help) {
          (rawval:String, op:MultiValueOption[T]) => {
            val converted = convert(rawval, option.wrapSingle)
            checkChoices(converted, choices, canonicalize)
          }
        }
      opts(control) = option.asInstanceOf[AnyRef]
      null.asInstanceOf[Seq[T]]
    }
  }

  def multiOption[T](opt1:String, opt2:String=null, opt3:String=null,
                opt4:String=null, opt5:String=null, opt6:String=null,
                opt7:String=null, opt8:String=null, opt9:String=null,
                default:T=null.asInstanceOf[T],
                metavar:String=null,
                choices:Seq[T]=null,
                canonicalize:Map[T, T]=null,
                help:String="")
               (implicit convert: (String, OptionSingle[T]) => T) = {
    multiOptionSeq[T](nonNullVals(opt1, opt2, opt3, opt4, opt5, opt6, opt7, opt8,
                             opt9),
                 metavar=metavar, default=default, choices=choices,
                 canonicalize=canonicalize, help=help)(convert)
  }

  def parse(obj:AnyRef, args:Seq[String]) {
    val objargs = obj.getClass.getDeclaredMethods
    for (arg <- objargs) {
      arg.invoke(obj)
    }
    op.parse(args.toList)
  }
}

object TestOpts extends App {
  val op = new OptionParser("test")
  object Opts {
    import locamedia._
    def foo = op.option[Int]("foo", default=5)
    def bar = op.option[String]("bar", default="fuck")
    def baz = op.multiOption[String]("baz")
    def bat = op.multiOption[Int]("bat")
    def blop = op.option[String]("blop", choices=Seq("mene", "tekel", "upharsin"), canonicalize=Map()
    def blop2 = op.multiOption[String]("blop2", choices=Seq("mene", "tekel", "upharsin"))
  }
  // op.parse(Opts, List("--foo", "7"))
  op.parse(Opts, args)
  println("foo: %s" format Opts.foo)
  println("bar: %s" format Opts.bar)
  println("baz: %s" format Opts.baz)
  println("bat: %s" format Opts.bat)
  println("blop: %s" format Opts.blop)
  println("blop2: %s" format Opts.blop2)
}

