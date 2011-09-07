  object Re {
    def unapplySeq(x:Tuple2[String, String]) = {
      val (re, str) = x
      re.r.unapplySeq(str)
    }
  }

  ("foo (.*)", "foo bar") match {
    case Re(x) => println("matched 1 %s" format x)
    case _ => println("no match 1")
  }

  ("bar (.*)", "foo bar") match {
    case Re(x) => println("matched 2 %s" format x)
    case _ => println("no match 2")
  }

