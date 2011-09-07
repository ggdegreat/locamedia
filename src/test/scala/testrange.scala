      println("before z")
      val z = for (x <- 1 to 100) yield {println(x); x}
      println("after z")
      println(z)
      println("before zz")
      val zz = for (x <- Stream.range(1,100)) yield {println(x); x}
      println("after zz")
      println(zz)
      for (y <- zz) {}
      println("before zzz")
      val zzz = for (x <- 1 to 100 view) yield {println(x); x}
      println("after zzz")
      println(zzz)
      for (y <- zzz) {}
