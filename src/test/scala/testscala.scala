class Foo(bar:Int, baz:Int=0) {
}

class Bar {
  val x = new Foo(1)
  val y = new Foo(1,2)
}

class Baz
extends Bar {
    val z = 2
}
