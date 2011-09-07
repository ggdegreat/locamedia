val maximum_longind = 200
val minimum_longind = -200
def testiter(latind1:Int, latind2:Int, longind1:Int, longind2:Int) = {
  for {i <- latind1 to latind2 view
       val it = if (longind1 <= longind2) longind1 to longind2 view
                else (longind1 to maximum_longind view) ++
                     (minimum_longind to longind2 view)
       j <- it
       if (j % 2) == 0
      } yield {println(i, j); (i, j)}
}

println(testiter(3,20,8,21))
println(testiter(3,20,8,21).toList)
println(testiter(3,20,180,-180).toList)
// vim: set ts=4 sw=4 et:
