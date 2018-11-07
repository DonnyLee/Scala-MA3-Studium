println("Henlo Warldo")

type IntInt = Int => Int
val square = (x:Int) => x*x
val compose:(IntInt, IntInt) => IntInt =
  (f,g) => {x => f(g(x))}
println(square(square(square(2))))

val numbers = List(1,2,3)
println(numbers)

val strlen: String => Int
println(strlen("henlo"))


