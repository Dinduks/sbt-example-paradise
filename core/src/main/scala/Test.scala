@hello
object Test extends App {
  case class C1()
  case class C2(foo: C1 = C1(), bar: Int, baz: String = "hello")
  println(hello)
}
//
//case class C(x: Int = 2, y: String, z: Boolean = true)(t: String = "hello")
//object Test1 extends App {
//  println(Macros.extractor[C])
//}