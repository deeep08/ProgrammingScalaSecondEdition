case class Address(no: Int, street: String, city: String)
case class Person(name: String, address: Address)

trait toJsonConverter {
  def toJson(level: Int = 0): String

  var INDENTATION = "    "
  def indentation(level: Int = 0) = {
    (INDENTATION * level, INDENTATION * (level+1))
  }
}

object Address {
  implicit class AddressToJson(address: Address) extends toJsonConverter {
    override def toJson(level: Int = 0): String = {
      var (out, in) = indentation(level)
      s"""{
         |$in"no" : ${address.no},
         |$in"street" : ${address.street}
         |$in"city" : ${address.city}
         |$out}""".stripMargin
    }
  }
}

object Person {
  implicit class PersonToJson(person: Person) extends toJsonConverter {
    override def toJson(level: Int): String = {
      val (out, in) = indentation(level)
      s"""{
         |$in"name" : ${person.name}
         |$in"address" : ${person.address.toJson(1)}
         |$out}""".stripMargin
    }
  }
}

{
  var a = Address(56, "Empire way", "London")
  var p = Person("Deep Bhatnagar", a)

  println(a.toJson())
  println()
  println(p.toJson())

  println(Person.PersonToJson(p).toJson())

  1.toLong
}