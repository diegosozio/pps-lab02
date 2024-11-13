object Lab02 extends App {
    println("HelloScala")

    // Parte 3A (i) assegnazione funzione letterale
    val positiveLambda: Int => String = (x: Int) => x match {
        case x if x >= 0 => "positive"
        case _ => "negative"
    }

    // Parte 3A (ii) assegnazione di un metodo
    private def positiveLambdaMethod(x: Int): String = x match {
        case x if x >= 0 => "positive"
        case _ => "negative"
    }

    // Parte 3B
    // Implementazione funzione NEG
    val negFunction: (String => Boolean) => (String => Boolean) = (predicate: String => Boolean) => (s: String) => !predicate(s)

    // Implementazione metodo NEG
    def neg(predicate: String => Boolean): String => Boolean = (s: String) => !predicate(s)

    // Test funzione NEG
    val empty: String => Boolean = _ == ""
    val notEmpty = neg(empty)

    println(notEmpty("foo")) // true
    println(notEmpty("")) // false
    println(notEmpty("foo") && !notEmpty("")) // true

    // Parte 3C
    // Rendere NEG generico
    def negGeneric[X](predicate: X => Boolean): X => Boolean = (x: X) => !predicate(x)
    val isEmpty: String => Boolean = _ == ""
    val isNotEmpty = negGeneric(isEmpty)

    println("NegGeneric section:")
    println(isNotEmpty("foo")) // true
    println(isNotEmpty("")) // false
    println(isNotEmpty("foo") && !isNotEmpty("")) // true

    // Parte 4
    println("Parte 4:")
    // Curried
    val p1: Int => Int => Int => Boolean = x => y => z => (x <= y) == (y == z)
    // Not curried
    val p2: (Int, Int, Int) => Boolean = (x, y, z) => (x <= y) == (y == z)

    // Metodo curried
    def p3(x: Int)(y: Int)(z: Int): Boolean = (x <= y) == (y == z)

    // Metodo not curried
    def p4(x: Int, y: Int, z: Int): Boolean = (x <= y) == (y == z)

    // Parte 5
    println("Parte 5:")

    def compose(f: Int => Int, g: Int => Int): Int => Int = {
        (x: Int) => f(g(x))
    }

    // Funzione generica
    def composeG[A, B, C](f: B => C, g: A => B): A => C = {
        (x: A) => f(g(x))
    }

    // Parte 6
    println("Parte 6:")

    def gcd(a: Int, b: Int): Int = {
        var x = a
        var y = b
        while (y != 0) {
            val temp = y
            y = x % y
            x = temp
        }
        x
    }

    println(s"gcd(12, 8) = ${gcd(12, 8)}")
    println(s"gcd(14, 7) = ${gcd(14, 7)}")
    println(s"gcd(48, 18) = ${gcd(48, 18)}")
    println(s"gcd(101, 103) = ${gcd(101, 103)}")

    // Versione ricorsiva di coda
    def gcdRec(a: Int, b: Int): Int = {
        if (b == 0) a
        else gcdRec(b, a % b)
    }

    println(s"gcdRec(12, 8) = ${gcdRec(12, 8)}")
    println(s"gcdRec(14, 7) = ${gcdRec(14, 7)}")
    println(s"gcdRec(48, 18) = ${gcdRec(48, 18)}")
    println(s"gcdRec(101, 103) = ${gcdRec(101, 103)}")

    // Parte 7
    println("Parte 7:")

    // Definizione dell'enum
    sealed trait Shape

    // Definizione dei tipi concreti
    case class Rectangle(width: Double, height: Double) extends Shape
    case class Circle(radius: Double) extends Shape
    case class Square(side: Double) extends Shape

    // Definizione del modulo per calcolare il perimetro
    def perimeter(shape: Shape): Double = shape match {
        case Rectangle(width, height) => 2 * (width + height)
        case Circle(radius) => 2 * Math.PI * radius
        case Square(side) => 4 * side
    }

    // Definizione del metodo per scalare una forma
    def scale(shape: Shape, alpha: Double): Shape = shape match {
        case Rectangle(width, height) => Rectangle(width * alpha, height * alpha)
        case Circle(radius) => Circle(radius * alpha)
        case Square(side) => Square(side * alpha)
    }
}

// Test per il modulo ShapeUtils
object ShapeUtilsTest extends App {
    import Lab02._

    // Test per calcolare il perimetro delle forme
    assert(perimeter(Rectangle(3, 4)) == 14.0)
    assert(perimeter(Circle(2)) == 2 * Math.PI * 2)
    assert(perimeter(Square(2)) == 8.0)

    // Test per scalare le forme
    assert(scale(Rectangle(3, 4), 2) == Rectangle(6, 8))
    assert(scale(Circle(2), 0.5) == Circle(1))
    assert(scale(Square(2), 3) == Square(6))

    println("All tests passed!")
}
