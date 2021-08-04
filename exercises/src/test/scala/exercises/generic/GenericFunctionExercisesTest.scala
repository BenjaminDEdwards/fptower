package exercises.generic

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import exercises.generic.GenericFunctionExercises._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Try
import java.sql.Date

class GenericFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  ////////////////////
  // Exercise 1: Pair
  ////////////////////

  test("Pair swap") {
    assert( Pair("John", "Doe").swap == Pair("Doe", "John") )
    assert( Pair(0,1).swap == Pair(1,0) )
  }

  test("Pair map") {
    assert( Pair("John", "Doe").map(_.length) == Pair(4,3) )

    assert( Pair( 2,3 ).map( (a: Int) => a + 1 ) == Pair( 3,4) )

    // Answe on fp-tower - we dont need a different list
    assert( Pair(0,1).map(identity) == Pair(0,1) )
  }

  test("Pair decoded") {}

  test("Pair zipWith") {
    assert( Pair(0, 2).zipWith(Pair(3, 4))( (x, y) => { x + y } ) == Pair(3, 6))
    //Pair(2, 3).zipWith( Pair("Hello ", "World ") )(replicate) == Pair("Hello Hello ", "World World World ")
  }

  test("Pair productNames") {}

  ////////////////////////////
  // Exercise 2: Predicate
  ////////////////////////////

  test("Predicate &&") {
    assert ( (isEven && isPositive)(12) == true  )
    assert ( (isEven && isPositive)(11) == false )
    assert ( (isEven && isPositive)(-4) == false )
    assert ( (isEven && isPositive)(-7) == false )
  } 

  test("Predicate && for all Integers") {
    forAll {
      ( number: Int, function1: Int => Boolean, function2: Int => Boolean ) => {
        val predicate1 = Predicate(function1)
        val predicate2 = Predicate(function2)

        def always_false[A] = Predicate[A]( ( value : A ) => false )

        // These two are the same
        assert( (predicate1 && always_false)(number) == false )
        assert( predicate2.&&(always_false).eval(number) == false )

        //assert( (predicate1 && predicate2 )(number) == function1(number) && function2(number) )
      }
    }
  }

  test("Predicate ||") {
    assert((isEven || isPositive)(12) == true  )
    assert((isEven || isPositive)(11) == true  ) 
    assert((isEven || isPositive)(-4) == true  )
    assert((isEven || isPositive)(-7) == false )
  }

  test("Predicate || forall") {
    forAll {
      (number: Int, function1: Int => Boolean) => {
        def predicate1 = Predicate(function1)
        def false_predicate[A] = Predicate[A]( ( value: A) => false ) 
        assert(
          (false_predicate || predicate1)(number) == function1(number)
        )
        assert(
          (predicate1 || false_predicate)(number) == function1(number)
        )
      }
    }
  }

  test("Predicate flip") {
    assert(isEven.flip(11) == true)
    forAll {
      ( number: Int, function: Int => Boolean ) => {
        def predicate1 = Predicate(function)
        assert(
          predicate1.flip(number) == !function(number)
        )
      }
    }
  }

  test ("isValidUser") {
    assert( isValidUser(User("John", 20)) == true  ) // 
    assert( isValidUser(User("John", 17)) == false ) // user is not an adult
    assert( isValidUser(User("john", 20)) == false ) // name is not capitalized
    assert( isValidUser(User("x"   , 23)) == false ) // name is too small
  }


  ////////////////////////////
  // Exercise 3: JsonDecoder
  ////////////////////////////

  test("JsonDecoder UserId") {
    forAll {
      (number: Int) => {
        assert(userIdDecoder.decode(number.toString()) == UserId(number))
      }
    }
    forAll {
      (str: String) => {
        assert( Try( userIdDecoder.decode(str)).isFailure )
      }
    }
  }

  test("JsonDecoder LocalDate") {
    assert(localDateDecoder.decode("\"2020-03-26\"") == LocalDate.of(2020,3,26))
    assert( Try(localDateDecoder.decode("hello")).isFailure  )
    assert( Try(localDateDecoder.decode("2020-03-26")).isFailure )
    forAll(localDateGenerator) {
      (test: LocalDate) => {
        val string = "\"" + DateTimeFormatter.ISO_LOCAL_DATE.format(test) + "\""
        assert( localDateDecoder.decode(string) == test )
      }
    }
  }

  val localDateGenerator: Gen[LocalDate] = Gen.choose(LocalDate.MIN.toEpochDay(), LocalDate.MAX.toEpochDay() ).map( (f:Long) => { LocalDate.ofEpochDay(f) } )
  //implicit val arbitaryDate: Arbitrary[LocalDate] = Arbitrary(localDateGenerator)

  test("JsonDecoder weirdLocalDateDecoder") {}

}
