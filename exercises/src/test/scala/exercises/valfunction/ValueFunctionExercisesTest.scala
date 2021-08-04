package exercises.valfunction

import exercises.valfunction.ValueFunctionExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ValueFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  /////////////////////////////////////////////////////
  // Exercise 1: String API with higher-order functions
  /////////////////////////////////////////////////////

  // replace `ignore` by `test` to enable the test
  test("selectDigits examples") {
    assert(selectDigits("hello4world-80") == "480")
    assert(selectDigits("welcome") == "")
  }

  test( "selectDigitsNeverLetters") {
    forAll { (text:String)  =>
      assert(  selectDigits(text).filter( (c: Char) => c.isLetter ).length   == 0  )
    }
  }

  test( "selettDiggitsSelectsAllDigits" ) {

    forAll { (text:String) =>
      val expected_number = text.filter( _.isDigit ).length;
      val actual_output = selectDigits(text)
      assert( expected_number == actual_output.length() )
    }
  }
  // replace `ignore` by `test` to enable the test

  // BDE NOTE: the examples given are somewhat limited. Go throgh what makes a sensible forAll test
  ignore("selectDigits length is smaller") {
    forAll { (text: String) =>
      assert(selectDigits(text).length <= text.length)
    }
  }

  test( "Secret by example") {
    assert( secret("Secret") == "******" ) 
  } 

  test( "Secret must give back string of same length") {
    forAll { (text: String) =>
      assert( secret(text).length == text.length )
    }
  }

  test( "must be all asterisks") {
    forAll { (text: String) =>
      assert( secret(text).filter( c => c == '*' ).length == text.length )
    }
  }


  // isValidUserNameChar
  test( "- is valid") {
    assert( isValidUsernameCharacter('-') == true )
  }

  test( "^ is invalid") {
    assert( isValidUsernameCharacter('^') == false )
  }

  //isValidUserName

  test( "Invalid Char should always fail") {
    forAll{ ( text: String) =>
      val inCorrectText = text.concat("*")
      assert( isValidUsername(inCorrectText) == false)
    }
  }

  test( "all positive") {
    forAll{ ( x: Int, y: Int, z: Int ) =>
      assert( Point(x.max(0), y.max(0), z.max(0) ).isPositive == true )
    }
  }

  test ( "all zeroe y:s") {
    assert( Point(0, 0,0).isPositive == true ) 
  }

  test( "negative") {
    assert( Point(0,-2,1).isPositive == false )
  }

  test ( "odd number") {
    forAll{ ( x: Int, y: Int, z: Int) =>
      val odd = if ( x % 2 == 0 ) x + 1 else x 
      assert( Point( odd, y, z).isEven == false) 
    }
  }

  test ("forall predicate") {
    Point(1,2,5).forAll(_ == 1) == false
    Point(1,1,1).forAll(_ == 1) == true
  }

  test( "forAll") {
    forAll { (x: Int, y: Int, z: Int, predicate: Int => Boolean ) =>
      assert( Point(x,y,z).forAll(predicate) == List(x,y,z).forall(predicate) )
    }
  }
}


  ///////////////////////
  // Exercise 2: Point
  ///////////////////////



