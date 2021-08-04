
import exercises.generic.GenericFunctionExercises._


val test = Pair("foo","bar")

secret

secret.map( _.reverse )

def decode( list: List[Byte] ) : String = {
  val chars = list.map( _.toChar )
  return chars.mkString
}

secret.map(
  ( list: List[Byte] ) => {  decode(list.reverse) }
).swap

secret.map(
  ( list: List[Byte] ) => { list.map( _.toChar) }
).first