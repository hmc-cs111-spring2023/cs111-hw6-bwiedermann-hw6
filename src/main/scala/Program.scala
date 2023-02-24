import machines.regex._

@main
def main() = {

  //////////////////////////////////////////////////////////////////////////////
  // Part 1
  //////////////////////////////////////////////////////////////////////////////
  //
  // TODO: Make it possible to replace the definitions with:
  //
  //     val zero = '0'
  //     val one  = '1'
  //     etc.
  //

  val zero = Character('0')
  val one = Character('1')
  val two = Character('2')
  val three = Character('3')
  val four = Character('4')
  val five = Character('5')
  val six = Character('6')
  val seven = Character('7')
  val eight = Character('8')
  val nine = Character('9')

  require(zero matches "0")
  require(one matches "1")
  require(two matches "2")
  require(three matches "3")
  require(four matches "4")
  require(five matches "5")
  require(six matches "6")
  require(seven matches "7")
  require(eight matches "8")
  require(nine matches "9")

  //////////////////////////////////////////////////////////////////////////////
  // Part 2
  //////////////////////////////////////////////////////////////////////////////
  //
  // TODO: Make it possible to replace the definition with:
  //
  //     val answer = "42"
  //

  val answer = Concat(four, two)

  require(answer matches "42")

  //////////////////////////////////////////////////////////////////////////////
  // Part 3a
  //////////////////////////////////////////////////////////////////////////////
  //
  // TODO: Make it possible to replace the definition with:
  //
  //    val digit = '0' || '1' || '2' || '3' || '4' || '5' || '6' || '7' || '8' || '9'
  //

  val digit = Union(
    zero,
    Union(
      one,
      Union(
        two,
        Union(
          three,
          Union(four, Union(five, Union(six, Union(seven, Union(eight, nine)))))
        )
      )
    )
  )

  require(digit matches "0")
  require(digit matches "1")
  require(digit matches "2")
  require(digit matches "3")
  require(digit matches "4")
  require(digit matches "5")
  require(digit matches "6")
  require(digit matches "7")
  require(digit matches "8")
  require(digit matches "9")

  //////////////////////////////////////////////////////////////////////////////
  // Part 3b
  //////////////////////////////////////////////////////////////////////////////
  //
  // TODO:  Make it possible to replace the definition with:
  //
  //     val pi = '3' ~ '1' ~ '4'
  //

  val pi = Concat(Character('3'), Concat(Character('1'), Character('4')))

  require(pi matches "314")

  //////////////////////////////////////////////////////////////////////////////
  // Part 3c
  //////////////////////////////////////////////////////////////////////////////
  //
  // TODO:  Make it possible to replace the definition with:
  //
  //     val zeroOrMoreDigits = digit <*>
  //

  val zeroOrMoreDigits = Star(digit)

  require(zeroOrMoreDigits matches "")
  require(zeroOrMoreDigits matches "0")
  require(zeroOrMoreDigits matches "9")
  require(zeroOrMoreDigits matches "09")
  require(zeroOrMoreDigits matches "987651234")

  //////////////////////////////////////////////////////////////////////////////
  // Part 3d
  //////////////////////////////////////////////////////////////////////////////
  //
  // TODO:  Make it possible to replace the definition with:
  //
  //     val number = digit <+>
  //

  val number = Concat(digit, zeroOrMoreDigits)

  require(!(number matches ""))
  require(number matches "0")
  require(number matches "9")
  require(number matches "09")
  require(number matches "987651234")

  //////////////////////////////////////////////////////////////////////////////
  // Part 3e
  //////////////////////////////////////////////////////////////////////////////
  //
  // TODO:  Make it possible to replace the definition with:
  //
  //     val cThree = 'c'{3}
  //

  val cThree = Concat(Character('c'), Concat(Character('c'), Character('c')))

  require(cThree matches "ccc")

  //////////////////////////////////////////////////////////////////////////////
  // Additional pattern
  //////////////////////////////////////////////////////////////////////////////
  //
  // Once you've added all the operators, it should be
  // possible to replace the following several definitions with:
  //
  //    val pattern = "42" || ( ('a' <*>) ~ ('b' <+>) ~ ('c'{3}))
  //

  val aStar = Star(Character('a'))
  val bPlus = Concat(Character('b'), Star(Character('b')))
  val pattern = Union(answer, Concat(aStar, Concat(bPlus, cThree)))

  require(pattern matches "42")
  require(pattern matches "bccc")
  require(pattern matches "abccc")
  require(pattern matches "aabccc")
  require(pattern matches "aabbccc")
  require(pattern matches "aabbbbccc")

  //////////////////////////////////////////////////////////////////////////////
  // Additional pattern
  //////////////////////////////////////////////////////////////////////////////
  //
  // Once you've added all the operators, it should be
  // possible to replace the following several definitions with:
  //
  //    val message = ("hello" <*>) ~ "world"
  //

  val hello = Concat(
    Character('h'),
    Concat(
      Character('e'),
      Concat(Character('l'), Concat(Character('l'), Character('o')))
    )
  )

  val world = Concat(
    Character('w'),
    Concat(
      Character('o'),
      Concat(Character('r'), Concat(Character('l'), Character('d')))
    )
  )

  val message = Concat(Star(hello), world)

  require(message matches "helloworld")
  require(message matches "world")
  require(message matches "hellohelloworld")

  //////////////////////////////////////////////////////////////////////////////
  // Additional pattern
  //////////////////////////////////////////////////////////////////////////////
  //
  // Once you've added all the operators, it should be
  // possible to replace the following several definitions with:
  //
  //    val telNumber = '(' ~ digit{3} ~ ')' ~ digit{3} ~ '-' ~ digit{4}
  //

  val threeDigits = Concat(digit, Concat(digit, digit))
  val fourDigits = Concat(threeDigits, digit)
  val areaCode = Concat(Character('('), Concat(threeDigits, Character(')')))
  val telNumber =
    Concat(areaCode, Concat(threeDigits, Concat(Character('-'), fourDigits)))

  require(telNumber matches "(202)456-1111")

  println("All the tests passed!")
}
