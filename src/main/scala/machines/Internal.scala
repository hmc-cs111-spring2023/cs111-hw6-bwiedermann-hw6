package machines

import regex._
import dfa._

//////////////////////////////////////////////////////////////////////////////
// Implicit conversions
//////////////////////////////////////////////////////////////////////////////

// Char ↦ RegularLanguage
given Conversion[Char, RegularLanguage] = Character(_)

// String ↦ RegularLanguage
given Conversion[String, RegularLanguage] = s =>
  s.map(Character(_)).reduce(Concat(_, _))

// RegularLanguage ↦ DFA
given Conversion[RegularLanguage, DFA] = r => r.toDFA(using chars(r))

//////////////////////////////////////////////////////////////////////////////
// Extensions
//////////////////////////////////////////////////////////////////////////////
extension (r: RegularLanguage)

  def ||(other: RegularLanguage) = Union(r, other)

  def ~(other: RegularLanguage) = Concat(r, other)

  def <*> = Star(r)

  def <+> = Concat(r, Star(r))

  // allow the end user to write things such as r{3}
  def apply(n: Int): RegularLanguage =
    if n == 0 then Epsilon
    else Concat(r, r(n - 1))

  // generate a DFA, based on a contextual parameter for the alphabet
  def toDFA(using alphabet: Set[Char]): DFA = regexToDFA(r, alphabet)

//////////////////////////////////////////////////////////////////////////////
// Helper functions
//////////////////////////////////////////////////////////////////////////////

/** Extract the characters used in the regular expression. */
private def chars(r: RegularLanguage): Set[Char] = r match
  case Empty          => Set.empty
  case Epsilon        => Set.empty
  case Character(c)   => Set(c)
  case Union(r1, r2)  => chars(r1) ++ chars(r2)
  case Concat(r1, r2) => chars(r1) ++ chars(r2)
  case Star(r)        => chars(r)
