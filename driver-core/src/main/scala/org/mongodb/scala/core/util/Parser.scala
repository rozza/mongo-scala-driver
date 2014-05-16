/**
 * Copyright (c) 2002-2013 EPFL
 * Copyright (c) 2011-2013 Typesafe, Inc.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimer.
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *       this list of conditions and the following disclaimer in the documentation
 *       and/or other materials provided with the distribution.
 *  * Neither the name of the EPFL nor the names of its contributors
 *       may be used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.mongodb.scala.core.util

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

import org.mongodb.scala.core.util.bson._

import org.bson.types.ObjectId
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader._
import scala.Some
import org.mongodb.scala.core.util.bson.BsonDocument

object BsonParser extends BsonParser {
  /**
   * Parse the given `BSON` string and return a `BsonDocument`.
   *
   * @param input the given `Bson` string.
   * @return      an optional `BsonDocument` element.
   */
  def parse(input : String) : Option[BsonDocument] =
    phrase(root)(new lexical.Scanner(input)) match {
      case Success(result, _) => Some(result.toBson)
      case _ => None
    }
}

class BsonParser extends StdTokenParsers with ImplicitConversions {
  // Fill in abstract defs
  type Tokens = MongoDBLexer
  val lexical = new Tokens

  // Configure lexical parsing
  lexical.reserved ++= List("true", "false", "null")
  lexical.delimiters ++= List("{", "}", "[", "]", ":", ",")

  // Define the grammar
  def root       = bsonObj
  def bsonObj    = "{" ~> repsep(objEntry, ",") <~ "}" ^^ { case vals : List[_] => Map(vals : _*) }
  def bsonArray  = "[" ~> repsep(value, ",") <~ "]" ^^ { case vals : List[_] => vals }
  def objEntry   = stringVal ~ (":" ~> value) ^^ { case x ~ y => (x, y) }
  def value: Parser[Any] = (bsonObj | bsonArray | objectIdVal | intVal | doubleVal | "true" ^^^ true | "false" ^^^ false | "null" ^^^ null | stringVal )
  def stringVal  = accept("string", { case lexical.StringLit(n) => n} )
  def intVal     = accept("number", { case lexical.NumericLit(n) if !n.contains(".") => n.toInt })
  def doubleVal  = accept("number", { case lexical.NumericLit(n) if n.contains(".") => n.toDouble })
  def objectIdVal =accept("objectId", { case lexical.ObjectIdLit(n) => new ObjectId(n)} )
}

class MongoDBLexer extends StdLexical with ImplicitConversions {

  override type Elem = Char
  override def token: Parser[Token] =
  //( '\"' ~ rep(charSeq | letter) ~ '\"' ^^ lift(StringLit)
    ( string ^^ StringLit
      | repN(24, hexDigit ) ^^ { case hexes => ObjectIdLit(hexes mkString "") }
      | number ~ letter ^^ { case n ~ l => ErrorToken("Invalid number format : " + n + l) }
      | '-' ~> whitespace ~ number ~ letter ^^ { case ws ~ num ~ l => ErrorToken("Invalid number format : -" + num + l) }
      | '-' ~> whitespace ~ number ^^ { case ws ~ num => NumericLit("-" + num) }
      | number ^^ NumericLit
      | EofCh ^^^ EOF
      | delim
      | '\"' ~> failure("Unterminated string")
      | rep(letter) ^^ checkKeyword
      | failure("Illegal character")
      )

  def checkKeyword(xs : List[Any]) = {
    val strRep = xs mkString ""
    if (reserved contains strRep) Keyword(strRep) else ErrorToken("Not a keyword: " + strRep)
  }

  /** A string is a collection of zero or more Unicode characters, wrapped in
    *  double quotes, using backslash escapes (cf. http://www.json.org/).
    */
  def string = '\"' ~> rep(charSeq | chrExcept('\"', '\n', EofCh)) <~ '\"' ^^ { _ mkString "" }

  override def whitespace = rep(whitespaceChar)

  def number = intPart ~ opt(fracPart) ~ opt(expPart) ^^ { case i ~ f ~ e =>
    i + optString(".", f) + optString("", e)
  }
  def intPart = zero | intList
  def intList = nonzero ~ rep(digit) ^^ {case x ~ y => (x :: y) mkString ""}
  def fracPart = '.' ~> rep(digit) ^^ { _ mkString "" }
  def expPart = exponent ~ opt(sign) ~ rep1(digit) ^^ { case e ~ s ~ d =>
    e + optString("", s) + d.mkString("")
  }

  private def optString[A](pre: String, a: Option[A]) = a match {
    case Some(x) => pre + x.toString
    case None => ""
  }

  def zero: Parser[String] = '0' ^^^ "0"
  def nonzero = elem("nonzero digit", d => d.isDigit && d != '0')
  def exponent = elem("exponent character", d => d == 'e' || d == 'E')
  def sign = elem("sign character", d => d == '-' || d == '+')

  def charSeq: Parser[String] =
    ('\\' ~ '\"' ^^^ "\""
      |'\\' ~ '\\' ^^^ "\\"
      |'\\' ~ '/'  ^^^ "/"
      |'\\' ~ 'b'  ^^^ "\b"
      |'\\' ~ 'f'  ^^^ "\f"
      |'\\' ~ 'n'  ^^^ "\n"
      |'\\' ~ 'r'  ^^^ "\r"
      |'\\' ~ 't'  ^^^ "\t"
      |'\\' ~> 'u' ~> unicodeBlock)

  val hexDigits = Set[Char]() ++ "0123456789abcdefABCDEF".toArray
  def hexDigit = elem("hex digit", hexDigits.contains(_))

  private def unicodeBlock = hexDigit ~ hexDigit ~ hexDigit ~ hexDigit ^^ {
    case a ~ b ~ c ~ d =>
      new String(Array(Integer.parseInt(List(a, b, c, d) mkString "", 16)), 0, 1)
  }

  /** The class of numeric literal tokens */
  case class ObjectIdLit(chars: String) extends Token {
    override def toString = chars
  }

}
