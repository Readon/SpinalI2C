package spinal

package object i2c {
  def camelToSnake(s: String): String = {
    val regex = "([a-z])([A-Z])".r
    regex.replaceAllIn(s, "$1_$2").toLowerCase
  }

  def replacePrefixWithMap(str: String, prefixMap: Map[String, String]): String = {
    prefixMap.find { case (prefix, _) => str.startsWith(prefix) } match {
      case Some((prefix, replacement)) => replacement + str.drop(prefix.length)
      case None                        => str
    }
  }

  def replaceSuffixWithMap(str: String, suffixMap: Map[String, String]): String = {
    suffixMap.find { case (suffix, _) => str.endsWith(suffix) } match {
      case Some((suffix, replacement)) => str.dropRight(suffix.length) + replacement
      case None                        => str
    }
  }
}
