package com.knoldus

class TimeStamp {
  def unapply(timeStamp: String) = {
    val parts = timeStamp.split("-|\\s+")
    if (parts.length == 4) Some(parts(0)) else None
  }
}

class Url {
  def apply(protocol: String, domain: String, path: String, params: Map[String, String]) = {
    s"$protocol://$domain$path?" + params.map { case (key, value) => s"$key=$value" }.mkString("&")
  }

  def unapply(url: String) = {
    val part1 = url.split("://")
    val part2 = part1(1).split("/", 2)
    val part3 = part2(1).split("\\?")
    val part4 = part3(1).split("&")
    val part5 = for (i <- part4) yield i.split("=")
    val map = scala.collection.mutable.Map[String, String]()
    for (j <- part5) map.+=(j(0) -> j(1))
    if (part1 != null & part2 != null & part3 != null & map != null) Some(part1(0), part2(0), part3(0), map) else None
  }
}

class Email {
  def unapply(email: String) = {
    if (email.matches("^\\w+@[a-zA-Z_]+?\\.[a-zA-Z]{2,3}$")) {
      val parts = email.split("@")
      if (parts.length == 2) Some(parts(0), parts(1)) else None
    }
    else None
  }
}

object Driver extends App {
  val timeStamp = new TimeStamp
  timeStamp.unapply("07-05-1998 12:00:00")

  val url = new Url
  url.apply("https", "aws.amazon.com", "/console/home", Map("state" -> "hash", "isauthcode" -> "true", "code" -> "112"))
  url.unapply("https://aws.amazon.com/console/home?state=hash&isauthcode=true&code=112")

  val email = new Email
  email.unapply("knol@knoldus.com")
}


