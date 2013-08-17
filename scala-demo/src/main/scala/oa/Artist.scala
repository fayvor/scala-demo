package oa

case class Particle(name: String)

object TwitterSource {
  def particles = List("twitter_p1", "twitter_p2", "twitter_p3", "twitter_p4").map(new Particle(_)).iterator
}

object FacebookSource {
  def videoParticles = List("facebook_video_p1", "facebook_video_p2", "facebook_video_p3").map(new Particle(_)).iterator
  def photoParticles = List("facebook_photo_p1", "facebook_photo_p2", "facebook_photo_p3").map(new Particle(_)).iterator
  
  def particles = videoParticles.zip(photoParticles).flatMap(x => List(x._1, x._2)) 
}

object Artist {

  def particles = TwitterSource.particles.zip(FacebookSource.particles).flatMap(x => List(x._1, x._2))
  
  def main(args: Array[String]) {
    particles.take(8).foreach(x => println(x.name))
  }
  
    def stepBy[T](n: Int) = {
    var i = 0
    (myList: List[T]) => {
      val ret = List(i)
      i = i + n
      ret
    }
  }
}