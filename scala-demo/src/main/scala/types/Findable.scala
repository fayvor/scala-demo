package types

trait Anchor {
  type Id
}

trait Findable {
 type Id
 type DAO
}

trait Finder[T <: Findable] {
 type Id = T#Id
 type DAO = T#DAO

 val dataSource: DAO

 def byId(id: Id): Option[T]
 def byAnchorId(id: Anchor#Id, limit: Int, offset: Int): Iterator[T]
 def byIds(ids: List[Id]): Iterator[T]
}

trait AsyncFinder[T <: Findable] {
 val finder: Finder[T]

 def byId[T <: Findable](id: T#Id): Option[T]
}

class Particle extends Findable {
  type Id = Int
  type DAO = String
}

abstract class ParticleFinder extends Finder[Particle] {
  
}