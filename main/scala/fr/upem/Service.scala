package fr.upem

import java.io.File
import scala.io.Source

object Direction extends Enumeration {
  val N, E, W, S = Value
}
object Commande extends Enumeration {
  val D, G, A = Value
}
case class coordinates(x : Int, y : Int)
case class coordinatesPelouse(cP : coordinates)
case class currentCoordinates(c : coordinates, d : Direction.Value) {
  def print = this.c.x + " " + this.c.y + " " + this.d.toString
}

class Service {


//Déplacement de la tondeuse
    //calculer les nouveaux coordonnees de la tondeuse
  def calcul(curC : coordinates, d : Direction.Value) : coordinates = d match {
    case Direction.N => curC.copy(y = curC.y + 1)
    case Direction.S => curC.copy(y = curC.y - 1)
    case Direction.E => curC.copy(x = curC.y + 1)
    case Direction.W => curC.copy(x = curC.x - 1)
    case _ => curC
  }

  //Pour se deplacer soit a gauche, soit a droite
  def pivot(d : Direction.Value) : (Direction.Value,Direction.Value) = d match{
    case Direction.N => (Direction.E, Direction.W)
    case Direction.E => (Direction.S, Direction.N)
    case Direction.S => (Direction.W, Direction.E)
    case Direction.W => (Direction.N,Direction.S)
    case _ => (d, d)
  }

  //Pour avancer
  def move(i : currentCoordinates, c : Commande.Value) : currentCoordinates = {
    c match {
      case Commande.G => i.copy(d = pivot(i.d)._2)
      case Commande.D => i.copy(d = pivot(i.d)._1)
      case Commande.A => {
        val nc = calcul(i.c,i.d)
        i
      }
      case _ => i
    }
  }
  
  //Parsing du fichier

  def pelouse(file : String) : Option[coordinatesPelouse] = {
    val i = Source.fromFile(file)
    val l = i.getLines().toList
    if(!l.isEmpty) {
      val c = l(0).split(" ")
        while(c.length == 2) {
          Some(coordinates(c(0).toInt,c(1).toInt))
        }
    }
    None
  }

  def position(file : String) : List[currentCoordinates] = {
    val i = Source.fromFile(file)
    val list = i.getLines().toList
    while (!list.isEmpty && list.length >= 3) {
      val lines = list.zipWithIndex.filter{ case (v,k) => (k % 2 != 0 && k > 0)}.map{_._1}
      lines.filter(x => x.split(" ").length == 3).map(l => {
        val p = l.split(" ")
        currentCoordinates(coordinates(p(0).toInt,p(1).toInt),Direction.withName(p(2)))
      })
      }
      List.empty[currentCoordinates]
    }

  def commande(file : String) : List[List[Commande.Value]] = {
    val i = Source.fromFile(file)
    val list = i.getLines().toList
    while (!list.isEmpty && list.length >= 3) {
      val lines = list.zipWithIndex.filter{ case (v,k) => (k % 2 == 0 && k > 0)}.map{_._1}
        val l = lines.map(l => l.toList)
        val cmd = l.map(c => Commande.withName(c.toString))
      }
    List.empty[List[Commande.Value]]
  }
}


