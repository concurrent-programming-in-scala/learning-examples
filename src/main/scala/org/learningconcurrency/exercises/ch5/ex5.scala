package org.learningconcurrency.exercises.ch5

import scala.swing.Panel

/**
  * Implement a parallel Barnes-Hut N-Body simulation algorithm.
  */
object Ex5 extends scala.swing.SimpleSwingApplication {

  import math._

  type Mass = Double
  type Distance = Double
  type Angle = Double

  object Vector {
    val empty = new Vector(Point.empty, 0, 0)
    val x = new Vector(Point.empty, 0, 1)
  }

  case class Vector(point: Point, angle: Angle = 0f, length: Double = 0f) {
    def this(from: Point, to: Point, length: Double) = {
      this(from, from.angle(to), length)
    }

    def this(from: Point, to: Point) = {
      this(from, from.angle(to), from.distance(to))
    }

    def +(other: Vector): Vector = {
      if (other == Vector.empty) {
        this
      } else {
        val newProjectionX = length * cos(angle) + other.length * cos(other.angle)
        val newProjectionY = length * sin(angle) + other.length * sin(other.angle)
        val newPoint = Point(point.x + other.point.x, point.y + other.point.y)
        val res = new Vector(newPoint, newPoint + Point(newProjectionX, newProjectionY))
        res
      }
    }

    def unit = new Vector(Point(point.x / length, point.y / length), angle, 1f)

    def to: Point = Point(point.x + length * cos(angle), point.y + length * sin(angle))

    def /(n: Double): Vector = copy(length = length / n)

    def *(n: Double): Vector = copy(length = length * n)

    def *(other: Vector): Vector = new Vector(point * other.point, point * other.point)

    def rotate(n: Angle): Vector = copy(angle = angle + n)

    //   todo def rotateTo(n: Angle): Vector = copy(angle = angle + n)

    override def toString: String = {
      f"Vector[point: $point, angle: $angle%.3f rad (${toDegrees(angle)}%.1f degrees), length: $length]"
    }
  }

  object Point {
    val empty = Point(0, 0)
  }

  case class Point(x: Double, y: Double) {
    def +(other: Point) =
      Point(x + other.x, y + other.y)

    def *(other: Point) =
      Point(x * other.x, y * other.y)

    def distance(other: Point): Distance =
      sqrt(pow(x - other.x, 2) + pow(y - other.y, 2))

    def angle(other: Point /*, axleVector: Vector = Vector.x*/): Angle = {
      val d = distance(other)
      if (d == 0) {
        0f
      } else {
        // todo?
        //        if (difference == 0) {
        //          0f
        //        } else {
        //          val cos_a = difference / d
        //          assert(cos_a <= 1)
        //          (if (difference < 0) -1 else 1) * acos(cos_a)
        //        }
        //                val d = distance(other)
        // todo файри прислал ФОРМУЛУ
        //                val axleVectorFromPoint = axleVector.copy(point = this, length = distance(other))
        //                val distanceBetweenOtherAndAxleVector = other.distance(axleVectorFromPoint.to)
        //                val vectorBetweenPoints = new Vector(this, other)
        //                val cosA = (axleVectorFromPoint * this) / d * axleVectorFromPoint.length
        //                val r = acos(cosA)
        //                r
        val differenceX = other.x - x
        val differenceY = other.y - y
        (if (differenceY < 0 || differenceY < 0) -1 else 1) *
          acos(differenceX / sqrt(pow(differenceX, 2) + pow(differenceY, 2)))
      }
    }

    //    override def toString: String = f"Point[$x, $y]"
  }

  abstract class Force {
    def vector: Vector

    def apply(body: Body): Body

    def +(other: Force): Force

    override def toString: String = {
      s"${getClass.getSimpleName}[$vector]"
    }
  }

  object Gravity {
    val g = 6.67e-11f

    def apply(actBody: Body, toBody: Body): Gravity = {
      val distance = actBody.position.distance(toBody.position)
      val value = if (distance == 0) {
        0f
      } else {
        g * (actBody.mass * toBody.mass / pow(distance, 2))
      }
      new Gravity(new Vector(toBody.position, actBody.position, value))
    }
  }

  class Gravity(val vector: Vector = Vector.empty) extends Force {
    def apply(body: Body): Body = {
      body.copy(velocity = body.velocity.map(_ + Velocity((vector / body.mass).copy(point = Point.empty))).orElse(Some(Velocity((vector / body.mass).copy(point = Point.empty)))))
    }

    def +(other: Force): Force = {
      ???
    }
  }

  // todo эти силы всегда направлены от тела
  case class Velocity(val vector: Vector = Vector.empty) extends Force {
    def apply(body: Body): Body = {
      body.copy(speed = body.speed.map(_ + new Speed(vector)).orElse(Some(new Speed(vector))))
    }

    override def +(other: Force): Force = {
      new Velocity(vector + other.vector)
    }
  }

  // todo эти силы всегда направлены от тела
  class Speed(val vector: Vector = Vector.empty) extends Force {
    def apply(body: Body): Body = {
      body.copy(position = body.position + vector.to)
    }

    def +(other: Force): Force = {
      new Speed(vector + other.vector)
    }
  }

  object Body {
    def massCenter(bodies: Seq[Body]): Body = {
      bodies.reduce[Body] {
        case (first, second) =>
          val mass = first.mass + second.mass
          val x = (first.position.x * first.mass) + (second.position.x * second.mass) / mass
          val y = (first.position.y * first.mass) + (second.position.y * second.mass) / mass
          static(None, mass, Point(x, y))
      }
    }

    def act(id: Option[Int], mass: Mass, position: Point): Body = {
      Body(id, mass, position, None, None, act = true)
    }

    def static(id: Option[Int], mass: Mass, position: Point): Body = {
      Body(id, mass, position, None, None, act = false)
    }
  }

  case class Body private(id: Option[Int], mass: Mass, position: Point, speed: Option[Force], velocity: Option[Force], act: Boolean = false) {
    override def toString: String = s"Body[id: $id, mass: $mass, position: $position, speed: $speed, velocity: $velocity, act: $act]"
  }

  trait Simulator {
    type State = Seq[Body]

    def bodies: State

    def nextStates(n: Int): Iterator[State]
  }

  class BruteForceNaiveSimulator(override val bodies: Body*) extends Simulator {
    def nextStates(n: Int): Iterator[State] = {
      Iterator.iterate(bodies)(newState).take(n)
    }

    protected def newState(currentState: State): State = {
      val (actBodies, staticBodies) = currentState.partition(_.act)
      val r = actBodies.map(body => {
        val otherBodies = currentState.filterNot(_.id == body.id)
        val forces = otherBodies.map(otherBody => Gravity(otherBody, body))
        newBodyState(body, forces)
      })
      r ++ staticBodies
    }

    protected def newBodyState(body: Body, forces: Seq[Force]): Body = {
      // todo fold or sum?
      //      forces.reduce(_ + _).apply(body)
      val bodyAfterForces = forces
        .filterNot(_.vector == Vector.empty)
        .foldLeft(body) {
          case (b, force) =>
            println(s"Apply $force")
            val res = force(b)
            //            println(s"Body after apply $force:\n $res")
            res
        }
      println(s"Body after forces: $bodyAfterForces")
      val bodyAfterVelocity = bodyAfterForces.velocity.map(_.apply(bodyAfterForces)).getOrElse(bodyAfterForces)
      println(s"Body after velocity ${bodyAfterForces.velocity}: $bodyAfterVelocity")
      val bodyAfterSpeed = bodyAfterVelocity.speed.map(s => s.apply(bodyAfterVelocity)).getOrElse(bodyAfterVelocity)
      println(s"Body after speed ${bodyAfterVelocity.speed}: $bodyAfterSpeed")
      bodyAfterSpeed
    }
  }

  val earth = Body.act(Some(1), "5.972e24".toDouble, Point(0, 0))
  val sun = Body.act(Some(2), "1.989e30".toDouble, Point("1.496e11".toDouble, 0))
  val simulator = new BruteForceNaiveSimulator(earth, sun)

  class SimulatorStatePanel(simulator: Simulator) extends Panel {
    private val statesIterator = simulator.nextStates(Int.MaxValue)
    private var currentStateOpt: Option[Simulator#State] = None

    private def scaleF: Double = "1.496e12".toDouble / size.width

    private def scale(value: Double): Int = (value / scaleF).toInt

    private val maxRadius = 10
    private var currentStateNumberOpt: Option[Int] = None

    listenTo(mouse.clicks)
    nextState()

    def nextState(): Unit = {
      val by = 1000
      currentStateOpt = Some(statesIterator.drop(by).next())
      currentStateNumberOpt = Some(currentStateNumberOpt.getOrElse(0) + by)
      println(s"New step: $currentStateOpt")
      repaint()
    }

    override protected def paintComponent(g: swing.Graphics2D): Unit = {
      val maxMass = currentStateOpt.map(_.map(_.mass).max).getOrElse(0L)
      currentStateOpt match {
        case Some(currentState) => currentState.foreach(body => {
          //        val radius = (maxRadius * (body.mass / maxMass)).toInt
          val radius = maxRadius
          val x = scale(body.position.x) - radius
          val y = scale(body.position.y) - radius
          println(s"Draw x: $x y: $y radius: $radius")
          g.drawOval(x, y, 2 * radius, 2 * radius)
          g.drawString(body.id.get.toString, 6 + x + (g.getFontMetrics.getWidths.head / 2).toFloat, y + g.getFontMetrics.getHeight.toFloat)
          g.drawString(s"Step ${currentStateNumberOpt.get}", 10, 100)
        })
        case None =>
          println("There're no state")
      }
    }
  }

  // todo вынести
  // todo доделать задание и все из этой главы
  import rx.lang.scala._

  import scala.swing._
  import scala.swing.event._

  def top = new MainFrame {
    title = "Swing Observables"

    //      val button = new Button {
    //        text = "Click"
    //      }

    val panel = new SimulatorStatePanel(simulator)
    //      listenTo(panel.mouse.clicks)
    contents = panel

    size = new Dimension(800, 600)

    val panelClicks = Observable[Unit] { sub =>
      panel.reactions += {
        case e: MouseClicked => sub.onNext(())
      }
    }

    panelClicks.subscribe(_ => {
      panel.nextState
    })
  }
}
