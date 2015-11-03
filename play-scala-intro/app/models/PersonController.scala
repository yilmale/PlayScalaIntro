package controllers

import play.api._
import play.api.mvc._
import play.api.i18n._
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.libs.json.Json
import models._
import dal._
import play.api.libs.json.JsValue

import play.api.libs.json._

import scala.concurrent.{ ExecutionContext, Future }

import javax.inject._


import edu.uci.ics.jung.algorithms.layout.AbstractLayout
import edu.uci.ics.jung.algorithms.layout.FRLayout2
import edu.uci.ics.jung.algorithms.layout.util.Relaxer
import edu.uci.ics.jung.graph.UndirectedSparseMultigraph
import edu.uci.ics.jung.graph.DirectedSparseMultigraph;
import edu.uci.ics.jung.graph.Graph
import edu.uci.ics.jung.graph.ObservableGraph
import edu.uci.ics.jung.graph.util.Graphs
import edu.uci.ics.jung.visualization.VisualizationViewer
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import edu.uci.ics.jung.visualization.renderers.Renderer

import scala.collection.mutable.ListBuffer

case class Location(lat: Double, long: Double)
case class Resident(name: String, age: Int, role: Option[String])
case class MRow(name: String, edgeWeights: Seq[Double])
case class AdjMatrix(name: String, nodes: Seq[String], activations: Seq[Double], adjList: Seq[MRow])
case class Place(name: String, location: Location, residents: Seq[Resident])

class PersonController @Inject() (repo: PersonRepository, val messagesApi: MessagesApi)
                                 (implicit ec: ExecutionContext) extends Controller with I18nSupport{
  var g: Graph[String,Int] = null;
  var og: ObservableGraph[String,Int] =null;
  var ig = Graphs.synchronizedUndirectedGraph[String,Int](new UndirectedSparseMultigraph[String,Int]())
  og = new ObservableGraph[String,Int](ig)   
  g = og
  
  val ACTIVATION = 0.01
  val ACTIVATIONTHRESHOLD = 0.20
  val INITIALTHRESHOLD = 0.5
  val CTHRESHOLD = 0.001 
  val MAXITERATION = 200
  val MAX = 1.0
  val MIN = -1.0
  val DECAYRATE = 0.05
  var activations:Map[String,Double]  = Map()
  var edgeWeights:Map[Int,Double] = Map()
  
  /**
   * The mapping for the person form.
   */
  val personForm: Form[CreatePersonForm] = Form {
    mapping(
      "name" -> nonEmptyText,
      "age" -> number.verifying(min(0), max(140))
    )(CreatePersonForm.apply)(CreatePersonForm.unapply)
  }
  
  implicit val locationWrites = new Writes[Location] {
  def writes(location: Location) = Json.obj(
    "lat" -> location.lat,
    "long" -> location.long
  )
}
  
  implicit val mrowWrites = new Writes[MRow] {
      def writes(mrow: MRow) = Json.obj(
        "name" -> mrow.name,
        "edgeWeights" -> mrow.edgeWeights
      )
  }
  
  implicit val adjmatrixWrites = new Writes[AdjMatrix] {
      def writes(adjmatrix: AdjMatrix) = Json.obj(
        "name" -> adjmatrix.name,
        "nodes" -> adjmatrix.nodes,
        "activations" -> adjmatrix.activations,
        "adjList" -> adjmatrix.adjList
      )
  }
  
  implicit val residentWrites = new Writes[Resident] {
  def writes(resident: Resident) = Json.obj(
    "name" -> resident.name,
    "age" -> resident.age,
    "role" -> resident.role
  )
}

implicit val placeWrites = new Writes[Place] {
  def writes(place: Place) = Json.obj(
    "name" -> place.name,
    "location" -> place.location,
    "residents" -> place.residents)
}


  
  
  def initializeGraphModel() {
      if (g.getVertexCount>0) {
            var V = g.getVertices
            var vertices=V.toArray()
            for (i <- 0 to g.getVertexCount-1) {
                g.removeVertex(vertices(i).asInstanceOf[String])
            }
         } 
        var eC= g.getEdgeCount()
        if (eC>0) {
            for (i <- 0 to eC-1) {
                g.removeEdge(i)
            }
        }
  }

  /**
   * The index action.
   */
  def index = Action {
    Ok(views.html.index(personForm))
  }

  /**
   * The add person action.
   *
   * This is asynchronous, since we're invoking the asynchronous methods on PersonRepository.
   */
  def addPerson = Action.async { implicit request =>
    // Bind the form first, then fold the result, passing a function to handle errors, and a function to handle succes.
    personForm.bindFromRequest.fold(
      // The error function. We return the index page with the error form, which will render the errors.
      // We also wrap the result in a successful future, since this action is synchronous, but we're required to return
      // a future because the person creation function returns a future.
      errorForm => {
        Future.successful(Ok(views.html.index(errorForm)))
      },
      // There were no errors in the from, so create the person.
      person => {
        repo.create(person.name, person.age).map { _ =>
          // If successful, we simply redirect to the index page.
          Redirect(routes.PersonController.index)
        }
      }
    )
  }
  
  def getEntities = Action { 
  Ok("Got request [" + "]")
}


/*def addEntity = Action { implicit request =>
  val body: AnyContent = request.body
  val textBody: Option[String] = body.asText 
  textBody.map { text =>
    Ok("Got: " + text)
  }.getOrElse {
    BadRequest("Expecting text/plain request body")  
  }
  
}*/


def addEntity = Action { implicit request =>
  System.out.println(request.body)
  val name = request.body.asFormUrlEncoded.get("name")
  
  System.out.println(name)
  System.out.println(name.length)
  System.out.println(name(0))  
  var mystr:String = new String(name(0))
  System.out.println("Derived string is " + mystr)
  val age = request.body.asFormUrlEncoded.get("age[0][]")
  System.out.println(age)
  //Ok("Got request [" + request.body + "]")
  //var pt : Point = new Point()
  val myStock = new Stock("GOOG", 650.0)
  Ok(Json.toJson(myStock))
}

def addEntityJSON = Action { implicit request =>
  System.out.println(request.body)
  val myModel = request.body.asFormUrlEncoded.get("model")
  var mystr:String = new String(myModel(0))
  System.out.println("Received model specification is " + mystr)
  
  initializeGraphModel()
  var mParser = new ModelParser(mystr,g)
  mParser.parse()
  mParser.graphIterator()
  
  val place = Place(
  "Watership Down",
  Location(51.235685, -1.309197),
  Seq(
    Resident("Fiver", 4, None),
    Resident("Bigwig", 6, Some("Owsla"))
  )
)

  val myNet  = AdjMatrix(
    "TestNetwork",
    Seq(
      "N1",
      "N2",
      "N3"
    ),
    Seq(
      0.5,
      0.0,
      0.1
    ),
    Seq(
      MRow("N1",Seq(0.0, 0.5, 0.3)),
      MRow("N2",Seq(0.0, 0.0, 0.5)),
      MRow("N3",Seq(0.0, 0.0, 0.0))
    )
    )
  var json = Json.toJson(place)
  
  val age = request.body.asFormUrlEncoded.get("age[0][]")
  System.out.println(age)
  val myStock = new Stock("GOOG", 650.0)
  //Ok(Json.toJson(myStock))
  
  
  var nodeList = new ListBuffer[String]()
  
  nodeList = mParser.getNodes()
  
  System.out.println("NodeList is  " + nodeList)
  
  var activationList = new ListBuffer[Double]()
  
  activationList = mParser.getActivations()
  
  activationList(0) = 0.5
  //nodeList += myNet.nodes(0)
  //nodeList += myNet.nodes(1)
  //nodeList += myNet.nodes(2)
  
  var rowList = new ListBuffer[MRow]()
  
  //var row1 = MRow(myNet.nodes(0),myNet.adjList(0).edgeWeights)
  //var row2 = MRow(myNet.nodes(1),myNet.adjList(1).edgeWeights)
  //var row3 = MRow(myNet.nodes(2),myNet.adjList(2).edgeWeights)
  for(i <- 0 to nodeList.length-1) {
      var edgeW = new ListBuffer[Double]()
      System.out.println("Connections for "+ nodeList(i))
      for (j <- 0 to nodeList.length-1) {
          var eW = mParser.checkWeight(nodeList(i),nodeList(j))
          System.out.print(" "+ "(" + nodeList(j) +"," + eW + ") ")
          edgeW += eW
      }
      System.out.println(" ")
      var newRow = MRow(nodeList(i),edgeW)
      rowList+=newRow
  }
  //row1 = MRow(nodeList)
  //rowList+=row1
  //rowList+=row2
  //rowList+=row3
  
  var myNet1 = new AdjMatrix("TestNetwork",nodeList, activationList, rowList)
  
   Ok(Json.toJson(myNet1))
}

def getEntityJSONs = Action {
    val myStock = new Stock("GOOG", 650.0)
    Ok(Json.toJson(myStock))
  }

  /**
   * A REST endpoint that gets all the people as JSON.
   */
  def getPersons = Action.async {
    repo.list().map { people =>
      Ok(Json.toJson(people))
    }
  }
}

/**
 * The create person form.
 *
 * Generally for forms, you should define separate objects to your models, since forms very often need to present data
 * in a different way to your models.  In this case, it doesn't make sense to have an id parameter in the form, since
 * that is generated once it's created.
 */
case class CreatePersonForm(name: String, age: Int)