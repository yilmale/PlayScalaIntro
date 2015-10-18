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


import scala.concurrent.{ ExecutionContext, Future }

import javax.inject._

class PersonController @Inject() (repo: PersonRepository, val messagesApi: MessagesApi)
                                 (implicit ec: ExecutionContext) extends Controller with I18nSupport{

  /**
   * The mapping for the person form.
   */
  val personForm: Form[CreatePersonForm] = Form {
    mapping(
      "name" -> nonEmptyText,
      "age" -> number.verifying(min(0), max(140))
    )(CreatePersonForm.apply)(CreatePersonForm.unapply)
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
  val myStock = new Stock("GOOG", 650.0)
  Ok(Json.toJson(myStock))
}

def addEntityJSON = Action { implicit request =>
  System.out.println(request.body)
  val myModel = request.body.asFormUrlEncoded.get("model")
  var mystr:String = new String(myModel(0))
  System.out.println("Received model specification is " + mystr)
  val age = request.body.asFormUrlEncoded.get("age[0][]")
  System.out.println(age)
  val myStock = new Stock("GOOG", 650.0)
  Ok(Json.toJson(myStock))
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