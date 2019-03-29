import scala.xml.{NodeSeq, XML}

object XMLParser {
  val xmlFile = XML.loadFile("../Resources/ericson_decoder_resources/PmEventSpecification.xml")

  ///bookstore/book[price>35.00] -- How to Access Node in XPath

  def getEvent(eventID: Int): NodeSeq = {
    var eventSet: NodeSeq = null
    if (eventID < 384) {
      eventSet = xmlFile \ "externalevents" \ "event"
    }
    else {
      eventSet = xmlFile \ "internalevents" \ "event"
    }
    eventSet.filter(x => (x \ "eventid").text == eventID.toString)
  }


  def getParameter(paramname : String): NodeSeq ={
    val paramData = xmlFile \ "parameters" \ "parameter"
    paramData.filter(x => (x \ "paramname").text == paramname)
  }
}
