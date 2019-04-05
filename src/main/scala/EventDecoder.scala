import scala.annotation.tailrec
import scala.xml.NodeSeq

object EventDecoder {

  //This Parameters Should Be Changed Accordingly
  val numBitsEventLength = 16 //Originally it's 10
  val numBitsRecordType = 8

  val numBitsScannerID = 24
  val numBitsTimestamp_Hour = 5
  val numBitsTimestamp_Minute = 6
  val numBitsTimestamp_Second = 6
  val numBitsTimestamp_Milisec = 11
  val numBitsEventID = 11

  val numBitsBetweenRecordType2EventId = numBitsScannerID + numBitsTimestamp_Hour + numBitsTimestamp_Minute + numBitsTimestamp_Second + numBitsTimestamp_Milisec

  def decodeEventHead(binaryString : String): Unit = {

    var binaryStringPointer = 0

    val eventLength: Int = Integer.parseInt(binaryString.slice(binaryStringPointer,binaryStringPointer+numBitsEventLength).toString,2); binaryStringPointer+=numBitsEventLength
    val recordType: Int = Integer.parseInt(binaryString.slice(binaryStringPointer,binaryStringPointer+numBitsRecordType).toString,2); binaryStringPointer+=numBitsRecordType
//    val scannerID: String = binaryString.slice(binaryStringPointer,binaryStringPointer+numBitsScannerID).toString; binaryStringPointer+=numBitsScannerID
//    val timestampHour: Int = Integer.parseInt(binaryString.slice(binaryStringPointer,binaryStringPointer+numBitsTimestamp_Hour).toString,2); binaryStringPointer+=numBitsTimestamp_Hour
//    val timestampMinute: Int = Integer.parseInt(binaryString.slice(binaryStringPointer,binaryStringPointer+numBitsTimestamp_Minute).toString,2); binaryStringPointer+=numBitsTimestamp_Minute
//    val timestampSecond: Int = Integer.parseInt(binaryString.slice(binaryStringPointer,binaryStringPointer+numBitsTimestamp_Second).toString,2); binaryStringPointer+=numBitsTimestamp_Second
//    val timestampMilisec: Int = Integer.parseInt(binaryString.slice(binaryStringPointer,binaryStringPointer+numBitsTimestamp_Milisec).toString,2); binaryStringPointer+=numBitsTimestamp_Milisec
    val eventID: Int = Integer.parseInt(binaryString.slice(binaryStringPointer+numBitsBetweenRecordType2EventId,binaryStringPointer+numBitsBetweenRecordType2EventId+numBitsEventID).toString,2) // binaryStringPointer+=numBitsEventID

    val eventBodyBinaryString: String = binaryString.splitAt(binaryStringPointer)._2
    val eventBody: NodeSeq = XMLParser.getEvent(eventID)
    val eventParamList: List[String] = (eventBody \ "fileformat" \ "param").map(checkMaptoFunc).toList
//    val eventParamList: List[String] = (eventBody \ "intformat" \ "field").map(_.text).toList

//    val parameterList = getFields(eventFields.toList,binaryString.splitAt(binaryStringPointer)._2,List.empty[Parameter])

    val eventName = (eventBody \ "eventname").text

/*    println("Event Length : " + eventLength)
    println("Record Type : " + recordType)
    println("Scanner ID : " + scannerID)
    println("Timestamp Hour : " + timestampHour)
    println("Timestamp Minute : " + timestampMinute)
    println("Timestamp Second : " + timestampSecond)
    println("Timestamp Milisec : " + timestampMilisec)*/
    println("Event ID : " + eventID)
    println("Event Name : " + eventName)
    println("Event Fields : ")

    val abc = getFields(eventParamList,eventBodyBinaryString,List.empty[Parameter])

    abc.foreach(x=> println(x.paramName+": " + {x.value match {case Left(z) => z; case Right(z) => z}}))

    println()
  }

  def checkMaptoFunc(paramBody: NodeSeq): String ={

    val maptoAttr = paramBody \ "@mapto"
    if(maptoAttr.isEmpty){
      paramBody.text
    }
    else {
      maptoAttr.text
    }
  }

  def getParam(paramName: String): Unit ={

    val paramBody = XMLParser.getParameter(paramName)

//    val paramNumBits = Integer.parseInt((paramBody \ "numberofbits").text)

//    println("Parameter Name : "+paramName + ", Num Bits : "+paramNumBits)
  }

  @tailrec
  def getFields(fieldList: List[String], binaryString: String, resultlist: List[Parameter]): List[Parameter] = fieldList match {
    case Nil => resultlist
    case h :: tail => {
      h match {
        case "EVENT_PARAM_MESSAGE_CONTENTS" | "EVENT_PARAM_MESSAGE_CONTENTS_INTERNAL"  => { // "EVENT_PARAM_MESSAGE_CONTENTS_REF"

          val paramBody: NodeSeq = XMLParser.getParameter(h)
          val paramNumBits: Int = Integer.parseInt((paramBody \ "numberofbits").text)

          val messageLength: Long = resultlist.filter(x=> (x.paramName=="EVENT_PARAM_MESSAGE_LENGTH") || (x.paramName=="EVENT_PARAM_MESSAGE_LENGTH_INTERNAL")).head.value match {case Left(z)=> z; case _ => 0}

          val useValidBit: Boolean = if((paramBody \ "usevalidbit").text.toLowerCase=="yes") true else false
          val (paramBinStr,remBinStr) = binaryString.splitAt(messageLength.toInt*8)

          val (paramValidity: String, paramValue: String) = {
            if(useValidBit){
              val (validBit,valueBits) = paramBinStr.splitAt(1)
              (if (validBit == "0") "valid" else "invalid", valueBits)
            }
            else{
              ("valid", paramBinStr)
            }
          }
          getFields(tail,remBinStr,resultlist:+Parameter(h,paramValidity,paramNumBits, Right(paramValue)))
        }

        case _ => {
          val paramName: String = h
          val paramBody: NodeSeq = XMLParser.getParameter(paramName)
          val paramNumBits: Int = Integer.parseInt((paramBody \ "numberofbits").text)

          val useValidBit: Boolean = if((paramBody \ "usevalidbit").text.toLowerCase=="yes") true else false
          val (paramBinStr,remBinStr) = binaryString.splitAt(paramNumBits)

          val (paramValidity: String, paramValue: Int) = {
            if(useValidBit){
              val (validBit,valueBits) = paramBinStr.splitAt(1)
              (if (validBit == "0") "valid" else "invalid", Integer.parseInt(valueBits,2))
            }
            else{
              ("valid",Integer.parseInt(paramBinStr,2))
            }
          }

//          println("Param Name: "+ paramName + ", Param Validity: "+paramValidity +", Param Bits: "+ paramNumBits + ", Param Value: "+paramValue )

          getFields(tail,remBinStr,resultlist:+Parameter(paramName,paramValidity,paramNumBits, Left(paramValue)))
        }
      }
    }
  }

}