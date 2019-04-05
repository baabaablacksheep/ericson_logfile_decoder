object MainApp extends App{

  //Reading the Binary File
  val binaryFile = BinaryFileReader.getHexBinary()
  var binaryFilePointer:Int =0
  var eventNum:Int=0


  def traverseBinary(): Unit ={
    val eventLength:Int = getEventLength(binaryFile.slice(binaryFilePointer,binaryFilePointer+2))
    val eventBodyArr:Array[Short] = binaryFile.slice(binaryFilePointer,binaryFilePointer+eventLength)
    val eventBinaryString: String = toBinaryStringConverter(eventBodyArr)

    println("===========================================================")
    println("Event Num : " + eventNum)

    EventDecoder.decodeEventHead(eventBinaryString)

    binaryFilePointer=binaryFilePointer+eventLength
    eventNum+=1

  }

  def toBinaryStringConverter(shortArr:Array[Short]): String ={

    // Create a new StringBuilder.
    val builder = StringBuilder.newBuilder

    for(x<-shortArr){
      val binaryVal=String.format("%8s", Integer.toBinaryString(0xFFFF & x)).replace(' ', '0')
      builder.append(binaryVal)
    }

    // Convert StringBuilder to a string.
    val result = builder.toString
    result

  }

  def toDecimalConverter(binaryStr: String): Int ={
    Integer.parseInt(binaryStr,2)
  }

  def getEventLength(lengthValArr: Array[Short]): Int ={

    if(lengthValArr.length>0){
      val lengthBinary:String = toBinaryStringConverter(lengthValArr)
      toDecimalConverter(lengthBinary)
    }
    else{
      0
    }
  }

  1 to 5 foreach { _=> traverseBinary() }

}
