object MainApp extends App{

  //Reading the Binary File
  val binaryFile = BinaryFileReader.getHexBinary()
  var binaryFilePointer:Int =1
  var eventNum:Int=0

  def traverseBinary(): Unit ={
    val eventLength:Int=binaryFile(binaryFilePointer)
    val eventBodyArr:Array[Short]=binaryFile.slice(binaryFilePointer,binaryFilePointer+eventLength)

    println("Event Num : " + eventNum)
    println("Event Length : "+eventLength)


    val binaryString : String = toBinaryStringConverter(eventBodyArr)
    EventHeadDecoder.decodeEventHead(binaryString)

    eventNum+=1
    binaryFilePointer=binaryFilePointer+eventLength


  }

  def toBinaryStringConverter(shortArr:Array[Short]): String ={

    // Create a new StringBuilder.
    val builder = StringBuilder.newBuilder

    for(x<-shortArr){
      val binaryVal=String.format("%8s", Integer.toBinaryString(0xFFFF & x)).replace(' ', '0')
      builder.append(binaryVal)
    }

    // Convert StringBuilder to a string.
    val result = builder.toString()
    result

  }

  1 to 10 foreach { _=> traverseBinary() }

}
