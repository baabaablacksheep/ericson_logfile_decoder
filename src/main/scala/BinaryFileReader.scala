import java.nio.file.{Files, Path, Paths}

object BinaryFileReader {

  //Read The Binary File
  def getHexBinary(): Array[Short] ={
    val filepath : Path = Paths.get("../Resources/ericson_decoder_resources/binary.bin")
    val bytes = Files.readAllBytes(filepath)
    toUnsignedByte(bytes)
  }

  def toUnsignedByte(bytes:Array[Byte]):Array[Short] = {
    bytes.map(x=>{
      val aByte:Int = 0xff & x.asInstanceOf[Int]
      aByte.asInstanceOf[Short]
    })
  }
}