object EitherExample {

  def lookupId(key: String): Either[String, Int] = 
    databaseCall(nextInt).toRight("failed to find id based on key")

  def lookupRecord(id: Int): Either[String, Record] =
    databaseCall(Record(id)).toRight("failed to find record based on id")

  def nextInt = math.abs(scala.util.Random.nextInt)

  def main(args: Array[String]): Unit = {
    runEitherExample
  }

  def runEitherExample =
    (1 to 1000).foreach(i => {
      lookupId("key"+i).right
      .flatMap(lookupRecord(_)).fold(
        s => println("ERROR - " + s),
        s => println("looked up record " + s)
      )
    })

  def databaseCall[A](value: A) = 
    if (nextInt % 5 == 0) None else Some(value)
}

case class Record(id: Int)
