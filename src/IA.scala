object IA{
  def main(args: Array[String]): Unit = {
    val tabReine = new Array[Int](8)
    parcourt(tabReine,0,0)
    println(toString(tabReine))
  }

  @annotation.tailrec
  def parcourt(tabReine :Array[Int],ligne:Int,col:Int): Unit =
  {
    if(col == tabReine.length)
    {
      if(ligne>0)
      {
        tabReine(ligne)=0
        parcourt(tabReine,ligne-1,tabReine(ligne-1)+1)
      }
      else
        println("impossible")
    }

    else if(ligne<tabReine.length){
      tabReine(ligne)=col
      //println(toString(tabReine))
      if(estEmprise(tabReine,ligne))
      {
        if(col==tabReine.length)
        {
          tabReine(ligne)=0
          parcourt(tabReine,ligne-1,tabReine(ligne-1)+1)
        }
        else
          parcourt(tabReine,ligne,col+1)
      }
      else
      {
        parcourt(tabReine,ligne+1,0)
      }
    }

  }

  def estEmprise(tabReine :Array[Int],ligne:Int):Boolean =
  {
    estEmprise(tabReine,ligne,ligne-1)
  }

  @annotation.tailrec
  def estEmprise(tabReine :Array[Int],ligne:Int,i:Int):Boolean =
  {
    if(i == -1)
      false
    else if(tabReine(ligne)==tabReine(i))
      true
    else if(tabReine(ligne)-(ligne-i)==tabReine(i))
      true
    else if(tabReine(ligne)+(ligne-i)==tabReine(i))
      true
    else
      estEmprise(tabReine,ligne,i-1)
  }

  def toString(tabReine :Array[Int]): String = {

    var s = " "
    var i = 0
    var j = 0
    while (j < tabReine.length) {
        s += "| "+j+" "
      j = j + 1
    }
    s+="|\n-"
    j=0
    while (j < tabReine.length) {
      s += "+---"
      j = j + 1
    }
    s += "+\n"
    while (i < tabReine.length) {
      j = 0
      s+=i
      while (j < tabReine.length) {
        if (tabReine(i) == j)
          s += "| x "
        else
          s += "|   "
        j = j + 1
      }
      s+="|\n-"
      j = 0
      while (j < tabReine.length) {
          s += "+---"
        j = j + 1
      }
      s += "+\n"
      i = i + 1
    }
    s
  }
}