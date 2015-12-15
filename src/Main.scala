object Main extends App {
  ClothesKneeso.run
}

object ClothesKneeso {
  /**
    * 特に問題も無し。コードも短めにできたかな？
    */
  def run = {
    val sc = new java.util.Scanner(System.in)
    val n, m = sc.nextInt
    println((("R"*n+"W"*n)*(1+m/(n*2))).take(m))
  }
}

object EyeGantai {
  /**
    * 普通に集合の演算。Javaとかだと回して求めるのかな？
    */
  def run = {
    val sc = new java.util.Scanner(System.in)
    val _, have = sc.nextInt
    val haveList = (for (_<- 1 to have) yield sc.nextInt).toSet
    val sell = sc.nextInt
    val sellList = (for (_<- 1 to sell) yield sc.nextInt).toSet
    val buyList = sellList &~ haveList
    println(if (buyList.isEmpty) "None" else buyList.toList.sortWith((a,b)=>a<b).mkString(" "))
  }
}

object Santa {

  /**
    * ちょっとScala的に書けた気がするぞ
    * 体積を求める前に、X軸、Y軸それぞれでの最短距離を求めてるとこがポイント
    */
  def run = {
    def calcMin(min: Int, cur: List[Int]): Int = cur match {
      case _ :: Nil => min
      case h1 :: h2 :: _ if h2-h1 < min => calcMin(h2-h1, cur.tail)
      case h1 :: h2 :: _ => calcMin(min, cur.tail)
    }

    val sc = new java.util.Scanner(System.in)
    val x,y,z,n = sc.nextInt
    val list = for (_ <- 1 to n) yield (sc.nextInt, sc.nextInt)

    val xl = (List(0) :: list.collect{case (0, xx) => xx} :: List(x) :: Nil).flatten.sortWith((a,b)=>a<b)
    val yl = (List(0) :: list.collect{case (1, yy) => yy} :: List(y) :: Nil).flatten.sortWith((a,b)=>a<b)

    val xmin = calcMin(x, xl)
    val ymin = calcMin(y, yl)
    println(xmin * ymin * z)
  }
}

object Megane {

  /**
    * ダサい。ダサすぎる。関数のカケラも無い。
    */
  def run = {
    import scala.util.control.Breaks

    def isMatchMarix(mArr: Array[Array[Int]], nArr: Array[Array[Int]], x: Int, y: Int, nDim: Int): Boolean = {
      for (xx <- 0 to nDim-1; yy <- 0 to nDim-1) {
        if (mArr(xx+x)(yy+y) != nArr(xx)(yy))
          return false
      }

      true
    }

    val sc = new java.util.Scanner(System.in)

    val mDim = sc.nextInt
    val mArr = Array.ofDim[Int](mDim, mDim)
    for (
      x <- 0 to mDim-1;
      y <- 0 to mDim-1
    ) {
      mArr(x)(y) = sc.nextInt
    }
    val nDim = sc.nextInt
    val nArr = Array.ofDim[Int](nDim, nDim)
    for (
      x <- 0 to nDim-1;
      y <- 0 to nDim-1
    ) {
      nArr(x)(y) = sc.nextInt
    }

//    println("M="+mArr.deep)
//    println("N="+nArr.deep)

    val b = new Breaks
    var xp, yp = -1
    b.breakable {
      for (
        x <- 0 to (mDim - nDim);
        y <- 0 to (mDim - nDim)
      ) {
        if (isMatchMarix(mArr, nArr, x, y, nDim)) {
          xp = x
          yp = y
          b.break
        }
      }
    }

//    println("(x,y)=("+xp+","+yp+")")
    println(xp+" "+yp)
  }
}

object Mizugi {
  /**
    * 0を取った9桁の数値だけを残していく方法にしてみる。
    * 途中の計算を９桁で切ってたら計算結果が間違う。
    * 頭が0になっているのだろう。
    * とりま12にしてみたら通ったのでよしとする
    * 先頭の0をtoIntで取ろうかと思ったが、８進数とかなったらウザいのでヤメた。(いけるか試してない)
    */
  def run: Unit = {
    def factorial0(i: Int, res: BigInt): BigInt = {
      if (i == 1)
        takeNbutZero(9, res)
      else
        factorial0(i - 1, takeNbutZero(12, res) * i)
    }

    def takeNbutZero(fig: Int, num: BigInt): BigInt = {
      BigInt(num.toString.reverse.dropWhile(_=='0').take(fig).reverse)
    }

    val sc = new java.util.Scanner(System.in)
    val no = sc.nextInt
    println(factorial0(no, 1))
  }

  /**
    * 真面目に計算したら時間かかりすぎてダメ
    */
  def run1Dame = {
    def factorial(i: Int, res: BigInt): BigInt = {
      if (i == 1) res
      else factorial(i-1, i*res)
    }

    val sc = new java.util.Scanner(System.in)
    val no = sc.nextInt
    println(no)
    val f = factorial(no, 1)
    println(f)
    val l1 = f.toString.reverse.toList.dropWhile(_ == '0')
    println(l1)
    val s2 = l1.take(9).reverse.dropWhile(_ == '0').mkString("")
    println(s2)
  }
}

object ClothesCardigan {
  def run: Unit = {
    def factorial(i: Int, res: Int): Int = {
      if (i == 1) res
      else factorial(i-1, i*res)
    }

    val sc = new java.util.Scanner(System.in)
    val cnt = sc.nextInt
    println(factorial(cnt, 1))
  }
}

object ClothesSailor {
  def run: Unit = {
    val sc = new java.util.Scanner(System.in)
    val cnt = sc.nextInt
    val ls = List.fill(cnt){sc.next}
    println(ls.mkString("_"))
  }
}

object HairTwinTail {
  def run = {
    val sc = new java.util.Scanner(System.in)
    val ac, ap, bc, bp = sc.nextInt
    println(if(ac.toDouble/ap > bc.toDouble/bp) 1 else 2)
  }
}

object HairPonyTail {
  def run: Unit = {
    def dispNo(c: Int):Unit = c match {
      case 0 => println("0!!")
      case _ => {
        println(c.toString)
        dispNo(c-1)
      }
    }

    val sc = new java.util.Scanner(System.in)
    dispNo(sc.nextInt)
  }
}

object HairLong {
  def run: Unit = {
    val sc = new java.util.Scanner(System.in)
    var a = 0
    var b = 0
    for (_ <- 1 to 5) {
      if (sc.nextLine() == "yes") a += 1 else b+=1
    }

    println(if (a>b) "yes" else "no")
  }

  def run2: Unit = {
    val sc = new java.util.Scanner(System.in)

    def whichIsMany(ls: List[String], yes: Int, no: Int): String = {
//      println(ls.toString()+" "+yes.toString+" "+no.toString)
      ls match {
        case Nil => if (yes>no) "yes" else "no"
        case h :: tail if h == "no" => whichIsMany(tail, yes, no+1)
        case h :: tail => whichIsMany(tail, yes+1, no)
      }
    }

    val l = List.fill(5){sc.nextLine()}
    println(whichIsMany(l, 0, 0))
  }
}

object EyeTsurime {
  def run {
    val sc = new java.util.Scanner(System.in)
    val cnt = sc.nextInt
    println("Ann"*cnt)
  }
}

object HairShort {
  def run: Unit = {
    val sc = new java.util.Scanner(System.in)
    val a, b = sc.nextInt
    println(a+b)
  }
}
