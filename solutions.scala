#!/usr/bin/env scala

var solutions: Seq[(Number, Number)] = Seq(
  (960075, 212900130),
  (564, 325),
  (218, 3847183340l),
  (219, 127),
  (928, 610),
  (6549, 3466),
  (222, 13264),
  (1753, 733),
  (556543474, 76096372),
  (1820, 3454189699072l),
)

def task(num: Int)(solution: => (Number, Number)): Unit = {
  try {
    val (a, b) = solution
    val (red, green, reset) = ("\u001B[31m", "\u001B[32m", "\u001B[0m")
    def colored(subtask: Int, res: Number): String = s"task $num.$subtask: " +
      (if (solutions.size < num) res.toString
      else {
        val should = subtask match { case 1 => solutions(num-1)._1; case _ => solutions(num-1)._2 }
        if (res == should) s"$green$res$reset"
        else s"$red$res (should be $should)$reset"
      })

    println(colored(1, a))
    println(colored(2, b))
  } catch {
    case e: Exception => println(s"task $num: $e")
  }
}

task(1) {
  val numbers = scala.io.Source.fromFile("adv1").getLines.map(_.toInt).toVector.sorted
  (
    (for (x <- numbers; y <- numbers; if x+y == 2020) yield x*y).head
  ,
    (for (x <- numbers; y <- numbers; z <- numbers; if x+y+z == 2020) yield x*y*z).head
  )
}

task(2) {
  val re = "([0-9]+)-([0-9]+) (.): (.*)".r
  (
    scala.io.Source.fromFile("adv2").getLines.count{line => val re(f, t, c, word) = line; val num = word.count(_ == c.head); f.toInt <= num && num <= t.toInt}
  ,
    scala.io.Source.fromFile("adv2").getLines.count{line => val re(sf, st, sc, word) = line; val f = sf.toInt; val t = st.toInt; val c = sc.head; word(f-1) == c ^ word(t-1) == c}
  )
}

task(3) (
  scala.io.Source.fromFile("adv3").getLines.zipWithIndex.map{case (row, i) => row((i*3) % row.length)}.count(_ == '#')
,
  {
    def slope(x: Int): Int = scala.io.Source.fromFile("adv3").getLines.zipWithIndex.map{case (row, i) => row((i*x) % row.length)}.count(_ == '#')
    Seq(1,3,5,7).map(slope).product.toLong *
      scala.io.Source.fromFile("adv3").getLines.sliding(1,2).map(_.head).zipWithIndex.map{case (row, i) => row((i*1) % row.length)}.count(_ == '#')
  }
)

task(4) (
  scala.io.Source.fromFile("adv4").mkString("").split("\n\n").map(_.split("\n| ")).count(p => (p.map(_.take(3)).filter(_ != "cid")).size == 7)
  ,
  {
    implicit class RS(s: String) {
      def in(lb: Int, ub: Int): Boolean = {
        val i = s.filter(c => c>='0' && c <= '9').toInt
        lb <= i && i <= ub
      }
    }

    scala.io.Source.fromFile("adv4").mkString("").split("\n\n").map(_.split("\n| "))
      .count{p =>
        (p.filter(e =>
          e.split(":") match {
            case Array("byr", y) => y in(1920,2002)
            case Array("iyr", y) => y.in(2010,2020)
            case Array("eyr", y) => y.in(2020,2030)
            case Array("hgt", hgt) => (hgt.takeRight(2), hgt.dropRight(2)) match {
              case ("cm", h) => h.in(150,193)
              case ("in", h) => h.in(59,76)
              case _ => false
            }
            case Array("hcl", c) => c.matches("#[0-9a-f]{6}")
            case Array("ecl", c) => c.matches("amb|blu|brn|gry|grn|hzl|oth")
            case Array("pid", p) => p.matches("[0-9]{9}")
            case _ => false
          })).size == 7
      }
  }
)

task(5) (
  scala.io.Source.fromFile("adv5").getLines.map{l => Integer.parseInt(l.map{case 'B' | 'R' => 1; case _ => 0}.mkString(""), 2)}.max
,
  {
    val others = scala.io.Source.fromFile("adv5").getLines.map{l => Integer.parseInt(l.map{case 'B' | 'R' => 1; case _ => 0}.mkString(""), 2)}.toSet
    (others.map(_-1) & others.map(_+1) -- others).head
  }
)

task(6) (
  scala.io.Source.fromFile("adv6").mkString("").split("\n\n").map(_.split(" |\n").map(_.toSet).reduce(_ | _)).map(_.size).sum
,
  scala.io.Source.fromFile("adv6").mkString("").split("\n\n").map(_.split(" |\n").map(_.toSet).reduce(_ & _).size).sum
)

task(7) ({
  val map = scala.io.Source.fromFile("adv7").getLines.map(s => s.take(s.indexOf(" bags")) -> s.replaceFirst("^.*contain ", "").dropRight(1).split(", *").toSet[String].map(x => x.replaceAll("([0-9]+ | bags?)", ""))).toMap
  var oldSize = 0
  var hasGold = Set("shiny gold")
  while (hasGold.size != oldSize) {
    oldSize = hasGold.size
    hasGold |= map.filter(bag => (bag._2 & hasGold).size > 0).keys.toSet[String]
  }
  hasGold.size - 1 // minus the golden bag
},{
  val map = scala.io.Source.fromFile("adv7").getLines.map(s => s.take(s.indexOf(" bags")) -> s.replaceFirst("^.*contain ", "").dropRight(1).split(", *").toSet[String].map(x => (x.take(x.indexOf(' ')).replace("no", "0").toInt -> x.replaceAll("([0-9]+ | bags?)", "")))).toMap
  lazy val numIn: Map[String, Int] = Map().withDefault(b => map(b).toSeq.map{case(0, _) => 0; case (n, b2) => n*(1+numIn(b2))}.sum)
  numIn("shiny gold")
})

task(8) ({
  val lines = scala.io.Source.fromFile("adv8").getLines.map{x => x.head -> x.drop(4).toInt}.toVector
  var seen = Set[Int]()
  var esp = 0
  var acc = 0
  while (!seen(esp)) { seen += esp; lines(esp) match { case ('a', i) => {acc += i; esp += 1}; case ('n', _) => esp += 1; case ('j', i) => esp += i; case _ => }}
  acc
},{
  val lines = scala.io.Source.fromFile("adv8").getLines.map{x => x.head -> x.drop(4).toInt}.toVector
  def run(mod: Int): Option[Int] = {var acc = 0; var esp: Int = 0; val lines2: Seq[(Char, Int)] = lines.updated(mod, (if (lines(mod)._1 == 'n') 'j' else 'n') -> lines(mod)._2); var seen = Set[Int](); if (lines(mod)._1 != 'a' && {while (!seen(esp) && esp < lines.size) { seen += esp; lines2(esp) match { case ('a', i) => {acc += i; esp += 1}; case ('n', _) => esp += 1; case ('j', i) => esp = esp + i; case _ => }}; esp == lines.size}) Some(acc) else None}
  (0 until 641).flatMap(run).head
})

task(9) {
  val firstBadNum = scala.io.Source.fromFile("adv9").getLines.map(_.toLong).sliding(26, 1).map(_.reverse.toList).find{case h :: t => !t.exists(x => t.exists(y => (x != y) && (x + y == h))); case _ => ???}.map(_.head).get
  (
    firstBadNum
  ,
    scala.io.Source.fromFile("adv9").getLines.map(_.toLong).toSeq.tails.flatMap(_.reverse.tails.filter(_.sum == firstBadNum)).filter(_.size > 1).map(s => s.min + s.max).next
  )
}

task(10) ({
  val adapters = {val a = scala.io.Source.fromFile("adv10").getLines.toSeq.map(_.toInt).sorted; 0 +: a :+ (a.last+3)}.toVector
  adapters.sliding(2, 1).count(l => l.last - l.head == 1) *
    adapters.sliding(2, 1).count(l => l.last - l.head == 3)
},{
  val adapters = {val a = scala.io.Source.fromFile("adv10").getLines.toSeq.map(_.toInt).sorted; 0 +: a :+ (a.last+3)}.toSet
  var combinationsTo: Map[Int, Long] = Map(-2 -> 0l, -1 -> 0l, 0 -> 1l)
  combinationsTo = combinationsTo.withDefault(i => {val res = if (!adapters(i)) 0 else combinationsTo(i-3) + combinationsTo(i-2) + combinationsTo(i-1); combinationsTo += (i -> res); res})
  combinationsTo(adapters.max)
})

