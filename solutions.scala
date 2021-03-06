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
  (2494, 2306),
  (582, 52069),
  (174, 780601154795940l),
  (8570568288597l, 3289441921203l),
  (929, 16671510),
  (29019, 517827547723l),
  (380, 2332),
  (7293529867931l, -1),
  (203, 304),
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

task(11) ({
  def pad(seats: Vector[String]): Vector[String] = (("."*(seats.head.size+2)) +: seats.map('.' + _ + '.') :+ ("."*(seats.head.size+2))).toVector
  def iter(seats: Vector[String]): Vector[String] = pad(seats).sliding(3, 1).map(_.toList).toVector.map(_.transpose.sliding(3,1).toVector).toVector.map(_.map{sq: List[List[Char]] => {val flat = sq.flatten; (flat(4), (flat.take(4) ++ flat.drop(5)).count(_ == '#')) match {case ('L', 0) => '#'; case ('#', s) if s >= 4 => 'L'; case (x, _) => x}}}).map(_.mkString(""))
  var seats = scala.io.Source.fromFile("adv11").getLines.toVector
  var lastSeats = Vector[String]()
  while (lastSeats != seats) {lastSeats = seats; seats = iter(seats)}
  seats.map(_.count(_ == '#')).sum
},{
  def around(x0: Int, y0: Int, seats: Vector[String]): Int = Set((-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)).count{case (dx, dy) => try{Stream.from(1).map{i => seats(x0+i*dx)(y0+i*dy)}.dropWhile(_ == '.').head == '#'} catch {case _: IndexOutOfBoundsException => false}}
  def iter(seats: Vector[String]): Vector[String] = seats.zipWithIndex.map{case (v, i) => v.zipWithIndex.map{case (e, j) => (e, around(i, j, seats)) match {case ('L', 0) => '#'; case ('#', s) if s >= 5 => 'L'; case (x, _) => x}}}.map(_.mkString(""))
  var seats = scala.io.Source.fromFile("adv11").getLines.toVector
  var lastSeats = Vector[String]()
  while (lastSeats != seats) {lastSeats = seats; seats = iter(seats)}
  seats.map(_.count(_ == '#')).sum
})

task(12) (
  scala.io.Source.fromFile("adv12").getLines.map(l => (l.head, l.drop(1).toInt)).foldLeft((0, 0, (1, 0))){case ((x: Int, y: Int, (dx: Int, dy: Int)), (c: Char, n: Int)) => c match {case 'E' => (x + n, y, (dx, dy)); case 'S' => (x, y - n, (dx, dy)); case 'W' => (x - n, y, (dx, dy)); case 'N' => (x, y + n, (dx, dy)); case 'F' => (x+n*dx, y+n*dy, (dx, dy)); case rl => (x, y, (rl match {case 'L' => n; case _ => 360-n}) match {case 90 => (-dy, dx); case 270 => (dy, -dx); case _ => (-dx, -dy)})}} match {case (x, y, _) => math.abs(x)+math.abs(y)}
,
  scala.io.Source.fromFile("adv12").getLines.map(l => (l.head, l.drop(1).toInt)).foldLeft((0, 0, (10, 1))){case ((x: Int, y: Int, (dx: Int, dy: Int)), (c: Char, n: Int)) => c match {case 'E' => (x, y, (dx+n, dy)); case 'S' => (x, y, (dx, dy-n)); case 'W' => (x, y, (dx-n, dy)); case 'N' => (x, y, (dx, dy+n)); case 'F' => (x+n*dx, y+n*dy, (dx, dy)); case rl => (x, y, (rl match {case 'L' => n; case _ => 360-n}) match {case 90 => (-dy, dx); case 270 => (dy, -dx); case _ => (-dx, -dy)})}} match {case (x, y, _) => math.abs(x)+math.abs(y)}
)

task(13) (
  scala.io.Source.fromFile("adv13").getLines.toList match {case List(time, list) => list.split(",").filter(_ != "x").map(_.toInt).minBy(x => x-(x % time.toInt)) match {case x => x*(x-time.toInt%x)}}
,
  scala.io.Source.fromFile("adv13").getLines.toList(1).split(",").zipWithIndex.filter(_._1 != "x").map{case (l, i) => l.toInt -> (l.toInt*i-i)%l.toInt}.foldLeft((0l, 1l)){case ((rem, fac), (bus, busRem)) => val x = (0 until bus).find(x => (rem+x*fac)%bus == busRem).get; (rem+x*fac, fac*bus)}._1
)

task(14) (
  scala.io.Source.fromFile("adv14").getLines.map(s => if (s.startsWith("mask = ")) s.drop(7) else s.drop(4).split("] = ") match {case Array(idx, value) => idx.toInt -> value.toLong}).foldLeft((Map[Int, Long](), "")){case ((mem, mask), s: String) => mem -> s; case ((mem, mask), (idx: Int, value: Long)) => (mem + (idx -> mask.foldRight((value, 1l, 0l)){case ('X', (v, p, res)) => (v/2, p*2, res + p*(v%2)); case (d, (v, p, res)) => (v/2, p*2, res + p*(d - '0'));}._3), mask); case _ => ???}._1.values.sum
,
  scala.io.Source.fromFile("adv14").getLines.map(s => if (s.startsWith("mask = ")) s.drop(7) else s.drop(4).split("] = ") match {case Array(idx, value) => idx.toLong -> value.toLong}).foldLeft((Map[Long, Long](), "")){case ((mem, mask), s: String) => mem -> s; case ((mem, mask), (idx: Long, value: Long)) => (mem.toMap[Long, Long] ++ mask.foldRight((idx, 1l, Set[Long](0l))){case ('X', (i, p, keys)) => (i/2, p*2, keys ++ keys.map(_+p)); case ('1', (i, p, keys)) => (i/2, p*2, keys.map(_ + p)); case (_, (i, p, keys)) => (i/2, p*2, keys.map(_ + p*(i%2)))}._3.map(_ -> value).toMap[Long, Long], mask); case _ => ???}._1.values.sum
)

task(15) ({
  var lastIdx = Seq(16,1,0,18,12,14).zipWithIndex.toMap; var last = 19; for (i <- lastIdx.size until 2020-1) {val next = i-lastIdx.getOrElse(last, i); lastIdx += (last ->i); last = next}
  last
},{
  //val start = System.currentTimeMillis
  //var lastIdx = scala.collection.mutable.Map[Int, Int](Seq(16,1,0,18,12,14).zipWithIndex.toMap.toArray: _*); var last = 19; for (i <- lastIdx.size until 30000000-1) {val next = i-lastIdx.getOrElse(last, i); lastIdx += last ->i; last = next} // Map[Int, _] is much slower than Array[_]

  val n = 30000000
  var lastIdx = new Array[Int](n)
  Seq(16,1,0,18,12,14).zipWithIndex.foreach{case (n, i) => lastIdx(n) = i+1}
  var last = 19
  for (i <- 6 until n-1) {
    val l = lastIdx(last)
    val next = if (l == 0) 0 else i-(l-1)
    lastIdx(last) = i+1
    last = next
  }
  //println("time: " + (System.currentTimeMillis - start))
  last
})

task(16) {
  val valid = scala.io.Source.fromFile("adv16").getLines.takeWhile(_.nonEmpty).map(s => s.drop(s.indexOf(":")+2)).flatMap(_.split(" or ")).map(_.split("-").map(_.toInt) match {case Array(f, t) => (f to t).toSet}).reduce(_ | _)
  (
    scala.io.Source.fromFile("adv16").getLines.dropWhile(_ != "nearby tickets:").drop(1).map(_.split(",").map(_.toInt)).flatten.filterNot(valid).sum
  ,{
    val classes = scala.io.Source.fromFile("adv16").getLines.takeWhile(_.nonEmpty).map(s => s.takeWhile(_!=':') -> (s.drop(s.indexOf(":")+2)).split(" or ").map(_.split("-").map(_.toInt) match {case Array(f, t) => (f to t).toSet}).reduce(_ | _)).toSeq
    var possibleClasses = scala.io.Source.fromFile("adv16").getLines.dropWhile(_ != "nearby tickets:").drop(1).map(_.split(",").map(_.toInt)).filter(_.forall(valid)).toSeq.transpose.map(_.toSet).map(s => classes.filter(s subsetOf _._2).map(_._1).toVector).toVector
    while (possibleClasses.exists(_.size > 1)) possibleClasses.filter(_.size == 1).foreach{case Vector(fix) => possibleClasses = possibleClasses.map(p => if (p.size <= 1) p else p.filter(_ != fix))}
    (possibleClasses.flatten zip scala.io.Source.fromFile("adv16").getLines.dropWhile(_ != "your ticket:").drop(1).next.split(",").map(_.toInt)).filter(_._1.startsWith("departure")).map(_._2.toLong).product
  })
}

task(17) ({
  def around(x: Int, y: Int, z: Int, seats: Vector[Vector[String]]): Int = (Seq((-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)).flatMap{case (i,j) => (-1 to 1).map(k => (i,j,k))} :+ (0, 0, -1) :+ (0, 0, 1)).count{case (dx, dy, dz) => seats(x+1+dx)(y+1+dy)(z+1+dz) == '#'}
  def pad(seats: Vector[Vector[String]]): Vector[Vector[String]] = Vector.fill(seats.head.size+2)("."*(seats.head.size+2)) +: seats.map(s => ((("."*(s.head.size+2)) +: s.map('.' + _ + '.')) :+ ("."*(s.head.size+2))).toVector) :+ Vector.fill(seats.head.size+2)("."*(seats.head.size+2))
  def iter(seats: Vector[Vector[String]]): Vector[Vector[String]] = {val ps = pad(seats); val pps = pad(ps); ps.zipWithIndex.map{case (v, i) => v.zipWithIndex.map{case (w, j) => w.zipWithIndex.map{case (e, k) => (e, around(i, j, k, pps)) match {case ('#', s) => if (s == 2 || s == 3) '#' else '.'; case (_, s) => if (s==3) '#' else '.'}}}}.map(_.map(_.mkString("")))}
  var seats = Vector(scala.io.Source.fromFile("adv17").getLines.toVector)
  for (_ <- 1 to 6) seats = iter(seats)
  seats.flatten.flatten.count(_ == '#')
},{
  def around(x: Int, y: Int, z: Int, w: Int, seats: Vector[Vector[Vector[String]]]): Int = {((Seq((-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)).flatMap{case (i,j) => (-1 to 1).map(k => (i,j,k))} :+ (0, 0, -1) :+ (0, 0, 1)).flatMap{case (i,j,k) => (-1 to 1).map(l => (i,j,k,l))} :+ (0,0,0,-1) :+ (0,0,0,1)).count{case (dx, dy, dz, dw) => seats(x+1+dx)(y+1+dy)(z+1+dz)(w+1+dw) == '#'}}
  def pad(hseats: Vector[Vector[Vector[String]]]): Vector[Vector[Vector[String]]] = Vector.fill(hseats.head.size+2, hseats.head.head.size+2)("."*(hseats.head.head.size+2)) +: hseats.map(seats => Vector.fill(seats.head.size+2)("."*(seats.head.size+2)) +: seats.map(s => ((("."*(s.head.size+2)) +: s.map('.' + _ + '.')) :+ ("."*(s.head.size+2))).toVector) :+ Vector.fill(seats.head.size+2)("."*(seats.head.size+2))) :+ Vector.fill(hseats.head.size+2, hseats.head.head.size+2)("."*(hseats.head.head.size+2))
  def iter(seats: Vector[Vector[Vector[String]]]): Vector[Vector[Vector[String]]] = {val ps = pad(seats); val pps = pad(ps); ps.zipWithIndex.map{case (v, i) => v.zipWithIndex.map{case (w, j) => w.zipWithIndex.map{case (u, k) => u.zipWithIndex.map{case (e, l) => (e, around(i, j, k, l, pps)) match {case ('#', s) => if (s == 2 || s == 3) '#' else '.'; case (_, s) => if (s==3) '#' else '.'}}}}}.map(_.map(_.map(_.mkString(""))))}
  var seats = Vector(Vector(scala.io.Source.fromFile("adv17").getLines.toVector))
  for (_ <- 1 to 6) seats = iter(seats)
  seats.flatten.flatten.flatten.count(_ == '#')
})

task(19) {
  def countMatches(rawInput: Seq[String]): Int = {
    val rules = rawInput.takeWhile(_.contains(":")).map(s => s.split(": ").toList match {
      case List(n, l) => n -> l.split(" \\| ").toList.map(_.replace("\"", "").split(" ").toList)
    }).toMap
    var known: Map[(String, String), Boolean] = Map()
    def matches(s: String, rule: String): Boolean = known.get((s, rule)) match {
      case Some(b) => b
      case None =>
        val res = rules(rule).exists{
          case List(r) if r.matches("[0-9]+") => matches(s, r)
          case List(c) => s == c
          case List(x, y) =>
            (1 until s.size).exists(i =>
                matches(s.take(i), x) && matches(s.drop(i), y)
            )
        }
        known += (s, rule) -> res
        res
      }
    rawInput.dropWhile(_.contains(":")).count(matches(_, "0"))
  }
  val input = scala.io.Source.fromFile("adv19").getLines.toSeq
  (
    countMatches(input),
    countMatches("1111: 42 11" +: input.map{
      case "11: 42 31" => "11: 42 31 | 1111 31"
      case "8: 42" => "8: 42 | 42 8"
      case line => line
    })
  )
}

