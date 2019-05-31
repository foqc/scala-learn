object HelloWorld {
  def main(args: Array[String]) {
    val donutsToBuy: Int = 5
    lazy val donutService = "initialize some donut service"
    println("Step 1: Using String interpolation to print a variable")
    val favoriteDonut: String = "Glazed Donut"
    println(s"My favorite donut = $favoriteDonut")

    println("\nStep 2: Using String interpolation on object properties")
    case class Donut(name: String, tasteLevel: String)
    val favoriteDonut2: Donut = Donut("Glazed Donut", "Very Tasty")
    println(s"My favorite donut name = ${favoriteDonut2.name}, tasteLevel = ${favoriteDonut2.tasteLevel}")

    println("\nStep 3: Using triple quotes \"\"\" to escape characters")
    val donutJson3: String = """{"donut_name":"Glazed Donut","taste_level":"Very Tasty","price":2.50}"""
    println(s"donutJson3 = $donutJson3")

    println("Step 1: A simple for loop from 1 to 5 inclusive")
    for(numberOfDonuts <- 1 to 5){
      println(s"Number of donuts to buy = $numberOfDonuts")
    }

//    println("\nStep 3: Filter values using if conditions in for loop")
//    val donutIngredients = List("flour", "sugar", "egg yolks", "syrup", "flavouring")
//    for(ingredient <- donutIngredients if ingredient == "sugar"){
//      println(s"Found sweetening ingredient = $ingredient")
//    }

//    println("\nStep 4: Filter values using if conditions in for loop and return the result back using the yield keyword")
//    val sweeteningIngredients = for {
//      ingredient <- donutIngredients
//      if (ingredient == "sugar" || ingredient == "syrup")
//    } yield ingredient
//    println(s"Sweetening ingredients = $sweeteningIngredients")

    val twoDimensionalArray = Array.ofDim[String](2,2)
    twoDimensionalArray(0)(0) = "flour"
    twoDimensionalArray(0)(1) = "sugar"
    twoDimensionalArray(1)(0) = "egg"
    twoDimensionalArray(1)(1) = "syrup"

    for { x <- 0 until 2
          y <- 0 until 2
    }yield println(s"Donut ingredient at index ${(x,y)} = ${twoDimensionalArray(x)(y)}")

    println("Step 1: Create a simple numeric range from 1 to 5 inclusive")
    val from1To5 = 1 to 5
    println(s"Range from 1 to 5 inclusive = $from1To5")

    println("\nStep 3: Create a numeric range from 0 to 10 but increment with multiples of 2")
    val from0To10By2 = 0 to 10 by 2
    println(s"Range from 0 to 10 with multiples of 2 = $from0To10By2")

    println("\nStep 6: Storing our ranges into collections")
    val listFrom1To5 = (1 to 5).toList
    println(s"Range to list = ${listFrom1To5.mkString(" ")}")
    println(s"""Total cost = ${calculateDonutCost("Glazed Donut", 5, None)}""")

    println("\nStep 3: Calling applyDiscount function with String or Double parameter types")
    applyDiscount("COUPON_1234")
    applyDiscount(10)

    println("\nStep 5: How to call a function which has typed parameters")
    applyDiscount[String]("COUPON_123")
    applyDiscount[Double](10)

    println(s"\nStep 6: How to call a function which has typed parameters and return type ${applyDiscountWithReturnType[Double](56)}")
  }

  def calculateDonutCost(donutName: String, quantity: Int, couponCode: Option[String]): Double = {
    println(s"Calculating cost for $donutName, quantity = $quantity")

    couponCode match {
      case Some(coupon) =>
        val discount = 0.1 // Let's simulate a 10% discount
      val totalCost = 2.50 * quantity * (1 - discount)
        totalCost

      case None => 2.50 * quantity
    }
  }

  def dailyCouponCode(): Option[String] = {
    // look up in database if we will provide our customers with a coupon today
    val couponFromDb = "COUPON_1234"
    Option(couponFromDb).filter(_.nonEmpty)
  }

  def applyDiscount(percentageDiscount: Double) {
    println(s"$percentageDiscount discount will be applied")
  }

  def applyDiscount(couponCode: String) {
    println(s"Lookup percentage discount in database for $couponCode")
  }

  def applyDiscount[T](discount: T) {
    discount match {
      case d: String =>
        println(s"Lookup percentage discount in database for $d")

      case d: Double =>
        println(s"$d discount will be applied")

      case _ =>
        println("Unsupported discount type")
    }
  }

  def applyDiscountWithReturnType[T](discount: T): Seq[T] = {
    discount match {
      case d: String =>
        println(s"Lookup percentage discount in database for $d")
        Seq[T](discount)

      case d: Double =>
        println(s"$d discount will be applied")
        Seq[T](discount)

      case d @ _ =>
        println("Unsupported discount type")
        Seq[T](discount)
    }
  }
}