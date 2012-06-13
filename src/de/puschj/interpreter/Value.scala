package de.puschj.interpreter



sealed trait Value {
  def getIntValue(): Int
  def getBoolValue(): Boolean
}

case class IntValue(i: Int) extends Value {
  def getIntValue(): Int = {
    return i
  }
  
  def getBoolValue(): Boolean = {
    throw new IllegalCallException("called getBoolValue on IntValue")
  }
}

case class BoolValue(b: Boolean) extends Value {
  def getIntValue(): Int = {
    throw new IllegalCallException("called getIntValue on BoolValue")
  }
  
  def getBoolValue(): Boolean = {
    return b
  }
}

sealed case class ErrorValue(s: String) extends Value {
  private lazy val name = getClass().getCanonicalName();
  
  def getIntValue(): Int = {
    throw new IllegalCallException("called getIntValue on "+name+": "+s)
  }
  def getBoolValue(): Boolean = {
    throw new IllegalCallException("called getBoolValue on "+name+": "+s)
  }
}

case class UndefinedValue(override val s: String) extends ErrorValue(s)
case class NotANumberValue(override val s: String) extends ErrorValue(s)