package de.puschj.interpreter

import de.fosd.typechef.conditional.Conditional

sealed trait Value {
  def getIntValue(): Int
  def getBoolValue(): Boolean
}

case class IntValue(i: Int) extends Value {
  def getIntValue() = i
  
  def getBoolValue(): Boolean = {
    throw new IllegalCallException("called getBoolValue on IntValue")
  }
}

case class BoolValue(b: Boolean) extends Value {
  def getIntValue(): Int = {
    throw new IllegalCallException("called getIntValue on BoolValue")
  }
  
  def getBoolValue() = b
}

case class NullValue extends Value {
  def getIntValue(): Int = {
    throw new NullPointerException("called getIntValue on NullValue")
  }
  
  def getBoolValue(): Boolean = {
    throw new NullPointerException("called getIntValue on NullValue")
  }
  
  override def equals(any: Any) = {
    any.isInstanceOf[NullValue]
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
case class IllegalOPValue(override val s: String) extends ErrorValue(s)

sealed abstract case class ObjectValue[T] extends Value {
  def getFieldValue(fieldName: String): T
  
  def getIntValue(): Int = {
    throw new IllegalCallException("called getIntValue on Object")
  }
  
  def getBoolValue(): Boolean = {
    throw new IllegalCallException("called getBoolValue on Object")
  }
}

case class PlainObjectValue(className: String, vars: PlainStore) extends ObjectValue[Value] {

  def getFieldValue(fieldName: String) = {
    vars.get(fieldName)
  }
}

case class VAObjectValue(className: String, vars: VAStore) extends ObjectValue[Conditional[Value]] {
  
  def getFieldValue(fieldName: String) = {
    vars.get(fieldName)
  }
}