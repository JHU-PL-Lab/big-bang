# This example cover Object Oriented principles of BigBang such as objects,
# object extensions and constructors.

# Raw object.
let point = object
  public
  ref x
  ref y
end

# Other raw object.
let color = object
  public
  ref red
  ref green
  ref blue
end

# Object that composes both raw objects.
let coloredPoint = object
  include
    point
    color
end

# Same thing with classes.
class Point
  public
  ref x
  ref y

  fun constructor(x, y) =
    self.x = x
    self.y = y
  end
end

class Color
  public
  ref red
  ref blue
  ref green

  fun constructor(red, blue, green) =
    self.red   = red
    self.blue  = blue
    self.green = green
  end
end

class ColoredPoint
  public
  include Point
  include Color
end

let pointFromClass = Point:new(x = 2, y = 3)
pointFromClass:x # => 2
pointFromClass:y # => 3
pointFromClass:x = 4 # Updates field x to 4.
