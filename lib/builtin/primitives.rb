# Functions that create new functions

# (define) binds values to names in the current scope. If the
# first parameter is a list it creates a function, otherwise
# it eval's the second parameter and binds it to the name
# given by the first.
syntax('define') do |scope, cells|
  name = cells.car
  Cons === name ?
      scope.define(name.car, name.cdr, cells.cdr) :
      scope[name] = Heist.evaluate(cells.cdr.car, scope)
end

# (lambda) returns an anonymous function whose arguments are
# named by the first parameter and whose body is given by the
# remaining parameters.
syntax('lambda') do |scope, cells|
  Function.new(scope, cells.car, cells.cdr)
end

# (set!) reassigns the value of an existing bound variable,
# in the innermost scope responsible for binding it.
syntax('set!') do |scope, cells|
  scope.set!(cells.car, Heist.evaluate(cells.cdr.car, scope))
end

#----------------------------------------------------------------

# Macros

syntax('define-syntax') do |scope, cells|
  scope[cells.car] = Heist.evaluate(cells.cdr.car, scope)
end

syntax('syntax-rules') do |scope, cells|
  Macro.new(scope, cells.car, cells.cdr)
end

#----------------------------------------------------------------

# Continuations

# Creates a +Continuation+ encapsulating the current +Stack+
# state, and returns the result of calling the second parameter
# (which should evaluate to a +Function+) with the continuation
# as the argument.
syntax('call-with-current-continuation') do |scope, cells|
  continuation = Continuation.new(scope.runtime.stack)
  callback = Heist.evaluate(cells.car, scope)
  callback.call(scope, Cons.new(continuation))
end

#----------------------------------------------------------------

# Quoting functions

# (quote) treats its argument as a literal. Returns the given
# portion of the parse tree as a list
syntax('quote') do |scope, cells|
  node = cells.car
  node.freeze! if node.respond_to?(:freeze!)
  node
end

#----------------------------------------------------------------

# Control structures

# (if) evaluates the consequent if the condition eval's to
# true, otherwise it evaluates the alternative
syntax('if') do |scope, cells|
  which = Heist.evaluate(cells.car, scope) ? cells.cdr : cells.cdr.cdr
  which.null? ? which : Frame.new(which.car, scope)
end

#----------------------------------------------------------------

# Runtime utilities

# (exit) causes the host Ruby process to quit
define('exit') { exit }

# (runtime) returns the amount of time the host +Runtime+ has
# been alive, in microseconds. Not a standard function, but
# used in SICP.
syntax('runtime') do |scope, cells|
  scope.runtime.elapsed_time
end

# (eval) evaluates Scheme code and returns the result. The
# argument can be a string or a list containing a valid
# Scheme expression.
syntax('eval') do |scope, cells|
  scope.eval(Heist.evaluate(cells.car, scope))
end

# (display) prints the given value to the console
define('display') do |expression|
  print expression
end

# (load) loads a file containing Scheme code and executes its
# contents. The path can be relative to the current file, or
# it can be the name of a file in the Heist library.
syntax('load') do |scope, cells|
  scope.load(cells.car)
end

# (error) raises an error with the given message. Additional
# arguments are appended to the message.
define('error') do |message, *args|
  raise RuntimeError.new("#{ message } :: #{ args * ', ' }")
end

#----------------------------------------------------------------

# Comparators

# TODO write a more exact implementation, and implement (eq?)
define('eqv?') do |op1, op2|
  (Identifier === op1 and op1 == op2) or op1.equal?(op2)
end

define('equal?') do |op1, op2|
  op1 == op2
end

# Returns true iff the given number is exact i.e. an integer, a
# rational, or a complex made of integers
define('exact?') do |value|
  call('rational?', value) || (Complex === value &&
                               call('rational?', value.real) &&
                               call('rational?', value.imag))
end

# TODO raise an exception if they're not numeric
# Returns true iff all arguments are numerically equal
define('=') do |*args|
  args.all? { |arg| arg == args.first }
end

# Returns true iff the arguments are monotonically decreasing
define('>') do |*args|
  result = true
  args.inject { |former, latter| result = false unless former > latter }
  result
end

# Returns true iff the arguments are monotonically non-increasing
define('>=') do |*args|
  result = true
  args.inject { |former, latter| result = false unless former >= latter }
  result
end

# Returns true iff the arguments are monotonically increasing
define('<') do |*args|
  result = true
  args.inject { |former, latter| result = false unless former < latter }
  result
end

# Returns true iff the arguments are monotonically non-decreasing
define('<=') do |*args|
  result = true
  args.inject { |former, latter| result = false unless former <= latter }
  result
end

#----------------------------------------------------------------

# Type-checking predicates

define('complex?') do |value|
  Complex === value || call('real?', value)
end

define('real?') do |value|
  Float === value || call('rational?', value)
end

define('rational?') do |value|
  Rational === value || call('integer?', value)
end

define('integer?') do |value|
  Integer === value
end

define('string?') do |value|
  String === value
end

define('symbol?') do |value|
  Symbol === value or Identifier === value
end

define('procedure?') do |value|
  Function === value
end

define('pair?') do |value|
  Cons === value and value.pair?
end

#----------------------------------------------------------------

# Numerical functions
# TODO implement rationalize, exact->inexact and vice versa

# Returns the sum of all arguments passed
define('+') do |*args|
  args.any? { |arg| String === arg } ?
      args.inject("") { |str, arg| str + arg.to_s } :
      args.inject(0)  { |sum, arg| sum + arg }
end

# Subtracts the second argument from the first
define('-') do |op1, op2|
  op2.nil? ? 0 - op1 : op1 - op2
end

# Returns the product of all arguments passed
define('*') do |*args|
  args.inject(1) { |prod, arg| prod * arg }
end

# Returns the first argument divided by the second, or the
# reciprocal of the first if only one argument is given
define('/') do |op1, op2|
  op2.nil? ? Heist.divide(1, op1) : Heist.divide(op1, op2)
end

# Returns the numerator of a number
define('numerator') do |value|
  Rational === value ? value.numerator : value
end

# Returns the denominator of a number
define('denominator') do |value|
  Rational === value ? value.denominator : 1
end

%w[floor ceil truncate round].each do |symbol|
  define(symbol) do |number|
    number.__send__(symbol)
  end
end

%w[exp log sin cos tan asin acos sqrt].each do |symbol|
  define(symbol) do |number|
    Math.__send__(symbol, number)
  end
end

define('atan') do |op1, op2|
  op2.nil? ? Math.atan(op1) : Math.atan2(op1, op2)
end

# Returns the result of raising the first argument to the
# power of the second
define('expt') do |op1, op2|
  op1 ** op2
end

# Returns a new complex number with the given real and
# imaginary parts
define('make-rectangular') do |real, imag|
  Complex.new(real, imag)
end

# Returns the real part of a number
define('real-part') do |value|
  Complex === value ? value.real : value
end

# Returns the imaginary part of a number, which is zero
# unless the number is not real
define('imag-part') do |value|
  Complex === value ? value.imag : 0
end

# Returns a random number in the range 0...max
define('random') do |max|
  rand(max)
end

# Casts a number to a string
define('number->string') do |number, radix|
  number.to_s(radix || 10)

end
# Casts a string to a number
define('string->number') do |string, radix|
  radix.nil? ? string.to_f : string.to_i(radix)
end

#----------------------------------------------------------------

# List/pair functions

# Allocates and returns a new pair from its arguments
define('cons') do |car, cdr|
  Cons.new(car, cdr)
end

# car/cdr accessors (dynamically generated)
Cons::ACCESSORS.each do |accsr|
  define(accsr) do |cons|
    cons.__send__(accsr)
  end
end

# Mutators for car/cdr fields
define('set-car!') do |cons, value|
  cons.car = value
end
define('set-cdr!') do |cons, value|
  cons.cdr = value
end

#----------------------------------------------------------------

# Control features

# Calls a function using a list for the arguments
# TODO take multiple argument values instead of a single list
define('apply') do |function, list|
  function.apply(list.to_a)
end

