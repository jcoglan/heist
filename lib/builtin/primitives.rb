# Functions that create new functions

# (define) binds values to names in the current scope.
# If the first parameter is a list it creates a function,
# otherwise it eval's the second parameter and binds it
# to the name given by the first.
syntax('define') do |scope, cells|
  name = cells.car
  Heist.list?(name) ?
      scope.define(name.car, name.cdr, cells.cdr) :
      scope[name] = Heist.evaluate(cells.cdr.car, scope)
end

# (lambda) returns an anonymous function whose arguments
# are named by the first parameter and whose body is given
# by the remaining parameters.
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

syntax('let-syntax') do |*args|
  call('let', *args)
end

syntax('letrec-syntax') do |*args|
  call('letrec', *args)
end

syntax('syntax-rules') do |scope, cells|
  Macro.new(scope, cells.car, cells.cdr)
end

#----------------------------------------------------------------

# Continuations

syntax('call-with-current-continuation') do |scope, callback|
  continuation = Continuation.new(scope.runtime.stack)
  callback = Heist.evaluate(callback, scope)
  callback.call(scope, [continuation])
end

#----------------------------------------------------------------

# Quoting functions

# (quote) casts identifiers to symbols. If given a list, it
# quotes all items in the list recursively.
syntax('quote') do |scope, cells|
  Heist.quote(cells.car)
end

# (quasiquote) is similar to (quote), except that when it
# encounters an (unquote) or (unquote-splicing) expression
# it will evaluate it and insert the result into the
# surrounding quoted list.
syntax('quasiquote') do |scope, cells|
  Heist.quasiquote(cells.car, scope)
end

#----------------------------------------------------------------

# Control structures

# (begin) simply executes a series of lists in the current scope.
syntax('begin') do |scope, cells|
  Body.new(cells, scope)
end

# (if) evaluates the consequent if the condition eval's to
# true, otherwise it evaluates the alternative
syntax('if') do |scope, cells|
  which = Heist.evaluate(cells.car, scope) ? cells.cdr.car : cells.cdr.cdr.car
  Frame.new(which, scope)
end

#----------------------------------------------------------------

# Runtime utilities

define('exit') { exit }

syntax('runtime') do |scope, cells|
  scope.runtime.elapsed_time
end

syntax('eval') do |scope, cells|
  scope.eval(Heist.evaluate(cells.car, scope))
end

define('display') do |expression|
  print expression
end

syntax('load') do |scope, cells|
  scope.load(cells.car)
end

define('error') do |message, *args|
  raise RuntimeError.new("#{ message } :: #{ args * ', ' }")
end

#----------------------------------------------------------------

# Comparators

# TODO write a more exact implementation, and implement (eq?)
define('eqv?') do |op1, op2|
  op1.equal?(op2)
end

define('equal?') do |op1, op2|
  op1 == op2
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
  call('real?', value) # || TODO
end

define('real?') do |value|
  call('rational?', value) || Float === value
end

define('rational?') do |value|
  call('integer?', value) || Float === value # TODO handle this properly
end

define('integer?') do |value|
  Integer === value
end

define('string?') do |value|
  String === value
end

define('symbol?') do |value|
  Symbol === value
end

define('procedure?') do |value|
  Function === value
end

define('null?') do |value|
  Cons::NULL == value
end

define('list?') do |value|
  Cons === value and value.list?
end

define('pair?') do |value|
  Cons === value and value.pair?
end

#----------------------------------------------------------------

# Numerical functions
# TODO implement exact?, inexact?, numerator, denominator, rationalize,
#                complex functions, exact->inexact and vice versa

# TODO implement max/min in Scheme

# Returns the maximum value in the list of arguments
define('max') do |*args|
  args.max
end

# Returns the minimum value in the list of arguments
define('min') do |*args|
  args.min
end

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
  op2.nil? ? 1.0 / op1 : op1 / op2.to_f
end

# (quotient) and (remainder) satisfy
# 
# (= n1 (+ (* n2 (quotient n1 n2))
#          (remainder n1 n2)))

# Returns the quotient of two numbers, i.e. performs n1/n2
# and rounds toward zero.
define('quotient') do |op1, op2|
  result = op1.to_i.to_f / op2.to_i
  result > 0 ? result.floor : result.ceil
end

# Returns the remainder after dividing the first operand
# by the second
define('remainder') do |op1, op2|
  op1.to_i - op2.to_i * call('quotient', op1, op2)
end

# Returns the first operand modulo the second
define('modulo') do |op1, op2|
  op1.to_i % op2.to_i
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

# Returns a random number in the range 0...max
define('random') do |max|
  rand(max)
end

define('number->string') do |number, radix|
  number.to_s(radix || 10)
end

define('string->number') do |string, radix|
  radix.nil? ? string.to_f : string.to_i(radix)
end

#----------------------------------------------------------------

# List/pair functions

# Allocates and returns a new pair from its arguments
define('cons') do |car, cdr|
  Cons.new(car, cdr)
end

# Allocates and returns a new list from its arguments
define('list') do |*values|
  Cons.construct(values)
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

# Returns the length of a proper list, and throws an
# exception for the length of improper lists
define('length') do |list|
  list.length
end

