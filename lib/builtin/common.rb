# Functions that create other functions

# (define) binds values to names in the current scope.
# If the first parameter is a list it creates a function,
# otherwise it eval's the second parameter and binds it
# to the name given by the first.
metadef('define') do |frame, scope, names, *body|
  List === names ?
      scope.define(names.first, names.rest, body) :
      scope[names] = Heist.value_of(body.first, scope)
end

# (lambda) returns an anonymous function whose arguments
# are named by the first parameter and whose body is given
# by the remaining parameters.
metadef('lambda') do |frame, scope, names, *body|
  formals = names.map { |cell| cell.to_s }
  Function.new(scope, formals, body)
end

#----------------------------------------------------------------

# Functions that create new scopes

# (let) creates a new scope and binds values to names before
# executing a series of lists. Variable definitions cannot
# refer to each other: symbols used in definitions refer to
# the outer scope.
metadef('let') do |frame, scope, values, *body|
  closure = Scope.new(scope)
  closure.bind(values, scope)
  body[0...-1].each { |part| Heist.value_of(part, closure) }
  frame.push(body.last, closure)
end

# (let*) creates a new scope and binds values to names before
# executing a series of lists. Variable definitions can
# refer to prior definitions; each definition is executed
# in series within the newly created scope.
metadef('let*') do |frame, scope, values, *body|
  closure = Scope.new(scope)
  closure.bind(values, closure)
  body[0...-1].each { |part| Heist.value_of(part, closure) }
  frame.push(body.last, closure)
end

#----------------------------------------------------------------

# Control structures

# (begin) simply executes a series of lists in the current scope.
metadef('begin') do |frame, scope, *body|
  body[0...-1].each { |part| Heist.value_of(part, scope) }
  frame.push(body.last, scope)
end

# (cond) acts like the 'switch' statement in C-style languages.
# Once a matching precondition is found, its consequent is
# tail-called and no further preconditions are evaluated.
metadef('cond') do |frame, scope, *pairs|
  matched = false
  pairs.each do |list|
    next if matched
    matched = Heist.value_of(list.first, scope)
    next unless matched
    frame.push(list.last, scope)
  end
  nil
end

# 'else' should really only be used inside (cond) blocks.
define('else') { true }

#----------------------------------------------------------------

# Boolean combinators

# (and) evaluates its arguments until one of them returns
# false. Returns true iff all arguments eval to true.
metadef('and') do |frame, scope, *args|
  result = true
  args.each do |arg|
    next if !result
    result = arg.eval(scope)
  end
  result
end

# (or) evaluates its arguments until one of them returns
# true. Returns false iff all arguments eval to false.
metadef('or') do |frame, scope, *args|
  result = false
  args.each do |arg|
    next if result
    result = arg.eval(scope)
  end
  result
end

#----------------------------------------------------------------

# Runtime utilities

define('exit') { exit }

metadef('runtime') do |frame, scope|
  scope.runtime.elapsed_time
end

metadef('eval') do |frame, scope, string|
  scope.eval(Heist.value_of(string, scope))
end

define('display') do |expression|
  puts expression
end

metadef('load') do |frame, scope, file|
  scope.load(file)
end

#----------------------------------------------------------------

# Arithmetic and other math functions

define('+') do |*args|
  args.any? { |arg| String === arg } ?
      args.inject("") { |str, arg| str + arg.to_s } :
      args.inject(0)  { |sum, arg| sum + arg }
end

define('-') do |op1, op2|
  op2.nil? ? 0 - op1 : op1 - op2
end

define('*') do |*args|
  args.inject(1) { |prod, arg| prod * arg }
end

define('/') do |op1, op2|
  op2.nil? ? 1.0 / op1 : op1 / op2.to_f
end

define('expt') do |op1, op2|
  op1 ** op2
end

define('remainder') do |op1, op2|
  op1 % op2
end

define('random') do |max|
  rand(max)
end

define('max') do |*args|
  args.max
end

define('min') do |*args|
  args.min
end

#----------------------------------------------------------------

# Comparators

define('eqv?') do |op1, op2|
  op1.class == op2.class && op1 == op2
end

define('=') do |op1, op2|
  # TODO raise an exception if they're not numeric
  Numeric === op1 && Numeric === op2 && op1 == op2
end

define('>') do |op1, op2|
  op1 > op2
end

define('<') do |op1, op2|
  op1 < op2
end

#----------------------------------------------------------------

# Type-checking predicates

define('boolean?') do |value|
  [true, false].include?(value)
end

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

