metadef('define') do |frame, scope, names, *body|
  List === names ?
      scope.define(names.first, names.rest, body) :
      scope[names] = Heist.value_of(body.first, scope)
end

metadef('lambda') do |frame, scope, names, *body|
  formals = names.map { |cell| cell.to_s }
  Function.new(scope, formals, body)
end

metadef('let') do |frame, scope, values, *body|
  
end

metadef('let*') do |frame, scope, values, *body|
  
end

metadef('runtime') do |frame, scope|
  
end

metadef('eval') do |frame, scope, string|
  
end

define('display') do |expression|
  puts expression
end

metadef('load') do |frame, scope, file|
  
end

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

metadef('begin') do |frame, scope, *args|
  
end

define('exit') { exit }

# Lazy mode currently complains if this does not return a value
metadef('cond') do |frame, scope, *pairs|
  
end

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

metadef('and') do |frame, scope, *args|
  
end

metadef('or') do |frame, scope, *args|
  
end

define('boolean?') do |value|
  [true, false].include?(value)
end

define('complex?') do |value|
  self["real?"].call(self, value) # || TODO
end

define('real?') do |value|
  self["rational?"].call(self, value) || Float === value
end

define('rational?') do |value|
  self["integer?"].call(self, value) || Float === value # TODO handle this properly
end

define('integer?') do |value|
  Integer === value
end

