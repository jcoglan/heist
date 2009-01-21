metadef('define') do |frame, scope, names, *body|
  names = names.as_string
  scope[names.first] = (Array === names) ?
      Function.new(scope, names[1..-1], body) :
      body.first.eval(scope)
end

metadef('lambda') do |frame, scope, names, *body|
  Function.new(scope, names.as_string, body)
end

metadef('let') do |frame, scope, values, *body|
  closure = Scope.new(scope)
  closure.bind(values.cells, scope)
  body[0...-1].each { |part| part.eval(closure) }
  frame.push(body.last, closure)
end

metadef('let*') do |frame, scope, values, *body|
  closure = Scope.new(scope)
  closure.bind(values.cells, closure)
  body[0...-1].each { |part| part.eval(closure) }
  frame.push(body.last, closure)
end

metadef('runtime') do |frame, scope|
  scope.runtime.elapsed_time
end

metadef('eval') do |frame, scope, string|
  scope.eval(string.eval(scope))
end

define('display') do |expression|
  puts expression
end

metadef('load') do |frame, scope, file|
  scope.load(file.eval(scope))
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
  args[0...-1].each { |arg| arg.eval(scope) }
  frame.push(args.last, scope)
end

define('exit') { exit }

# Lazy mode currently complains if this does not return a value
metadef('cond') do |frame, scope, *pairs|
  matched, result = false, nil
  pairs.each do |pair|
    next if matched
    matched = pair.cells.first.eval(scope)
    
    if matched ||
        (pair == pairs.last && pair.cells.first.as_string == "else")
      which  = pair.cells.last
      frame.push(which, scope)
      result = which.eval(scope) unless Scheme::List === which
    end
  end
  result
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
  result = true
  args.each do |arg|
    next if !result
    result = arg.eval(scope)
  end
  result
end

metadef('or') do |frame, scope, *args|
  result = false
  args.each do |arg|
    next if result
    result = arg.eval(scope)
  end
  result
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

