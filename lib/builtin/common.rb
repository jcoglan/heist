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
  closure = Scope.new(scope)
  closure.bind(values, scope)
  body[0...-1].each { |part| Heist.value_of(part, closure) }
  frame.push(body.last, closure)
end

metadef('let*') do |frame, scope, values, *body|
  closure = Scope.new(scope)
  closure.bind(values, closure)
  body[0...-1].each { |part| Heist.value_of(part, closure) }
  frame.push(body.last, closure)
end

metadef('begin') do |frame, scope, *body|
  body[0...-1].each { |part| Heist.value_of(part, scope) }
  frame.push(body.last, scope)
end

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

define('else') { true }

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

