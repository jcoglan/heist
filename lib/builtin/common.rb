self["define"] = MetaFunction.new(self) do |frame, scope, names, body|
  names = names.as_string
  scope[names.first] = (Array === names) ?
      Function.new(scope, names[1..-1], body) :
      body.eval(scope)
end

self["lambda"] = MetaFunction.new(self) do |frame, scope, names, body|
  Function.new(scope, names.as_string, body)
end

self["display"] = Function.new(self) do |expression|
  puts expression
end

self["load"] = Function.new(self) do |file|
  path = LOAD_PATH.find { |p| File.file?(p + file) || File.file?(p + file + FILE_EXT) }
  if path.nil?
    false
  else
    Heist.run(path + file, self)
    true
  end
end

self["+"] = Function.new(self) do |*args|
  args.any? { |arg| String === arg } ?
      args.inject("") { |str, arg| str + arg.to_s } :
      args.inject(0)  { |sum, arg| sum + arg }
end

self["-"] = Function.new(self) do |op1, op2|
  op2.nil? ? 0 - op1 : op1 - op2
end

self["*"] = Function.new(self) do |*args|
  args.inject(1) { |prod, arg| prod * arg }
end

self["/"] = Function.new(self) do |op1, op2|
  op2.nil? ? 1.0 / op1 : op1 / op2.to_f
end

self["expt"] = Function.new(self) do |op1, op2|
  op1 ** op2
end

self["max"] = Function.new(self) do |*args|
  args.max
end

self["min"] = Function.new(self) do |*args|
  args.min
end

self["min"] = Function.new(self) do |*args|
  args.min
end

self["begin"] = MetaFunction.new(self) do |frame, scope, *args|
  args[0...-1].each { |arg| arg.eval(scope) }
  frame.push(args.last, scope)
end

self["exit"] = Function.new(self) { exit }

# Lazy mode currently complains if this does not return a value
self["cond"] = MetaFunction.new(self) do |frame, scope, *pairs|
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

self["eqv?"] = Function.new(self) do |op1, op2|
  op1.class == op2.class && op1 == op2
end

self["="] = Function.new(self) do |op1, op2|
  # TODO raise an exception if they're not numeric
  Numeric === op1 && Numeric === op2 && op1 == op2
end

self[">"] = Function.new(self) do |op1, op2|
  op1 > op2
end

self["<"] = Function.new(self) do |op1, op2|
  op1 < op2
end

self["and"] = MetaFunction.new(self) do |frame, scope, *args|
  result = true
  args.each do |arg|
    next if !result
    result = arg.eval(scope)
  end
  result
end

self["or"] = MetaFunction.new(self) do |frame, scope, *args|
  result = false
  args.each do |arg|
    next if result
    result = arg.eval(scope)
  end
  result
end

self["boolean?"] = Function.new(self) do |value|
  [true, false].include?(value)
end

self["complex?"] = Function.new(self) do |value|
  self["real?"].call(self, value) # || TODO
end

self["real?"] = Function.new(self) do |value|
  self["rational?"].call(self, value) || Float === value
end

self["rational?"] = Function.new(self) do |value|
  self["integer?"].call(self, value) || Float === value # TODO handle this properly
end

self["integer?"] = Function.new(self) do |value|
  Integer === value
end

