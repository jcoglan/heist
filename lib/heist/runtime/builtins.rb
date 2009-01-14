self["define"] = MetaFunction.new(self) do |scope, names, body|
  names = names.as_string
  scope[names.first] = (Array === names) ?
      Function.new(scope, names[1..-1], body) :
      body.eval(scope)
end

self["lambda"] = MetaFunction.new(self) do |scope, names, body|
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
  op1 / op2.to_f
end

self["begin"] = Function.new(self) { |*args| args.last }

self["exit"] = Function.new(self) { exit }

self["cond"] = MetaFunction.new(self) do |scope, *pairs|
  matched, result = false, nil
  pairs.each do |pair|
    next if matched
    matched = pair.cells.first.eval(scope)
    
    if matched ||
        (pair == pairs.last && pair.cells.first.as_string == "else")
      result  = pair.cells.last.eval(scope)
    end
  end
  result
end

self["="] = Function.new(self) do |op1, op2|
  op1 == op2
end

self[">"] = Function.new(self) do |op1, op2|
  op1 > op2
end

self["<"] = Function.new(self) do |op1, op2|
  op1 < op2
end

self["and"] = MetaFunction.new(self) do |scope, *args|
  result = true
  args.each do |arg|
    next if !result
    result = arg.eval(scope)
  end
  result
end

self["or"] = MetaFunction.new(self) do |scope, *args|
  result = false
  args.each do |arg|
    next if result
    result = arg.eval(scope)
  end
  result
end

self["not"] = Function.new(self) do |expr|
  !expr
end

