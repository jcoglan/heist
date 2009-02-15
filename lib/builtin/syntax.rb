# Control structures

# (cond) acts like the 'switch' statement in C-style languages.
# Once a matching precondition is found, its consequent is
# tail-called and no further preconditions are evaluated.
metadef('cond') do |scope, *pairs|
  result = nil
  pairs.each do |list|
    next if result
    test = Heist.evaluate(list.first, scope)
    next unless test
    result = list[1].to_s == '=>' ?
             Heist.evaluate(list[2], scope).call(scope, [test]) :
             Body.new(list[1..-1], scope)
  end
  result
end

# 'else' should really only be used inside (cond) blocks.
define('else') { true }

#----------------------------------------------------------------

# Binding constructs

# (let), (let*) and (letrec) each create a new scope and bind
# values to some symbols before executing a series of lists.
# They differ according to how they evaluate the bound values.

# (let) evaluates values in the enclosing scope, so lambdas will
# not be able to refer to other values assigned using the (let).
metadef('let') do |scope, assignments, *body|
  if Identifier === assignments
    name = assignments
    assignments = body.first
    formals = assignments.map { |pair| pair.first }
    values  = assignments.map { |pair| pair.last }
    closure = Scope.new(scope)
    closure[name] = Function.new(closure, formals, body[1..-1])
    closure[name].call(scope, values)
  else
    closure = Scope.new(scope)
    assignments.each do |assign|
      closure[assign.first] = Heist.evaluate(assign.last, scope)
    end
    call('begin', closure, *body)
  end
end

# (let*) creates a new scope for each variable and evaluates
# each expression in its enclosing scope. Basically a shorthand
# for several nested (let)s. Variables may refer to those that
# preceed them but not vice versa.
metadef('let*') do |scope, assignments, *body|
  closure = assignments.inject(scope) do |outer, assign|
    inner = Scope.new(outer)
    inner[assign.first] = Heist.evaluate(assign.last, outer)
    inner
  end
  call('begin', closure, *body)
end

# (letrec) evaluates values in the inner scope, so lambdas are
# able to refer to other values assigned using the (letrec).
metadef('letrec') do |scope, assignments, *body|
  closure = Scope.new(scope)
  assignments.each do |assign|
    closure[assign.first] = Heist.evaluate(assign.last, closure)
  end
  call('begin', closure, *body)
end

metadef('let-syntax') do |*args|
  call('let', *args)
end

metadef('letrec-syntax') do |*args|
  call('letrec', *args)
end

#----------------------------------------------------------------

# Iteration

# (do) is similar to the 'while' construct in procedural
# languages. It assigns initial values to a set of variables,
# then performs the list of given commands in a loop. If
# before any iteration the test is found to be false, the
# loop is halted and the value of the expression following
# the test is returned.
metadef('do') do |scope, assignments, test, *commands|
  closure = Scope.new(scope)
  assignments.each do |assign|
    closure[assign.first] = Heist.evaluate(assign[1], scope)
  end
  while not Heist.evaluate(test.first, closure)
    commands.each { |expr| Heist.evaluate(expr, closure) }
    assignments.each do |assign|
      step = assign[2] || assign[0]
      closure[assign.first] = Heist.evaluate(step, closure)
    end
  end
  call('begin', closure, *test[1..-1])
end

