# Control structures

# (cond) goes through a list of tests, evaluating each one
# in order of appearance. Once a matching precondition is
# found, its consequent is tail-called and no further
# preconditions are evaluated.
syntax('cond') do |scope, *pairs|
  result = nil
  pairs.each do |list|
    next if result
    test = list.first.to_s == 'else' || Heist.evaluate(list.first, scope)
    next unless test
    result = list[1].to_s == '=>' ?
             Heist.evaluate(list[2], scope).call(scope, [test]) :
             Body.new(list.rest, scope)
  end
  result
end

# (case) acts like Ruby's case statement. The value of the
# given expression is compared against a series of lists;
# once a list is found to include the value, the expressions
# following the list are evaluated and no further lists
# are tested.
syntax('case') do |scope, key, *clauses|
  value = Heist.evaluate(key, scope)
  result = nil
  clauses.each do |list|
    next if result
    values = call('quote', scope, list.first)
    result = Body.new(list.rest, scope) if values == :else or
                                           values.include?(value)
  end
  result
end

#----------------------------------------------------------------

# Binding constructs

# (let), (let*) and (letrec) each create a new scope and bind
# values to some symbols before executing a series of lists.
# They differ according to how they evaluate the bound values.

# (let) evaluates values in the enclosing scope, so lambdas will
# not be able to refer to other values assigned using the (let).
syntax('let') do |scope, assignments, *body|
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
syntax('let*') do |scope, assignments, *body|
  closure = assignments.inject(scope) do |outer, assign|
    inner = Scope.new(outer)
    inner[assign.first] = Heist.evaluate(assign.last, outer)
    inner
  end
  call('begin', closure, *body)
end

# (letrec) evaluates values in the inner scope, so lambdas are
# able to refer to other values assigned using the (letrec).
syntax('letrec') do |scope, assignments, *body|
  closure = Scope.new(scope)
  assignments.each do |assign|
    closure[assign.first] = Heist.evaluate(assign.last, closure)
  end
  call('begin', closure, *body)
end

syntax('let-syntax') do |*args|
  call('let', *args)
end

syntax('letrec-syntax') do |*args|
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
syntax('do') do |scope, assignments, test, *commands|
  closure = Scope.new(scope)
  assignments.each do |assign|
    closure[assign.first] = Heist.evaluate(assign[1], scope)
  end
  while not Heist.evaluate(test.first, closure)
    commands.each { |expr| Heist.evaluate(expr, closure) }
    temp = {}
    assignments.each do |assign|
      step = assign[2] || assign[0]
      temp[assign.first] = Heist.evaluate(step, closure)
    end
    assignments.each do |assign|
      closure[assign.first] = temp[assign.first]
    end
  end
  call('begin', closure, *test.rest)
end

#----------------------------------------------------------------

# Boolean combinators

# (and) returns the first falsey value returned by the list
# of expressions, or returns the value of the last expression
# if all values are truthy.
syntax('and') do |scope, *args|
  result = true
  args.each do |arg|
    next if not result
    result = Heist.evaluate(arg, scope)
  end
  result
end

# (or) returns the first truthy value returned by the list
# of expressions, or returns the value of the last expression
# if all values are falsey.
syntax('or') do |scope, *args|
  result = false
  args.each do |arg|
    next if result
    result = Heist.evaluate(arg, scope)
  end
  result
end

#----------------------------------------------------------------

# Delayed evaluation

# (delay) allows the evaluation of an expression to be delayed
# by wrapping it in a promise. Use (force) to evaluate the promise
# at a later time. The expression inside a promise is only
# ever evaluated once, so a promise can be implemented as a
# memoized closure.
syntax('delay') do |scope, expression|
  promise = Binding.new(expression, scope)
  Function.new(scope) { promise.extract }
end

