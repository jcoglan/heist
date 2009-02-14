metadef('let-syntax') do |*args|
  call('let', *args)
end

metadef('letrec-syntax') do |*args|
  call('letrec', *args)
end

#----------------------------------------------------------------

# Control structures

# (cond) acts like the 'switch' statement in C-style languages.
# Once a matching precondition is found, its consequent is
# tail-called and no further preconditions are evaluated.
metadef('cond') do |scope, *pairs|
  result = nil
  pairs.each do |list|
    next if result
    next unless Heist.value_of(list.first, scope)
    result = Body.new(list[1..-1], scope)
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
  closure = Scope.new(scope)
  assignments.each do |assign|
    closure[assign.first] = Heist.value_of(assign.last, scope)
  end
  call('begin', closure, *body)
end

# (let*) creates a new scope for each variable and evaluates
# each expression in its enclosing scope. Basically a shorthand
# for several nested (let)s. Variables may refer to those that
# preceed them but not vice versa.
metadef('let*') do |scope, assignments, *body|
  closure = assignments.inject(scope) do |outer, assign|
    inner = Scope.new(outer)
    inner[assign.first] = Heist.value_of(assign.last, outer)
    inner
  end
  call('begin', closure, *body)
end

# (letrec) evaluates values in the inner scope, so lambdas are
# able to refer to other values assigned using the (letrec).
metadef('letrec') do |scope, assignments, *body|
  closure = Scope.new(scope)
  assignments.each do |assign|
    closure[assign.first] = Heist.value_of(assign.last, closure)
  end
  call('begin', closure, *body)
end

