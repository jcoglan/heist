# if -- cannot be written in Scheme using applicative order
self["if"] = MetaFunction.new(self) do |frame, scope, cond, cons, alt|
  which = cond.eval(scope) ? cons : alt
  if Scheme::List === which
    bindings = which.arguments.map { |arg| Binding.new(arg, scope) }
    frame.push(which.function(scope), scope, bindings)
  else
    frame.send(which.eval(scope))
  end
end

